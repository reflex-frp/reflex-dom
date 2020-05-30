{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

import Reflex.Host.Headless
import Reflex.Dom
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Text
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Language.Javascript.JSaddle
import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Const
import Data.Default
import Control.Concurrent.Async
import Control.Concurrent
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Functor

newtype Sequence m = Sequence { unSequence :: m () }

instance Applicative m => Semigroup (Sequence m) where
  Sequence a <> Sequence b = Sequence $ a *> b

instance Applicative m => Monoid (Sequence m) where
  mempty = Sequence $ pure ()

--TODO: This builds up a rather large action in its event; it would be better if we could inline this action more thoroughly into the surrounding program, and only send a piece of data back with the event
newtype LazyBuilder build t m a = LazyBuilder { unLazyBuilder :: ReaderT (Document, IORef Node) (WriterT (Sequence build, Event t (Sequence build)) m) a }
  deriving (Functor, Applicative, Monad)

--dyn' :: Dynamic t (m a) -> m (Dynamic t b)
--dyn' d = Builder (fst <$> sample (current d), leftmost [ undefined <$> updated d, switch (snd <$> current d) ], trd =<< d)

data LazyDomSpace

instance Default a => Default (Const a b) where
  def = Const def

instance DomSpace LazyDomSpace where
  type EventSpec LazyDomSpace = Const ()
  type RawTextNode LazyDomSpace = ()

instance (Reflex t, Monad m, Applicative build) => NotReady t (LazyBuilder build t m) where
  notReadyUntil = undefined
  notReady = undefined

instance (Reflex t, Monad m, Applicative build) => Adjustable t (LazyBuilder build t m)

instance (Reflex t, build ~ JSM, MonadIO m) => DomBuilder t (LazyBuilder build t m) where
  type DomBuilderSpace (LazyBuilder build t m) = LazyDomSpace
  textNode cfg = LazyBuilder $ do
    (doc, nodeRef) <- ask
    thisRef :: IORef Text <- liftIO $ newIORef $ error "textNode: thisRef not yet initialized"
    let create = Sequence $ do
          node <- liftIO $ readIORef nodeRef
          this <- createTextNode doc $ _textNodeConfig_initialContents cfg
          appendChild node this
          pure ()
        update = fromMaybe never (_textNodeConfig_setContents cfg) <&> \t -> Sequence $ do
          this <- liftIO $ readIORef thisRef
          setNodeValue this $ Just t
          pure ()
    tell (create, update)
    pure $ TextNode ()
  commentNode = undefined
  element = undefined
  inputElement = undefined
  textAreaElement = undefined
  selectElement = undefined
  placeRawElement = undefined
  wrapRawElement = undefined

main :: IO ()
main = do
  toRun :: Chan (JSM ()) <- newChan
  envVar <- newEmptyMVar
  let jsmRunner = run $ do
        globalDoc <- currentDocumentUnchecked
        bodyElement <- getBodyUnchecked globalDoc
        bodyElementRef <- liftIO $ newIORef $ toNode bodyElement
        liftIO $ putMVar envVar (globalDoc, bodyElementRef)
        forever $ join $ liftIO $ readChan toRun
  withAsync jsmRunner $ \_ -> do
    env <- takeMVar envVar
    runHeadlessApp $ do
      ((), (a0, a')) <- runWriterT $ runReaderT (unLazyBuilder testWidget) env
      liftIO $ writeChan toRun $ unSequence a0
      performEvent_ $ liftIO . writeChan toRun . unSequence <$> a'
      pure never

testWidget :: DomBuilder t m => m ()
testWidget = do
  text "Qwer"
