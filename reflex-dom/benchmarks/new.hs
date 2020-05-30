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
import Data.Maybe (fromMaybe)
import Data.Functor

newtype Sequence m = Sequence { unSequence :: m () }

instance Applicative m => Semigroup (Sequence m) where
  Sequence a <> Sequence b = Sequence $ a *> b

instance Applicative m => Monoid (Sequence m) where
  mempty = Sequence $ pure ()

--TODO: This builds up a rather large action in its event; it would be better if we could inline this action more thoroughly into the surrounding program, and only send a piece of data back with the event
newtype LazyBuilder build t m a = LazyBuilder { unLazyBuilder :: ReaderT (Document, Node) (WriterT (Sequence (EventWriterT t (Sequence build) build)) m) a }
  deriving (Functor, Applicative, Monad)

--dyn' :: Dynamic t (m a) -> m (Dynamic t b)
--dyn' d = Builder (fst <$> sample (current d), leftmost [ undefined <$> updated d, switch (snd <$> current d) ], trd =<< d)

data LazyDomSpace

instance Default a => Default (Const a b) where
  def = Const def

instance DomSpace LazyDomSpace where
  type EventSpec LazyDomSpace = Const ()
  type RawTextNode LazyDomSpace = ()

instance (Reflex t, Monad m, Monad build) => NotReady t (LazyBuilder build t m) where
  notReadyUntil = undefined
  notReady = undefined

instance (Reflex t, Monad m, Monad build) => Adjustable t (LazyBuilder build t m)

instance (Reflex t, build ~ JSM, MonadIO m) => DomBuilder t (LazyBuilder build t m) where
  type DomBuilderSpace (LazyBuilder build t m) = LazyDomSpace
  textNode cfg = LazyBuilder $ do
    (doc, parent) <- ask
    let create = Sequence $ do
          this <- createTextNode doc $ _textNodeConfig_initialContents cfg
          appendChild parent this
          tellEvent $ fromMaybe never (_textNodeConfig_setContents cfg) <&> \t -> Sequence $ do
            setNodeValue this $ Just t
          pure ()
    tell create
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
  let jsmRunner = run $ forever $ join $ liftIO $ readChan toRun
  let runJSM :: MonadIO m => JSM a -> m a
      runJSM a = liftIO $ do
        resultVar <- newEmptyMVar
        writeChan toRun $ do
          result <- a
          liftIO $ putMVar resultVar result
        takeMVar resultVar
  withAsync jsmRunner $ \_ -> do
    env <- runJSM $ do
      globalDoc <- currentDocumentUnchecked
      bodyElement <- getBodyUnchecked globalDoc
      pure (globalDoc, toNode bodyElement)
    runHeadlessApp $ do
      ((), Sequence a0) <- runWriterT $ runReaderT (unLazyBuilder testWidget) env
      ((), a') <- runJSM $ runEventWriterT a0
      performEvent_ $ liftIO . writeChan toRun . unSequence <$> a'
      pure never

testWidget :: DomBuilder t m => m ()
testWidget = do
  text "Qwer"
