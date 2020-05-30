{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Language.Javascript.JSaddle (JSM)
import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Const
import Data.Default
import Control.Concurrent.Async
import Control.Concurrent
import Data.Maybe (fromMaybe, catMaybes)
import Data.Functor
import qualified Reflex.Spider.Internal as Spider
import Data.IORef
import qualified Control.Exception
import Data.Dependent.Sum
import Reflex.Host.Class
import Control.Monad.Ref
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Functor.Identity

newtype Sequence m = Sequence { unSequence :: m () }

instance Applicative m => Semigroup (Sequence m) where
  Sequence a <> Sequence b = Sequence $ a *> b

instance Applicative m => Monoid (Sequence m) where
  mempty = Sequence $ pure ()

newtype UniqueM t a = UniqueM { unUniqueM :: PushM t a }

deriving instance Reflex t => Functor (UniqueM t)
deriving instance Reflex t => Applicative (UniqueM t)
deriving instance Reflex t => Monad (UniqueM t)
deriving instance Reflex t => MonadFix (UniqueM t)
deriving instance Reflex t => MonadSample t (UniqueM t)
deriving instance Reflex t => MonadHold t (UniqueM t)
instance Spider.HasSpiderTimeline x => MonadUnique (SpiderTimeline x) (UniqueM (SpiderTimeline x)) where
  uniqueOccurrences e = UniqueM $ do
    let e' = pushAlways unUniqueM e
    Spider.SpiderPushM $ Spider.EventM $ Control.Exception.evaluate e'
    --TODO: Write a test that if we run uniqueOccurrences twice, then fill one of the IORefs, the other IORef doesn't get filled
  uniqueIORef = UniqueM . Spider.SpiderPushM . Spider.EventM . newIORef

instance Spider.HasSpiderTimeline x => MonadUnique (SpiderTimeline x) (Spider.SpiderHostFrame x) where
  uniqueOccurrences e = Spider.SpiderHostFrame $ do
    let e' = pushAlways unUniqueM e
    Spider.EventM $ Control.Exception.evaluate e'
    --TODO: Write a test that if we run uniqueOccurrences twice, then fill one of the IORefs, the other IORef doesn't get filled
  uniqueIORef = Spider.SpiderHostFrame . Spider.EventM . newIORef

class MonadHold t m => MonadUnique t m where
  uniqueOccurrences :: Event t (UniqueM t a) -> m (Event t a)
  uniqueIORef :: a -> m (IORef a)

--TODO: This builds up a rather large action in its event; it would be better if we could inline this action more thoroughly into the surrounding program, and only send a piece of data back with the event
newtype LazyBuilder build t a = LazyBuilder { unLazyBuilder :: ReaderT (Document, IORef Node) (WriterT (Sequence build, Event t (Sequence build)) (UniqueM t)) a }
  deriving (Functor, Applicative, Monad)

{-
dyn' :: Dynamic t (m a) -> m (Dynamic t b)
dyn' child = do
  env@(doc, parent) <- ask
  let processedChild = runWriter . flip runReaderT env <$> child
  tell $ Sequence $ do
    (_, Sequence buildChild0) <- sample $ current processedChild
    ((), updateChild0) <- runEventWriterT buildChild0
    updateChild <- switchHoldPromptOnly updateChild0
    let newChild = updated processedChild
    tellEvent $ leftmost [updated processedChild]
  pure $ fst =<< processedChild
--dyn' d = Builder (fst <$> sample (current d), leftmost [ undefined <$> updated d, switch (snd <$> current d) ], trd =<< d)
-}

data LazyDomSpace

instance Default a => Default (Const a b) where
  def = Const def

instance DomSpace LazyDomSpace where
  type EventSpec LazyDomSpace = Const ()
  type RawTextNode LazyDomSpace = ()

instance (Reflex t, Monad build) => NotReady t (LazyBuilder build t) where
  notReadyUntil = undefined
  notReady = undefined

instance (Reflex t, Monad build) => Adjustable t (LazyBuilder build t)

instance (Reflex t, t ~ SpiderTimeline x, Spider.HasSpiderTimeline x, build ~ JSM) => DomBuilder t (LazyBuilder build t) where
  type DomBuilderSpace (LazyBuilder build t) = LazyDomSpace
  textNode cfg = LazyBuilder $ do
    (doc, parentRef) <- ask
    thisRef <- lift $ lift $ uniqueIORef $ error "textNode: not initialized"
    let create = Sequence $ do
          this <- createTextNode doc $ _textNodeConfig_initialContents cfg
          liftIO $ writeIORef thisRef this
          parent <- liftIO $ readIORef parentRef
          appendChild parent this
          pure ()
        update = fromMaybe never (_textNodeConfig_setContents cfg) <&> \t -> Sequence $ do
          this <- liftIO $ readIORef thisRef
          setNodeValue this $ Just t
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
      doc <- currentDocumentUnchecked
      body <- getBodyUnchecked doc
      bodyRef <- liftIO $ newIORef $ toNode body
      pure (doc, bodyRef)
    runHeadlessApp' $ do
      ((), (Sequence a0, a')) <- lift $ lift $ PerformEventT $ lift $ Spider.SpiderHostFrame $ (\(Spider.SpiderPushM x) -> x) $ unUniqueM $ runWriterT $ runReaderT (unLazyBuilder testWidget) env
      runJSM a0
      performEvent_ $ liftIO . runJSM . unSequence <$> a'
      pure never

testWidget :: DomBuilder t m => m ()
testWidget = do
  text "Qwer"

runHeadlessApp'
  :: (forall x. Spider.HasSpiderTimeline x => TriggerEventT
                                      (SpiderTimeline x)
                                      (PostBuildT
                                         (SpiderTimeline x)
                                         (PerformEventT (SpiderTimeline x) (SpiderHost x))) (Event (SpiderTimeline x) ()))
  -- ^ The action to be run in the headless FRP network. The FRP network is
  -- closed at the first occurrence of the resulting 'Event'.
  -> IO ()
runHeadlessApp' guest =
  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline.
  withSpiderTimeline $ runSpiderHostForTimeline $ do
    -- Create the "post-build" event and associated trigger. This event fires
    -- once, when the application starts.
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    -- Create a queue to which we will write 'Event's that need to be
    -- processed.
    events <- liftIO newChan
    -- Run the "guest" application, providing the appropriate context. We'll
    -- pure the result of the action, and a 'FireCommand' that will be used to
    -- trigger events.
    (result, fc@(FireCommand fire)) <- do
      hostPerformEventT $                 -- Allows the guest app to run
                                          -- 'performEvent', so that actions
                                          -- (e.g., IO actions) can be run when
                                          -- 'Event's fire.

        flip runPostBuildT postBuild $    -- Allows the guest app to access to
                                          -- a "post-build" 'Event'

          flip runTriggerEventT events $  -- Allows the guest app to create new
                                          -- events and triggers and write
                                          -- those triggers to a channel from
                                          -- which they will be read and
                                          -- processed.
            guest

    -- Read the trigger reference for the post-build event. This will be
    -- 'Nothing' if the guest application hasn't subscribed to this event.
    mPostBuildTrigger <- readRef postBuildTriggerRef

    -- When there is a subscriber to the post-build event, fire the event.
    for_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ pure ()

    -- Subscribe to an 'Event' of that the guest application can use to
    -- request application shutdown. We'll check whether this 'Event' is firing
    -- to determine whether to terminate.
    shutdown <- subscribeEvent result

    -- The main application loop. We wait for new events and fire those that
    -- have subscribers. If we detect a shutdown request, the application
    -- terminates.
    fix $ \loop -> do
      -- Read the next event (blocking).
      ers <- liftIO $ readChan events
      stop <- do
        -- Fire events that have subscribers.
        fireEventTriggerRefs fc ers $
          -- Check if the shutdown 'Event' is firing.
          readEvent shutdown >>= \case
            Nothing -> pure False
            Just _ -> pure True
      if or stop
        then pure ()
        else loop
  where
    -- Use the given 'FireCommand' to fire events that have subscribers
    -- and call the callback for the 'TriggerInvocation' of each.
    fireEventTriggerRefs
      :: MonadIO m
      => FireCommand t m
      -> [DSum (EventTriggerRef t) TriggerInvocation]
      -> ReadPhase m a
      -> m [a]
    fireEventTriggerRefs (FireCommand fire) ers rcb = do
      mes <- liftIO $
        for ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          pure $ fmap (==> a) me
      a <- fire (catMaybes mes) rcb
      liftIO $ for_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      pure a
