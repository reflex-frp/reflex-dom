{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fspecialise-aggressively #-}
module Reflex.Dom.Main where

import Prelude hiding (concat, mapM, mapM_, sequence, sequence_)

import Reflex.Adjustable.Class
import Reflex.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PostBuild.Base
import Reflex.Spider (Global, Spider, SpiderHost, runSpiderHost)
import Reflex.TriggerEvent.Base
import Reflex.TriggerEvent.Class
#ifdef PROFILE_REFLEX
import Reflex.Profiled
#endif

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Reader hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Control.Monad.Ref
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum (..))
import Data.Foldable (for_)
import Data.IORef
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Types (JSM)
import qualified GHCJS.DOM.Types as DOM

#ifdef PROFILE_REFLEX
import Reflex.Profiled
#endif

{-# INLINE mainHydrationWidgetWithHead #-}
mainHydrationWidgetWithHead :: (forall x. HydrationWidget x ()) -> (forall x. HydrationWidget x ()) -> JSM ()
mainHydrationWidgetWithHead = mainHydrationWidgetWithHead'

{-# INLINABLE mainHydrationWidgetWithHead' #-}
-- | Warning: `mainHydrationWidgetWithHead'` is provided only as performance tweak. It is expected to disappear in future releases.
mainHydrationWidgetWithHead' :: HydrationWidget () () -> HydrationWidget () () -> JSM ()
mainHydrationWidgetWithHead' = mainHydrationWidgetWithSwitchoverAction' (pure ())

{-# INLINE mainHydrationWidgetWithSwitchoverAction #-}
mainHydrationWidgetWithSwitchoverAction :: JSM () -> (forall x. HydrationWidget x ()) -> (forall x. HydrationWidget x ()) -> JSM ()
mainHydrationWidgetWithSwitchoverAction = mainHydrationWidgetWithSwitchoverAction'

{-# INLINABLE mainHydrationWidgetWithSwitchoverAction' #-}
-- | Warning: `mainHydrationWidgetWithSwitchoverAction'` is provided only as performance tweak. It is expected to disappear in future releases.
mainHydrationWidgetWithSwitchoverAction' :: JSM () -> HydrationWidget () () -> HydrationWidget () () -> JSM ()
mainHydrationWidgetWithSwitchoverAction' = mainHydrationWidgetWithSwitchoverActionWithFailure' (pure ())


{-# INLINABLE mainHydrationWidgetWithSwitchoverActionWithFailure' #-}
-- | Warning: `mainHydrationWidgetWithSwitchoverActionWithFaiilure'` is provided only as performance tweak. It is expected to disappear in future releases.
mainHydrationWidgetWithSwitchoverActionWithFailure' :: IO () -> JSM () -> HydrationWidget () () -> HydrationWidget () () -> JSM ()
mainHydrationWidgetWithSwitchoverActionWithFailure' onFailure switchoverAction head' body = do
  runHydrationWidgetWithHeadAndBodyWithFailure onFailure switchoverAction $ \appendHead appendBody -> do
    appendHead head'
    appendBody body

{-# INLINABLE attachHydrationWidget #-}
attachHydrationWidget
  :: JSM ()
  -> JSContextSingleton ()
  -> ( Event DomTimeline ()
    -> IORef HydrationMode
    -> Maybe (IORef [(Node, HydrationRunnerT DomTimeline (DomCoreWidget ()) ())])
    -> EventChannel
    -> PerformEventT DomTimeline DomHost (a, IORef (Maybe (EventTrigger DomTimeline ())))
     )
  -> IO (a, FireCommand DomTimeline DomHost)
attachHydrationWidget = attachHydrationWidgetWithFailure (pure ())

{-# INLINABLE attachHydrationWidgetWithFailure #-}
attachHydrationWidgetWithFailure
  :: IO ()
  -> JSM ()
  -> JSContextSingleton ()
  -> ( Event DomTimeline ()
    -> IORef HydrationMode
    -> Maybe (IORef [(Node, HydrationRunnerT DomTimeline (DomCoreWidget ()) ())])
    -> EventChannel
    -> PerformEventT DomTimeline DomHost (a, IORef (Maybe (EventTrigger DomTimeline ())))
     )
  -> IO (a, FireCommand DomTimeline DomHost)
attachHydrationWidgetWithFailure onFailure switchoverAction jsSing w = do
  hydrationMode <- liftIO $ newIORef HydrationMode_Hydrating
  rootNodesRef <- liftIO $ newIORef []
  events <- newChan
  runDomHost $ flip runTriggerEventT events $ mdo
    (syncEvent, fireSync) <- newTriggerEvent
    ((result, postBuildTriggerRef), fc@(FireCommand fire)) <- lift $ hostPerformEventT $ do
      a <- w syncEvent hydrationMode (Just rootNodesRef) events
      _ <- runWithReplace (return ()) $ delayedAction <$ syncEvent
      pure a
    mPostBuildTrigger <- readRef postBuildTriggerRef
    lift $ forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
    liftIO $ fireSync ()
    rootNodes <- liftIO $ readIORef rootNodesRef
    let delayedAction = do
          for_ (reverse rootNodes) $ \(rootNode, runner) -> do
            let hydrate = runHydrationRunnerTWithFailure runner onFailure Nothing rootNode events
            void $ runWithJSContextSingleton (runPostBuildT hydrate never) jsSing
          liftIO $ writeIORef hydrationMode HydrationMode_Immediate
          runWithJSContextSingleton (DOM.liftJSM switchoverAction) jsSing
    pure (result, fc)

type HydrationWidget x a = HydrationDomBuilderT HydrationDomSpace DomTimeline (DomCoreWidget x) a

-- | A widget that isn't attached to any particular part of the DOM hierarchy
type FloatingWidget x = TriggerEventT DomTimeline (DomCoreWidget x)

type DomCoreWidget x = PostBuildT DomTimeline (WithJSContextSingleton x (PerformEventT DomTimeline DomHost))

{-# INLINABLE runHydrationWidgetWithHeadAndBody #-}
runHydrationWidgetWithHeadAndBody
  :: JSM ()
  -> (   (forall c. HydrationWidget () c -> FloatingWidget () c) -- "Append to head" --TODO: test invoking this more than once
      -> (forall c. HydrationWidget () c -> FloatingWidget () c) -- "Append to body" --TODO: test invoking this more than once
      -> FloatingWidget () ()
     )
  -> JSM ()
runHydrationWidgetWithHeadAndBody = runHydrationWidgetWithHeadAndBodyWithFailure (pure ())

{-# INLINABLE runHydrationWidgetWithHeadAndBodyWithFailure #-}
runHydrationWidgetWithHeadAndBodyWithFailure
  :: IO ()
  -> JSM ()
  -> (   (forall c. HydrationWidget () c -> FloatingWidget () c) -- "Append to head" --TODO: test invoking this more than once
      -> (forall c. HydrationWidget () c -> FloatingWidget () c) -- "Append to body" --TODO: test invoking this more than once
      -> FloatingWidget () ()
     )
  -> JSM ()
runHydrationWidgetWithHeadAndBodyWithFailure onFailure switchoverAction app = withJSContextSingletonMono $ \jsSing -> do
  globalDoc <- currentDocumentUnchecked
  headElement <- getHeadUnchecked globalDoc
  bodyElement <- getBodyUnchecked globalDoc
  (events, fc) <- liftIO . attachHydrationWidgetWithFailure onFailure switchoverAction jsSing $ \switchover hydrationMode hydrationResult events -> do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    let hydrateDom :: DOM.Node -> HydrationWidget () c -> FloatingWidget () c
        hydrateDom n w = do
          delayed <- liftIO $ newIORef $ pure ()
          unreadyChildren <- liftIO $ newIORef 0
          lift $ do
            let builderEnv = HydrationDomBuilderEnv
                  { _hydrationDomBuilderEnv_document = globalDoc
                  , _hydrationDomBuilderEnv_parent = Left $ toNode n
                  , _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
                  , _hydrationDomBuilderEnv_commitAction = pure ()
                  , _hydrationDomBuilderEnv_hydrationMode = hydrationMode
                  , _hydrationDomBuilderEnv_switchover = switchover
                  , _hydrationDomBuilderEnv_delayed = delayed
                  }
            a <- runHydrationDomBuilderT w builderEnv events
            forM_ hydrationResult $ \hr -> do
              res <- liftIO $ readIORef delayed
              liftIO $ modifyIORef' hr ((n, res) :)
            pure a
    runWithJSContextSingleton (runPostBuildT (runTriggerEventT (app (hydrateDom $ toNode headElement) (hydrateDom $ toNode bodyElement)) events) postBuild) jsSing
    return (events, postBuildTriggerRef)
  liftIO $ processAsyncEvents events fc

{-# INLINE mainWidget #-}
mainWidget :: (forall x. Widget x ()) -> JSM ()
mainWidget = mainWidget'

{-# INLINABLE mainWidget' #-}
-- | Warning: `mainWidget'` is provided only as performance tweak. It is expected to disappear in future releases.
mainWidget' :: Widget () () -> JSM ()
mainWidget' w = withJSContextSingletonMono $ \jsSing -> do
  doc <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  attachWidget body jsSing w

--TODO: The x's should be unified here
{-# INLINABLE mainWidgetWithHead #-}
mainWidgetWithHead :: (forall x. Widget x ()) -> (forall x. Widget x ()) -> JSM ()
mainWidgetWithHead h b = withJSContextSingletonMono $ \jsSing -> do
  doc <- currentDocumentUnchecked
  headElement <- getHeadUnchecked doc
  attachWidget headElement jsSing h
  body <- getBodyUnchecked doc
  attachWidget body jsSing b

{-# INLINABLE mainWidgetWithCss #-}
mainWidgetWithCss :: ByteString -> (forall x. Widget x ()) -> JSM ()
mainWidgetWithCss css w = withJSContextSingleton $ \jsSing -> do
  doc <- currentDocumentUnchecked
  headElement <- getHeadUnchecked doc
  setInnerHTML headElement $ "<style>" <> T.unpack (decodeUtf8 css) <> "</style>" --TODO: Fix this
  body <- getBodyUnchecked doc
  attachWidget body jsSing w

-- | The Reflex timeline for interacting with the DOM
type DomTimeline =
#ifdef PROFILE_REFLEX
  ProfiledTimeline
#endif
  Spider

-- | The ReflexHost the DOM lives in
type DomHost =
#ifdef PROFILE_REFLEX
  ProfiledM
#endif
  (SpiderHost Global)

runDomHost :: DomHost a -> IO a
runDomHost = runSpiderHost
#ifdef PROFILE_REFLEX
  . runProfiledM
#endif

type Widget x = ImmediateDomBuilderT DomTimeline (DomCoreWidget x)

{-# INLINABLE attachWidget #-}
attachWidget :: DOM.IsElement e => e -> JSContextSingleton x -> Widget x a -> JSM a
attachWidget rootElement wv w = fst <$> attachWidget' rootElement wv w

{-# INLINABLE runImmediateWidgetWithHeadAndBody #-}
runImmediateWidgetWithHeadAndBody
  :: (   (forall c. Widget () c -> FloatingWidget () c) -- "Append to head"
      -> (forall c. Widget () c -> FloatingWidget () c) -- "Append to body"
      -> FloatingWidget () ()
     )
  -> JSM ()
runImmediateWidgetWithHeadAndBody app = withJSContextSingletonMono $ \jsSing -> do
  globalDoc <- currentDocumentUnchecked
  headElement <- getHeadUnchecked globalDoc
  bodyElement <- getBodyUnchecked globalDoc
  headFragment <- createDocumentFragment globalDoc
  bodyFragment <- createDocumentFragment globalDoc
  (events, fc) <- liftIO . attachImmediateWidget $ \hydrationMode events -> do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    let go :: forall c. DOM.DocumentFragment -> Widget () c -> FloatingWidget () c
        go df w = do
          unreadyChildren <- liftIO $ newIORef 0
          delayed <- liftIO $ newIORef $ pure ()
          let builderEnv = HydrationDomBuilderEnv
                { _hydrationDomBuilderEnv_document = globalDoc
                , _hydrationDomBuilderEnv_parent = Left $ toNode df
                , _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
                , _hydrationDomBuilderEnv_commitAction = pure () --TODO: possibly `replaceElementContents n f`
                , _hydrationDomBuilderEnv_hydrationMode = hydrationMode
                , _hydrationDomBuilderEnv_switchover = never
                , _hydrationDomBuilderEnv_delayed = delayed
                }
          lift $ runHydrationDomBuilderT w builderEnv events
    runWithJSContextSingleton (runPostBuildT (runTriggerEventT (app (go headFragment) (go bodyFragment)) events) postBuild) jsSing
    return (events, postBuildTriggerRef)
  replaceElementContents headElement headFragment
  replaceElementContents bodyElement bodyFragment
  liftIO $ processAsyncEvents events fc

-- | Warning: `mainWidgetWithHead'` is provided only as performance tweak. It is expected to disappear in future releases.
mainWidgetWithHead' :: (a -> Widget () b, b -> Widget () a) -> JSM ()
mainWidgetWithHead' (h, b) = runImmediateWidgetWithHeadAndBody $ \appendHead appendBody -> do
  rec hOut <- appendHead $ h bOut
      bOut <- appendBody $ b hOut
  pure ()

replaceElementContents :: DOM.IsElement e => e -> DOM.DocumentFragment -> JSM ()
replaceElementContents e df = do
  setInnerHTML e ("" :: String)
  _ <- appendChild e df
  return ()

{-# INLINABLE attachWidget' #-}
attachWidget' :: DOM.IsElement e => e -> JSContextSingleton x -> Widget x a -> JSM (a, FireCommand DomTimeline DomHost)
attachWidget' rootElement jsSing w = do
  doc <- getOwnerDocumentUnchecked rootElement
  df <- createDocumentFragment doc
  ((a, events), fc) <- liftIO . attachImmediateWidget $ \hydrationMode events -> do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    unreadyChildren <- liftIO $ newIORef 0
    delayed <- liftIO $ newIORef $ pure ()
    let builderEnv = HydrationDomBuilderEnv
          { _hydrationDomBuilderEnv_document = toDocument doc
          , _hydrationDomBuilderEnv_parent = Left $ toNode df
          , _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
          , _hydrationDomBuilderEnv_commitAction = return () --TODO
          , _hydrationDomBuilderEnv_switchover = never
          , _hydrationDomBuilderEnv_delayed = delayed
          , _hydrationDomBuilderEnv_hydrationMode = hydrationMode
          }
    a <- runWithJSContextSingleton (runPostBuildT (runHydrationDomBuilderT w builderEnv events) postBuild) jsSing
    return ((a, events), postBuildTriggerRef)
  replaceElementContents rootElement df
  liftIO $ processAsyncEvents events fc
  return (a, fc)

type EventChannel = Chan [DSum (EventTriggerRef DomTimeline) TriggerInvocation]

{-# INLINABLE attachImmediateWidget #-}
attachImmediateWidget
  :: (   IORef HydrationMode
      -> EventChannel
      -> PerformEventT DomTimeline DomHost (a, IORef (Maybe (EventTrigger DomTimeline ())))
     )
  -> IO (a, FireCommand DomTimeline DomHost)
attachImmediateWidget w = do
  hydrationMode <- liftIO $ newIORef HydrationMode_Immediate
  events <- newChan
  runDomHost $ do
    ((result, postBuildTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT $ w hydrationMode events
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
    return (result, fc)

processAsyncEvents :: EventChannel -> FireCommand DomTimeline DomHost -> IO ()
processAsyncEvents events (FireCommand fire) = void $ forkIO $ forever $ do
  ers <- readChan events
  _ <- runDomHost $ do
    mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
      me <- readIORef er
      return $ fmap (\e -> e :=> Identity a) me
    _ <- fire (catMaybes mes) $ return ()
    liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
  return ()

-- | Run a reflex-dom application inside of an existing DOM element with the given ID
mainWidgetInElementById :: Text -> (forall x. Widget x ()) -> JSM ()
mainWidgetInElementById eid w = withJSContextSingleton $ \jsSing -> do
  doc <- currentDocumentUnchecked
  root <- getElementByIdUnchecked doc eid
  attachWidget root jsSing w

newtype AppInput t = AppInput
  { _appInput_window :: Window t
  }

newtype AppOutput t = AppOutput --TODO: Add quit event
  { _appOutput_windowConfig :: WindowConfig t
  }

runApp' :: (t ~ DomTimeline) => (forall x. AppInput t -> Widget x (AppOutput t)) -> JSM ()
runApp' app = withJSContextSingleton $ \jsSing -> do
  doc <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  win <- getDefaultViewUnchecked doc
  rec o <- attachWidget body jsSing $ do
        w <- wrapWindow win $ _appOutput_windowConfig o
        app $ AppInput
          { _appInput_window = w
          }
  return ()

{-# DEPRECATED attachWidget'' "Use 'attachImmediateWidget . const' instead" #-}
{-# INLINABLE attachWidget'' #-}
attachWidget'' :: (EventChannel -> PerformEventT DomTimeline DomHost (a, IORef (Maybe (EventTrigger DomTimeline ())))) -> IO (a, FireCommand DomTimeline DomHost)
attachWidget'' = attachImmediateWidget . const
