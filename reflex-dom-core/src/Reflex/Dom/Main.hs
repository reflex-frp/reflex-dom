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

import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PostBuild.Base
import Reflex.Spider (Global, Spider, SpiderHost, runSpiderHost)
import Reflex.TriggerEvent.Base

import Reflex.Dom.Specializations () -- For SPECIALIZATION pragmas, which are always re-exported

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Reader hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Control.Monad.Ref
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum (..))
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

type Widget x = PostBuildT DomTimeline (ImmediateDomBuilderT DomTimeline (WithJSContextSingleton x (PerformEventT DomTimeline DomHost))) --TODO: Make this more abstract --TODO: Put the WithJSContext underneath PerformEventT - I think this would perform better because it could avoid fmapping over every performEvent

{-# INLINABLE attachWidget #-}
attachWidget :: DOM.IsElement e => e -> JSContextSingleton x -> Widget x a -> JSM a
attachWidget rootElement wv w = fst <$> attachWidget' rootElement wv w

-- | Warning: `mainWidgetWithHead'` is provided only as performance tweak. It is expected to disappear in future releases.
mainWidgetWithHead' :: (a -> Widget () b, b -> Widget () a) -> JSM ()
mainWidgetWithHead' widgets = withJSContextSingletonMono $ \jsSing -> do
  doc <- currentDocumentUnchecked
  headElement <- getHeadUnchecked doc
  headFragment <- createDocumentFragment doc
  bodyElement <- getBodyUnchecked doc
  bodyFragment <- createDocumentFragment doc
  childrenTriggerMountedActionsRef <- liftIO $ newIORef $ Just $ return ()
  ((events, postMountTriggerRef), fc) <- liftIO . attachWidget'' $ \events -> do
    let (headWidget, bodyWidget) = widgets
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    (postMount, postMountTriggerRef) <- newEventWithTriggerRef
    let go :: forall c. Widget () c -> DOM.DocumentFragment -> PerformEventT DomTimeline DomHost c
        go w df = do
          unreadyChildren <- liftIO $ newIORef 0
          let builderEnv = ImmediateDomBuilderEnv
                { _immediateDomBuilderEnv_document = toDocument doc
                , _immediateDomBuilderEnv_parent = toNode df
                , _immediateDomBuilderEnv_unreadyChildren = unreadyChildren
                , _immediateDomBuilderEnv_commitAction = return () --TODO
                , _immediateDomBuilderEnv_mounted = postMount
                , _immediateDomBuilderEnv_childrenPendingMountedTriggerActions = childrenTriggerMountedActionsRef
                }
          runWithJSContextSingleton (runImmediateDomBuilderT (runPostBuildT w postBuild) builderEnv events) jsSing
    rec b <- go (headWidget a) headFragment
        a <- go (bodyWidget b) bodyFragment
    return ((events, postMountTriggerRef), postBuildTriggerRef)
  replaceElementContents headElement headFragment
  replaceElementContents bodyElement bodyFragment
  liftIO $ do
    Just childTriggerActions <- readIORef childrenTriggerMountedActionsRef
    writeIORef childrenTriggerMountedActionsRef Nothing
    liftIO . runSpiderHost $ do
      mPostMountTrigger <- readRef postMountTriggerRef
      forM_ mPostMountTrigger $ \ postMountTrigger -> runFireCommand fc [postMountTrigger :=> Identity ()] $ return ()
    childTriggerActions
  liftIO $ processAsyncEvents events fc

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
  childrenTriggerMountedActionsRef <- liftIO $ newIORef $ Just $ return ()
  ((a, events, postMountTriggerRef), fc) <- liftIO . attachWidget'' $ \events -> do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    (postMount, postMountTriggerRef) <- newEventWithTriggerRef
    unreadyChildren <- liftIO $ newIORef 0
    let builderEnv = ImmediateDomBuilderEnv
          { _immediateDomBuilderEnv_document = toDocument doc
          , _immediateDomBuilderEnv_parent = toNode df
          , _immediateDomBuilderEnv_unreadyChildren = unreadyChildren
          , _immediateDomBuilderEnv_commitAction = return () --TODO
          , _immediateDomBuilderEnv_mounted = postMount
          , _immediateDomBuilderEnv_childrenPendingMountedTriggerActions = childrenTriggerMountedActionsRef
          }
    a <- runWithJSContextSingleton (runImmediateDomBuilderT (runPostBuildT w postBuild) builderEnv events) jsSing
    return ((a, events, postMountTriggerRef), postBuildTriggerRef)
  replaceElementContents rootElement df
  liftIO $ do
    Just childTriggerActions <- readIORef childrenTriggerMountedActionsRef
    writeIORef childrenTriggerMountedActionsRef Nothing
    liftIO . runSpiderHost $ do
      mPostMountTrigger <- readRef postMountTriggerRef
      forM_ mPostMountTrigger $ \ postMountTrigger -> runFireCommand fc [postMountTrigger :=> Identity ()] $ return ()
    childTriggerActions
  liftIO $ processAsyncEvents events fc
  return (a, fc)

type EventChannel = Chan [DSum (EventTriggerRef DomTimeline) TriggerInvocation]

{-# INLINABLE attachWidget'' #-}
attachWidget'' :: (EventChannel -> PerformEventT DomTimeline DomHost (a, IORef (Maybe (EventTrigger DomTimeline ())))) -> IO (a, FireCommand DomTimeline DomHost)
attachWidget'' w = do
  events <- newChan
  runDomHost $ do
    ((result, postBuildTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT $ w events
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
        w <- lift $ wrapWindow win $ _appOutput_windowConfig o
        app $ AppInput
          { _appInput_window = w
          }
  return ()
