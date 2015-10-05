{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TypeOperators, DeriveDataTypeable, PackageImports, TemplateHaskell, LambdaCase, ConstraintKinds, CPP #-}
module Reflex.Dom.Internal where

import Prelude hiding (mapM, mapM_, concat, sequence, sequence_)

import Reflex.Dom.Internal.Foreign
import Reflex.Dom.Class

import GHCJS.DOM hiding (runWebGUI)
import GHCJS.DOM.Types hiding (Widget, unWidget, Event)
import GHCJS.DOM.Node
import GHCJS.DOM.Element
import GHCJS.DOM.Document
import Reflex.Class
import Reflex.Host.Class
import Reflex.Spider (Spider, SpiderHost (..))
import Control.Lens
import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.Reader hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.Ref
import Control.Monad.State.Strict hiding (mapM, mapM_, forM, forM_, sequence, sequence_, get)
import Control.Monad.Exception
import Control.Concurrent
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum (..))
import Data.Foldable
import Data.Traversable
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Monoid ((<>))

data GuiEnv t h
   = GuiEnv { _guiEnvDocument :: !HTMLDocument
            , _guiEnvPostGui :: !(h () -> IO ())
            , _guiEnvRunWithActions :: !([DSum (EventTrigger t)] -> h ())
            , _guiEnvWebView :: !WebView
            }

--TODO: Poorly named
newtype Gui t h m a = Gui { unGui :: ReaderT (GuiEnv t h) m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException, MonadAsyncException)

runGui :: Gui t h m a -> GuiEnv t h -> m a
runGui (Gui g) env = runReaderT g env

instance MonadTrans (Gui t h) where
  lift = Gui . lift

instance MonadRef m => MonadRef (Gui t h m) where
  type Ref (Gui t h m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (Gui t h m) where
  atomicModifyRef r f = lift $ atomicModifyRef r f

instance MonadSample t m => MonadSample t (Gui t h m) where
  sample b = lift $ sample b

instance MonadHold t m => MonadHold t (Gui t h m) where
  hold a0 e = lift $ hold a0 e

instance (Reflex t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (Gui t h m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

data WidgetEnv
   = WidgetEnv { _widgetEnvParent :: !Node
               }

data WidgetState t m
   = WidgetState { _widgetStatePostBuild :: !(m ())
                 , _widgetStateVoidActions :: ![Event t (m ())] --TODO: Would it help to make this a strict list?
                 }

liftM concat $ mapM makeLenses
  [ ''WidgetEnv
  , ''WidgetState
  , ''GuiEnv
  ]

instance Monad m => HasDocument (Gui t h m) where
  askDocument = Gui $ view guiEnvDocument

instance HasDocument m => HasDocument (Widget t m) where
  askDocument = lift askDocument

instance Monad m => HasWebView (Gui t h m) where
  askWebView = Gui $ view guiEnvWebView

instance MonadIORestore m => MonadIORestore (Gui t h m) where
  askRestore = Gui $ do
    r <- askRestore
    return $ Restore $ restore r . unGui

instance HasWebView m => HasWebView (Widget t m) where
  askWebView = lift askWebView

instance (MonadRef h, Ref h ~ Ref m, MonadRef m) => HasPostGui t h (Gui t h m) where
  askPostGui = Gui $ view guiEnvPostGui
  askRunWithActions = Gui $ view guiEnvRunWithActions

instance HasPostGui t h m => HasPostGui t h (Widget t m) where
  askPostGui = lift askPostGui
  askRunWithActions = lift askRunWithActions

type WidgetInternal t m a = ReaderT WidgetEnv (StateT (WidgetState t m) m) a

instance MonadTrans (Widget t) where
  lift = Widget . lift . lift

newtype Widget t m a = Widget { unWidget :: WidgetInternal t m a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance MonadSample t m => MonadSample t (Widget t m) where
  sample b = lift $ sample b

instance MonadHold t m => MonadHold t (Widget t m) where
  hold v0 e = lift $ hold v0 e

-- Need to build FRP circuit first, then elements
  -- Can't read from FRP until the whole thing is built
--TODO: Use JSString when in JS

instance MonadRef m => MonadRef (Widget t m) where
  type Ref (Widget t m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (Widget t m) where
  atomicModifyRef r f = lift $ atomicModifyRef r f

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (Widget t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance ( MonadRef m, Ref m ~ Ref IO, MonadRef h, Ref h ~ Ref IO --TODO: Shouldn't need to be IO
         , MonadIO m, MonadAsyncException m, MonadIO h, MonadAsyncException h, Functor m
         , ReflexHost t, MonadReflexCreateTrigger t m, MonadSample t m, MonadHold t m
         , MonadFix m, HasWebView h, HasPostGui t h h
         ) => MonadWidget t (Widget t (Gui t h m)) where
  type WidgetHost (Widget t (Gui t h m)) = Gui t h m
  type GuiAction (Widget t (Gui t h m)) = h
  askParent = Widget $ view widgetEnvParent
  --TODO: Use types to separate cohorts of possibly-recursive events/behaviors
  -- | Schedule an action to occur after the current cohort has been built; this is necessary because Behaviors built in the current cohort may not be read until after it is complete
  --schedulePostBuild :: Monad m => m () -> WidgetInternal t m ()
  liftWidgetHost = Widget . lift . lift
  schedulePostBuild a = Widget $ widgetStatePostBuild %= (a>>) --TODO: Can this >> be made strict?

  --addVoidAction :: Monad m => Event t (m ()) -> WidgetInternal t m ()
  addVoidAction a = Widget $ widgetStateVoidActions %= (a:)
  subWidget n child = Widget $ local (widgetEnvParent .~ toNode n) $ unWidget child
  subWidgetWithVoidActions n child = Widget $ do
    oldActions <- use widgetStateVoidActions
    widgetStateVoidActions .= []
    result <- local (widgetEnvParent .~ toNode n) $ unWidget child
    actions <- use widgetStateVoidActions
    widgetStateVoidActions .= oldActions
    return (result, mergeWith (>>) actions)
--  runWidget :: (Monad m, IsNode n, Reflex t) => n -> Widget t m a -> m (a, Event t (m ()))
  getRunWidget = return runWidget

runWidget :: (Monad m, Reflex t, IsNode n) => n -> Widget t (Gui t h m) a -> WidgetHost (Widget t (Gui t h m)) (a, WidgetHost (Widget t (Gui t h m)) (), Event t (WidgetHost (Widget t (Gui t h m)) ()))
runWidget rootElement w = do
  (result, WidgetState postBuild voidActions) <- runStateT (runReaderT (unWidget w) (WidgetEnv $ toNode rootElement)) (WidgetState (return ()) [])
  let voidAction = mergeWith (>>) voidActions
  return (result, postBuild, voidAction)

holdOnStartup :: MonadWidget t m => a -> WidgetHost m a -> m (Behavior t a)
holdOnStartup a0 ma = do
  (startupDone, startupDoneTriggerRef) <- newEventWithTriggerRef
  schedulePostBuild $ do
    a <- ma
    runFrameWithTriggerRef startupDoneTriggerRef a
  hold a0 startupDone

mainWidget :: Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
mainWidget w = runWebGUI $ \webView -> do
  Just doc <- liftM (fmap castToHTMLDocument) $ webViewGetDomDocument webView
  Just body <- getBody doc
  attachWidget body webView w

mainWidgetWithHead :: Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
mainWidgetWithHead h b = runWebGUI $ \webView -> do
  Just doc <- liftM (fmap castToHTMLDocument) $ webViewGetDomDocument webView
  Just headElement <- liftM (fmap castToHTMLElement) $ getHead doc
  attachWidget headElement webView h
  Just body <- getBody doc
  attachWidget body webView b

mainWidgetWithCss :: ByteString -> Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
mainWidgetWithCss css w = runWebGUI $ \webView -> do
  Just doc <- liftM (fmap castToHTMLDocument) $ webViewGetDomDocument webView
  Just headElement <- liftM (fmap castToHTMLElement) $ getHead doc
  setInnerHTML headElement . Just $ "<style>" <> T.unpack (decodeUtf8 css) <> "</style>" --TODO: Fix this
  Just body <- getBody doc
  attachWidget body webView w

newtype WithWebView m a = WithWebView { unWithWebView :: ReaderT WebView m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException, MonadAsyncException)

instance (Monad m) => HasWebView (WithWebView m) where
  askWebView = WithWebView ask

instance HasPostGui t h m => HasPostGui t (WithWebView h) (WithWebView m) where
  askPostGui = do
    postGui <- lift askPostGui
    webView <- askWebView
    return $ \h -> postGui $ runWithWebView h webView
  askRunWithActions = do
    runWithActions <- lift askRunWithActions
    return $ lift . runWithActions

instance HasPostGui Spider SpiderHost SpiderHost where
  askPostGui = return $ \h -> liftIO $ runSpiderHost h
  askRunWithActions = return fireEvents

instance MonadTrans WithWebView where
  lift = WithWebView . lift

instance MonadRef m => MonadRef (WithWebView m) where
  type Ref (WithWebView m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (WithWebView m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (WithWebView m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (WithWebView m) where
  subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (WithWebView m) where
  type ReadPhase (WithWebView m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  runHostFrame = lift . runHostFrame

runWithWebView :: WithWebView m a -> WebView -> m a
runWithWebView = runReaderT . unWithWebView

attachWidget :: (IsHTMLElement e) => e -> WebView -> Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) a -> IO a
attachWidget rootElement wv w = runSpiderHost $ flip runWithWebView wv $ do --TODO: It seems to re-run this handler if the URL changes, even if it's only the fragment
  Just doc <- liftM (fmap castToHTMLDocument) $ getOwnerDocument rootElement
  frames <- liftIO newChan
  rec let guiEnv = GuiEnv doc (writeChan frames . runSpiderHost . flip runWithWebView wv) runWithActions wv :: GuiEnv Spider (WithWebView SpiderHost)
          runWithActions dm = do
            voidActionNeeded <- fireEventsAndRead dm $ do
              sequence =<< readEvent voidActionHandle
            runHostFrame $ runGui (sequence_ voidActionNeeded) guiEnv
      Just df <- createDocumentFragment doc
      (result, voidAction) <- runHostFrame $ flip runGui guiEnv $ do
        (r, postBuild, va) <- runWidget df w
        postBuild -- This probably shouldn't be run inside the frame; we need to make sure we don't run a frame inside of a frame
        return (r, va)
      setInnerHTML rootElement $ Just ""
      _ <- appendChild rootElement $ Just df
      voidActionHandle <- subscribeEvent voidAction --TODO: Should be unnecessary
  --postGUISync seems to leak memory on GHC (unknown on GHCJS)
  _ <- liftIO $ forkIO $ forever $ postGUISync =<< readChan frames -- postGUISync is necessary to prevent segfaults in GTK, which is not thread-safe
  return result

--type MonadWidget t h m = (t ~ Spider, h ~ Gui Spider SpiderHost (HostFrame Spider), m ~ Widget t h, Monad h, MonadHold t h, HasDocument h, MonadSample t h, MonadRef h, MonadIO h, Functor (Event t), Functor h, Reflex t) -- Locking down these types seems to help a little in GHCJS, but not really in GHC

