{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TypeOperators, DeriveDataTypeable, PackageImports, TemplateHaskell, LambdaCase, ConstraintKinds #-}
module Reflex.Dom.Internal where

import Prelude hiding (mapM, mapM_, concat, sequence, sequence_)

import Reflex.Dom.Class

import GHCJS.DOM
import GHCJS.DOM.Types hiding (Widget, unWidget, Event)
import GHCJS.DOM.Node
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.NamedNodeMap
import GHCJS.DOM.Document
import GHCJS.DOM.UIEvent
import GHCJS.DOM.EventM (event, Signal (..))
import Reflex.Class
import Reflex.Dynamic
import Reflex.Host.Class
import Reflex.Spider (Spider, SpiderHost (..))
import qualified Reflex.Spider
import Control.Lens
import Control.Applicative
import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.Reader hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.Ref
import Control.Monad.State.Strict hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent
import Data.Dependent.Sum (DSum (..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Foldable
import Data.Traversable
import Data.IORef
import Data.Align
import Data.These
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Monoid ((<>))

import System.IO.Unsafe

data GuiEnv t h
   = GuiEnv { _guiEnvDocument :: !HTMLDocument
            , _guiEnvPostGui :: !(h () -> IO ())
            , _guiEnvRunWithActions :: !([DSum (EventTrigger t)] -> h ())
            }

--TODO: Poorly named
newtype Gui t h m a = Gui { unGui :: ReaderT (GuiEnv t h) m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

runGui :: Gui t h m a -> GuiEnv t h -> m a
runGui (Gui g) env = runReaderT g env

instance MonadTrans (Gui t h) where
  lift = Gui . lift

instance MonadRef m => MonadRef (Gui t h m) where
  type Ref (Gui t h m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r
  atomicModifyRef r f = lift $ atomicModifyRef r f

instance MonadSample t m => MonadSample t (Gui t h m) where
  sample b = lift $ sample b

instance MonadHold t m => MonadHold t (Gui t h m) where
  hold a0 e = lift $ hold a0 e

instance (Reflex t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (Gui t h m) where
  newEventWithTrigger initializer = lift $ newEventWithTrigger initializer

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

instance (MonadRef h, Ref h ~ Ref m, MonadRef m) => HasPostGui t h (Gui t h m) where
  askPostGui = Gui $ view guiEnvPostGui
  askRunWithActions = Gui $ view guiEnvRunWithActions

instance HasPostGui t h m => HasPostGui t h (Widget t m) where
  askPostGui = lift askPostGui
  askRunWithActions = lift askRunWithActions

type WidgetInternal t m a = ReaderT WidgetEnv (StateT (WidgetState t m) m) a

instance MonadTrans (Widget t) where
  lift = Widget . lift . lift

newtype Widget t m a = Widget { unWidget :: WidgetInternal t m a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

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
  atomicModifyRef r f = lift $ atomicModifyRef r f

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (Widget t m) where
  newEventWithTrigger = lift . newEventWithTrigger

instance ( MonadRef m, Ref m ~ Ref IO, MonadRef h, Ref h ~ Ref IO --TODO: Shouldn't need to be IO
         , MonadIO m, Functor m
         , ReflexHost t, MonadReflexCreateTrigger t m, MonadSample t m, MonadHold t m
         , MonadFix m
         ) => MonadWidget t (Widget t (Gui t h m)) where
  type WidgetHost (Widget t (Gui t h m)) = Gui t h m
  type GuiAction (Widget t (Gui t h m)) = h
  askParent = Widget $ view widgetEnvParent
  --TODO: Use types to separate cohorts of possibly-recursive events/behaviors
  -- | Schedule an action to occur after the current cohort has been built; this is necessary because Behaviors built in the current cohort may not be read until after it is complete
  --schedulePostBuild :: Monad m => m () -> WidgetInternal t m ()
  schedulePostBuild a = Widget $ widgetStatePostBuild %= (a>>) --TODO: Can this >> be made strict?

  --addVoidAction :: Monad m => Event t (m ()) -> WidgetInternal t m ()
  addVoidAction a = Widget $ widgetStateVoidActions %= (a:)
  subWidget n child = Widget $ local (widgetEnvParent .~ toNode n) $ unWidget child
--  runWidget :: (Monad m, IsNode n, Reflex t) => n -> Widget t m a -> m (a, Event t (m ()))
  getRunWidget = return runWidget

runWidget rootElement w = do
  (result, WidgetState postBuild voidActions) <- runStateT (runReaderT (unWidget w) (WidgetEnv $ toNode rootElement)) (WidgetState (return ()) [])
  let voidAction = mergeWith (>>) voidActions
  postBuild --TODO: This should be run some other way; it seems to cause strictness problems when recursion crosses parent/child boundaries
  return (result, voidAction)

holdOnStartup :: MonadWidget t m => a -> WidgetHost m a -> m (Behavior t a)
holdOnStartup a0 ma = do
  (startupDone, startupDoneTriggerRef) <- newEventWithTriggerRef
  schedulePostBuild $ do
    a <- ma
    runFrameWithTriggerRef startupDoneTriggerRef a
  hold a0 startupDone

mainWidget :: Widget Spider (Gui Spider SpiderHost (HostFrame Spider)) () -> IO ()
mainWidget w = runWebGUI $ \webView -> do
  Just doc <- liftM (fmap castToHTMLDocument) $ webViewGetDomDocument webView
  Just body <- documentGetBody doc
  attachWidget body w

mainWidgetWithCss css w = runWebGUI $ \webView -> do
  Just doc <- liftM (fmap castToHTMLDocument) $ webViewGetDomDocument webView
  Just head <- liftM (fmap castToHTMLElement) $ documentGetHead doc
  htmlElementSetInnerHTML head $ "<style>" <> T.unpack (decodeUtf8 css) <> "</style>" --TODO: Fix this
  Just body <- documentGetBody doc
  attachWidget body w

attachWidget :: (IsHTMLElement e) => e -> Widget Spider (Gui Spider SpiderHost (HostFrame Spider)) a -> IO a
attachWidget rootElement w = runSpiderHost $ do --TODO: It seems to re-run this handler if the URL changes, even if it's only the fragment
  Just doc <- liftM (fmap castToHTMLDocument) $ liftIO $ nodeGetOwnerDocument rootElement
  liftIO $ putStrLn "Using Widget2"
  frames <- liftIO newChan
  rec let guiEnv = GuiEnv doc (writeChan frames . runSpiderHost) runWithActions :: GuiEnv Spider SpiderHost
          runWithActions dm = do
            voidActionNeeded <- fireEventsAndRead dm $ do
              sequence =<< readEvent voidAction
            runHostFrame $ runGui (sequence_ voidActionNeeded) guiEnv
      Just df <- liftIO $ documentCreateDocumentFragment doc
      (result, voidAction) <- runHostFrame $ runGui (runWidget df w) guiEnv
      liftIO $ htmlElementSetInnerHTML rootElement ""
      liftIO $ nodeAppendChild rootElement $ Just df
      subscribeEvent voidAction --TODO: Should be unnecessary
--  currentFrame <- liftIO $ newEmptyMVar
--  idleAdd (tryTakeMVar currentFrame >>= maybe (return ()) id >> return True) priorityLow --TODO: avoid wasting CPU here
--  forkIO $ forever $ putMVar currentFrame =<< readChan frames
  --postGUISync seems to leak memory on GHC (unknown on GHCJS)
  liftIO $ forkIO $ forever $ postGUISync =<< readChan frames -- postGUISync is necessary to prevent segfaults in GTK, which is not thread-safe
  return result

--type MonadWidget t h m = (t ~ Spider, h ~ Gui Spider SpiderHost (HostFrame Spider), m ~ Widget t h, Monad h, MonadHold t h, HasDocument h, MonadSample t h, MonadRef h, MonadIO h, Functor (Event t), Functor h, Reflex t) -- Locking down these types seems to help a little in GHCJS, but not really in GHC

