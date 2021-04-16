{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
-- | This is a builder to be used on the client side. It can be run in two modes:
--
--  1. in "hydration mode", reusing DOM nodes already in the page (as produced
--  by 'Reflex.Dom.Builder.Static.renderStatic')
--  2. in "immediate mode", creating and appending DOM nodes as required
--
-- In "hydration mode", the preexisting DOM __must contain__ what the builder
-- will expect at switchover time (the time at which parity with the static
-- renderer is reached, and the time after which the page is "live").
--
-- For example, displaying the current time as text should be done inside
-- 'Reflex.Dom.Prerender.prerender' to ensure that we don't attempt to hydrate the incorrect text.
-- The server will prerender a text node with time A, and the client will expect
-- a text node with time B. Barring a miracle, time A and time B will not match,
-- and hydration will fail.
module Reflex.Dom.Builder.Immediate
  ( HydrationDomBuilderT (..)
  , HydrationDomBuilderEnv (..)
  , HydrationMode (..)
  , HydrationRunnerT (..)
  , runHydrationRunnerT
  , runHydrationRunnerTWithFailure
  , ImmediateDomBuilderT
  , runHydrationDomBuilderT
  , getHydrationMode
  , addHydrationStep
  , addHydrationStepWithSetup
  , setPreviousNode
  , insertAfterPreviousNode
  , hydrateComment
  , askParent
  , askEvents
  , append
  , textNodeInternal
  , removeSubsequentNodes
  , deleteBetweenExclusive
  , extractBetweenExclusive
  , deleteUpTo
  , extractUpTo
  , SupportsHydrationDomBuilder
  , collectUpTo
  , collectUpToGivenParent
  , EventTriggerRef (..)
  , EventFilterTriggerRef (..)
  , wrap
  , elementInternal
  , HydrationDomSpace
  , GhcjsDomSpace
  , GhcjsDomHandler (..)
  , GhcjsDomHandler1 (..)
  , GhcjsDomEvent (..)
  , GhcjsEventFilter (..)
  , Pair1 (..)
  , Maybe1 (..)
  , GhcjsEventSpec (..)
  , HasDocument (..)
  , ghcjsEventSpec_filters
  , ghcjsEventSpec_handler
  , GhcjsEventHandler (..)
  , drawChildUpdate
  , ChildReadyState (..)
  , mkHasFocus
  , insertBefore
  , EventType
  , defaultDomEventHandler
  , defaultDomWindowEventHandler
  , withIsEvent
  , showEventName
  , elementOnEventName
  , windowOnEventName
  , wrapDomEvent
  , subscribeDomEvent
  , wrapDomEventMaybe
  , wrapDomEventsMaybe
  , getKeyEvent
  , getMouseEventCoords
  , getTouchEvent
  , WindowConfig (..)
  , Window (..)
  , wrapWindow
  -- * Attributes for controlling hydration
  , hydratableAttribute
  , skipHydrationAttribute
  -- * Internal
  , traverseDMapWithKeyWithAdjust'
  , hoistTraverseWithKeyWithAdjust
  , traverseIntMapWithKeyWithAdjust'
  , hoistTraverseIntMapWithKeyWithAdjust
  ) where

import Control.Concurrent
import Control.Exception (bracketOnError)
import Control.Lens (Identity(..), imapM_, iforM_, (^.), makeLenses)
import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict (StateT, mapStateT, get, modify', gets, runStateT)
import Data.Bitraversable
import Data.Default
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum
import Data.FastMutableIntMap (PatchIntMap (..))
import Data.Foldable (for_, traverse_)
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Misc
import Data.Functor.Product
import Data.GADT.Compare (GCompare)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Some (Some(..))
import Data.GADT.Compare (GCompare)
import Data.String (IsString)
import Data.Text (Text)
import Foreign.JavaScript.Internal.Utils
import Foreign.JavaScript.TH
import GHCJS.DOM.ClipboardEvent as ClipboardEvent
import GHCJS.DOM.Document (Document, createDocumentFragment, createElement, createElementNS, createTextNode, createComment)
import GHCJS.DOM.Element (getScrollTop, removeAttribute, removeAttributeNS, setAttribute, setAttributeNS, hasAttribute)
import GHCJS.DOM.EventM (EventM, event, on)
import GHCJS.DOM.KeyboardEvent as KeyboardEvent
import GHCJS.DOM.MouseEvent
import GHCJS.DOM.Node (appendChild_, getOwnerDocumentUnchecked, getParentNodeUnchecked, setNodeValue, toNode)
import GHCJS.DOM.Types (liftJSM, askJSM, runJSM, JSM, MonadJSM, FocusEvent, IsElement, IsEvent, IsNode, KeyboardEvent, Node, TouchEvent, WheelEvent, uncheckedCastTo, ClipboardEvent)
import GHCJS.DOM.UIEvent
import Language.Javascript.JSaddle (call, eval)
import Reflex.Adjustable.Class
import Reflex.Class as Reflex
import Reflex.Dom.Builder.Class
import Reflex.Dynamic
import Reflex.Host.Class
import Reflex.Patch.DMapWithMove (PatchDMapWithMove(..))
import Reflex.Patch.MapWithMove (PatchMapWithMove(..))
import Reflex.PerformEvent.Base (PerformEventT)
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Base (PostBuildT)
import Reflex.PostBuild.Class
#ifdef PROFILE_REFLEX
import Reflex.Profiled
#endif
import Reflex.Requester.Base
import Reflex.Requester.Class
import Reflex.Spider (Spider, SpiderHost, Global)
import Reflex.TriggerEvent.Base hiding (askEvents)
import Reflex.TriggerEvent.Class

import qualified Data.Dependent.Map as DMap
import qualified Data.FastMutableIntMap as FastMutableIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.DataTransfer as DataTransfer
import qualified GHCJS.DOM.DocumentAndElementEventHandlers as Events
import qualified GHCJS.DOM.DocumentOrShadowRoot as Document
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.Event as Event
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.FileList as FileList
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.HTMLSelectElement as Select
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Text as DOM
import qualified GHCJS.DOM.Touch as Touch
import qualified GHCJS.DOM.TouchEvent as TouchEvent
import qualified GHCJS.DOM.TouchList as TouchList
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.WheelEvent as WheelEvent
import qualified Reflex.Patch.DMap as PatchDMap
import qualified Reflex.Patch.DMapWithMove as PatchDMapWithMove
import qualified Reflex.Patch.MapWithMove as PatchMapWithMove
import qualified Reflex.TriggerEvent.Base as TriggerEventT (askEvents)

#ifndef USE_TEMPLATE_HASKELL
import Data.Functor.Contravariant (phantom)
import Control.Lens (Lens', Getter)
#endif

#ifndef ghcjs_HOST_OS
import GHCJS.DOM.Types (MonadJSM (..))

instance MonadJSM m => MonadJSM (HydrationRunnerT t m) where
  {-# INLINABLE liftJSM' #-}
  liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (HydrationDomBuilderT s t m) where
  {-# INLINABLE liftJSM' #-}
  liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (DomRenderHookT t m) where
  {-# INLINABLE liftJSM' #-}
  liftJSM' = lift . liftJSM'
#endif

data HydrationDomBuilderEnv t m = HydrationDomBuilderEnv
  { _hydrationDomBuilderEnv_document :: {-# UNPACK #-} !Document
  -- ^ Reference to the document
  , _hydrationDomBuilderEnv_parent :: !(Either Node (IORef Node))
  -- ^ This is in an IORef because in the time up to hydration we can't actually know what the
  -- parent is - we populate this reference during the DOM traversal at hydration time
  , _hydrationDomBuilderEnv_unreadyChildren :: {-# UNPACK #-} !(IORef Word)
  -- ^ Number of children who still aren't fully rendered
  , _hydrationDomBuilderEnv_commitAction :: !(JSM ())
  -- ^ Action to take when all children are ready --TODO: we should probably get rid of this once we invoke it
  , _hydrationDomBuilderEnv_hydrationMode :: {-# UNPACK #-} !(IORef HydrationMode)
  -- ^ In hydration mode? Should be switched to `HydrationMode_Immediate` after hydration is finished
  , _hydrationDomBuilderEnv_switchover :: !(Event t ())
  , _hydrationDomBuilderEnv_delayed :: {-# UNPACK #-} !(IORef (HydrationRunnerT t m ()))
  }

-- | A monad for DomBuilder which just gets the results of children and pushes
-- work into an action that is delayed until after postBuild (to match the
-- static builder). The action runs in 'HydrationRunnerT', which performs the
-- DOM takeover and sets up the events, after which point this monad will
-- continue in the vein of 'ImmediateDomBuilderT'.
newtype HydrationDomBuilderT s t m a = HydrationDomBuilderT { unHydrationDomBuilderT :: ReaderT (HydrationDomBuilderEnv t m) (DomRenderHookT t m) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
#if MIN_VERSION_base(4,9,1)
           , MonadAsyncException
#endif
           )

instance PrimMonad m => PrimMonad (HydrationDomBuilderT s t m) where
  type PrimState (HydrationDomBuilderT s t m) = PrimState m
  primitive = lift . primitive

instance MonadTrans (HydrationDomBuilderT s t) where
  lift = HydrationDomBuilderT . lift . lift

instance (Reflex t, MonadFix m) => DomRenderHook t (HydrationDomBuilderT s t m) where
  withRenderHook hook = HydrationDomBuilderT . mapReaderT (withRenderHook hook) . unHydrationDomBuilderT
  requestDomAction = HydrationDomBuilderT . lift . requestDomAction
  requestDomAction_ = HydrationDomBuilderT . lift . requestDomAction_

-- | The monad which performs the delayed actions to reuse prerendered nodes and set up events.
-- State contains reference to the previous node sibling, if any, and the reader contains reference to the parent node.
newtype HydrationRunnerT t m a = HydrationRunnerT { unHydrationRunnerT :: StateT HydrationState (ReaderT Node (DomRenderHookT t m)) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
#if MIN_VERSION_base(4,9,1)
           , MonadAsyncException
#endif
           )

data HydrationState = HydrationState
  { _hydrationState_previousNode :: !(Maybe Node)
  , _hydrationState_failed :: !Bool
  }

{-# INLINABLE localRunner #-}
localRunner :: (MonadJSM m, Monad m) => HydrationRunnerT t m a -> Maybe Node -> Node -> HydrationRunnerT t m a
localRunner (HydrationRunnerT m) s parent = do
  s0 <- HydrationRunnerT get
  (a, s') <- HydrationRunnerT $ lift $ local (\_ -> parent) $ runStateT m (s0 { _hydrationState_previousNode = s })
  traverse_ removeSubsequentNodes $ _hydrationState_previousNode s'
  HydrationRunnerT $ modify' $ \hs -> hs { _hydrationState_failed = _hydrationState_failed s' }
  pure a

{-# INLINABLE runHydrationRunnerT #-}
runHydrationRunnerT
  :: (MonadRef m, Ref m ~ IORef, Monad m, PerformEvent t m, MonadFix m, MonadReflexCreateTrigger t m, MonadJSM m, MonadJSM (Performable m))
  => HydrationRunnerT t m a -> Maybe Node -> Node -> Chan [DSum (EventTriggerRef t) TriggerInvocation] -> m a
runHydrationRunnerT m = runHydrationRunnerTWithFailure m (pure ())

{-# INLINABLE runHydrationRunnerTWithFailure #-}
runHydrationRunnerTWithFailure
  :: (MonadRef m, Ref m ~ IORef, Monad m, PerformEvent t m, MonadFix m, MonadReflexCreateTrigger t m, MonadJSM m, MonadJSM (Performable m))
  => HydrationRunnerT t m a -> IO () -> Maybe Node -> Node -> Chan [DSum (EventTriggerRef t) TriggerInvocation] -> m a
runHydrationRunnerTWithFailure (HydrationRunnerT m) onFailure s parent events = flip runDomRenderHookT events $ flip runReaderT parent $ do
  (a, s') <- runStateT m (HydrationState s False)
  traverse_ removeSubsequentNodes $ _hydrationState_previousNode s'
  when (_hydrationState_failed s') $ liftIO $ putStrLn "reflex-dom warning: hydration failed: the DOM was not as expected at switchover time. This may be due to invalid HTML which the browser has altered upon parsing, some external JS altering the DOM, or the page being served from an outdated cache."
  when (_hydrationState_failed s') $ liftIO onFailure
  pure a



instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (HydrationRunnerT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadTrans (HydrationRunnerT t) where
  {-# INLINABLE lift #-}
  lift = HydrationRunnerT . lift . lift . lift

instance MonadSample t m => MonadSample t (HydrationRunnerT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance (Reflex t, MonadFix m) => DomRenderHook t (HydrationRunnerT t m) where
  withRenderHook hook = HydrationRunnerT . mapStateT (mapReaderT (withRenderHook hook)) . unHydrationRunnerT
  requestDomAction = HydrationRunnerT . lift . lift . requestDomAction
  requestDomAction_ = HydrationRunnerT . lift . lift . requestDomAction_

-- | Add a hydration step which depends on some computation that should only be
-- done *before* the switchover to immediate mode - this is most likely some
-- form of 'hold' which we want to remove after hydration is done
{-# INLINABLE addHydrationStepWithSetup #-}
addHydrationStepWithSetup :: MonadIO m => m a -> (a -> HydrationRunnerT t m ()) -> HydrationDomBuilderT s t m ()
addHydrationStepWithSetup setup f = getHydrationMode >>= \case
  HydrationMode_Immediate -> pure ()
  HydrationMode_Hydrating -> do
    s <- lift setup
    addHydrationStep (f s)

-- | Add a hydration step
{-# INLINABLE addHydrationStep #-}
addHydrationStep :: MonadIO m => HydrationRunnerT t m () -> HydrationDomBuilderT s t m ()
addHydrationStep m = do
  delayedRef <- HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_delayed
  liftIO $ modifyIORef' delayedRef (>> m)

-- | Shared behavior for HydrationDomBuilderT and HydrationRunnerT
newtype DomRenderHookT t m a = DomRenderHookT { unDomRenderHookT :: RequesterT t JSM Identity (TriggerEventT t m) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
#if MIN_VERSION_base(4,9,1)
           , MonadAsyncException
#endif
           )

{-# INLINABLE runDomRenderHookT #-}
runDomRenderHookT
  :: (MonadFix m, PerformEvent t m, MonadReflexCreateTrigger t m, MonadJSM m, MonadJSM (Performable m), MonadRef m, Ref m ~ IORef)
  => DomRenderHookT t m a
  -> Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -> m a
runDomRenderHookT (DomRenderHookT a) events = do
  flip runTriggerEventT events $ do
    rec (result, req) <- runRequesterT a rsp
        rsp <- performEventAsync $ ffor req $ \rm f -> liftJSM $ runInAnimationFrame f $
          traverseRequesterData (fmap Identity) rm
    return result
  where
    runInAnimationFrame f x = void . DOM.inAnimationFrame' $ \_ -> do
        v <- synchronously x
        void . liftIO $ f v

instance MonadTrans (DomRenderHookT t) where
  {-# INLINABLE lift #-}
  lift = DomRenderHookT . lift . lift

instance (Reflex t, MonadFix m) => DomRenderHook t (DomRenderHookT t m) where
  withRenderHook hook (DomRenderHookT a) = do
    DomRenderHookT $ withRequesting $ \rsp -> do
      (x, req) <- lift $ runRequesterT a $ runIdentity <$> rsp
      return (ffor req $ \rm -> hook $ traverseRequesterData (fmap Identity) rm, x)
  requestDomAction = DomRenderHookT . requestingIdentity
  requestDomAction_ = DomRenderHookT . requesting_

{-# INLINABLE runHydrationDomBuilderT #-}
runHydrationDomBuilderT
  :: ( MonadFix m
     , PerformEvent t m
     , MonadReflexCreateTrigger t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadRef m
     , Ref m ~ IORef
     )
  => HydrationDomBuilderT s t m a
  -> HydrationDomBuilderEnv t m
  -> Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -> m a
runHydrationDomBuilderT (HydrationDomBuilderT a) env = runDomRenderHookT (runReaderT a env)

instance (RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document, Monad m) => HasDocument (HydrationDomBuilderT s t m) where
  {-# INLINABLE askDocument #-}
  askDocument = HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_document

{-# INLINABLE askParent #-}
askParent :: Monad m => HydrationRunnerT t m DOM.Node
askParent = HydrationRunnerT ask

{-# INLINABLE getParent #-}
getParent :: MonadIO m => HydrationDomBuilderT s t m DOM.Node
getParent = either pure (liftIO . readIORef) =<< HydrationDomBuilderT (asks _hydrationDomBuilderEnv_parent)

{-# INLINABLE askEvents #-}
askEvents :: Monad m => HydrationDomBuilderT s t m (Chan [DSum (EventTriggerRef t) TriggerInvocation])
askEvents = HydrationDomBuilderT . lift . DomRenderHookT . lift $ TriggerEventT.askEvents

{-# INLINABLE localEnv #-}
localEnv :: Monad m => (HydrationDomBuilderEnv t m -> HydrationDomBuilderEnv t m) -> HydrationDomBuilderT s t m a -> HydrationDomBuilderT s t m a
localEnv f = HydrationDomBuilderT . local (f $!) . unHydrationDomBuilderT

{-# INLINABLE append #-}
append :: MonadJSM m => DOM.Node -> HydrationDomBuilderT s t m ()
append n = do
  p <- getParent
  liftJSM $ appendChild_ p n
  return ()

{-# SPECIALIZE append
  :: DOM.Node
  -> HydrationDomBuilderT s Spider HydrationM ()
  #-}

data HydrationMode
  = HydrationMode_Hydrating
  -- ^ The time from initial load to parity with static builder
  | HydrationMode_Immediate
  -- ^ After hydration
  deriving (Eq, Ord, Show)

{-# INLINABLE getPreviousNode #-}
getPreviousNode :: Monad m => HydrationRunnerT t m (Maybe DOM.Node)
getPreviousNode = HydrationRunnerT $ gets _hydrationState_previousNode

{-# INLINABLE setPreviousNode #-}
setPreviousNode :: Monad m => Maybe DOM.Node -> HydrationRunnerT t m ()
setPreviousNode n = HydrationRunnerT $ modify' (\hs -> hs { _hydrationState_previousNode = n })

{-# INLINABLE askUnreadyChildren #-}
askUnreadyChildren :: Monad m => HydrationDomBuilderT s t m (IORef Word)
askUnreadyChildren = HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_unreadyChildren

{-# INLINABLE askCommitAction #-}
askCommitAction :: Monad m => HydrationDomBuilderT s t m (JSM ())
askCommitAction = HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_commitAction

{-# INLINABLE getHydrationMode #-}
getHydrationMode :: MonadIO m => HydrationDomBuilderT s t m HydrationMode
getHydrationMode = liftIO . readIORef =<< HydrationDomBuilderT (asks _hydrationDomBuilderEnv_hydrationMode)

-- | Remove all nodes after given node
removeSubsequentNodes :: (MonadJSM m, IsNode n) => n -> m ()
removeSubsequentNodes n = liftJSM $ do
  f <- eval ("(function(n) { while (n.nextSibling) { (n.parentNode).removeChild(n.nextSibling); }; })" :: Text)
  void $ call f f [n]

-- | s and e must both be children of the same node and s must precede e;
--   all nodes between s and e will be removed, but s and e will not be removed
deleteBetweenExclusive :: (MonadJSM m, IsNode start, IsNode end) => start -> end -> m ()
deleteBetweenExclusive s e = liftJSM $ do
  df <- createDocumentFragment =<< getOwnerDocumentUnchecked s
  extractBetweenExclusive df s e -- In many places in ImmediateDomBuilderT, we assume that things always have a parent; by adding them to this DocumentFragment, we maintain that invariant

-- | s and e must both be children of the same node and s must precede e; all
--   nodes between s and e will be moved into the given DocumentFragment, but s
--   and e will not be moved
extractBetweenExclusive :: (MonadJSM m, IsNode start, IsNode end) => DOM.DocumentFragment -> start -> end -> m ()
extractBetweenExclusive df s e = liftJSM $ do
  f <- eval ("(function(df,s,e) { var x; for(;;) { x = s['nextSibling']; if(e===x) { break; }; df['appendChild'](x); } })" :: Text)
  void $ call f f (df, s, e)

-- | s and e must both be children of the same node and s must precede e;
--   s and all nodes between s and e will be removed, but e will not be removed
{-# INLINABLE deleteUpTo #-}
deleteUpTo :: (MonadJSM m, IsNode start, IsNode end) => start -> end -> m ()
deleteUpTo s e = do
  df <- createDocumentFragment =<< getOwnerDocumentUnchecked s
  extractUpTo df s e -- In many places in ImmediateDomBuilderT, we assume that things always have a parent; by adding them to this DocumentFragment, we maintain that invariant

extractUpTo :: (MonadJSM m, IsNode start, IsNode end) => DOM.DocumentFragment -> start -> end -> m ()
#ifdef ghcjs_HOST_OS
--NOTE: Although wrapping this javascript in a function seems unnecessary, GHCJS's optimizer will break it if it is entered without that wrapping (as of 2017-09-04)
foreign import javascript unsafe
  "(function() { var x = $2; while(x !== $3) { var y = x['nextSibling']; $1['appendChild'](x); x = y; } })()"
  extractUpTo_ :: DOM.DocumentFragment -> DOM.Node -> DOM.Node -> IO ()
extractUpTo df s e = liftJSM $ extractUpTo_ df (toNode s) (toNode e)
#else
extractUpTo df s e = liftJSM $ do
  f <- eval ("(function(df,s,e){ var x = s; var y; for(;;) { y = x['nextSibling']; df['appendChild'](x); if(e===y) { break; } x = y; } })" :: Text)
  void $ call f f (df, s, e)
#endif

type SupportsHydrationDomBuilder t m = (Reflex t, MonadJSM m, MonadHold t m, MonadFix m, MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref JSM, Adjustable t m, PrimMonad m, PerformEvent t m, MonadJSM (Performable m))

{-# INLINABLE collectUpTo #-}
collectUpTo :: (MonadJSM m, IsNode start, IsNode end) => start -> end -> m DOM.DocumentFragment
collectUpTo s e = do
  currentParent <- getParentNodeUnchecked e -- May be different than it was at initial construction, e.g., because the parent may have dumped us in from a DocumentFragment
  collectUpToGivenParent currentParent s e

{-# INLINABLE collectUpToGivenParent #-}
collectUpToGivenParent :: (MonadJSM m, IsNode parent, IsNode start, IsNode end) => parent -> start -> end -> m DOM.DocumentFragment
collectUpToGivenParent currentParent s e = do
  doc <- getOwnerDocumentUnchecked currentParent
  df <- createDocumentFragment doc
  extractUpTo df s e
  return df

newtype EventFilterTriggerRef t er (en :: EventTag) = EventFilterTriggerRef (IORef (Maybe (EventTrigger t (er en))))

-- | This 'wrap' is only partial: it doesn't create the 'EventSelector' itself
{-# INLINE wrap #-}
wrap
  :: forall s m er t. (Reflex t, MonadJSM m, MonadReflexCreateTrigger t m, DomRenderHook t m, EventSpec s ~ GhcjsEventSpec)
  => Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -> DOM.Element
  -> RawElementConfig er t s
  -> m (DMap EventName (EventFilterTriggerRef t er))
wrap events e cfg = do
  forM_ (_rawElementConfig_modifyAttributes cfg) $ \modifyAttrs -> requestDomAction_ $ ffor modifyAttrs $ imapM_ $ \(AttributeName mAttrNamespace n) mv -> case mAttrNamespace of
    Nothing -> maybe (removeAttribute e n) (setAttribute e n) mv
    Just ns -> maybe (removeAttributeNS e (Just ns) n) (setAttributeNS e (Just ns) n) mv
  eventTriggerRefs :: DMap EventName (EventFilterTriggerRef t er) <- liftJSM $ fmap DMap.fromList $ forM (DMap.toList $ _ghcjsEventSpec_filters $ _rawElementConfig_eventSpec cfg) $ \(en :=> GhcjsEventFilter f) -> do
    triggerRef <- liftIO $ newIORef Nothing
    _ <- elementOnEventName en e $ do --TODO: Something safer than this cast
      evt <- DOM.event
      (flags, k) <- liftJSM $ f $ GhcjsDomEvent evt
      when (_eventFlags_preventDefault flags) $ withIsEvent en DOM.preventDefault
      case _eventFlags_propagation flags of
        Propagation_Continue -> return ()
        Propagation_Stop -> withIsEvent en DOM.stopPropagation
        Propagation_StopImmediate -> withIsEvent en DOM.stopImmediatePropagation
      mv <- liftJSM k --TODO: Only do this when the event is subscribed
      liftIO $ forM_ mv $ \v -> writeChan events [EventTriggerRef triggerRef :=> TriggerInvocation v (return ())]
    return $ en :=> EventFilterTriggerRef triggerRef
  return eventTriggerRefs

{-# SPECIALIZE wrap
  :: Chan [DSum (EventTriggerRef DomTimeline) TriggerInvocation]
  -> DOM.Element
  -> RawElementConfig er DomTimeline HydrationDomSpace
  -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM (DMap EventName (EventFilterTriggerRef DomTimeline er))
  #-}

{-# SPECIALIZE wrap
  :: Chan [DSum (EventTriggerRef DomTimeline) TriggerInvocation]
  -> DOM.Element
  -> RawElementConfig er DomTimeline GhcjsDomSpace
  -> HydrationDomBuilderT GhcjsDomSpace DomTimeline HydrationM (DMap EventName (EventFilterTriggerRef DomTimeline er))
  #-}

{-# INLINE triggerBody #-}
triggerBody
  :: forall s er t x. EventSpec s ~ GhcjsEventSpec
  => DOM.JSContextRef
  -> RawElementConfig er t s
  -> Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -> DMap EventName (EventFilterTriggerRef t er)
  -> DOM.Element
  -> WrapArg er EventName x
  -> EventTrigger t x
  -> IO (IO ())
triggerBody ctx cfg events eventTriggerRefs e (WrapArg en) t = case DMap.lookup en eventTriggerRefs of
  Just (EventFilterTriggerRef r) -> do
    writeIORef r $ Just t
    return $ do
      writeIORef r Nothing
  Nothing -> (`runJSM` ctx) <$> (`runJSM` ctx) (elementOnEventName en e $ do
    evt <- DOM.event
    mv <- lift $ unGhcjsEventHandler handler (en, GhcjsDomEvent evt)
    case mv of
      Nothing -> return ()
      Just v -> liftIO $ do
        --TODO: I don't think this is quite right: if a new trigger is created between when this is enqueued and when it fires, this may not work quite right
        ref <- newIORef $ Just t
        writeChan events [EventTriggerRef ref :=> TriggerInvocation v (return ())])
  where
    -- Note: this needs to be done strictly and outside of the newFanEventWithTrigger, so that the newFanEventWithTrigger doesn't
    -- retain the entire cfg, which can cause a cyclic dependency that the GC won't be able to clean up
    handler :: GhcjsEventHandler er
    !handler = _ghcjsEventSpec_handler $ _rawElementConfig_eventSpec cfg

{-# SPECIALIZE triggerBody
  :: DOM.JSContextRef
  -> RawElementConfig er DomTimeline HydrationDomSpace
  -> Chan [DSum (EventTriggerRef DomTimeline) TriggerInvocation]
  -> DMap EventName (EventFilterTriggerRef DomTimeline er)
  -> DOM.Element
  -> WrapArg er EventName x
  -> EventTrigger DomTimeline x
  -> IO (IO ())
  #-}

{-# SPECIALIZE triggerBody
  :: DOM.JSContextRef
  -> RawElementConfig er DomTimeline GhcjsDomSpace
  -> Chan [DSum (EventTriggerRef DomTimeline) TriggerInvocation]
  -> DMap EventName (EventFilterTriggerRef DomTimeline er)
  -> DOM.Element
  -> WrapArg er EventName x
  -> EventTrigger DomTimeline x
  -> IO (IO ())
  #-}

newtype GhcjsDomHandler a b = GhcjsDomHandler { unGhcjsDomHandler :: a -> JSM b }

newtype GhcjsDomHandler1 a b = GhcjsDomHandler1 { unGhcjsDomHandler1 :: forall (x :: EventTag). a x -> JSM (b x) }

newtype GhcjsDomEvent en = GhcjsDomEvent { unGhcjsDomEvent :: EventType en }

data GhcjsDomSpace

instance DomSpace GhcjsDomSpace where
  type EventSpec GhcjsDomSpace = GhcjsEventSpec
  type RawDocument GhcjsDomSpace = DOM.Document
  type RawTextNode GhcjsDomSpace = DOM.Text
  type RawCommentNode GhcjsDomSpace = DOM.Comment
  type RawElement GhcjsDomSpace = DOM.Element
  type RawInputElement GhcjsDomSpace = DOM.HTMLInputElement
  type RawTextAreaElement GhcjsDomSpace = DOM.HTMLTextAreaElement
  type RawSelectElement GhcjsDomSpace = DOM.HTMLSelectElement
  addEventSpecFlags _ en f es = es
    { _ghcjsEventSpec_filters =
        let f' = Just . GhcjsEventFilter . \case
              Nothing -> \evt -> do
                mEventResult <- unGhcjsEventHandler (_ghcjsEventSpec_handler es) (en, evt)
                return (f mEventResult, return mEventResult)
              Just (GhcjsEventFilter oldFilter) -> \evt -> do
                (oldFlags, oldContinuation) <- oldFilter evt
                mEventResult <- oldContinuation
                let newFlags = oldFlags <> f mEventResult
                return (newFlags, return mEventResult)
        in DMap.alter f' en $ _ghcjsEventSpec_filters es
    }

newtype GhcjsEventFilter er en = GhcjsEventFilter (GhcjsDomEvent en -> JSM (EventFlags, JSM (Maybe (er en))))

data Pair1 (f :: k -> *) (g :: k -> *) (a :: k) = Pair1 (f a) (g a)

data Maybe1 f a = Nothing1 | Just1 (f a)

data GhcjsEventSpec er = GhcjsEventSpec
  { _ghcjsEventSpec_filters :: DMap EventName (GhcjsEventFilter er)
  , _ghcjsEventSpec_handler :: GhcjsEventHandler er
  }

newtype GhcjsEventHandler er = GhcjsEventHandler { unGhcjsEventHandler :: forall en. (EventName en, GhcjsDomEvent en) -> JSM (Maybe (er en)) }

#ifndef USE_TEMPLATE_HASKELL
ghcjsEventSpec_filters :: forall er . Lens' (GhcjsEventSpec er) (DMap EventName (GhcjsEventFilter er))
ghcjsEventSpec_filters f (GhcjsEventSpec a b) = (\a' -> GhcjsEventSpec a' b) <$> f a
{-# INLINE ghcjsEventSpec_filters #-}
ghcjsEventSpec_handler :: forall er en . Getter (GhcjsEventSpec er) ((EventName en, GhcjsDomEvent en) -> JSM (Maybe (er en)))
ghcjsEventSpec_handler f (GhcjsEventSpec _ (GhcjsEventHandler b)) = phantom (f b)
{-# INLINE ghcjsEventSpec_handler #-}
#endif

instance er ~ EventResult => Default (GhcjsEventSpec er) where
  def = GhcjsEventSpec
    { _ghcjsEventSpec_filters = mempty
    , _ghcjsEventSpec_handler = GhcjsEventHandler $ \(en, GhcjsDomEvent evt) -> do
        t :: DOM.EventTarget <- withIsEvent en $ Event.getTargetUnchecked evt --TODO: Rework this; defaultDomEventHandler shouldn't need to take this as an argument
        let e = uncheckedCastTo DOM.Element t
        runReaderT (defaultDomEventHandler e en) evt
    }

{-# INLINE makeElement #-}
makeElement :: MonadJSM m => Document -> Text -> ElementConfig er t s -> m DOM.Element
makeElement doc elementTag cfg = do
  e <- uncheckedCastTo DOM.Element <$> case cfg ^. namespace of
    Nothing -> createElement doc elementTag
    Just ens -> createElementNS doc (Just ens) elementTag
  iforM_ (cfg ^. initialAttributes) $ \(AttributeName mAttrNamespace n) v -> case mAttrNamespace of
    Nothing -> setAttribute e n v
    Just ans -> setAttributeNS e (Just ans) n v
  pure e

{-# INLINE elementImmediate #-}
elementImmediate
  :: ( RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document, EventSpec s ~ GhcjsEventSpec
     , MonadJSM m, Reflex t, MonadReflexCreateTrigger t m, MonadFix m )
  => Text
  -> ElementConfig er t s
  -> HydrationDomBuilderT s t m a
  -> HydrationDomBuilderT s t m (Element er GhcjsDomSpace t, a)
elementImmediate elementTag cfg child = do
  doc <- askDocument
  ctx <- askJSM
  events <- askEvents
  parent <- getParent
  e <- makeElement doc elementTag cfg
  appendChild_ parent e
  -- Run the child builder with updated parent and previous sibling references
  result <- localEnv (\env -> env { _hydrationDomBuilderEnv_parent = Left $ toNode e }) child
  let rawCfg = extractRawElementConfig cfg
  eventTriggerRefs <- wrap events e rawCfg
  es <- newFanEventWithTrigger $ triggerBody ctx rawCfg events eventTriggerRefs e
  return (Element es e, result)

{-# SPECIALIZE elementImmediate
  :: Text
  -> ElementConfig er DomTimeline HydrationDomSpace
  -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM a
  -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM (Element er GhcjsDomSpace DomTimeline, a)
  #-}

{-# SPECIALIZE elementImmediate
  :: Text
  -> ElementConfig er DomTimeline GhcjsDomSpace
  -> HydrationDomBuilderT GhcjsDomSpace DomTimeline HydrationM a
  -> HydrationDomBuilderT GhcjsDomSpace DomTimeline HydrationM (Element er GhcjsDomSpace DomTimeline, a)
  #-}

-- For specialisation

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

type DomCoreWidget x = PostBuildT DomTimeline (WithJSContextSingleton x (PerformEventT DomTimeline DomHost))
type HydrationM = DomCoreWidget ()

{-# INLINE elementInternal #-}
elementInternal
  :: (MonadJSM m, Reflex t, MonadReflexCreateTrigger t m, MonadFix m)
  => Text
  -> ElementConfig er t HydrationDomSpace
  -> HydrationDomBuilderT HydrationDomSpace t m a
  -> HydrationDomBuilderT HydrationDomSpace t m (Element er HydrationDomSpace t, a)
elementInternal elementTag cfg child = getHydrationMode >>= \case
  HydrationMode_Immediate -> do
    (Element es _, result) <- elementImmediate elementTag cfg child
    return (Element es (), result)
  HydrationMode_Hydrating -> fst <$> hydrateElement elementTag cfg child

{-# SPECIALIZE elementInternal
  :: Text
  -> ElementConfig er DomTimeline HydrationDomSpace
  -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM a
  -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM (Element er HydrationDomSpace DomTimeline, a)
  #-}

-- | An attribute which causes hydration to skip over an element completely.
skipHydrationAttribute :: IsString s => s
skipHydrationAttribute = "data-hydration-skip"

-- | An attribute which signals that an element should be hydrated.
hydratableAttribute :: IsString s => s
hydratableAttribute = "data-ssr"

{-# INLINE hydrateElement #-}
hydrateElement
  :: forall er t m a. (MonadJSM m, Reflex t, MonadReflexCreateTrigger t m, MonadFix m)
  => Text
  -> ElementConfig er t HydrationDomSpace
  -> HydrationDomBuilderT HydrationDomSpace t m a
  -> HydrationDomBuilderT HydrationDomSpace t m ((Element er HydrationDomSpace t, a), IORef DOM.Element)
hydrateElement elementTag cfg child = do
  ctx <- askJSM
  events <- askEvents
  -- Schedule everything for after postBuild, except for getting the result itself
  parentRef <- liftIO $ newIORef $ error "Parent not yet initialized"
  e' <- liftIO $ newIORef $ error "hydrateElement: Element not yet initialized"
  env <- HydrationDomBuilderT ask
  childDelayedRef <- liftIO $ newIORef $ pure ()
  let env' = env
        { _hydrationDomBuilderEnv_parent = Right parentRef
        , _hydrationDomBuilderEnv_delayed = childDelayedRef
        }
  result <- HydrationDomBuilderT $ lift $ runReaderT (unHydrationDomBuilderT child) env'
  wrapResult <- liftIO newEmptyMVar
  let -- Determine if we should skip an element. We currently skip elements for
      -- two reasons:
      -- 1) it was not produced by a static builder which supports hydration
      -- 2) it is explicitly marked to be skipped
      shouldSkip :: DOM.Element -> HydrationRunnerT t m Bool
      shouldSkip e = do
        skip <- hasAttribute e (skipHydrationAttribute :: DOM.JSString)
        hydratable <- hasAttribute e (hydratableAttribute :: DOM.JSString)
        pure $ skip || not hydratable
  childDom <- liftIO $ readIORef childDelayedRef
  let rawCfg = extractRawElementConfig cfg
  doc <- askDocument
  addHydrationStep $ do
    parent <- askParent
    lastHydrationNode <- getPreviousNode
    let go mLastNode = maybe (Node.getFirstChild parent) Node.getNextSibling mLastNode >>= \case
          Nothing -> do -- ran out of nodes, create the element
            HydrationRunnerT $ modify' $ \s -> s { _hydrationState_failed = True }
            e <- makeElement doc elementTag cfg
            insertAfterPreviousNode e
            pure e
          Just node -> DOM.castTo DOM.Element node >>= \case
            Nothing -> go (Just node) -- this node is not an element, skip
            Just e -> shouldSkip e >>= \case
              True -> go (Just node) -- this element should be skipped by hydration
              False -> do
                t <- Element.getTagName e
                -- TODO: check attributes?
                if T.toCaseFold elementTag == T.toCaseFold t
                  then pure e
                  -- we came to some other statically rendered element, so something has gone wrong
                  else do
                    HydrationRunnerT $ modify' $ \s -> s { _hydrationState_failed = True }
                    n <- makeElement doc elementTag cfg
                    insertAfterPreviousNode n
                    pure n
    e <- go lastHydrationNode
    setPreviousNode $ Just $ toNode e
    -- Update the parent node used by the children
    liftIO $ writeIORef parentRef $ toNode e
    liftIO $ writeIORef e' e
    -- Setup events, store the result so we can wait on it later
    refs <- wrap events e rawCfg
    liftIO $ putMVar wrapResult (e, refs)
    localRunner childDom Nothing $ toNode e
  -- We need the EventSelector to switch to the real event handler after activation
  es <- newFanEventWithTrigger $ \(WrapArg en) t -> do
    cleanup <- newEmptyMVar
    threadId <- forkIO $ do
      -- Wait on the data we need from the delayed action
      (e, eventTriggerRefs) <- readMVar wrapResult
      bracketOnError
        -- Run the setup, acquiring the cleanup action
        (triggerBody ctx rawCfg events eventTriggerRefs e (WrapArg en) t)
        -- Run the cleanup, if we have it - but only when an exception is
        -- raised (we might get killed between acquiring the cleanup action
        -- from 'triggerBody' and putting it in the MVar)
        id
        -- Try to put this action into the cleanup MVar
        (putMVar cleanup)
    pure $ do
      tryReadMVar cleanup >>= \case
        Nothing -> killThread threadId
        Just c -> c
  return ((Element es (), result), e')

{-# SPECIALIZE hydrateElement
  :: Text
  -> ElementConfig er DomTimeline HydrationDomSpace
  -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM a
  -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM ((Element er HydrationDomSpace DomTimeline, a), IORef DOM.Element)
  #-}

{-# INLINE inputElementImmediate #-}
inputElementImmediate
  :: ( RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document, EventSpec s ~ GhcjsEventSpec
     , MonadJSM m, Reflex t, MonadReflexCreateTrigger t m, MonadFix m, MonadHold t m
     , MonadRef m, Ref m ~ IORef )
  => InputElementConfig er t s -> HydrationDomBuilderT s t m (InputElement er GhcjsDomSpace t)
inputElementImmediate cfg = do
  (e@(Element eventSelector domElement), ()) <- elementImmediate "input" (_inputElementConfig_elementConfig cfg) $ return ()
  let domInputElement = uncheckedCastTo DOM.HTMLInputElement domElement
  Input.setValue domInputElement $ cfg ^. inputElementConfig_initialValue
  v0 <- Input.getValue domInputElement
  let getMyValue = Input.getValue domInputElement
  valueChangedByUI <- requestDomAction $ liftJSM getMyValue <$ Reflex.select eventSelector (WrapArg Input)
  valueChangedBySetValue <- case _inputElementConfig_setValue cfg of
    Nothing -> return never
    Just eSetValue -> requestDomAction $ ffor eSetValue $ \v' -> do
      Input.setValue domInputElement v'
      getMyValue -- We get the value after setting it in case the browser has mucked with it somehow
  v <- holdDyn v0 $ leftmost
    [ valueChangedBySetValue
    , valueChangedByUI
    ]
  Input.setChecked domInputElement $ _inputElementConfig_initialChecked cfg
  checkedChangedByUI <- wrapDomEvent domInputElement (`on` Events.click) $ do
    Input.getChecked domInputElement
  checkedChangedBySetChecked <- case _inputElementConfig_setChecked cfg of
    Nothing -> return never
    Just eNewchecked -> requestDomAction $ ffor eNewchecked $ \newChecked -> do
      oldChecked <- Input.getChecked domInputElement
      Input.setChecked domInputElement newChecked
      return $ if newChecked /= oldChecked
                  then Just newChecked
                  else Nothing
  c <- holdDyn (_inputElementConfig_initialChecked cfg) $ leftmost
    [ fmapMaybe id checkedChangedBySetChecked
    , checkedChangedByUI
    ]
  hasFocus <- mkHasFocus e
  files <- holdDyn mempty <=< wrapDomEvent domInputElement (`on` Events.change) $ do
    mfiles <- Input.getFiles domInputElement
    let getMyFiles xs = fmap catMaybes . mapM (FileList.item xs) . flip take [0..] . fromIntegral =<< FileList.getLength xs
    maybe (return []) getMyFiles mfiles
  checked <- holdUniqDyn c
  return $ InputElement
    { _inputElement_value = v
    , _inputElement_checked = checked
    , _inputElement_checkedChange =  checkedChangedByUI
    , _inputElement_input = valueChangedByUI
    , _inputElement_hasFocus = hasFocus
    , _inputElement_element = e
    , _inputElement_raw = domInputElement
    , _inputElement_files = files
    }

{-# INLINE inputElementInternal #-}
inputElementInternal
  :: ( MonadJSM m, Reflex t, MonadReflexCreateTrigger t m, MonadFix m, MonadHold t m
     , MonadRef m, Ref m ~ IORef )
  => InputElementConfig er t HydrationDomSpace -> HydrationDomBuilderT HydrationDomSpace t m (InputElement er HydrationDomSpace t)
inputElementInternal cfg = getHydrationMode >>= \case
  HydrationMode_Immediate -> ffor (inputElementImmediate cfg) $ \result -> result
    { _inputElement_element = Element (_element_events $ _inputElement_element result) ()
    , _inputElement_raw = ()
    }
  HydrationMode_Hydrating -> do
  ((e, _), domElementRef) <- hydrateElement "input" (cfg ^. inputElementConfig_elementConfig) $ return ()
  (valueChangedByUI, triggerChangeByUI) <- newTriggerEvent
  (valueChangedBySetValue, triggerChangeBySetValue) <- newTriggerEvent
  (focusChange, triggerFocusChange) <- newTriggerEvent
  (checkedChangedByUI, triggerCheckedChangedByUI) <- newTriggerEvent
  (checkedChangedBySetChecked, triggerCheckedChangedBySetChecked) <- newTriggerEvent
  (fileChange, triggerFileChange) <- newTriggerEvent
  doc <- askDocument
  -- Expected initial value from config
  let v0 = _inputElementConfig_initialValue cfg
      c0 = _inputElementConfig_initialChecked cfg
      valuesAtSwitchover = do
        v <- maybe (pure $ pure v0) (hold v0) (_inputElementConfig_setValue cfg)
        c <- maybe (pure $ pure c0) (hold c0) (_inputElementConfig_setChecked cfg)
        pure (v, c)
  addHydrationStepWithSetup valuesAtSwitchover $ \(switchoverValue', switchoverChecked') -> do
    switchoverValue <- sample switchoverValue'
    switchoverChecked <- sample switchoverChecked'
    domElement <- liftIO $ readIORef domElementRef
    let domInputElement = uncheckedCastTo DOM.HTMLInputElement domElement
        getValue = Input.getValue domInputElement
    -- When the value has been updated by setValue before switchover, we must
    -- send an update here to remain in sync. This is because the later
    -- requestDomAction based on the setValue event will not capture events
    -- happening before postBuild, because this code runs after switchover.
    when (v0 /= switchoverValue) $ liftIO $ triggerChangeBySetValue switchoverValue
    -- The user could have altered the value before switchover. This must be
    -- triggered after the setValue one in order for the events to be in the
    -- correct order.
    liftJSM getValue >>= \realValue -> when (realValue /= switchoverValue) $ liftIO $ triggerChangeByUI realValue
    -- Watch for user interaction and trigger event accordingly
    requestDomAction_ $ (liftJSM getValue >>= liftIO . triggerChangeByUI) <$ Reflex.select (_element_events e) (WrapArg Input)
    for_ (_inputElementConfig_setValue cfg) $ \eSetValue ->
      requestDomAction_ $ ffor eSetValue $ \v' -> do
        Input.setValue domInputElement v'
        v <- getValue -- We get the value after setting it in case the browser has mucked with it somehow
        liftIO $ triggerChangeBySetValue v
    let focusChange' = leftmost
          [ False <$ Reflex.select (_element_events e) (WrapArg Blur)
          , True <$ Reflex.select (_element_events e) (WrapArg Focus)
          ]
    liftIO . triggerFocusChange =<< Node.isSameNode (toNode domElement) . fmap toNode =<< Document.getActiveElement doc
    requestDomAction_ $ liftIO . triggerFocusChange <$> focusChange'
    -- When the checked state has been updated by setChecked before
    -- switchover, we must send an update here to remain in sync. This is
    -- because the later requestDomAction based on the setChecked event will not
    -- capture events happening before postBuild, because this code runs after
    -- switchover.
    when (c0 /= switchoverChecked) $ liftIO $ triggerCheckedChangedBySetChecked switchoverChecked
    -- The user could have clicked the checkbox before switchover, we only
    -- detect cases where they flipped the state. This must be triggered after
    -- the setValue one in order for the events to be in the correct order.
    liftJSM (Input.getChecked domInputElement) >>= \realChecked -> when (realChecked /= switchoverChecked) $
      liftIO $ triggerCheckedChangedByUI realChecked
    _ <- liftJSM $ domInputElement `on` Events.click $ do
      liftIO . triggerCheckedChangedByUI =<< Input.getChecked domInputElement
    for_ (_inputElementConfig_setChecked cfg) $ \eNewchecked ->
      requestDomAction $ ffor eNewchecked $ \newChecked -> do
        oldChecked <- Input.getChecked domInputElement
        Input.setChecked domInputElement newChecked
        when (newChecked /= oldChecked) $ liftIO $ triggerCheckedChangedBySetChecked newChecked
    _ <- liftJSM $ domInputElement `on` Events.change $ do
      mfiles <- Input.getFiles domInputElement
      let getMyFiles xs = fmap catMaybes . mapM (FileList.item xs) . flip take [0..] . fromIntegral =<< FileList.getLength xs
      liftIO . triggerFileChange =<< maybe (return []) getMyFiles mfiles
    return ()
  checked' <- holdDyn c0 $ leftmost
    [ checkedChangedBySetChecked
    , checkedChangedByUI
    ]
  checked <- holdUniqDyn checked'
  let initialFocus = False -- Assume it isn't focused, but we update the actual focus state at switchover
  hasFocus <- holdUniqDyn =<< holdDyn initialFocus focusChange
  v <- holdDyn v0 $ leftmost
    [ valueChangedBySetValue
    , valueChangedByUI
    ]
  files <- holdDyn mempty fileChange
  return $ InputElement
    { _inputElement_value = v
    , _inputElement_checked = checked
    , _inputElement_checkedChange = checkedChangedByUI
    , _inputElement_input = valueChangedByUI
    , _inputElement_hasFocus = hasFocus
    , _inputElement_element = e
    , _inputElement_raw = ()
    , _inputElement_files = files
    }

{-# INLINE textAreaElementImmediate #-}
textAreaElementImmediate
  :: ( RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document, EventSpec s ~ GhcjsEventSpec
     , MonadJSM m, Reflex t, MonadReflexCreateTrigger t m, MonadFix m, MonadHold t m
     , MonadRef m, Ref m ~ IORef )
  => TextAreaElementConfig er t s -> HydrationDomBuilderT s t m (TextAreaElement er GhcjsDomSpace t)
textAreaElementImmediate cfg = do
  (e@(Element eventSelector domElement), _) <- elementImmediate "textarea" (cfg ^. textAreaElementConfig_elementConfig) $ return ()
  let domTextAreaElement = uncheckedCastTo DOM.HTMLTextAreaElement domElement
  TextArea.setValue domTextAreaElement $ cfg ^. textAreaElementConfig_initialValue
  v0 <- TextArea.getValue domTextAreaElement
  let getMyValue = TextArea.getValue domTextAreaElement
  valueChangedByUI <- requestDomAction $ liftJSM getMyValue <$ Reflex.select eventSelector (WrapArg Input)
  valueChangedBySetValue <- case _textAreaElementConfig_setValue cfg of
    Nothing -> return never
    Just eSetValue -> requestDomAction $ ffor eSetValue $ \v' -> do
      TextArea.setValue domTextAreaElement v'
      getMyValue -- We get the value after setting it in case the browser has mucked with it somehow
  v <- holdDyn v0 $ leftmost
    [ valueChangedBySetValue
    , valueChangedByUI
    ]
  hasFocus <- mkHasFocus e
  return $ TextAreaElement
    { _textAreaElement_value = v
    , _textAreaElement_input = valueChangedByUI
    , _textAreaElement_hasFocus = hasFocus
    , _textAreaElement_element = e
    , _textAreaElement_raw = domTextAreaElement
    }

{-# INLINE textAreaElementInternal #-}
textAreaElementInternal
  :: ( MonadJSM m, Reflex t, MonadReflexCreateTrigger t m, MonadFix m, MonadHold t m
     , MonadRef m, Ref m ~ IORef )
    => TextAreaElementConfig er t HydrationDomSpace -> HydrationDomBuilderT HydrationDomSpace t m (TextAreaElement er HydrationDomSpace t)
textAreaElementInternal cfg = getHydrationMode >>= \case
  HydrationMode_Immediate -> ffor (textAreaElementImmediate cfg) $ \result -> result
    { _textAreaElement_element = Element (_element_events $ _textAreaElement_element result) ()
    , _textAreaElement_raw = ()
    }
  HydrationMode_Hydrating -> do
  ((e, _), domElementRef) <- hydrateElement "textarea" (cfg ^. textAreaElementConfig_elementConfig) $ return ()
  (valueChangedByUI, triggerChangeByUI) <- newTriggerEvent
  (valueChangedBySetValue, triggerChangeBySetValue) <- newTriggerEvent
  (focusChange, triggerFocusChange) <- newTriggerEvent
  doc <- askDocument
  -- Expected initial value from config
  let v0 = _textAreaElementConfig_initialValue cfg
      valueAtSwitchover = maybe (pure $ pure v0) (hold v0) (_textAreaElementConfig_setValue cfg)
  addHydrationStepWithSetup valueAtSwitchover $ \switchoverValue' -> do
    switchoverValue <- sample switchoverValue'
    domElement <- liftIO $ readIORef domElementRef
    let domTextAreaElement = uncheckedCastTo DOM.HTMLTextAreaElement domElement
        getValue = TextArea.getValue domTextAreaElement
    -- When the value has been updated by setValue before switchover, we must
    -- send an update here to remain in sync. This is because the later
    -- requestDomAction based on the setValue event will not capture events
    -- happening before postBuild, because this code runs after switchover.
    when (v0 /= switchoverValue) $ liftIO $ triggerChangeBySetValue switchoverValue
    -- The user could have altered the value before switchover. This must be
    -- triggered after the setValue one in order for the events to be in the
    -- correct order.
    liftJSM getValue >>= \realValue -> when (realValue /= switchoverValue) $ liftIO $ triggerChangeByUI realValue
    -- Watch for user interaction and trigger event accordingly
    requestDomAction_ $ (liftJSM getValue >>= liftIO . triggerChangeByUI) <$ Reflex.select (_element_events e) (WrapArg Input)
    for_ (_textAreaElementConfig_setValue cfg) $ \eSetValue ->
      requestDomAction_ $ ffor eSetValue $ \v' -> do
        TextArea.setValue domTextAreaElement v'
        v <- getValue -- We get the value after setting it in case the browser has mucked with it somehow
        liftIO $ triggerChangeBySetValue v
    let focusChange' = leftmost
          [ False <$ Reflex.select (_element_events e) (WrapArg Blur)
          , True <$ Reflex.select (_element_events e) (WrapArg Focus)
          ]
    liftIO . triggerFocusChange =<< Node.isSameNode (toNode domElement) . fmap toNode =<< Document.getActiveElement doc
    requestDomAction_ $ liftIO . triggerFocusChange <$> focusChange'
  let initialFocus = False -- Assume it isn't focused, but we update the actual focus state at switchover
  hasFocus <- holdUniqDyn =<< holdDyn initialFocus focusChange
  v <- holdDyn v0 $ leftmost
    [ valueChangedBySetValue
    , valueChangedByUI
    ]
  return $ TextAreaElement
    { _textAreaElement_value = v
    , _textAreaElement_input = valueChangedByUI
    , _textAreaElement_hasFocus = hasFocus
    , _textAreaElement_element = e
    , _textAreaElement_raw = ()
    }

{-# INLINE selectElementImmediate #-}
selectElementImmediate
  :: ( EventSpec s ~ GhcjsEventSpec, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document
     , MonadJSM m, Reflex t, MonadReflexCreateTrigger t m, MonadFix m, MonadHold t m )
  => SelectElementConfig er t s
  -> HydrationDomBuilderT s t m a
  -> HydrationDomBuilderT s t m (SelectElement er GhcjsDomSpace t, a)
selectElementImmediate cfg child = do
  (e@(Element eventSelector domElement), result) <- elementImmediate "select" (cfg ^. selectElementConfig_elementConfig) child
  let domSelectElement = uncheckedCastTo DOM.HTMLSelectElement domElement
  Select.setValue domSelectElement $ cfg ^. selectElementConfig_initialValue
  v0 <- Select.getValue domSelectElement
  let getMyValue = Select.getValue domSelectElement
  valueChangedByUI <- requestDomAction $ liftJSM getMyValue <$ Reflex.select eventSelector (WrapArg Change)
  valueChangedBySetValue <- case _selectElementConfig_setValue cfg of
    Nothing -> return never
    Just eSetValue -> requestDomAction $ ffor eSetValue $ \v' -> do
      Select.setValue domSelectElement v'
      getMyValue -- We get the value after setting it in case the browser has mucked with it somehow
  v <- holdDyn v0 $ leftmost
    [ valueChangedBySetValue
    , valueChangedByUI
    ]
  hasFocus <- mkHasFocus e
  let wrapped = SelectElement
        { _selectElement_value = v
        , _selectElement_change = valueChangedByUI
        , _selectElement_hasFocus = hasFocus
        , _selectElement_element = e
        , _selectElement_raw = domSelectElement
        }
  return (wrapped, result)

{-# INLINE selectElementInternal #-}
selectElementInternal
  :: ( MonadJSM m, Reflex t, MonadReflexCreateTrigger t m, MonadFix m, MonadHold t m
     , MonadRef m, Ref m ~ IORef )
    => SelectElementConfig er t HydrationDomSpace
    -> HydrationDomBuilderT HydrationDomSpace t m a
    -> HydrationDomBuilderT HydrationDomSpace t m (SelectElement er HydrationDomSpace t, a)
selectElementInternal cfg child = getHydrationMode >>= \case
  HydrationMode_Immediate -> ffor (selectElementImmediate cfg child) $ \(e, result) -> (e
    { _selectElement_element = Element (_element_events $ _selectElement_element e) ()
    , _selectElement_raw = ()
    }, result)
  HydrationMode_Hydrating -> do
  ((e, result), domElementRef) <- hydrateElement "select" (cfg ^. selectElementConfig_elementConfig) child
  (valueChangedByUI, triggerChangeByUI) <- newTriggerEvent
  (valueChangedBySetValue, triggerChangeBySetValue) <- newTriggerEvent
  (focusChange, triggerFocusChange) <- newTriggerEvent
  doc <- askDocument
  -- Expected initial value from config
  let v0 = _selectElementConfig_initialValue cfg
  addHydrationStep $ do
    domElement <- liftIO $ readIORef domElementRef
    let domSelectElement = uncheckedCastTo DOM.HTMLSelectElement domElement
        getValue = Select.getValue domSelectElement
    -- The browser might have messed with the value, or the user could have
    -- altered it before activation, so we set it if it isn't what we expect
    liftJSM getValue >>= \v0' -> do
      when (v0' /= v0) $ liftIO $ triggerChangeByUI v0'
    -- Watch for user interaction and trigger event accordingly
    requestDomAction_ $ (liftJSM getValue >>= liftIO . triggerChangeByUI) <$ Reflex.select (_element_events e) (WrapArg Change)
    for_ (_selectElementConfig_setValue cfg) $ \eSetValue ->
      requestDomAction_ $ ffor eSetValue $ \v' -> do
        Select.setValue domSelectElement v'
        v <- getValue -- We get the value after setting it in case the browser has mucked with it somehow
        liftIO $ triggerChangeBySetValue v
    let focusChange' = leftmost
          [ False <$ Reflex.select (_element_events e) (WrapArg Blur)
          , True <$ Reflex.select (_element_events e) (WrapArg Focus)
          ]
    liftIO . triggerFocusChange =<< Node.isSameNode (toNode domElement) . fmap toNode =<< Document.getActiveElement doc
    requestDomAction_ $ liftIO . triggerFocusChange <$> focusChange'
  let initialFocus = False -- Assume it isn't focused, but we update the actual focus state at switchover
  hasFocus <- holdUniqDyn =<< holdDyn initialFocus focusChange
  v <- holdDyn v0 $ leftmost
    [ valueChangedBySetValue
    , valueChangedByUI
    ]
  return $ (,result) $ SelectElement
    { _selectElement_value = v
    , _selectElement_change = valueChangedByUI
    , _selectElement_hasFocus = hasFocus
    , _selectElement_element = e
    , _selectElement_raw = ()
    }

{-# INLINE textNodeImmediate #-}
textNodeImmediate
  :: (RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document, MonadJSM m, Reflex t, MonadFix m)
  => TextNodeConfig t -> HydrationDomBuilderT s t m DOM.Text
textNodeImmediate (TextNodeConfig !t mSetContents) = do
  p <- getParent
  doc <- askDocument
  n <- createTextNode doc t
  appendChild_ p n
  mapM_ (requestDomAction_ . fmap (setNodeValue n . Just)) mSetContents
  pure n

{-# SPECIALIZE textNodeImmediate
  :: TextNodeConfig DomTimeline
  -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM DOM.Text
  #-}

{-# SPECIALIZE textNodeImmediate
  :: TextNodeConfig DomTimeline
  -> HydrationDomBuilderT GhcjsDomSpace DomTimeline HydrationM DOM.Text
  #-}

{-# INLINE textNodeInternal #-}
textNodeInternal
  :: (Adjustable t m, MonadHold t m, MonadJSM m, MonadFix m, Reflex t)
  => TextNodeConfig t -> HydrationDomBuilderT HydrationDomSpace t m (TextNode HydrationDomSpace t)
textNodeInternal tc@(TextNodeConfig !t mSetContents) = do
  doc <- askDocument
  getHydrationMode >>= \case
    HydrationMode_Immediate -> void $ textNodeImmediate tc
    HydrationMode_Hydrating -> addHydrationStepWithSetup (maybe (pure $ pure t) (hold t) mSetContents) $ \currentText -> do
      n <- hydrateTextNode doc =<< sample currentText
      mapM_ (requestDomAction_ . fmap (setNodeValue n . Just)) mSetContents
  pure $ TextNode ()

{-# SPECIALIZE textNodeInternal
  :: TextNodeConfig DomTimeline
  -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM (TextNode HydrationDomSpace DomTimeline)
  #-}

-- | The static builder mashes adjacent text nodes into one node: we check the
-- text content of each node we come to, comparing it to the content we
-- expect. We also have a special case for empty text nodes - we always create
-- the and add them after the previous node reference.
{-# INLINE hydrateTextNode #-}
hydrateTextNode :: MonadJSM m => Document -> Text -> HydrationRunnerT t m DOM.Text
hydrateTextNode doc t@"" = do
  tn <- createTextNode doc t
  insertAfterPreviousNode tn
  pure tn
hydrateTextNode doc t = do
  n <- join $ go <$> askParent <*> getPreviousNode
  setPreviousNode $ Just $ toNode n
  return n
  where
    go parent mLastNode = maybe (Node.getFirstChild parent) Node.getNextSibling mLastNode >>= \case
      Nothing -> do
        HydrationRunnerT $ modify' $ \s -> s { _hydrationState_failed = True }
        n <- createTextNode doc t
        insertAfterPreviousNode n
        pure n
      Just node -> DOM.castTo DOM.Text node >>= \case
        Nothing -> go parent $ Just node
        Just originalNode -> do
          originalText <- Node.getTextContentUnchecked originalNode
          case T.stripPrefix t originalText of
            Just "" -> return originalNode
            Just _ -> do
              -- If we have the right prefix, we split the text node into a node containing the
              -- required text and a subsequent sibling node containing the rest of the text.
              DOM.splitText_ originalNode $ fromIntegral $ T.length t
              return originalNode
            Nothing -> do
              HydrationRunnerT $ modify' $ \s -> s { _hydrationState_failed = True }
              n <- createTextNode doc t
              insertAfterPreviousNode n
              pure n

{-# INLINE commentNodeImmediate #-}
commentNodeImmediate
  :: (RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document, MonadJSM m, Reflex t, MonadFix m)
  => CommentNodeConfig t -> HydrationDomBuilderT s t m DOM.Comment
commentNodeImmediate (CommentNodeConfig !t mSetContents) = do
  p <- getParent
  doc <- askDocument
  n <- createComment doc t
  appendChild_ p n
  mapM_ (requestDomAction_ . fmap (setNodeValue n . Just)) mSetContents
  pure n

{-# INLINE commentNodeInternal #-}
commentNodeInternal
  :: (Ref m ~ IORef, MonadRef m, PerformEvent t m, MonadReflexCreateTrigger t m, MonadJSM (Performable m), MonadJSM m, MonadFix m, Reflex t, Adjustable t m, MonadHold t m, MonadSample t m)
  => CommentNodeConfig t -> HydrationDomBuilderT HydrationDomSpace t m (CommentNode HydrationDomSpace t)
commentNodeInternal tc@(CommentNodeConfig t0 mSetContents) = do
  doc <- askDocument
  getHydrationMode >>= \case
    HydrationMode_Immediate -> void $ commentNodeInternal tc
    HydrationMode_Hydrating -> addHydrationStepWithSetup (maybe (pure $ pure t0) (hold t0) mSetContents) $ \bt -> do
      t <- sample bt
      void $ hydrateComment doc t mSetContents
  pure $ CommentNode ()

{-# INLINE hydrateComment #-}
hydrateComment :: (MonadJSM m, Reflex t, MonadFix m) => Document -> Text -> Maybe (Event t Text) -> HydrationRunnerT t m DOM.Comment
hydrateComment doc t mSetContents = do
  parent <- askParent
  let go mLastNode = maybe (Node.getFirstChild parent) Node.getNextSibling mLastNode >>= \case
        Nothing -> do
          c <- createComment doc t
          insertAfterPreviousNode c
          pure c
        Just node -> DOM.castTo DOM.Comment node >>= \case
          Nothing -> go (Just node)
          Just c -> do
            t' <- Node.getTextContentUnchecked c
            if t == t'
              then pure c
              else do
                c' <- createComment doc t
                insertAfterPreviousNode c'
                pure c'
  n <- go =<< getPreviousNode
  setPreviousNode $ Just $ toNode n
  mapM_ (requestDomAction_ . fmap (setNodeValue n . Just)) mSetContents
  pure n

-- | We leave markers in the static builder as comments, and rip these comments
-- out at hydration time, replacing them with empty text nodes.
{-# INLINABLE skipToAndReplaceComment #-}
skipToAndReplaceComment
  :: (MonadJSM m, Reflex t, MonadFix m, Adjustable t m, MonadHold t m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document)
  => Text
  -> IORef (Maybe Text)
  -> HydrationDomBuilderT s t m (HydrationRunnerT t m (), IORef DOM.Text, IORef (Maybe Text))
skipToAndReplaceComment prefix key0Ref = getHydrationMode >>= \case
  HydrationMode_Immediate -> do
    -- If we're in immediate mode, we don't try to replace an existing comment,
    -- and just return a dummy key
    t <- textNodeImmediate $ TextNodeConfig ("" :: Text) Nothing
    append $ toNode t
    textNodeRef <- liftIO $ newIORef t
    keyRef <- liftIO $ newIORef Nothing
    pure (pure (), textNodeRef, keyRef)
  HydrationMode_Hydrating -> do
    doc <- askDocument
    textNodeRef <- liftIO $ newIORef $ error "textNodeRef not yet initialized"
    keyRef <- liftIO $ newIORef $ error "keyRef not yet initialized"
    let
      go Nothing _ = do
        tn <- createTextNode doc ("" :: Text)
        insertAfterPreviousNode tn
        HydrationRunnerT $ modify' $ \s -> s { _hydrationState_failed = True }
        pure (tn, Nothing)
      go (Just key0) mLastNode = do
        parent <- askParent
        maybe (Node.getFirstChild parent) Node.getNextSibling mLastNode >>= \case
          Nothing -> go Nothing Nothing
          Just node -> DOM.castTo DOM.Comment node >>= \case
            Just comment -> do
              commentText <- fromMaybe (error "Cannot get text content of comment node") <$> Node.getTextContent comment
              case T.stripPrefix (prefix <> key0) commentText of -- 'key0' may be @""@ in which case we're just finding the actual key; TODO: Don't be clever.
                Just key -> do
                  -- Replace the comment with an (invisible) text node
                  tn <- createTextNode doc ("" :: Text)
                  Node.replaceChild_ parent tn comment
                  pure (tn, Just key)
                Nothing -> do
                  go (Just key0) (Just node)
            Nothing -> do
              go (Just key0) (Just node)
      switchComment = do
        key0 <- liftIO $ readIORef key0Ref
        (tn, key) <- go key0 =<< getPreviousNode
        setPreviousNode $ Just $ toNode tn
        liftIO $ do
          writeIORef textNodeRef tn
          writeIORef keyRef key
    pure (switchComment, textNodeRef, keyRef)

{-# INLINABLE skipToReplaceStart #-}
skipToReplaceStart :: (MonadJSM m, Reflex t, MonadFix m, Adjustable t m, MonadHold t m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document) => HydrationDomBuilderT s t m (HydrationRunnerT t m (), IORef DOM.Text, IORef (Maybe Text))
skipToReplaceStart = skipToAndReplaceComment "replace-start" =<< liftIO (newIORef $ Just "") -- TODO: Don't rely on clever usage @""@ to make this work.

{-# INLINABLE skipToReplaceEnd #-}
skipToReplaceEnd :: (MonadJSM m, Reflex t, MonadFix m, Adjustable t m, MonadHold t m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document) => IORef (Maybe Text) -> HydrationDomBuilderT s t m (HydrationRunnerT t m (), IORef DOM.Text)
skipToReplaceEnd key = fmap (\(m,e,_) -> (m,e)) $ skipToAndReplaceComment "replace-end" key

instance SupportsHydrationDomBuilder t m => NotReady t (HydrationDomBuilderT s t m) where
  notReadyUntil e = do
    eOnce <- headE e
    unreadyChildren <- askUnreadyChildren
    commitAction <- askCommitAction
    liftIO $ modifyIORef' unreadyChildren succ
    let ready = do
          old <- liftIO $ readIORef unreadyChildren
          let new = pred old
          liftIO $ writeIORef unreadyChildren $! new
          when (new == 0) commitAction
    requestDomAction_ $ ready <$ eOnce
  notReady = do
    unreadyChildren <- askUnreadyChildren
    liftIO $ modifyIORef' unreadyChildren succ

data HydrationDomSpace

instance DomSpace HydrationDomSpace where
  type EventSpec HydrationDomSpace = GhcjsEventSpec
  type RawDocument HydrationDomSpace = DOM.Document
  type RawTextNode HydrationDomSpace = ()
  type RawCommentNode HydrationDomSpace = ()
  type RawElement HydrationDomSpace = ()
  type RawInputElement HydrationDomSpace = ()
  type RawTextAreaElement HydrationDomSpace = ()
  type RawSelectElement HydrationDomSpace = ()
  addEventSpecFlags _ en f es = es
    { _ghcjsEventSpec_filters =
        let f' = Just . GhcjsEventFilter . \case
              Nothing -> \evt -> do
                mEventResult <- unGhcjsEventHandler (_ghcjsEventSpec_handler es) (en, evt)
                return (f mEventResult, return mEventResult)
              Just (GhcjsEventFilter oldFilter) -> \evt -> do
                (oldFlags, oldContinuation) <- oldFilter evt
                mEventResult <- oldContinuation
                let newFlags = oldFlags <> f mEventResult
                return (newFlags, return mEventResult)
        in DMap.alter f' en $ _ghcjsEventSpec_filters es
    }

instance SupportsHydrationDomBuilder t m => DomBuilder t (HydrationDomBuilderT HydrationDomSpace t m) where
  type DomBuilderSpace (HydrationDomBuilderT HydrationDomSpace t m) = HydrationDomSpace
  {-# INLINABLE element #-}
  element = elementInternal
  {-# INLINABLE textNode #-}
  textNode = textNodeInternal
  {-# INLINABLE commentNode #-}
  commentNode = commentNodeInternal
  {-# INLINABLE inputElement #-}
  inputElement = inputElementInternal
  {-# INLINABLE textAreaElement #-}
  textAreaElement = textAreaElementInternal
  {-# INLINABLE selectElement #-}
  selectElement = selectElementInternal
  placeRawElement () = pure ()
  wrapRawElement () _cfg = pure $ Element (EventSelector $ const never) ()

instance SupportsHydrationDomBuilder t m => DomBuilder t (HydrationDomBuilderT GhcjsDomSpace t m) where
  type DomBuilderSpace (HydrationDomBuilderT GhcjsDomSpace t m) = GhcjsDomSpace
  {-# INLINABLE element #-}
  element = elementImmediate
  {-# INLINABLE textNode #-}
  textNode = fmap TextNode . textNodeImmediate
  {-# INLINABLE commentNode #-}
  commentNode = fmap CommentNode . commentNodeImmediate
  {-# INLINABLE inputElement #-}
  inputElement = inputElementImmediate
  {-# INLINABLE textAreaElement #-}
  textAreaElement = textAreaElementImmediate
  {-# INLINABLE selectElement #-}
  selectElement = selectElementImmediate
  placeRawElement = append . toNode
  wrapRawElement e rawCfg = do
    events <- askEvents
    ctx <- askJSM
    eventTriggerRefs <- wrap events e rawCfg
    es <- newFanEventWithTrigger $ triggerBody ctx rawCfg events eventTriggerRefs e
    pure $ Element es e

data FragmentState
  = FragmentState_Unmounted
  | FragmentState_Mounted (DOM.Text, DOM.Text)

data ImmediateDomFragment = ImmediateDomFragment
  { _immediateDomFragment_document :: DOM.DocumentFragment
  , _immediateDomFragment_state :: IORef FragmentState
  }

extractFragment :: MonadJSM m => ImmediateDomFragment -> m ()
extractFragment fragment = do
  state <- liftIO $ readIORef $ _immediateDomFragment_state fragment
  case state of
    FragmentState_Unmounted -> return ()
    FragmentState_Mounted (before, after) -> do
      extractBetweenExclusive (_immediateDomFragment_document fragment) before after
      liftIO $ writeIORef (_immediateDomFragment_state fragment) FragmentState_Unmounted

instance SupportsHydrationDomBuilder t m => MountableDomBuilder t (HydrationDomBuilderT GhcjsDomSpace t m) where
  type DomFragment (HydrationDomBuilderT GhcjsDomSpace t m) = ImmediateDomFragment
  buildDomFragment w = do
    df <- createDocumentFragment =<< askDocument
    result <- flip localEnv w $ \env -> env
      { _hydrationDomBuilderEnv_parent = Left $ toNode df
      }
    state <- liftIO $ newIORef FragmentState_Unmounted
    return (ImmediateDomFragment df state, result)
  mountDomFragment fragment setFragment = do
    parent <- getParent
    extractFragment fragment
    before <- textNodeImmediate $ TextNodeConfig ("" :: Text) Nothing
    appendChild_ parent $ _immediateDomFragment_document fragment
    after <- textNodeImmediate $ TextNodeConfig ("" :: Text) Nothing
    xs <- foldDyn (\new (previous, _) -> (new, Just previous)) (fragment, Nothing) setFragment
    requestDomAction_ $ ffor (updated xs) $ \(childFragment, Just previousFragment) -> do
      extractFragment previousFragment
      extractFragment childFragment
      insertBefore (_immediateDomFragment_document childFragment) after
      liftIO $ writeIORef (_immediateDomFragment_state childFragment) $ FragmentState_Mounted (before, after)
    liftIO $ writeIORef (_immediateDomFragment_state fragment) $ FragmentState_Mounted (before, after)

instance (Reflex t, Monad m, Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (DomRenderHookT t m) where
  runWithReplace a0 a' = DomRenderHookT $ runWithReplace (unDomRenderHookT a0) (fmapCheap unDomRenderHookT a')
  traverseIntMapWithKeyWithAdjust f m = DomRenderHookT . traverseIntMapWithKeyWithAdjust (\k -> unDomRenderHookT . f k) m
  traverseDMapWithKeyWithAdjust f m = DomRenderHookT . traverseDMapWithKeyWithAdjust (\k -> unDomRenderHookT . f k) m
  traverseDMapWithKeyWithAdjustWithMove f m = DomRenderHookT . traverseDMapWithKeyWithAdjustWithMove (\k -> unDomRenderHookT . f k) m

instance (Adjustable t m, MonadJSM m, MonadHold t m, MonadFix m, PrimMonad m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document) => Adjustable t (HydrationDomBuilderT s t m) where
  {-# INLINABLE runWithReplace #-}
  runWithReplace a0 a' = do
    initialEnv <- HydrationDomBuilderT ask
    let hydrating = _hydrationDomBuilderEnv_hydrationMode initialEnv
    (hydrateStart, before, beforeKey) <- skipToReplaceStart
    let parentUnreadyChildren = _hydrationDomBuilderEnv_unreadyChildren initialEnv
    haveEverBeenReady <- liftIO $ newIORef False
    currentCohort <- liftIO $ newIORef (-1 :: Int) -- Equal to the cohort currently in the DOM
    let myCommitAction = do
          liftIO (readIORef haveEverBeenReady) >>= \case
            True -> return ()
            False -> do
              liftIO $ writeIORef haveEverBeenReady True
              old <- liftIO $ readIORef parentUnreadyChildren
              let new = pred old
              liftIO $ writeIORef parentUnreadyChildren $! new
              when (new == 0) $ _hydrationDomBuilderEnv_commitAction initialEnv
    -- We draw 'after' in this roundabout way to avoid using MonadFix
    doc <- askDocument
    parent <- getParent
    (hydrateEnd, after) <- skipToReplaceEnd beforeKey
    let drawInitialChild = do
          h <- liftIO $ readIORef hydrating
          p' <- case h of
            HydrationMode_Hydrating -> pure parent
            HydrationMode_Immediate -> toNode <$> createDocumentFragment doc
          unreadyChildren <- liftIO $ newIORef 0
          let a0' = case h of
                HydrationMode_Hydrating -> a0
                HydrationMode_Immediate -> do
                  a <- a0
                  insertBefore p' =<< liftIO (readIORef after)
                  pure a
          delayed <- case h of
            HydrationMode_Hydrating -> liftIO $ newIORef $ pure ()
            HydrationMode_Immediate -> pure $ _hydrationDomBuilderEnv_delayed initialEnv
          result <- runReaderT (unHydrationDomBuilderT a0') initialEnv
            { _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
            , _hydrationDomBuilderEnv_commitAction = myCommitAction
            , _hydrationDomBuilderEnv_parent = Left p'
            , _hydrationDomBuilderEnv_delayed = delayed
            }
          dom <- case h of
            HydrationMode_Hydrating -> liftIO $ readIORef delayed
            HydrationMode_Immediate -> pure $ pure ()
          liftIO $ readIORef unreadyChildren >>= \case
            0 -> writeIORef haveEverBeenReady True
            _ -> modifyIORef' parentUnreadyChildren succ
          return (dom, result)
    a'' <- numberOccurrences a'
    ((hydrate0, result0), child') <- HydrationDomBuilderT $ lift $ runWithReplace drawInitialChild $ ffor a'' $ \(cohortId, child) -> do
      h <- liftIO $ readIORef hydrating
      p' <- case h of
        HydrationMode_Hydrating -> pure parent
        HydrationMode_Immediate -> toNode <$> createDocumentFragment doc
      unreadyChildren <- liftIO $ newIORef 0
      let commitAction = do
            c <- liftIO $ readIORef currentCohort
            when (c <= cohortId) $ do -- If a newer cohort has already been committed, just ignore this
              !before' <- liftIO $ readIORef before
              !after' <- liftIO $ readIORef after
              deleteBetweenExclusive before' after'
              insertBefore p' after'
              liftIO $ writeIORef currentCohort cohortId
              myCommitAction
      delayed <- case h of
        HydrationMode_Hydrating -> liftIO $ newIORef $ pure ()
        HydrationMode_Immediate -> pure $ _hydrationDomBuilderEnv_delayed initialEnv
      result <- runReaderT (unHydrationDomBuilderT child) $ initialEnv
            { _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
            , _hydrationDomBuilderEnv_commitAction = case h of
              HydrationMode_Hydrating -> myCommitAction
              HydrationMode_Immediate -> commitAction
            , _hydrationDomBuilderEnv_parent = Left p'
            , _hydrationDomBuilderEnv_delayed = delayed
            }
      dom <- case h of
        HydrationMode_Hydrating -> liftIO $ readIORef delayed
        HydrationMode_Immediate -> pure $ pure ()
      uc <- liftIO $ readIORef unreadyChildren
      let commitActionToRunNow = if uc == 0
            then Just $ commitAction
            else Nothing -- A child will run it when unreadyChildren is decremented to 0
          actions = case h of
            HydrationMode_Hydrating -> Left dom
            HydrationMode_Immediate -> Right commitActionToRunNow
      return (actions, result)
    let (hydrate', commitAction) = fanEither $ fmap fst child'
    addHydrationStepWithSetup (hold hydrate0 hydrate') $ \contents -> do
      hydrateStart
      join $ sample contents
      hydrateEnd
    requestDomAction_ $ fmapMaybe id commitAction
    return (result0, snd <$> child')

  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust = traverseIntMapWithKeyWithAdjust'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust = traverseDMapWithKeyWithAdjust'
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove = do
    let updateChildUnreadiness (p :: PatchDMapWithMove k (Compose (TraverseChild t m (Some k)) v')) old = do
          let new :: forall a. k a -> PatchDMapWithMove.NodeInfo k (Compose (TraverseChild t m (Some k)) v') a -> IO (PatchDMapWithMove.NodeInfo k (Constant (IORef (ChildReadyState (Some k)))) a)
              new k = PatchDMapWithMove.nodeInfoMapFromM $ \case
                PatchDMapWithMove.From_Insert (Compose (TraverseChild (Left _hydration) _)) -> return PatchDMapWithMove.From_Delete
                PatchDMapWithMove.From_Insert (Compose (TraverseChild (Right immediate) _)) -> do
                  readIORef (_traverseChildImmediate_childReadyState immediate) >>= \case
                    ChildReadyState_Ready -> return PatchDMapWithMove.From_Delete
                    ChildReadyState_Unready _ -> do
                      writeIORef (_traverseChildImmediate_childReadyState immediate) $ ChildReadyState_Unready $ Just $ Some k
                      return $ PatchDMapWithMove.From_Insert $ Constant (_traverseChildImmediate_childReadyState immediate)
                PatchDMapWithMove.From_Delete -> return PatchDMapWithMove.From_Delete
                PatchDMapWithMove.From_Move fromKey -> return $ PatchDMapWithMove.From_Move fromKey
              deleteOrMove :: forall a. k a -> Product (Constant (IORef (ChildReadyState (Some k)))) (ComposeMaybe k) a -> IO (Constant () a)
              deleteOrMove _ (Pair (Constant sRef) (ComposeMaybe mToKey)) = do
                writeIORef sRef $ ChildReadyState_Unready $ Some <$> mToKey -- This will be Nothing if deleting, and Just if moving, so it works out in both cases
                return $ Constant ()
          p' <- fmap unsafePatchDMapWithMove $ DMap.traverseWithKey new $ unPatchDMapWithMove p
          _ <- DMap.traverseWithKey deleteOrMove $ PatchDMapWithMove.getDeletionsAndMoves p old
          return $ applyAlways p' old
    hoistTraverseWithKeyWithAdjust traverseDMapWithKeyWithAdjustWithMove mapPatchDMapWithMove updateChildUnreadiness $ \placeholders lastPlaceholder (p_ :: PatchDMapWithMove k (Compose (TraverseChild t m (Some k)) v')) -> do
      let p = unPatchDMapWithMove p_
      phsBefore <- liftIO $ readIORef placeholders
      let collectIfMoved :: forall a. k a -> PatchDMapWithMove.NodeInfo k (Compose (TraverseChild t m (Some k)) v') a -> JSM (Constant (Maybe DOM.DocumentFragment) a)
          collectIfMoved k e = do
            let mThisPlaceholder = Map.lookup (Some k) phsBefore -- Will be Nothing if this element wasn't present before
                nextPlaceholder = maybe lastPlaceholder snd $ Map.lookupGT (Some k) phsBefore
            case isJust $ getComposeMaybe $ PatchDMapWithMove._nodeInfo_to e of
              False -> do
                mapM_ (`deleteUpTo` nextPlaceholder) mThisPlaceholder
                return $ Constant Nothing
              True -> do
                Constant <$> mapM (`collectUpTo` nextPlaceholder) mThisPlaceholder
      collected <- DMap.traverseWithKey collectIfMoved p
      let !phsAfter = fromMaybe phsBefore $ apply filtered phsBefore
          weakened :: PatchMapWithMove (Some k) (Either (TraverseChildHydration t m) (TraverseChildImmediate (Some k)))
          weakened = weakenPatchDMapWithMoveWith (_traverseChild_mode . getCompose) p_
          filtered :: PatchMapWithMove (Some k) DOM.Text
          filtered = PatchMapWithMove $ flip Map.mapMaybe (unPatchMapWithMove weakened) $ \(PatchMapWithMove.NodeInfo from to) -> flip PatchMapWithMove.NodeInfo to <$> case from of
            PatchMapWithMove.From_Insert (Left _hydration) -> Nothing
            PatchMapWithMove.From_Insert (Right immediate) -> Just $ PatchMapWithMove.From_Insert $ _traverseChildImmediate_placeholder immediate
            PatchMapWithMove.From_Delete -> Just $ PatchMapWithMove.From_Delete
            PatchMapWithMove.From_Move k -> Just $ PatchMapWithMove.From_Move k
      let placeFragment :: forall a. k a -> PatchDMapWithMove.NodeInfo k (Compose (TraverseChild t m (Some k)) v') a -> JSM (Constant () a)
          placeFragment k e = do
            let nextPlaceholder = maybe lastPlaceholder snd $ Map.lookupGT (Some k) phsAfter
            case PatchDMapWithMove._nodeInfo_from e of
              PatchDMapWithMove.From_Insert (Compose (TraverseChild x _)) -> case x of
                Left _ -> pure ()
                Right immediate -> _traverseChildImmediate_fragment immediate `insertBefore` nextPlaceholder
              PatchDMapWithMove.From_Delete -> do
                return ()
              PatchDMapWithMove.From_Move fromKey -> do
                Just (Constant mdf) <- return $ DMap.lookup fromKey collected
                mapM_ (`insertBefore` nextPlaceholder) mdf
            return $ Constant ()
      mapM_ (\(k :=> v) -> void $ placeFragment k v) $ DMap.toDescList p -- We need to go in reverse order here, to make sure the placeholders are in the right spot at the right time
      liftIO $ writeIORef placeholders $! phsAfter

{-# INLINABLE traverseDMapWithKeyWithAdjust' #-}
traverseDMapWithKeyWithAdjust'
  :: forall s t m (k :: * -> *) v v'. (Adjustable t m, MonadHold t m, MonadFix m, MonadJSM m, PrimMonad m, GCompare k, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document)
  => (forall a. k a -> v a -> HydrationDomBuilderT s t m (v' a))
  -> DMap k v
  -> Event t (PatchDMap k v)
  -> HydrationDomBuilderT s t m (DMap k v', Event t (PatchDMap k v'))
traverseDMapWithKeyWithAdjust' = do
  let updateChildUnreadiness (p :: PatchDMap k (Compose (TraverseChild t m (Some k)) v')) old = do
        let new :: forall a. k a -> ComposeMaybe (Compose (TraverseChild t m (Some k)) v') a -> IO (ComposeMaybe (Constant (IORef (ChildReadyState (Some k)))) a)
            new k (ComposeMaybe m) = ComposeMaybe <$> case m of
              Nothing -> return Nothing
              Just (Compose (TraverseChild (Left _hydration) _)) -> pure Nothing
              Just (Compose (TraverseChild (Right immediate) _)) -> do
                readIORef (_traverseChildImmediate_childReadyState immediate) >>= \case
                  ChildReadyState_Ready -> return Nothing -- Delete this child, since it's ready
                  ChildReadyState_Unready _ -> do
                    writeIORef (_traverseChildImmediate_childReadyState immediate) $ ChildReadyState_Unready $ Just $ Some k
                    return $ Just $ Constant (_traverseChildImmediate_childReadyState immediate)
            delete _ (Constant sRef) = do
              writeIORef sRef $ ChildReadyState_Unready Nothing
              return $ Constant ()
        p' <- fmap PatchDMap $ DMap.traverseWithKey new $ unPatchDMap p
        _ <- DMap.traverseWithKey delete $ PatchDMap.getDeletions p old
        return $ applyAlways p' old
  hoistTraverseWithKeyWithAdjust traverseDMapWithKeyWithAdjust mapPatchDMap updateChildUnreadiness $ \placeholders lastPlaceholder (PatchDMap patch) -> do
    phs <- liftIO $ readIORef placeholders
    forM_ (DMap.toList patch) $ \(k :=> ComposeMaybe mv) -> do
      let nextPlaceholder = maybe lastPlaceholder snd $ Map.lookupGT (Some k) phs
      -- Delete old node
      forM_ (Map.lookup (Some k) phs) $ \thisPlaceholder -> do
        thisPlaceholder `deleteUpTo` nextPlaceholder
      -- Insert new node
      forM_ mv $ \(Compose (TraverseChild e _)) -> case e of
        Left _hydration -> pure ()
        Right immediate -> do
          _traverseChildImmediate_fragment immediate `insertBefore` nextPlaceholder
    let weakened :: PatchMap (Some k) (Either (TraverseChildHydration t m) (TraverseChildImmediate (Some k)))
        weakened = weakenPatchDMapWith (_traverseChild_mode . getCompose) $ PatchDMap patch
        filtered :: PatchMap (Some k) DOM.Text
        filtered = PatchMap $ flip Map.mapMaybe (unPatchMap weakened) $ \case
          Nothing -> Just Nothing -- deletion
          Just (Left _) -> Nothing
          Just (Right immediate) -> Just $ Just $ _traverseChildImmediate_placeholder immediate
    liftIO $ writeIORef placeholders $! fromMaybe phs $ apply filtered phs

{-# INLINABLE traverseIntMapWithKeyWithAdjust' #-}
traverseIntMapWithKeyWithAdjust'
  :: forall s t m v v'. (Adjustable t m, MonadJSM m, MonadFix m, PrimMonad m, MonadHold t m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document)
  => (IntMap.Key -> v -> HydrationDomBuilderT s t m v')
  -> IntMap v
  -> Event t (PatchIntMap v)
  -> HydrationDomBuilderT s t m (IntMap v', Event t (PatchIntMap v'))
traverseIntMapWithKeyWithAdjust' = do
  let updateChildUnreadiness (p@(PatchIntMap pInner) :: PatchIntMap (TraverseChild t m Int v')) old = do
        let new :: IntMap.Key -> Maybe (TraverseChild t m Int v') -> IO (Maybe (IORef (ChildReadyState Int)))
            new k m = case m of
              Nothing -> return Nothing
              Just (TraverseChild (Left _hydration) _) -> pure Nothing
              Just (TraverseChild (Right immediate) _) -> do
                let sRef = _traverseChildImmediate_childReadyState immediate
                readIORef sRef >>= \case
                  ChildReadyState_Ready -> return Nothing -- Delete this child, since it's ready
                  ChildReadyState_Unready _ -> do
                    writeIORef sRef $ ChildReadyState_Unready $ Just k
                    return $ Just sRef
            delete _ sRef = do
              writeIORef sRef $ ChildReadyState_Unready Nothing
              return ()
        p' <- PatchIntMap <$> IntMap.traverseWithKey new pInner
        _ <- IntMap.traverseWithKey delete $ FastMutableIntMap.getDeletions p old
        return $ applyAlways p' old
  hoistTraverseIntMapWithKeyWithAdjust traverseIntMapWithKeyWithAdjust updateChildUnreadiness $ \placeholders lastPlaceholder (PatchIntMap p) -> do
    phs <- liftIO $ readIORef placeholders
    forM_ (IntMap.toList p) $ \(k, mv) -> do
      let nextPlaceholder = maybe lastPlaceholder snd $ IntMap.lookupGT k phs
      -- Delete old node
      forM_ (IntMap.lookup k phs) $ \thisPlaceholder -> thisPlaceholder `deleteUpTo` nextPlaceholder
      -- Insert new node
      forM_ mv $ \(TraverseChild e _) -> case e of
        Left _hydration -> pure ()
        Right immediate -> do
          _traverseChildImmediate_fragment immediate `insertBefore` nextPlaceholder
    let filtered :: PatchIntMap DOM.Text
        filtered = PatchIntMap $ flip IntMap.mapMaybe p $ \case
          Nothing -> Just Nothing -- deletion
          Just tc
            | Right immediate <- _traverseChild_mode tc -> Just $ Just $ _traverseChildImmediate_placeholder immediate
            | otherwise -> Nothing
    liftIO $ writeIORef placeholders $! fromMaybe phs $ apply filtered phs

{-# SPECIALIZE traverseIntMapWithKeyWithAdjust'
  :: (IntMap.Key -> v -> HydrationDomBuilderT GhcjsDomSpace DomTimeline HydrationM v')
  -> IntMap v
  -> Event DomTimeline (PatchIntMap v)
  -> HydrationDomBuilderT GhcjsDomSpace DomTimeline HydrationM (IntMap v', Event DomTimeline (PatchIntMap v'))
  #-}

{-# SPECIALIZE traverseIntMapWithKeyWithAdjust'
  :: (IntMap.Key -> v -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM v')
  -> IntMap v
  -> Event DomTimeline (PatchIntMap v)
  -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM (IntMap v', Event DomTimeline (PatchIntMap v'))
  #-}

data ChildReadyState a
   = ChildReadyState_Ready
   | ChildReadyState_Unready !(Maybe a)
   deriving (Show, Read, Eq, Ord)

insertAfterPreviousNode :: (Monad m, MonadJSM m) => DOM.IsNode node => node -> HydrationRunnerT t m ()
insertAfterPreviousNode node = do
  parent <- askParent
  nextNode <- maybe (Node.getFirstChild parent) Node.getNextSibling =<< getPreviousNode
  Node.insertBefore_ parent node nextNode
  setPreviousNode $ Just $ toNode node

{-# INLINABLE hoistTraverseWithKeyWithAdjust #-}
hoistTraverseWithKeyWithAdjust
  ::
  ( Adjustable t m
  , MonadHold t m
  , GCompare k
  , MonadIO m
  , MonadJSM m
  , PrimMonad m
  , MonadFix m
  , Patch (p k v)
  , Patch (p k (Constant Int))
  , PatchTarget (p k (Constant Int)) ~ DMap k (Constant Int)
  , Patch (p k (Compose (TraverseChild t m (Some k)) v'))
  , PatchTarget (p k (Compose (TraverseChild t m (Some k)) v')) ~ DMap k (Compose (TraverseChild t m (Some k)) v')
  , Monoid (p k (Compose (TraverseChild t m (Some k)) v'))
  , RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document
  )
  => (forall vv vv'.
         (forall a. k a -> vv a -> DomRenderHookT t m (vv' a))
      -> DMap k vv
      -> Event t (p k vv)
      -> DomRenderHookT t m (DMap k vv', Event t (p k vv')))
  -- ^ The base monad's traversal
  -> (forall vv vv'. (forall a. vv a -> vv' a) -> p k vv -> p k vv')
  -- ^ A way of mapping over the patch type
  -> (p k (Compose (TraverseChild t m (Some k)) v') -> DMap k (Constant (IORef (ChildReadyState (Some k)))) -> IO (DMap k (Constant (IORef (ChildReadyState (Some k))))))
  -- ^ Given a patch for the children DOM elements, produce a patch for the childrens' unreadiness state
  -> (IORef (Map.Map (Some k) DOM.Text) -> DOM.Text -> p k (Compose (TraverseChild t m (Some k)) v') -> JSM ())
  -- ^ Apply a patch to the DOM
  -> (forall a. k a -> v a -> HydrationDomBuilderT s t m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> HydrationDomBuilderT s t m (DMap k v', Event t (p k v'))
hoistTraverseWithKeyWithAdjust base mapPatch updateChildUnreadiness applyDomUpdate_ f dm0 dm' = do
  doc <- askDocument
  initialEnv <- HydrationDomBuilderT ask
  let parentUnreadyChildren = _hydrationDomBuilderEnv_unreadyChildren initialEnv
  pendingChange :: IORef (DMap k (Constant (IORef (ChildReadyState (Some k)))), p k (Compose (TraverseChild t m (Some k)) v')) <- liftIO $ newIORef mempty
  haveEverBeenReady <- liftIO $ newIORef False
  placeholders <- liftIO $ newIORef Map.empty
  lastPlaceholder <- createTextNode doc ("" :: Text)
  let applyDomUpdate p = do
        applyDomUpdate_ placeholders lastPlaceholder p
        markSelfReady
        liftIO $ writeIORef pendingChange $! mempty
      markSelfReady = do
        liftIO (readIORef haveEverBeenReady) >>= \case
          True -> return ()
          False -> do
            liftIO $ writeIORef haveEverBeenReady True
            old <- liftIO $ readIORef parentUnreadyChildren
            let new = pred old
            liftIO $ writeIORef parentUnreadyChildren $! new
            when (new == 0) $ _hydrationDomBuilderEnv_commitAction initialEnv
      markChildReady :: IORef (ChildReadyState (Some k)) -> JSM ()
      markChildReady childReadyState = do
        liftIO (readIORef childReadyState) >>= \case
          ChildReadyState_Ready -> return ()
          ChildReadyState_Unready countedAt -> do
            liftIO $ writeIORef childReadyState ChildReadyState_Ready
            case countedAt of
              Nothing -> return ()
              Just (Some k) -> do -- This child has been counted as unready, so we need to remove it from the unready set
                (oldUnready, p) <- liftIO $ readIORef pendingChange
                when (not $ DMap.null oldUnready) $ do -- This shouldn't actually ever be null
                  let newUnready = DMap.delete k oldUnready
                  liftIO $ writeIORef pendingChange (newUnready, p)
                  when (DMap.null newUnready) $ do
                    applyDomUpdate p
  (children0 :: DMap k (Compose (TraverseChild t m (Some k)) v'), children' :: Event t (p k (Compose (TraverseChild t m (Some k)) v')))
    <- HydrationDomBuilderT $ lift $ base (\k v -> drawChildUpdate initialEnv markChildReady $ f k v) dm0 dm'
  let processChild k (Compose (TraverseChild e _)) = case e of
        Left _ -> pure $ ComposeMaybe Nothing
        Right immediate -> ComposeMaybe <$> do
          readIORef (_traverseChildImmediate_childReadyState immediate) >>= \case
            ChildReadyState_Ready -> return Nothing
            ChildReadyState_Unready _ -> do
              writeIORef (_traverseChildImmediate_childReadyState immediate) $ ChildReadyState_Unready $ Just $ Some k
              return $ Just $ Constant (_traverseChildImmediate_childReadyState immediate)
  initialUnready <- liftIO $ DMap.mapMaybeWithKey (\_ -> getComposeMaybe) <$> DMap.traverseWithKey processChild children0
  liftIO $ if DMap.null initialUnready
    then writeIORef haveEverBeenReady True
    else do
      modifyIORef' parentUnreadyChildren succ
      writeIORef pendingChange (initialUnready, mempty) -- The patch is always empty because it got applied implicitly when we ran the children the first time
  getHydrationMode >>= \case
    HydrationMode_Hydrating -> addHydrationStepWithSetup (holdIncremental children0 children') $ \children -> do
      dm :: DMap k (Compose (TraverseChild t m (Some k)) v') <- sample $ currentIncremental children
      phs <- traverse id $ weakenDMapWith (either _traverseChildHydration_delayed (pure . _traverseChildImmediate_placeholder) . _traverseChild_mode . getCompose) dm
      liftIO $ writeIORef placeholders $! phs
      insertAfterPreviousNode lastPlaceholder
    HydrationMode_Immediate -> do
      let activate i = do
            append $ toNode $ _traverseChildImmediate_fragment i
            pure $ _traverseChildImmediate_placeholder i
      phs <- traverse id $ weakenDMapWith (either (error "impossible") activate . _traverseChild_mode . getCompose) children0
      liftIO $ writeIORef placeholders $! phs
      append $ toNode lastPlaceholder
  requestDomAction_ $ ffor children' $ \p -> do
    (oldUnready, oldP) <- liftIO $ readIORef pendingChange
    newUnready <- liftIO $ updateChildUnreadiness p oldUnready
    let !newP = p <> oldP
    liftIO $ writeIORef pendingChange (newUnready, newP)
    when (DMap.null newUnready) $ do
      applyDomUpdate newP
  let result0 = DMap.map (_traverseChild_result . getCompose) children0
      result' = ffor children' $ mapPatch $ _traverseChild_result . getCompose
  return (result0, result')

{-# INLINE hoistTraverseIntMapWithKeyWithAdjust #-}
hoistTraverseIntMapWithKeyWithAdjust ::
  ( Adjustable t m
  , MonadHold t m
  , MonadJSM m
  , MonadFix m
  , PrimMonad m
  , Monoid (p (TraverseChild t m Int v'))
  , Functor p
  , PatchTarget (p (HydrationRunnerT t m ())) ~ IntMap (HydrationRunnerT t m ())
  , PatchTarget (p (TraverseChild t m Int v')) ~ IntMap (TraverseChild t m Int v')
  , Patch (p (HydrationRunnerT t m ()))
  , Patch (p (TraverseChild t m Int v'))
  , RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document
  )
  => ((IntMap.Key -> v -> DomRenderHookT t m (TraverseChild t m Int v'))
    -> IntMap v
    -> Event t (p v)
    -> DomRenderHookT t m (IntMap (TraverseChild t m Int v'), Event t (p (TraverseChild t m Int v'))))
  -- ^ The base monad's traversal
  -> (p (TraverseChild t m Int v')
    -> IntMap (IORef (ChildReadyState Int))
    -> IO (IntMap (IORef (ChildReadyState Int))))
  -- ^ Given a patch for the children DOM elements, produce a patch for the childrens' unreadiness state
  -> (IORef (IntMap DOM.Text)
    -> DOM.Text
    -> p (TraverseChild t m Int v')
    -> JSM ())
  -- ^ Apply a patch to the DOM
  -> (IntMap.Key -> v -> HydrationDomBuilderT s t m v')
  -> IntMap v
  -> Event t (p v)
  -> HydrationDomBuilderT s t m (IntMap v', Event t (p v'))
hoistTraverseIntMapWithKeyWithAdjust base updateChildUnreadiness applyDomUpdate_ f dm0 dm' = do
  doc <- askDocument
  initialEnv <- HydrationDomBuilderT ask
  let parentUnreadyChildren = _hydrationDomBuilderEnv_unreadyChildren initialEnv
  pendingChange :: IORef (IntMap (IORef (ChildReadyState Int)), p (TraverseChild t m Int v')) <- liftIO $ newIORef mempty
  haveEverBeenReady <- liftIO $ newIORef False
  placeholders <- liftIO $ newIORef IntMap.empty
  lastPlaceholder <- createTextNode doc ("" :: Text)
  let applyDomUpdate p = do
        applyDomUpdate_ placeholders lastPlaceholder p
        markSelfReady
        liftIO $ writeIORef pendingChange $! mempty
      markSelfReady = do
        liftIO (readIORef haveEverBeenReady) >>= \case
          True -> return ()
          False -> do
            liftIO $ writeIORef haveEverBeenReady True
            old <- liftIO $ readIORef parentUnreadyChildren
            let new = pred old
            liftIO $ writeIORef parentUnreadyChildren $! new
            when (new == 0) $ _hydrationDomBuilderEnv_commitAction initialEnv
      markChildReady :: IORef (ChildReadyState Int) -> JSM ()
      markChildReady childReadyState = do
        liftIO (readIORef childReadyState) >>= \case
          ChildReadyState_Ready -> return ()
          ChildReadyState_Unready countedAt -> do
            liftIO $ writeIORef childReadyState ChildReadyState_Ready
            case countedAt of
              Nothing -> return ()
              Just k -> do -- This child has been counted as unready, so we need to remove it from the unready set
                (oldUnready, p) <- liftIO $ readIORef pendingChange
                when (not $ IntMap.null oldUnready) $ do -- This shouldn't actually ever be null
                  let newUnready = IntMap.delete k oldUnready
                  liftIO $ writeIORef pendingChange (newUnready, p)
                  when (IntMap.null newUnready) $ do
                    applyDomUpdate p
  (children0 :: IntMap (TraverseChild t m Int v'), children' :: Event t (p (TraverseChild t m Int v')))
    <- HydrationDomBuilderT $ lift $ base (\k v -> drawChildUpdateInt initialEnv markChildReady $ f k v) dm0 dm'
  let processChild k (TraverseChild e _) = case e of
        Left _ -> pure Nothing
        Right immediate -> do
          readIORef (_traverseChildImmediate_childReadyState immediate) >>= \case
            ChildReadyState_Ready -> return Nothing
            ChildReadyState_Unready _ -> do
              writeIORef (_traverseChildImmediate_childReadyState immediate) $ ChildReadyState_Unready $ Just k
              return $ Just (_traverseChildImmediate_childReadyState immediate)
  initialUnready <- liftIO $ IntMap.mapMaybe id <$> IntMap.traverseWithKey processChild children0
  liftIO $ if IntMap.null initialUnready
    then writeIORef haveEverBeenReady True
    else do
      modifyIORef' parentUnreadyChildren succ
      writeIORef pendingChange (initialUnready, mempty) -- The patch is always empty because it got applied implicitly when we ran the children the first time
  getHydrationMode >>= \case
    HydrationMode_Hydrating -> addHydrationStepWithSetup (holdIncremental children0 children') $ \children -> do
      dm :: IntMap (TraverseChild t m Int v') <- sample $ currentIncremental children
      phs <- traverse (either _traverseChildHydration_delayed (pure . _traverseChildImmediate_placeholder) . _traverseChild_mode) dm
      liftIO $ writeIORef placeholders $! phs
      insertAfterPreviousNode lastPlaceholder
    HydrationMode_Immediate -> do
      let activate i = do
            append $ toNode $ _traverseChildImmediate_fragment i
            pure $ _traverseChildImmediate_placeholder i
      phs <- traverse (either (error "impossible") activate . _traverseChild_mode) children0
      liftIO $ writeIORef placeholders $! phs
      append $ toNode lastPlaceholder
  requestDomAction_ $ ffor children' $ \p -> do
    (oldUnready, oldP) <- liftIO $ readIORef pendingChange
    newUnready <- liftIO $ updateChildUnreadiness p oldUnready
    let !newP = p <> oldP
    liftIO $ writeIORef pendingChange (newUnready, newP)
    when (IntMap.null newUnready) $ do
      applyDomUpdate newP
  let result0 = IntMap.map _traverseChild_result children0
      result' = ffor children' $ fmap $ _traverseChild_result
  return (result0, result')

{-# SPECIALIZE hoistTraverseIntMapWithKeyWithAdjust
  :: ((IntMap.Key -> v -> DomRenderHookT DomTimeline HydrationM (TraverseChild DomTimeline HydrationM Int v'))
    -> IntMap v
    -> Event DomTimeline (PatchIntMap v)
    -> DomRenderHookT DomTimeline HydrationM (IntMap (TraverseChild DomTimeline HydrationM Int v'), Event DomTimeline (PatchIntMap (TraverseChild DomTimeline HydrationM Int v'))))
  -> (PatchIntMap (TraverseChild DomTimeline HydrationM Int v')
    -> IntMap (IORef (ChildReadyState Int))
    -> IO (IntMap (IORef (ChildReadyState Int))))
  -> (IORef (IntMap DOM.Text)
    -> DOM.Text
    -> PatchIntMap (TraverseChild DomTimeline HydrationM Int v')
    -> JSM ())
  -> (IntMap.Key -> v -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM v')
  -> IntMap v
  -> Event DomTimeline (PatchIntMap v)
  -> HydrationDomBuilderT HydrationDomSpace DomTimeline HydrationM (IntMap v', Event DomTimeline (PatchIntMap v'))
  #-}

{-# SPECIALIZE hoistTraverseIntMapWithKeyWithAdjust
  :: ((IntMap.Key -> v -> DomRenderHookT DomTimeline HydrationM (TraverseChild DomTimeline HydrationM Int v'))
    -> IntMap v
    -> Event DomTimeline (PatchIntMap v)
    -> DomRenderHookT DomTimeline HydrationM (IntMap (TraverseChild DomTimeline HydrationM Int v'), Event DomTimeline (PatchIntMap (TraverseChild DomTimeline HydrationM Int v'))))
  -> (PatchIntMap (TraverseChild DomTimeline HydrationM Int v')
    -> IntMap (IORef (ChildReadyState Int))
    -> IO (IntMap (IORef (ChildReadyState Int))))
  -> (IORef (IntMap DOM.Text)
    -> DOM.Text
    -> PatchIntMap (TraverseChild DomTimeline HydrationM Int v')
    -> JSM ())
  -> (IntMap.Key -> v -> HydrationDomBuilderT GhcjsDomSpace DomTimeline HydrationM v')
  -> IntMap v
  -> Event DomTimeline (PatchIntMap v)
  -> HydrationDomBuilderT GhcjsDomSpace DomTimeline HydrationM (IntMap v', Event DomTimeline (PatchIntMap v'))
  #-}

data TraverseChildImmediate k = TraverseChildImmediate
  { _traverseChildImmediate_fragment :: {-# UNPACK #-} !DOM.DocumentFragment
  -- ^ Child is appended to this fragment
  , _traverseChildImmediate_placeholder :: {-# UNPACK #-} !DOM.Text
  -- ^ Placeholder reference
  , _traverseChildImmediate_childReadyState :: {-# UNPACK #-} !(IORef (ChildReadyState k))
  }

newtype TraverseChildHydration t m = TraverseChildHydration
  { _traverseChildHydration_delayed :: HydrationRunnerT t m DOM.Text
  -- ^ Action to run at switchover, returns the placeholder
  }

data TraverseChild t m k a = TraverseChild
  { _traverseChild_mode :: !(Either (TraverseChildHydration t m) (TraverseChildImmediate k))
  , _traverseChild_result :: !a
  } deriving Functor

{-# INLINABLE drawChildUpdate #-}
drawChildUpdate :: (MonadJSM m, Reflex t)
  => HydrationDomBuilderEnv t m
  -> (IORef (ChildReadyState k) -> JSM ()) -- This will NOT be called if the child is ready at initialization time; instead, the ChildReadyState return value will be ChildReadyState_Ready
  -> HydrationDomBuilderT s t m (f a)
  -> DomRenderHookT t m (Compose (TraverseChild t m k) f a)
drawChildUpdate initialEnv markReady child = do
  let doc = _hydrationDomBuilderEnv_document initialEnv
  unreadyChildren <- liftIO $ newIORef 0
  liftIO (readIORef $ _hydrationDomBuilderEnv_hydrationMode initialEnv) >>= \case
    HydrationMode_Hydrating -> do
      childDelayedRef <- liftIO $ newIORef $ pure ()
      result <- runReaderT (unHydrationDomBuilderT child) initialEnv
        { _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
        , _hydrationDomBuilderEnv_delayed = childDelayedRef
        }
      childDelayed <- liftIO $ readIORef childDelayedRef
      return $ Compose $ TraverseChild
        { _traverseChild_result = result
        , _traverseChild_mode = Left TraverseChildHydration
          { _traverseChildHydration_delayed = do
            placeholder <- createTextNode doc ("" :: Text)
            insertAfterPreviousNode placeholder
            childDelayed
            pure placeholder
          }
        }
    HydrationMode_Immediate -> do
      childReadyState <- liftIO $ newIORef $ ChildReadyState_Unready Nothing
      df <- createDocumentFragment doc
      placeholder <- createTextNode doc ("" :: Text)
      Node.appendChild_ df placeholder
      result <- runReaderT (unHydrationDomBuilderT child) initialEnv
        { _hydrationDomBuilderEnv_parent = Left $ toNode df
        , _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
        , _hydrationDomBuilderEnv_commitAction = markReady childReadyState
        }
      u <- liftIO $ readIORef unreadyChildren
      when (u == 0) $ liftIO $ writeIORef childReadyState ChildReadyState_Ready
      return $ Compose $ TraverseChild
        { _traverseChild_result = result
        , _traverseChild_mode = Right TraverseChildImmediate
          { _traverseChildImmediate_fragment = df
          , _traverseChildImmediate_placeholder = placeholder
          , _traverseChildImmediate_childReadyState = childReadyState
          }
        }

{-# SPECIALIZE drawChildUpdate
  :: HydrationDomBuilderEnv DomTimeline HydrationM
  -> (IORef (ChildReadyState Int) -> JSM ())
  -> HydrationDomBuilderT s DomTimeline HydrationM (Identity a)
  -> DomRenderHookT DomTimeline HydrationM (Compose (TraverseChild DomTimeline HydrationM Int) Identity a)
  #-}

{-# SPECIALIZE drawChildUpdate
  :: HydrationDomBuilderEnv DomTimeline HydrationM
  -> (IORef (ChildReadyState (Some k)) -> JSM ())
  -> HydrationDomBuilderT s DomTimeline HydrationM (f a)
  -> DomRenderHookT DomTimeline HydrationM (Compose (TraverseChild DomTimeline HydrationM (Some k)) f a)
  #-}

{-# INLINABLE drawChildUpdateInt #-}
drawChildUpdateInt :: (MonadIO m, MonadJSM m, Reflex t)
  => HydrationDomBuilderEnv t m
  -> (IORef (ChildReadyState k) -> JSM ())
  -> HydrationDomBuilderT s t m v
  -> DomRenderHookT t m (TraverseChild t m k v)
drawChildUpdateInt env mark m = fmap runIdentity . getCompose <$> drawChildUpdate env mark (Identity <$> m)

{-# SPECIALIZE drawChildUpdateInt
  :: HydrationDomBuilderEnv DomTimeline HydrationM
  -> (IORef (ChildReadyState k) -> JSM ())
  -> HydrationDomBuilderT s DomTimeline HydrationM v
  -> DomRenderHookT DomTimeline HydrationM (TraverseChild DomTimeline HydrationM k v)
  #-}

{-# INLINE mkHasFocus #-}
mkHasFocus
  :: (HasDocument m, MonadJSM m, IsNode (RawElement d), MonadHold t m, Reflex t, DOM.IsDocumentOrShadowRoot (RawDocument (DomBuilderSpace m)))
  => Element er d t -> m (Dynamic t Bool)
mkHasFocus e = do
  doc <- askDocument
  initialFocus <- Node.isSameNode (toNode $ _element_raw e) . fmap toNode =<< Document.getActiveElement doc
  holdDyn initialFocus $ leftmost
    [ False <$ Reflex.select (_element_events e) (WrapArg Blur)
    , True <$ Reflex.select (_element_events e) (WrapArg Focus)
    ]

insertBefore :: (MonadJSM m, IsNode new, IsNode existing) => new -> existing -> m ()
insertBefore new existing = do
  p <- getParentNodeUnchecked existing
  Node.insertBefore_ p new (Just existing) -- If there's no parent, that means we've been removed from the DOM; this should not happen if the we're removing ourselves from the performEvent properly

type ImmediateDomBuilderT = HydrationDomBuilderT GhcjsDomSpace

instance PerformEvent t m => PerformEvent t (HydrationDomBuilderT s t m) where
  type Performable (HydrationDomBuilderT s t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance PostBuild t m => PostBuild t (HydrationDomBuilderT s t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (HydrationDomBuilderT s t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) => TriggerEvent t (HydrationDomBuilderT s t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = HydrationDomBuilderT . lift $ newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = HydrationDomBuilderT . lift $ newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = HydrationDomBuilderT . lift $ newEventWithLazyTriggerWithOnComplete f

instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) => TriggerEvent t (DomRenderHookT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = DomRenderHookT . lift $ newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = DomRenderHookT . lift $ newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = DomRenderHookT . lift $ newEventWithLazyTriggerWithOnComplete f

instance HasJSContext m => HasJSContext (HydrationDomBuilderT s t m) where
  type JSContextPhantom (HydrationDomBuilderT s t m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance MonadRef m => MonadRef (HydrationDomBuilderT s t m) where
  type Ref (HydrationDomBuilderT s t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (HydrationDomBuilderT s t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance (HasJS x m, ReflexHost t) => HasJS x (HydrationDomBuilderT s t m) where
  type JSX (HydrationDomBuilderT s t m) = JSX m
  liftJS = lift . liftJS

type family EventType en where
  EventType 'AbortTag = UIEvent
  EventType 'BlurTag = FocusEvent
  EventType 'ChangeTag = DOM.Event
  EventType 'ClickTag = MouseEvent
  EventType 'ContextmenuTag = MouseEvent
  EventType 'DblclickTag = MouseEvent
  EventType 'DragTag = MouseEvent
  EventType 'DragendTag = MouseEvent
  EventType 'DragenterTag = MouseEvent
  EventType 'DragleaveTag = MouseEvent
  EventType 'DragoverTag = MouseEvent
  EventType 'DragstartTag = MouseEvent
  EventType 'DropTag = MouseEvent
  EventType 'ErrorTag = UIEvent
  EventType 'FocusTag = FocusEvent
  EventType 'InputTag = DOM.Event
  EventType 'InvalidTag = DOM.Event
  EventType 'KeydownTag = KeyboardEvent
  EventType 'KeypressTag = KeyboardEvent
  EventType 'KeyupTag = KeyboardEvent
  EventType 'LoadTag = UIEvent
  EventType 'MousedownTag = MouseEvent
  EventType 'MouseenterTag = MouseEvent
  EventType 'MouseleaveTag = MouseEvent
  EventType 'MousemoveTag = MouseEvent
  EventType 'MouseoutTag = MouseEvent
  EventType 'MouseoverTag = MouseEvent
  EventType 'MouseupTag = MouseEvent
  EventType 'MousewheelTag = MouseEvent
  EventType 'ScrollTag = UIEvent
  EventType 'SelectTag = UIEvent
  EventType 'SubmitTag = DOM.Event
  EventType 'WheelTag = WheelEvent
  EventType 'BeforecutTag = ClipboardEvent
  EventType 'CutTag = ClipboardEvent
  EventType 'BeforecopyTag = ClipboardEvent
  EventType 'CopyTag = ClipboardEvent
  EventType 'BeforepasteTag = ClipboardEvent
  EventType 'PasteTag = ClipboardEvent
  EventType 'ResetTag = DOM.Event
  EventType 'SearchTag = DOM.Event
  EventType 'SelectstartTag = DOM.Event
  EventType 'TouchstartTag = TouchEvent
  EventType 'TouchmoveTag = TouchEvent
  EventType 'TouchendTag = TouchEvent
  EventType 'TouchcancelTag = TouchEvent

{-# INLINABLE defaultDomEventHandler #-}
defaultDomEventHandler :: IsElement e => e -> EventName en -> EventM e (EventType en) (Maybe (EventResult en))
defaultDomEventHandler e evt = fmap (Just . EventResult) $ case evt of
  Click -> return ()
  Dblclick -> getMouseEventCoords
  Keypress -> getKeyEvent
  Scroll -> fromIntegral <$> getScrollTop e
  Keydown -> getKeyEvent
  Keyup -> getKeyEvent
  Mousemove -> getMouseEventCoords
  Mouseup -> getMouseEventCoords
  Mousedown -> getMouseEventCoords
  Mouseenter -> return ()
  Mouseleave -> return ()
  Focus -> return ()
  Blur -> return ()
  Change -> return ()
  Drag -> return ()
  Dragend -> return ()
  Dragenter -> return ()
  Dragleave -> return ()
  Dragover -> return ()
  Dragstart -> return ()
  Drop -> return ()
  Abort -> return ()
  Contextmenu -> return ()
  Error -> return ()
  Input -> return ()
  Invalid -> return ()
  Load -> return ()
  Mouseout -> return ()
  Mouseover -> return ()
  Select -> return ()
  Submit -> return ()
  Beforecut -> return ()
  Cut -> return ()
  Beforecopy -> return ()
  Copy -> return ()
  Beforepaste -> return ()
  Paste -> getPasteData
  Reset -> return ()
  Search -> return ()
  Selectstart -> return ()
  Touchstart -> getTouchEvent
  Touchmove -> getTouchEvent
  Touchend -> getTouchEvent
  Touchcancel -> getTouchEvent
  Mousewheel -> return ()
  Wheel -> getWheelEvent

{-# INLINABLE defaultDomWindowEventHandler #-}
defaultDomWindowEventHandler :: DOM.Window -> EventName en -> EventM DOM.Window (EventType en) (Maybe (EventResult en))
defaultDomWindowEventHandler w evt = fmap (Just . EventResult) $ case evt of
  Click -> return ()
  Dblclick -> getMouseEventCoords
  Keypress -> getKeyEvent
  Scroll -> Window.getScrollY w
  Keydown -> getKeyEvent
  Keyup -> getKeyEvent
  Mousemove -> getMouseEventCoords
  Mouseup -> getMouseEventCoords
  Mousedown -> getMouseEventCoords
  Mouseenter -> return ()
  Mouseleave -> return ()
  Focus -> return ()
  Blur -> return ()
  Change -> return ()
  Drag -> return ()
  Dragend -> return ()
  Dragenter -> return ()
  Dragleave -> return ()
  Dragover -> return ()
  Dragstart -> return ()
  Drop -> return ()
  Abort -> return ()
  Contextmenu -> return ()
  Error -> return ()
  Input -> return ()
  Invalid -> return ()
  Load -> return ()
  Mouseout -> return ()
  Mouseover -> return ()
  Select -> return ()
  Submit -> return ()
  Beforecut -> return ()
  Cut -> return ()
  Beforecopy -> return ()
  Copy -> return ()
  Beforepaste -> return ()
  Paste -> getPasteData
  Reset -> return ()
  Search -> return ()
  Selectstart -> return ()
  Touchstart -> getTouchEvent
  Touchmove -> getTouchEvent
  Touchend -> getTouchEvent
  Touchcancel -> getTouchEvent
  Mousewheel -> return ()
  Wheel -> getWheelEvent

{-# INLINABLE withIsEvent #-}
withIsEvent :: EventName en -> (IsEvent (EventType en) => r) -> r
withIsEvent en r = case en of
  Click -> r
  Dblclick -> r
  Keypress -> r
  Scroll -> r
  Keydown -> r
  Keyup -> r
  Mousemove -> r
  Mouseup -> r
  Mousedown -> r
  Mouseenter -> r
  Mouseleave -> r
  Focus -> r
  Blur -> r
  Change -> r
  Drag -> r
  Dragend -> r
  Dragenter -> r
  Dragleave -> r
  Dragover -> r
  Dragstart -> r
  Drop -> r
  Abort -> r
  Contextmenu -> r
  Error -> r
  Input -> r
  Invalid -> r
  Load -> r
  Mouseout -> r
  Mouseover -> r
  Select -> r
  Submit -> r
  Beforecut -> r
  Cut -> r
  Beforecopy -> r
  Copy -> r
  Beforepaste -> r
  Paste -> r
  Reset -> r
  Search -> r
  Selectstart -> r
  Touchstart -> r
  Touchmove -> r
  Touchend -> r
  Touchcancel -> r
  Mousewheel -> r
  Wheel -> r

showEventName :: EventName en -> String
showEventName en = case en of
  Abort -> "Abort"
  Blur -> "Blur"
  Change -> "Change"
  Click -> "Click"
  Contextmenu -> "Contextmenu"
  Dblclick -> "Dblclick"
  Drag -> "Drag"
  Dragend -> "Dragend"
  Dragenter -> "Dragenter"
  Dragleave -> "Dragleave"
  Dragover -> "Dragover"
  Dragstart -> "Dragstart"
  Drop -> "Drop"
  Error -> "Error"
  Focus -> "Focus"
  Input -> "Input"
  Invalid -> "Invalid"
  Keydown -> "Keydown"
  Keypress -> "Keypress"
  Keyup -> "Keyup"
  Load -> "Load"
  Mousedown -> "Mousedown"
  Mouseenter -> "Mouseenter"
  Mouseleave -> "Mouseleave"
  Mousemove -> "Mousemove"
  Mouseout -> "Mouseout"
  Mouseover -> "Mouseover"
  Mouseup -> "Mouseup"
  Mousewheel -> "Mousewheel"
  Scroll -> "Scroll"
  Select -> "Select"
  Submit -> "Submit"
  Wheel -> "Wheel"
  Beforecut -> "Beforecut"
  Cut -> "Cut"
  Beforecopy -> "Beforecopy"
  Copy -> "Copy"
  Beforepaste -> "Beforepaste"
  Paste -> "Paste"
  Reset -> "Reset"
  Search -> "Search"
  Selectstart -> "Selectstart"
  Touchstart -> "Touchstart"
  Touchmove -> "Touchmove"
  Touchend -> "Touchend"
  Touchcancel -> "Touchcancel"

--TODO: Get rid of this hack
-- ElementEventTarget is here to allow us to treat SVG and HTML elements as the same thing; without it, we'll break any existing SVG code.
newtype ElementEventTarget = ElementEventTarget DOM.Element deriving (DOM.IsGObject, DOM.ToJSVal, DOM.IsSlotable, DOM.IsParentNode, DOM.IsNonDocumentTypeChildNode, DOM.IsChildNode, DOM.IsAnimatable, IsNode, IsElement)
instance DOM.FromJSVal ElementEventTarget where
  fromJSVal = fmap (fmap ElementEventTarget) . DOM.fromJSVal
instance DOM.IsEventTarget ElementEventTarget
instance DOM.IsGlobalEventHandlers ElementEventTarget
instance DOM.IsDocumentAndElementEventHandlers ElementEventTarget

{-# INLINABLE elementOnEventName #-}
elementOnEventName :: IsElement e => EventName en -> e -> EventM e (EventType en) () -> JSM (JSM ())
elementOnEventName en e_ = let e = ElementEventTarget (DOM.toElement e_) in case en of
  Abort -> on e Events.abort
  Blur -> on e Events.blur
  Change -> on e Events.change
  Click -> on e Events.click
  Contextmenu -> on e Events.contextMenu
  Dblclick -> on e Events.dblClick
  Drag -> on e Events.drag
  Dragend -> on e Events.dragEnd
  Dragenter -> on e Events.dragEnter
  Dragleave -> on e Events.dragLeave
  Dragover -> on e Events.dragOver
  Dragstart -> on e Events.dragStart
  Drop -> on e Events.drop
  Error -> on e Events.error
  Focus -> on e Events.focus
  Input -> on e Events.input
  Invalid -> on e Events.invalid
  Keydown -> on e Events.keyDown
  Keypress -> on e Events.keyPress
  Keyup -> on e Events.keyUp
  Load -> on e Events.load
  Mousedown -> on e Events.mouseDown
  Mouseenter -> on e Events.mouseEnter
  Mouseleave -> on e Events.mouseLeave
  Mousemove -> on e Events.mouseMove
  Mouseout -> on e Events.mouseOut
  Mouseover -> on e Events.mouseOver
  Mouseup -> on e Events.mouseUp
  Mousewheel -> on e Events.mouseWheel
  Scroll -> on e Events.scroll
  Select -> on e Events.select
  Submit -> on e Events.submit
  Wheel -> on e Events.wheel
  Beforecut -> on e Events.beforeCut
  Cut -> on e Events.cut
  Beforecopy -> on e Events.beforeCopy
  Copy -> on e Events.copy
  Beforepaste -> on e Events.beforePaste
  Paste -> on e Events.paste
  Reset -> on e Events.reset
  Search -> on e Events.search
  Selectstart -> on e Element.selectStart
  Touchstart -> on e Events.touchStart
  Touchmove -> on e Events.touchMove
  Touchend -> on e Events.touchEnd
  Touchcancel -> on e Events.touchCancel

{-# INLINABLE windowOnEventName #-}
windowOnEventName :: EventName en -> DOM.Window -> EventM DOM.Window (EventType en) () -> JSM (JSM ())
windowOnEventName en e = case en of
  Abort -> on e Events.abort
  Blur -> on e Events.blur
  Change -> on e Events.change
  Click -> on e Events.click
  Contextmenu -> on e Events.contextMenu
  Dblclick -> on e Events.dblClick
  Drag -> on e Events.drag
  Dragend -> on e Events.dragEnd
  Dragenter -> on e Events.dragEnter
  Dragleave -> on e Events.dragLeave
  Dragover -> on e Events.dragOver
  Dragstart -> on e Events.dragStart
  Drop -> on e Events.drop
  Error -> on e Events.error
  Focus -> on e Events.focus
  Input -> on e Events.input
  Invalid -> on e Events.invalid
  Keydown -> on e Events.keyDown
  Keypress -> on e Events.keyPress
  Keyup -> on e Events.keyUp
  Load -> on e Events.load
  Mousedown -> on e Events.mouseDown
  Mouseenter -> on e Events.mouseEnter
  Mouseleave -> on e Events.mouseLeave
  Mousemove -> on e Events.mouseMove
  Mouseout -> on e Events.mouseOut
  Mouseover -> on e Events.mouseOver
  Mouseup -> on e Events.mouseUp
  Mousewheel -> on e Events.mouseWheel
  Scroll -> on e Events.scroll
  Select -> on e Events.select
  Submit -> on e Events.submit
  Wheel -> on e Events.wheel
  Beforecut -> const $ return $ return () --TODO
  Cut -> const $ return $ return () --TODO
  Beforecopy -> const $ return $ return () --TODO
  Copy -> const $ return $ return () --TODO
  Beforepaste -> const $ return $ return () --TODO
  Paste -> const $ return $ return () --TODO
  Reset -> on e Events.reset
  Search -> on e Events.search
  Selectstart -> const $ return $ return () --TODO
  Touchstart -> on e Events.touchStart
  Touchmove -> on e Events.touchMove
  Touchend -> on e Events.touchEnd
  Touchcancel -> on e Events.touchCancel

{-# INLINABLE wrapDomEvent #-}
wrapDomEvent :: (TriggerEvent t m, MonadJSM m) => e -> (e -> EventM e event () -> JSM (JSM ())) -> EventM e event a -> m (Event t a)
wrapDomEvent el elementOnevent getValue = wrapDomEventMaybe el elementOnevent $ fmap Just getValue

{-# INLINABLE subscribeDomEvent #-}
subscribeDomEvent :: (EventM e event () -> JSM (JSM ()))
                  -> EventM e event (Maybe a)
                  -> Chan [DSum (EventTriggerRef t) TriggerInvocation]
                  -> EventTrigger t a
                  -> JSM (JSM ())
subscribeDomEvent elementOnevent getValue eventChan et = elementOnevent $ do
  mv <- getValue
  forM_ mv $ \v -> liftIO $ do
    --TODO: I don't think this is quite right: if a new trigger is created between when this is enqueued and when it fires, this may not work quite right
    etr <- newIORef $ Just et
    writeChan eventChan [EventTriggerRef etr :=> TriggerInvocation v (return ())]

{-# INLINABLE wrapDomEventMaybe #-}
wrapDomEventMaybe :: (TriggerEvent t m, MonadJSM m)
                  => e
                  -> (e -> EventM e event () -> JSM (JSM ()))
                  -> EventM e event (Maybe a)
                  -> m (Event t a)
wrapDomEventMaybe el elementOnevent getValue = do
  ctx <- askJSM
  newEventWithLazyTriggerWithOnComplete $ \trigger -> (`runJSM` ctx) <$> (`runJSM` ctx) (elementOnevent el $ do
    mv <- getValue
    forM_ mv $ \v -> liftIO $ trigger v $ return ())

{-# INLINABLE wrapDomEventsMaybe #-}
wrapDomEventsMaybe :: (MonadJSM m, MonadReflexCreateTrigger t m)
                   => e
                   -> (forall en. IsEvent (EventType en) => EventName en -> EventM e (EventType en) (Maybe (f en)))
                   -> (forall en. EventName en -> e -> EventM e (EventType en) () -> JSM (JSM ()))
                   -> ImmediateDomBuilderT t m (EventSelector t (WrapArg f EventName))
wrapDomEventsMaybe target handlers onEventName = do
  ctx <- askJSM
  eventChan <- askEvents
  e <- lift $ newFanEventWithTrigger $ \(WrapArg en) -> withIsEvent en
    (((`runJSM` ctx) <$>) . (`runJSM` ctx) . subscribeDomEvent (onEventName en target) (handlers en) eventChan)
  return $! e

{-# INLINABLE getKeyEvent #-}
getKeyEvent :: EventM e KeyboardEvent Word
getKeyEvent = do
  e <- event
  which <- KeyboardEvent.getWhich e
  if which /= 0 then return which else do
    charCode <- getCharCode e
    if charCode /= 0 then return charCode else
      getKeyCode e

{-# INLINABLE getMouseEventCoords #-}
getMouseEventCoords :: EventM e MouseEvent (Int, Int)
getMouseEventCoords = do
  e <- event
  bisequence (getClientX e, getClientY e)

{-# INLINABLE getPasteData #-}
getPasteData :: EventM e ClipboardEvent (Maybe Text)
getPasteData = do
  e <- event
  mdt <- ClipboardEvent.getClipboardData e
  case mdt of
    Nothing -> return Nothing
    Just dt -> Just <$> DataTransfer.getData dt ("text" :: Text)

{-# INLINABLE getTouchEvent #-}
getTouchEvent :: EventM e TouchEvent TouchEventResult
getTouchEvent = do
  let touchResults ts = do
          n <- TouchList.getLength ts
          forM (takeWhile (< n) [0..]) $ \ix -> do
            t <- TouchList.item ts ix
            identifier <- Touch.getIdentifier t
            screenX <- Touch.getScreenX t
            screenY <- Touch.getScreenY t
            clientX <- Touch.getClientX t
            clientY <- Touch.getClientY t
            pageX <- Touch.getPageX t
            pageY <- Touch.getPageY t
            return TouchResult
              { _touchResult_identifier = identifier
              , _touchResult_screenX = screenX
              , _touchResult_screenY = screenY
              , _touchResult_clientX = clientX
              , _touchResult_clientY = clientY
              , _touchResult_pageX = pageX
              , _touchResult_pageY = pageY
              }
  e <- event
  altKey <- TouchEvent.getAltKey e
  ctrlKey <- TouchEvent.getCtrlKey e
  shiftKey <- TouchEvent.getShiftKey e
  metaKey <- TouchEvent.getMetaKey e
  changedTouches <- touchResults =<< TouchEvent.getChangedTouches e
  targetTouches <- touchResults =<< TouchEvent.getTargetTouches e
  touches <- touchResults =<< TouchEvent.getTouches e
  return $ TouchEventResult
    { _touchEventResult_altKey = altKey
    , _touchEventResult_changedTouches = changedTouches
    , _touchEventResult_ctrlKey = ctrlKey
    , _touchEventResult_metaKey = metaKey
    , _touchEventResult_shiftKey = shiftKey
    , _touchEventResult_targetTouches = targetTouches
    , _touchEventResult_touches = touches
    }

{-# INLINABLE getWheelEvent #-}
getWheelEvent :: EventM e WheelEvent WheelEventResult
getWheelEvent = do
  e <- event
  dx :: Double <- WheelEvent.getDeltaX e
  dy :: Double <- WheelEvent.getDeltaY e
  dz :: Double <- WheelEvent.getDeltaZ e
  deltaMode :: Word <- WheelEvent.getDeltaMode e
  return $ WheelEventResult
    { _wheelEventResult_deltaX = dx
    , _wheelEventResult_deltaY = dy
    , _wheelEventResult_deltaZ = dz
    , _wheelEventResult_deltaMode = case deltaMode of
        0 -> DeltaPixel
        1 -> DeltaLine
        2 -> DeltaPage
        -- See https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent/deltaMode
        _ -> error "getWheelEvent: impossible encoding"
    }

instance MonadSample t m => MonadSample t (HydrationDomBuilderT s t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (HydrationDomBuilderT s t m) where
  {-# INLINABLE hold #-}
  hold v0 v' = lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = lift $ holdIncremental v0 v'
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE

data WindowConfig t = WindowConfig -- No config options yet

instance Default (WindowConfig t) where
  def = WindowConfig

data Window t = Window
  { _window_events :: EventSelector t (WrapArg EventResult EventName)
  , _window_raw :: DOM.Window
  }

wrapWindow :: (MonadJSM m, MonadReflexCreateTrigger t m) => DOM.Window -> WindowConfig t -> HydrationDomBuilderT GhcjsDomSpace t m (Window t)
wrapWindow wv _ = do
  events <- wrapDomEventsMaybe wv (defaultDomWindowEventHandler wv) windowOnEventName
  return $ Window
    { _window_events = events
    , _window_raw = wv
    }

#ifdef USE_TEMPLATE_HASKELL
makeLenses ''GhcjsEventSpec
#endif
