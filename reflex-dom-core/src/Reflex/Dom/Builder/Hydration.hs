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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Reflex.Dom.Builder.Hydration
       ( EventTriggerRef (..)
       , HydrationDomBuilderEnv (..)
       , HydrationDomBuilderT (..)
       , HydrationMode (..)
       , HydrationRunnerT (..)
       , runHydrationRunnerT
       , localEnv
       , addHydrationStepWithSetup
       , addHydrationStep
       , getHydrationMode
       , hydrateNode
       , getPreviousNode, setPreviousNode
       , runHydrationDomBuilderT
       , askParent
       , askEvents
       , append
       , textNodeInternal
       , SupportsHydrationDomBuilder
       , wrap
       , makeElement
       , drawChildUpdate
       -- * Internal
       , traverseDMapWithKeyWithAdjust'
       , hoistTraverseWithKeyWithAdjust
       , traverseIntMapWithKeyWithAdjust'
       , hoistTraverseIntMapWithKeyWithAdjust
       ) where

import Control.Concurrent
import Control.Exception (bracketOnError)
import Control.Lens hiding (element, ix)
import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum
import Data.FastMutableIntMap (PatchIntMap (..))
import Data.Foldable (for_)
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Misc
import Data.Functor.Product
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable)
import Foreign.JavaScript.Internal.Utils
import Foreign.JavaScript.TH
import GHCJS.DOM.Document (Document, createDocumentFragment, createElement, createElementNS, createTextNode, createComment)
import GHCJS.DOM.Element (removeAttribute, removeAttributeNS, setAttribute, setAttributeNS, hasAttribute, hasAttributeNS)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Node (appendChild_, setNodeValue, toNode)
import GHCJS.DOM.Types (liftJSM, askJSM, runJSM, JSM, MonadJSM, IsNode, Node, ToDOMString, uncheckedCastTo)
import Reflex.Adjustable.Class
import Reflex.Class as Reflex
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate hiding (askEvents, askParent, append, wrap, makeElement, textNodeInternal, traverseIntMapWithKeyWithAdjust', traverseDMapWithKeyWithAdjust', hoistTraverseWithKeyWithAdjust, hoistTraverseIntMapWithKeyWithAdjust, drawChildUpdate)
import Reflex.Dynamic
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Requester.Base
import Reflex.Requester.Class
import Reflex.TriggerEvent.Base hiding (askEvents)
import Reflex.TriggerEvent.Class

import qualified Data.Dependent.Map as DMap
import qualified Data.FastMutableIntMap as FastMutableIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import qualified Data.Some as Some
import qualified Data.Text as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.DocumentOrShadowRoot as Document
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.FileList as FileList
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.HTMLSelectElement as Select
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Text as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified Reflex.Patch.DMap as PatchDMap
import qualified Reflex.Patch.DMapWithMove as PatchDMapWithMove
import qualified Reflex.TriggerEvent.Base as TriggerEventT (askEvents)

#ifndef USE_TEMPLATE_HASKELL
import Data.Functor.Contravariant (phantom)
#endif

#ifndef ghcjs_HOST_OS
import GHCJS.DOM.Types (MonadJSM (..))

instance MonadJSM m => MonadJSM (HydrationRunnerT t m) where
    liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (HydrationDomBuilderT t m) where
    liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (DomRenderHookT t m) where
    liftJSM' = lift . liftJSM'
#endif

data HydrationDomBuilderEnv t = HydrationDomBuilderEnv
  { _hydrationDomBuilderEnv_document :: {-# UNPACK #-} !Document
  -- ^ Reference to the document
  , _hydrationDomBuilderEnv_parent :: {-# UNPACK #-} !(IORef Node)
  -- ^ This is in an IORef because in the time up to hydration we can't actually know what the
  -- parent is - we populate this reference during the DOM traversal at hydration time
  , _hydrationDomBuilderEnv_unreadyChildren :: {-# UNPACK #-} !(IORef Word)
  -- ^ Number of children who still aren't fully rendered
  , _hydrationDomBuilderEnv_commitAction :: !(JSM ())
  -- ^ Action to take when all children are ready --TODO: we should probably get rid of this once we invoke it
  , _hydrationDomBuilderEnv_hydrationMode :: {-# UNPACK #-} !(IORef HydrationMode)
  -- ^ In hydration mode? Should be switched to `HydrationMode_Immediate` after hydration is finished
  , _hydrationDomBuilderEnv_switchover :: !(Event t ())
  }

-- | The monad which performs the delayed actions to reuse prerendered nodes and set up events
-- State contains reference to the previous node sibling, if any.
newtype HydrationRunnerT t m a = HydrationRunnerT { unHydrationRunnerT :: StateT (Maybe Node) (ReaderT Node (DomRenderHookT t m)) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
#if MIN_VERSION_base(4,9,1)
           , MonadAsyncException
#endif
           )

localRunner :: (MonadJSM m, Monad m) => HydrationRunnerT t m a -> Maybe Node -> Node -> HydrationRunnerT t m a
localRunner (HydrationRunnerT m) s parent = HydrationRunnerT . lift $ local (\_ -> parent) (evalStateT m s)

runHydrationRunnerT
  :: (MonadRef m, Ref m ~ IORef, Monad m, PerformEvent t m, MonadFix m, MonadReflexCreateTrigger t m, MonadJSM m, MonadJSM (Performable m))
  => HydrationRunnerT t m a -> Maybe Node -> Node -> Chan [DSum (EventTriggerRef t) TriggerInvocation] -> m a
runHydrationRunnerT (HydrationRunnerT m) s parent = runDomRenderHookT (runReaderT (evalStateT m s) parent)

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (HydrationRunnerT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadTrans (HydrationRunnerT t) where
  lift = HydrationRunnerT . lift . lift . lift

instance MonadSample t m => MonadSample t (HydrationRunnerT t m) where
  sample = lift . sample

-- | Add a hydration step which depends on some computation that should only be
-- done *before* the switchover to immediate mode - this is most likely some
-- form of 'hold' which we want to remove after hydration is done
addHydrationStepWithSetup :: (Adjustable t m, MonadIO m) => m a -> (a -> HydrationRunnerT t m ()) -> HydrationDomBuilderT t m ()
addHydrationStepWithSetup setup f = getHydrationMode >>= \case
  HydrationMode_Immediate -> pure ()
  HydrationMode_Hydrating -> do
    switchover <- HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_switchover
    --s <- lift setup
    (s, _) <- lift $ runWithReplace setup $ return () <$ switchover -- 4% more memory than above
    HydrationDomBuilderT $ modify' (>> f s)

-- | Add a hydration step
addHydrationStep
  :: (MonadIO m, Ref m ~ IORef, MonadRef m, PerformEvent t m, MonadFix m, MonadReflexCreateTrigger t m, MonadJSM m, MonadJSM (Performable m))
  => HydrationRunnerT t m () -> HydrationDomBuilderT t m ()
addHydrationStep m = getHydrationMode >>= \case
  HydrationMode_Immediate -> lift . runHydrationRunnerT m Nothing undefined =<< askEvents
  HydrationMode_Hydrating -> HydrationDomBuilderT $ modify' (>> m)

-- | A monad for DomBuilder which just gets the results of children and pushes
-- work into an action that is delayed until after postBuild (to match the
-- static builder). The action runs in 'HydrationRunnerT', which performs the
-- DOM takeover and sets up the events, after which point this monad will
-- continue in the vein of 'ImmediateDomBuilderT'.
newtype HydrationDomBuilderT t m a = HydrationDomBuilderT { unHydrationDomBuilderT :: ReaderT (HydrationDomBuilderEnv t) (StateT (HydrationRunnerT t m ()) (DomRenderHookT t m)) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
#if MIN_VERSION_base(4,9,1)
           , MonadAsyncException
#endif
           )

instance PrimMonad m => PrimMonad (HydrationDomBuilderT x m) where
  type PrimState (HydrationDomBuilderT x m) = PrimState m
  primitive = lift . primitive

instance MonadTrans (HydrationDomBuilderT t) where
  lift = HydrationDomBuilderT . lift . lift . lift

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
          traverseRequesterData (\r -> Identity <$> r) rm
    return result
  where
    runInAnimationFrame f x = void . DOM.inAnimationFrame' $ \_ -> do
        v <- synchronously x
        void . liftIO $ f v

instance MonadTrans (DomRenderHookT t) where
  lift = DomRenderHookT . lift . lift

instance (Reflex t, MonadFix m) => DomRenderHook t (DomRenderHookT t m) where
  withRenderHook hook (DomRenderHookT a) = do
    DomRenderHookT $ withRequesting $ \rsp -> do
      (x, req) <- lift $ runRequesterT a $ runIdentity <$> rsp
      return (ffor req $ \rm -> hook $ traverseRequesterData (\r -> Identity <$> r) rm, x)
  requestDomAction = DomRenderHookT . requestingIdentity
  requestDomAction_ = DomRenderHookT . requesting_

instance (Reflex t, MonadFix m) => DomRenderHook t (HydrationDomBuilderT t m) where
  withRenderHook hook = HydrationDomBuilderT . mapReaderT (mapStateT (withRenderHook hook)) . unHydrationDomBuilderT
  requestDomAction = HydrationDomBuilderT . lift . lift . requestDomAction
  requestDomAction_ = HydrationDomBuilderT . lift . lift . requestDomAction_

instance (Reflex t, MonadFix m) => DomRenderHook t (HydrationRunnerT t m) where
  withRenderHook hook = HydrationRunnerT . mapStateT (mapReaderT (withRenderHook hook)) . unHydrationRunnerT
  requestDomAction = HydrationRunnerT . lift . lift . requestDomAction
  requestDomAction_ = HydrationRunnerT . lift . lift . requestDomAction_

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
  => HydrationDomBuilderT t m a
  -> HydrationDomBuilderEnv t
  -> Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -> m (a, HydrationRunnerT t m ())
runHydrationDomBuilderT (HydrationDomBuilderT a) env = runDomRenderHookT (runStateT (runReaderT a env) (pure ()))

instance Monad m => HasDocument (HydrationDomBuilderT t m) where
  {-# INLINABLE askDocument #-}
  askDocument = HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_document

{-# INLINABLE localEnv #-}
localEnv :: Monad m => (HydrationDomBuilderEnv t -> HydrationDomBuilderEnv t) -> HydrationDomBuilderT t m a -> HydrationDomBuilderT t m a
localEnv f = HydrationDomBuilderT . local f . unHydrationDomBuilderT

{-# INLINABLE append #-}
append :: MonadJSM m => DOM.Node -> HydrationDomBuilderT t m ()
append n = do
  p <- getParent
  appendChild_ p n
  return ()

data HydrationMode
  = HydrationMode_Hydrating
  | HydrationMode_Immediate
  deriving (Eq, Ord, Show)

{-# INLINABLE askParent #-}
askParent :: Monad m => HydrationRunnerT t m DOM.Node
askParent = HydrationRunnerT ask

{-# INLINABLE getPreviousNode #-}
getPreviousNode :: Monad m => HydrationRunnerT t m (Maybe DOM.Node)
getPreviousNode = HydrationRunnerT get

{-# INLINABLE setPreviousNode #-}
setPreviousNode :: Monad m => Maybe DOM.Node -> HydrationRunnerT t m ()
setPreviousNode = HydrationRunnerT . put

{-# INLINABLE getParent #-}
getParent :: MonadIO m => HydrationDomBuilderT t m DOM.Node
getParent = liftIO . readIORef =<< HydrationDomBuilderT (asks _hydrationDomBuilderEnv_parent)

{-# INLINABLE askUnreadyChildren #-}
askUnreadyChildren :: Monad m => HydrationDomBuilderT t m (IORef Word)
askUnreadyChildren = HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_unreadyChildren

{-# INLINABLE askCommitAction #-}
askCommitAction :: Monad m => HydrationDomBuilderT t m (JSM ())
askCommitAction = HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_commitAction

{-# INLINABLE getHydrationMode #-}
getHydrationMode :: MonadIO m => HydrationDomBuilderT t m HydrationMode
getHydrationMode = liftIO . readIORef =<< HydrationDomBuilderT (asks _hydrationDomBuilderEnv_hydrationMode)

{-# INLINABLE askEvents #-}
askEvents :: Monad m => HydrationDomBuilderT t m (Chan [DSum (EventTriggerRef t) TriggerInvocation])
askEvents = HydrationDomBuilderT . lift . lift . DomRenderHookT . lift $ TriggerEventT.askEvents

{-# INLINABLE makeNodeInternal #-}
makeNodeInternal
  :: forall node t m. (MonadJSM m, IsNode node, Typeable node)
  => HydrationDomBuilderT t m node -> HydrationDomBuilderT t m node
makeNodeInternal mkNode = do
  n <- mkNode
  append $ toNode n
  return n

{-# INLINABLE hydrateNode #-}
-- | This function expects the existing DOM at the current hydration node to be
-- correct. It skips any nodes that are the wrong type, or that fail the
-- @check@. The previous node reference is also updated.
hydrateNode
  :: (MonadJSM m, IsNode node, Typeable node)
  => (node -> HydrationRunnerT t m Bool) -> (DOM.JSVal -> node) -> HydrationRunnerT t m node
hydrateNode check constructor = do
  parent <- askParent
  lastHydrationNode <- getPreviousNode
  let go mLastNode = do
        node <- maybe (Node.getFirstChildUnchecked parent) Node.getNextSiblingUnchecked mLastNode
        DOM.castTo constructor node >>= \case
          Nothing -> go (Just node)
          Just tn -> check tn >>= \case
            True -> return tn
            False -> go (Just node)
  n <- go lastHydrationNode
  setPreviousNode $ Just $ toNode n
  return n

type SupportsHydrationDomBuilder t m = (Reflex t, MonadJSM m, MonadHold t m, MonadFix m, MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref JSM, Adjustable t m, PrimMonad m, PerformEvent t m, MonadJSM (Performable m))

-- | This 'wrap' is only partial: it doesn't create the 'EventSelector' itself
{-# INLINABLE wrap #-}
wrap
  :: forall m er t. (Reflex t, MonadJSM m, MonadReflexCreateTrigger t m, DomRenderHook t m)
  => Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -> DOM.Element
  -> RawElementConfig er t HydrationDomSpace
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

triggerBody
  :: forall er t x. DOM.JSContextRef
  -> ElementConfig er t HydrationDomSpace
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
    !handler = _ghcjsEventSpec_handler $ _rawElementConfig_eventSpec $ extractRawElementConfig cfg

{-# INLINABLE makeElement #-}
makeElement
  :: forall er t m a. (MonadJSM m, MonadHold t m, MonadFix m, MonadReflexCreateTrigger t m, Adjustable t m, Ref m ~ IORef, PerformEvent t m, MonadJSM (Performable m), MonadRef m)
  => Text
  -> ElementConfig er t HydrationDomSpace
  -> HydrationDomBuilderT t m a
  -> HydrationDomBuilderT t m ((Element er HydrationDomSpace t, a), IORef DOM.Element)
makeElement elementTag cfg child = do
  doc <- askDocument
  ctx <- askJSM
  events <- askEvents

  let buildElement :: MonadJSM n => n DOM.Element
      buildElement = do
        e <- uncheckedCastTo DOM.Element <$> case cfg ^. namespace of
          Nothing -> createElement doc elementTag
          Just ens -> createElementNS doc (Just ens) elementTag
        iforM_ (cfg ^. initialAttributes) $ \(AttributeName mAttrNamespace n) v -> case mAttrNamespace of
          Nothing -> setAttribute e n v
          Just ans -> setAttributeNS e (Just ans) n v
        pure e
      ssrAttr = "ssr" :: DOM.JSString
      hasSSRAttribute :: DOM.Element -> HydrationRunnerT t m Bool
      hasSSRAttribute e = case cfg ^. namespace of
        Nothing -> hasAttribute e ssrAttr -- TODO: disabled for debugging <* removeAttribute e ssrAttr
        Just ns -> hasAttributeNS e (Just ns) ssrAttr -- TODO: disabled for debugging <* removeAttributeNS e (Just ns) ssrAttr
  getHydrationMode >>= \case
    HydrationMode_Immediate -> do
      e <- makeNodeInternal buildElement
      p <- liftIO $ newIORef $ toNode e
      -- Run the child builder with updated parent and previous sibling references
      result <- localEnv (\env -> env { _hydrationDomBuilderEnv_parent = p }) child
      eventTriggerRefs <- wrap events e $ extractRawElementConfig cfg
      es <- newFanEventWithTrigger $ triggerBody ctx cfg events eventTriggerRefs e
      e' <- liftIO $ newIORef e
      return ((Element es (), result), e')
    HydrationMode_Hydrating -> do
      -- Schedule everything for after postBuild, except for getting the result itself
      parent <- liftIO $ newIORef $ error "Parent not yet initialized"
      e' <- liftIO $ newIORef $ error "makeElement: Element not yet initialized"
      env <- HydrationDomBuilderT ask
      let env' = env { _hydrationDomBuilderEnv_parent = parent }
      (result, childDom) <- HydrationDomBuilderT $ lift $ lift $ runStateT (runReaderT (unHydrationDomBuilderT child) env') (pure ())
      wrapResult <- liftIO newEmptyMVar
      let activateElement = do
            e <- hydrateNode hasSSRAttribute DOM.Element
            -- Update the parent node used by the children
            liftIO $ writeIORef parent $ toNode e
            liftIO $ writeIORef e' e
            -- Setup events, store the result so we can wait on it later
            refs <- wrap events e $ extractRawElementConfig cfg
            liftIO $ putMVar wrapResult (e, refs)
            pure $ toNode e
      addHydrationStep $ activateElement >>= localRunner childDom Nothing

      -- We need the EventSelector to switch to the real event handler after activation
      es <- newFanEventWithTrigger $ \(WrapArg en) t -> do
        cleanup <- newEmptyMVar
        threadId <- forkIO $ do
          -- Wait on the data we need from the delayed action
          (e, eventTriggerRefs) <- readMVar wrapResult
          bracketOnError
            -- Run the setup, acquiring the cleanup action
            (triggerBody ctx cfg events eventTriggerRefs e (WrapArg en) t)
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

      let dummyElement = error "makeElement: DOM.Element"
      return ((Element es dummyElement, result), e')

{-# INLINABLE textNodeInternal #-}
textNodeInternal :: (Adjustable t m, MonadHold t m, MonadJSM m, MonadFix m, Reflex t) => Text -> Maybe (Event t Text) -> HydrationDomBuilderT t m DOM.Text
textNodeInternal !t mSetContents = getHydrationMode >>= \case
  HydrationMode_Immediate -> do
    n <- makeNodeInternal (askDocument >>= \doc -> createTextNode doc t)
    mapM_ (requestDomAction_ . fmap (setNodeValue n . Just)) mSetContents
    pure n
  HydrationMode_Hydrating -> do
    addHydrationStepWithSetup (maybe (pure $ pure t) (hold t) mSetContents) $ \currentText -> do
      n <- hydrateTextNode =<< sample currentText
      mapM_ (requestDomAction_ . fmap (setNodeValue n . Just)) mSetContents
    pure $ error "textNodeInternal: DOM.Text"

-- | The static builder mashes adjacent text nodes into one node: we check the
-- text content of each node we come to, comparing it to the content we
-- expect. We also have a special case for empty text nodes - we always create
-- the and add them after the previous node reference.
{-# INLINABLE hydrateTextNode #-}
hydrateTextNode :: MonadJSM m => Text -> HydrationRunnerT t m DOM.Text
hydrateTextNode t@"" = do
  doc <- Node.getOwnerDocumentUnchecked =<< askParent
  tn <- createTextNode doc t
  insertAfterPreviousNode tn
  pure tn
hydrateTextNode t = do
  n <- join $ go <$> askParent <*> getPreviousNode
  setPreviousNode $ Just $ toNode n
  return n
  where
    go parent mLastNode = do
      node <- maybe (Node.getFirstChildUnchecked parent) Node.getNextSiblingUnchecked mLastNode
      DOM.castTo DOM.Text node >>= \case
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
            Nothing -> go parent $ Just $ toNode originalNode


{-# INLINABLE commentNodeInternal #-}
commentNodeInternal :: (Ref m ~ IORef, MonadRef m, PerformEvent t m, MonadReflexCreateTrigger t m, MonadJSM (Performable m), MonadJSM m, MonadFix m, ToDOMString contents, Reflex t, Adjustable t m) => contents -> Maybe (Event t contents) -> HydrationDomBuilderT t m DOM.Comment
commentNodeInternal !t mSetContents = getHydrationMode >>= \case
  HydrationMode_Immediate -> do
    n <- makeNodeInternal (askDocument >>= \doc -> createComment doc t)
    mapM_ (requestDomAction_ . fmap (setNodeValue n . Just)) mSetContents
    pure n
  HydrationMode_Hydrating -> do
    addHydrationStep $ do
      n <- hydrateNode (const $ pure True) DOM.Comment
      mapM_ (requestDomAction_ . fmap (setNodeValue n . Just)) mSetContents
    pure $ error "commentNodeInternal: DOM.Comment"

-- | We leave markers in the static builder as comments, and rip these comments
-- out at hydration time, replacing them with empty text nodes.
skipToAndReplaceComment
  :: (MonadJSM m, Reflex t, MonadFix m, Adjustable t m, MonadHold t m)
  => Text
  -> IORef Text
  -> HydrationDomBuilderT t m (HydrationRunnerT t m (), IORef DOM.Text, IORef Text)
skipToAndReplaceComment prefix key0Ref = getHydrationMode >>= \case
  HydrationMode_Immediate -> do
    -- If we're in immediate mode, we don't try to replace an existing comment,
    -- and just return a dummy key
    t <- textNodeInternal ("" :: Text) Nothing
    append $ toNode t
    textNodeRef <- liftIO $ newIORef t
    keyRef <- liftIO $ newIORef ""
    pure (pure (), textNodeRef, keyRef)
  HydrationMode_Hydrating -> do
    doc <- askDocument
    textNodeRef <- liftIO $ newIORef $ error "textNodeRef not yet initialized"
    keyRef <- liftIO $ newIORef $ error "keyRef not yet initialized"
    let go key0 mLastNode = do
          parent <- askParent
          node <- maybe (Node.getFirstChildUnchecked parent) Node.getNextSiblingUnchecked mLastNode
          DOM.castTo DOM.Comment node >>= \case
            Just comment -> do
              commentText <- Node.getTextContentUnchecked comment
              case T.stripPrefix (prefix <> key0) commentText of
                Just key -> do
                  -- Replace the comment with an (invisible) text node
                  tn <- createTextNode doc ("" :: Text)
                  Node.replaceChild_ parent tn comment
                  pure (tn, key)
                Nothing -> do
                  go key0 (Just node)
            Nothing -> do
              go key0 (Just node)
        switchComment = do
          key0 <- liftIO $ readIORef key0Ref
          (tn, key) <- go key0 =<< getPreviousNode
          setPreviousNode $ Just $ toNode tn
          liftIO $ do
            writeIORef textNodeRef tn
            writeIORef keyRef key
    pure (switchComment, textNodeRef, keyRef)

skipToReplaceStart :: (MonadJSM m, Reflex t, MonadFix m, Adjustable t m, MonadHold t m) => HydrationDomBuilderT t m (HydrationRunnerT t m (), IORef DOM.Text, IORef Text)
skipToReplaceStart = skipToAndReplaceComment "replace-start" =<< liftIO (newIORef "")

skipToReplaceEnd :: (MonadJSM m, Reflex t, MonadFix m, Adjustable t m, MonadHold t m) => IORef Text -> HydrationDomBuilderT t m (HydrationRunnerT t m (), IORef DOM.Text)
skipToReplaceEnd key = fmap (\(m,e,_) -> (m,e)) $ skipToAndReplaceComment "replace-end" key

instance SupportsHydrationDomBuilder t m => NotReady t (HydrationDomBuilderT t m) where
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

instance (SupportsHydrationDomBuilder t m) => DomBuilder t (HydrationDomBuilderT t m) where
  type DomBuilderSpace (HydrationDomBuilderT t m) = HydrationDomSpace
  {-# INLINABLE textNode #-}
  textNode (TextNodeConfig initialContents mSetContents) = do
    t <- textNodeInternal initialContents mSetContents
    return $ TextNode ()
  {-# INLINABLE commentNode #-}
  commentNode (CommentNodeConfig initialContents mSetContents) = do
    c <- commentNodeInternal initialContents mSetContents
    return $ CommentNode ()
  {-# INLINABLE element #-}
  element elementTag cfg child = do
    fst <$> makeElement elementTag cfg child
  {-# INLINABLE inputElement #-}
  inputElement cfg = do
    ((e, _), domElementRef) <- makeElement "input" (cfg ^. inputElementConfig_elementConfig) $ return ()

    (valueChangedByUI, triggerChangeByUI) <- newTriggerEvent
    (valueChangedBySetValue, triggerChangeBySetValue) <- newTriggerEvent

    (focusChange, triggerFocusChange) <- newTriggerEvent
    (checkedChangedByUI, triggerCheckedChangedByUI) <- newTriggerEvent
    (checkedChangedBySetChecked, triggerCheckedChangedBySetChecked) <- newTriggerEvent

    (fileChange, triggerFileChange) <- newTriggerEvent

    doc <- askDocument

    -- Expected initial value from config
    let v0 = _inputElementConfig_initialValue cfg

    addHydrationStep $ do
      domElement <- liftIO $ readIORef domElementRef
      let domInputElement = uncheckedCastTo DOM.HTMLInputElement domElement
          getValue = Input.getValue domInputElement

      -- The browser might have messed with the value, or the user could have
      -- altered it before activation, so we set it if it isn't what we expect
      liftJSM getValue >>= \v0' -> do
        when (v0' /= v0) $ liftIO $ triggerChangeByUI v0'

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

      Input.setChecked domInputElement $ _inputElementConfig_initialChecked cfg
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

    checked' <- holdDyn (_inputElementConfig_initialChecked cfg) $ leftmost
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
      , _inputElement_raw = undefined -- TODO domInputElement
      , _inputElement_files = files
      }

  {-# INLINABLE textAreaElement #-}
  textAreaElement cfg = do
    ((e, _), domElementRef) <- makeElement "textarea" (cfg ^. textAreaElementConfig_elementConfig) $ return ()

    (valueChangedByUI, triggerChangeByUI) <- newTriggerEvent
    (valueChangedBySetValue, triggerChangeBySetValue) <- newTriggerEvent

    (focusChange, triggerFocusChange) <- newTriggerEvent

    doc <- askDocument

    -- Expected initial value from config
    let v0 = _textAreaElementConfig_initialValue cfg

    addHydrationStep $ do
      domElement <- liftIO $ readIORef domElementRef
      let domTextAreaElement = uncheckedCastTo DOM.HTMLTextAreaElement domElement
          getValue = TextArea.getValue domTextAreaElement

      -- The browser might have messed with the value, or the user could have
      -- altered it before activation, so we set it if it isn't what we expect
      liftJSM getValue >>= \v0' -> do
        when (v0' /= v0) $ liftIO $ triggerChangeByUI v0'

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
      , _textAreaElement_raw = undefined -- TODO domTextAreaElement
      }

  {-# INLINABLE selectElement #-}
  selectElement cfg child = do
    ((e, result), domElementRef) <- makeElement "select" (cfg ^. selectElementConfig_elementConfig) child

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

    return $ flip (,) result $ SelectElement
      { _selectElement_value = v
      , _selectElement_change = valueChangedByUI
      , _selectElement_hasFocus = hasFocus
      , _selectElement_element = e
      , _selectElement_raw = undefined -- TODO domSelectElement
      }

  placeRawElement () = pure () -- TODO
--  placeRawElement e = getHydrationMode >>= \case
--    HydrationMode_Immediate -> append $ toNode e
--    HydrationMode_Hydrating -> addHydrationStep $ insertAfterPreviousNode e
  wrapRawElement () _cfg = pure $ Element (EventSelector $ const never) () -- TODO
--  wrapRawElement e rawCfg = do
--    ctx <- askJSM
--    events <- askEvents
--    let cfg = (def :: ElementConfig EventResult t HydrationDomSpace)
--          { _elementConfig_modifyAttributes = _rawElementConfig_modifyAttributes rawCfg
--          , _elementConfig_eventSpec = _rawElementConfig_eventSpec rawCfg
--          }
--    eventTriggerRefs <- wrap events e rawCfg
--    es <- newFanEventWithTrigger $ triggerBody ctx cfg events eventTriggerRefs e
--    return $ Element es e

data FragmentState
  = FragmentState_Unmounted
  | FragmentState_Mounted (DOM.Text, DOM.Text)

data HydrationDomFragment = HydrationDomFragment
  { _hydrationDomFragment_document :: DOM.DocumentFragment
  , _hydrationDomFragment_state :: IORef FragmentState
  }

extractFragment :: MonadJSM m => HydrationDomFragment -> m ()
extractFragment fragment = do
  s <- liftIO $ readIORef $ _hydrationDomFragment_state fragment
  case s of
    FragmentState_Unmounted -> return ()
    FragmentState_Mounted (before, after) -> do
      extractBetweenExclusive (_hydrationDomFragment_document fragment) before after
      liftIO $ writeIORef (_hydrationDomFragment_state fragment) FragmentState_Unmounted

instance SupportsHydrationDomBuilder t m => MountableDomBuilder t (HydrationDomBuilderT t m) where
  type DomFragment (HydrationDomBuilderT t m) = HydrationDomFragment
  buildDomFragment w = do
    df <- createDocumentFragment =<< askDocument
    p <- liftIO $ newIORef $ toNode df
    result <- localEnv (\env -> env { _hydrationDomBuilderEnv_parent = p }) w
    s <- liftIO $ newIORef FragmentState_Unmounted
    return (HydrationDomFragment df s, result)
  mountDomFragment fragment setFragment = do
    parent <- getParent
    extractFragment fragment
    before <- textNodeInternal ("" :: Text) Nothing
    appendChild_ parent $ _hydrationDomFragment_document fragment
    after <- textNodeInternal ("" :: Text) Nothing
    xs <- foldDyn (\new (previous, _) -> (new, Just previous)) (fragment, Nothing) setFragment
    requestDomAction_ $ ffor (updated xs) $ \(childFragment, Just previousFragment) -> do
      extractFragment previousFragment
      extractFragment childFragment
      insertBefore (_hydrationDomFragment_document childFragment) after
      liftIO $ writeIORef (_hydrationDomFragment_state childFragment) $ FragmentState_Mounted (before, after)
    liftIO $ writeIORef (_hydrationDomFragment_state fragment) $ FragmentState_Mounted (before, after)

instance (Reflex t, Monad m, Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (DomRenderHookT t m) where
  runWithReplace a0 a' = DomRenderHookT $ runWithReplace (unDomRenderHookT a0) (fmapCheap unDomRenderHookT a')
  traverseIntMapWithKeyWithAdjust f m = DomRenderHookT . traverseIntMapWithKeyWithAdjust (\k -> unDomRenderHookT . f k) m
  traverseDMapWithKeyWithAdjust f m = DomRenderHookT . traverseDMapWithKeyWithAdjust (\k -> unDomRenderHookT . f k) m
  traverseDMapWithKeyWithAdjustWithMove f m = DomRenderHookT . traverseDMapWithKeyWithAdjustWithMove (\k -> unDomRenderHookT . f k) m

instance (Adjustable t m, MonadJSM m, MonadHold t m, MonadFix m, PrimMonad m) => Adjustable t (HydrationDomBuilderT t m) where
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
          p <- liftIO $ newIORef p'
          unreadyChildren <- liftIO $ newIORef 0
          let a0' = case h of
                HydrationMode_Hydrating -> a0
                HydrationMode_Immediate -> do
                  a <- a0
                  insertBefore p' =<< liftIO (readIORef after)
                  pure a
          (result, dom) <- flip runStateT (pure ()) $ runReaderT (unHydrationDomBuilderT a0') initialEnv
            { _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
            , _hydrationDomBuilderEnv_commitAction = myCommitAction
            , _hydrationDomBuilderEnv_parent = p
            }
          liftIO $ readIORef unreadyChildren >>= \case
            0 -> writeIORef haveEverBeenReady True
            _ -> modifyIORef' parentUnreadyChildren succ
          return (dom, result)
    a'' <- numberOccurrences a'
    ((hydrate0, result0), child') <- HydrationDomBuilderT $ lift $ lift $ runWithReplace drawInitialChild $ ffor a'' $ \(cohortId, child) -> do
      h <- liftIO $ readIORef hydrating
      p' <- case h of
        HydrationMode_Hydrating -> pure parent
        HydrationMode_Immediate -> toNode <$> createDocumentFragment doc
      p <- liftIO $ newIORef p'
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
      (result, dom) <- flip runStateT (pure ()) $ runReaderT (unHydrationDomBuilderT child) $ initialEnv
            { _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
            , _hydrationDomBuilderEnv_commitAction = case h of
              HydrationMode_Hydrating -> myCommitAction
              HydrationMode_Immediate -> commitAction
            , _hydrationDomBuilderEnv_parent = p
            }
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
    let updateChildUnreadiness (p :: PatchDMapWithMove k (Compose (TraverseDMapChild t m k) v')) old = do
          let new :: forall a. k a -> PatchDMapWithMove.NodeInfo k (Compose (TraverseDMapChild t m k) v') a -> IO (PatchDMapWithMove.NodeInfo k (Constant (IORef (ChildReadyState k))) a)
              new k = PatchDMapWithMove.nodeInfoMapFromM $ \case
                PatchDMapWithMove.From_Insert (Compose (TraverseDMapChild (Right myTypeImmediate) _)) -> do
                  readIORef (_traverseDMapChildImmediate_childReadyState myTypeImmediate) >>= \case
                    ChildReadyState_Ready -> return PatchDMapWithMove.From_Delete
                    ChildReadyState_Unready _ -> do
                      writeIORef (_traverseDMapChildImmediate_childReadyState myTypeImmediate) $ ChildReadyState_Unready $ Just $ Some.This k
                      return $ PatchDMapWithMove.From_Insert $ Constant (_traverseDMapChildImmediate_childReadyState myTypeImmediate)
                PatchDMapWithMove.From_Delete -> return PatchDMapWithMove.From_Delete
                PatchDMapWithMove.From_Move fromKey -> return $ PatchDMapWithMove.From_Move fromKey
              deleteOrMove :: forall a. k a -> Product (Constant (IORef (ChildReadyState k))) (ComposeMaybe k) a -> IO (Constant () a)
              deleteOrMove _ (Pair (Constant sRef) (ComposeMaybe mToKey)) = do
                writeIORef sRef $ ChildReadyState_Unready $ Some.This <$> mToKey -- This will be Nothing if deleting, and Just if moving, so it works out in both cases
                return $ Constant ()
          p' <- fmap unsafePatchDMapWithMove $ DMap.traverseWithKey new $ unPatchDMapWithMove p
          _ <- DMap.traverseWithKey deleteOrMove $ PatchDMapWithMove.getDeletionsAndMoves p old
          return $ applyAlways p' old
    hoistTraverseWithKeyWithAdjust traverseDMapWithKeyWithAdjustWithMove mapPatchDMapWithMove updateChildUnreadiness $ \placeholders lastPlaceholder (p_ :: PatchDMapWithMove k (Compose (TraverseDMapChild t m k) v')) -> do
      let p = unPatchDMapWithMove p_
      phsBefore <- liftIO $ readIORef placeholders
      let collectIfMoved :: forall a. k a -> PatchDMapWithMove.NodeInfo k (Compose (TraverseDMapChild t m k) v') a -> JSM (Constant (Maybe DOM.DocumentFragment) a)
          collectIfMoved k e = do
            let mThisPlaceholder = Map.lookup (Some.This k) phsBefore -- Will be Nothing if this element wasn't present before
                nextPlaceholder = maybe lastPlaceholder snd $ Map.lookupGT (Some.This k) phsBefore
            case isJust $ getComposeMaybe $ PatchDMapWithMove._nodeInfo_to e of
              False -> do
                mapM_ (`deleteUpTo` nextPlaceholder) mThisPlaceholder
                return $ Constant Nothing
              True -> do
                Constant <$> mapM (`collectUpTo` nextPlaceholder) mThisPlaceholder
      collected <- DMap.traverseWithKey collectIfMoved p
      let !phsAfter = fromMaybe phsBefore $ apply (weakenPatchDMapWithMoveWith (_traverseDMapChildImmediate_placeholder . error "FIXME" . _traverseDMapChild_mode . getCompose) p_) phsBefore --TODO: Don't recompute this
      let placeFragment :: forall a. k a -> PatchDMapWithMove.NodeInfo k (Compose (TraverseDMapChild t m k) v') a -> JSM (Constant () a)
          placeFragment k e = do
            let nextPlaceholder = maybe lastPlaceholder snd $ Map.lookupGT (Some.This k) phsAfter
            case PatchDMapWithMove._nodeInfo_from e of
              PatchDMapWithMove.From_Insert (Compose (TraverseDMapChild x _)) -> case x of
                Left _ -> pure ()
                Right immediate -> _traverseDMapChildImmediate_fragment immediate `insertBefore` nextPlaceholder
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
  :: forall t m (k :: * -> *) v v'. (Adjustable t m, MonadHold t m, MonadFix m, MonadJSM m, PrimMonad m, DMap.GCompare k)
  => (forall a. k a -> v a -> HydrationDomBuilderT t m (v' a))
  -> DMap k v
  -> Event t (PatchDMap k v)
  -> HydrationDomBuilderT t m (DMap k v', Event t (PatchDMap k v'))
traverseDMapWithKeyWithAdjust' = do
  let updateChildUnreadiness (p :: PatchDMap k (Compose (TraverseDMapChild t m k) v')) old = do
        let new :: forall a. k a -> ComposeMaybe (Compose (TraverseDMapChild t m k) v') a -> IO (ComposeMaybe (Constant (IORef (ChildReadyState k))) a)
            new k (ComposeMaybe m) = ComposeMaybe <$> case m of
              Nothing -> return Nothing
              Just (Compose (TraverseDMapChild (Left hydration) _)) -> pure Nothing
              Just (Compose (TraverseDMapChild (Right immediate) _)) -> do
                readIORef (_traverseDMapChildImmediate_childReadyState immediate) >>= \case
                  ChildReadyState_Ready -> return Nothing -- Delete this child, since it's ready
                  ChildReadyState_Unready _ -> do
                    writeIORef (_traverseDMapChildImmediate_childReadyState immediate) $ ChildReadyState_Unready $ Just $ Some.This k
                    return $ Just $ Constant (_traverseDMapChildImmediate_childReadyState immediate)
            delete _ (Constant sRef) = do
              writeIORef sRef $ ChildReadyState_Unready Nothing
              return $ Constant ()
        p' <- fmap PatchDMap $ DMap.traverseWithKey new $ unPatchDMap p
        _ <- DMap.traverseWithKey delete $ PatchDMap.getDeletions p old
        return $ applyAlways p' old
  hoistTraverseWithKeyWithAdjust traverseDMapWithKeyWithAdjust mapPatchDMap updateChildUnreadiness $ \placeholders lastPlaceholder (PatchDMap patch) -> do
    phs <- liftIO $ readIORef placeholders
    forM_ (DMap.toList patch) $ \(k :=> ComposeMaybe mv) -> do
      let nextPlaceholder = maybe lastPlaceholder snd $ Map.lookupGT (Some.This k) phs
      -- Delete old node
      forM_ (Map.lookup (Some.This k) phs) $ \thisPlaceholder -> do
        thisPlaceholder `deleteUpTo` nextPlaceholder
      -- Insert new node
      forM_ mv $ \(Compose (TraverseDMapChild e _)) -> case e of
        Left _hydration -> pure ()
        Right immediate -> do
          _traverseDMapChildImmediate_fragment immediate `insertBefore` nextPlaceholder
    let weakened :: PatchMap (Some.Some k) (Either (TraverseDMapChildHydration t m) (TraverseDMapChildImmediate k))
        weakened = weakenPatchDMapWith (_traverseDMapChild_mode . getCompose) $ PatchDMap patch
        filtered :: PatchMap (Some.Some k) DOM.Text
        filtered = PatchMap $ flip Map.mapMaybe (unPatchMap weakened) $ \case
          Nothing -> Just Nothing -- deletion
          Just (Left _) -> Nothing
          Just (Right immediate) -> Just $ Just $ _traverseDMapChildImmediate_placeholder immediate
        a = apply filtered phs
    liftIO $ writeIORef placeholders $! fromMaybe phs a

{-# INLINABLE traverseIntMapWithKeyWithAdjust' #-}
traverseIntMapWithKeyWithAdjust'
  :: forall t m v v'. (Adjustable t m, MonadJSM m, MonadFix m, PrimMonad m, MonadHold t m)
  => (IntMap.Key -> v -> HydrationDomBuilderT t m v')
  -> IntMap v
  -> Event t (PatchIntMap v)
  -> HydrationDomBuilderT t m (IntMap v', Event t (PatchIntMap v'))
traverseIntMapWithKeyWithAdjust' = do
  let updateChildUnreadiness (p@(PatchIntMap pInner) :: PatchIntMap (HydrationRunnerT t m (), DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v')) old = do
        let new :: IntMap.Key -> Maybe (HydrationRunnerT t m (), DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v') -> IO (Maybe (IORef ChildReadyStateInt))
            new k m = case m of
              Nothing -> return Nothing
              Just (_, _, _, sRef, _) -> do
                readIORef sRef >>= \case
                  ChildReadyStateInt_Ready -> return Nothing -- Delete this child, since it's ready
                  ChildReadyStateInt_Unready _ -> do
                    writeIORef sRef $ ChildReadyStateInt_Unready $ Just k
                    return $ Just sRef
            delete _ sRef = do
              writeIORef sRef $ ChildReadyStateInt_Unready Nothing
              return ()
        p' <- PatchIntMap <$> IntMap.traverseWithKey new pInner
        _ <- IntMap.traverseWithKey delete $ FastMutableIntMap.getDeletions p old
        return $ applyAlways p' old
  hoistTraverseIntMapWithKeyWithAdjust traverseIntMapWithKeyWithAdjust updateChildUnreadiness $ \placeholders lastPlaceholderRef (PatchIntMap p) -> do
    phs <- liftIO $ readIORef placeholders
    forM_ (IntMap.toList p) $ \(k, mv) -> do
      lastPlaceholder <- liftIO $ readIORef lastPlaceholderRef
      let nextPlaceholder = maybe lastPlaceholder snd $ IntMap.lookupGT k phs
      forM_ (IntMap.lookup k phs) $ \thisPlaceholder -> thisPlaceholder `deleteUpTo` nextPlaceholder
      forM_ mv $ \(_, df, _, _, _) -> df `insertBefore` nextPlaceholder
    liftIO $ writeIORef placeholders $! fromMaybe phs $ apply ((\(_, _, ph, _, _) -> ph) <$> PatchIntMap p) phs

{-# INLINE hoistTraverseIntMapWithKeyWithAdjust #-}
hoistTraverseIntMapWithKeyWithAdjust :: forall v v' t m p.
  ( Adjustable t m
  , MonadHold t m
  , MonadJSM m
  , MonadFix m
  , PrimMonad m
  , Monoid (p (HydrationRunnerT t m (), DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v'))
  , Functor p
  , PatchTarget (p (HydrationRunnerT t m ())) ~ IntMap (HydrationRunnerT t m ())
  , Patch (p (HydrationRunnerT t m ()))
  )
  => (   (IntMap.Key -> v -> DomRenderHookT t m (HydrationRunnerT t m (), DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v'))
      -> IntMap v
      -> Event t (p v)
      -> DomRenderHookT t m (IntMap (HydrationRunnerT t m (), DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v'), Event t (p (HydrationRunnerT t m (), DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v')))
     ) -- ^ The base monad's traversal
  -> (p (HydrationRunnerT t m (), DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v') -> IntMap (IORef ChildReadyStateInt) -> IO (IntMap (IORef ChildReadyStateInt))) -- ^ Given a patch for the children DOM elements, produce a patch for the childrens' unreadiness state
  -> (IORef (IntMap DOM.Text) -> IORef DOM.Text -> p (HydrationRunnerT t m (), DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v') -> JSM ()) -- ^ Apply a patch to the DOM
  -> (IntMap.Key -> v -> HydrationDomBuilderT t m v')
  -> IntMap v
  -> Event t (p v)
  -> HydrationDomBuilderT t m (IntMap v', Event t (p v'))
hoistTraverseIntMapWithKeyWithAdjust base updateChildUnreadiness applyDomUpdate_ f dm0 dm' = do
  initialEnv <- HydrationDomBuilderT ask
  let parentUnreadyChildren = _hydrationDomBuilderEnv_unreadyChildren initialEnv
  pendingChange :: IORef (IntMap (IORef ChildReadyStateInt), p (HydrationRunnerT t m (), DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v')) <- liftIO $ newIORef mempty
  haveEverBeenReady <- liftIO $ newIORef False
  placeholders <- liftIO $ newIORef $ error "placeholders not yet initialized"
  lastPlaceholderRef <- liftIO $ newIORef $ error "lastPlaceholderRef not yet initialized"
  let applyDomUpdate p = do
        applyDomUpdate_ placeholders lastPlaceholderRef p
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
      markChildReady :: IORef ChildReadyStateInt -> JSM ()
      markChildReady childReadyState = do
        liftIO (readIORef childReadyState) >>= \case
          ChildReadyStateInt_Ready -> return ()
          ChildReadyStateInt_Unready countedAt -> do
            liftIO $ writeIORef childReadyState ChildReadyStateInt_Ready
            case countedAt of
              Nothing -> return ()
              Just k -> do -- This child has been counted as unready, so we need to remove it from the unready set
                (oldUnready, p) <- liftIO $ readIORef pendingChange
                when (not $ IntMap.null oldUnready) $ do -- This shouldn't actually ever be null
                  let newUnready = IntMap.delete k oldUnready
                  liftIO $ writeIORef pendingChange (newUnready, p)
                  when (IntMap.null newUnready) $ do
                    applyDomUpdate p
  (children0, children') <- HydrationDomBuilderT $ lift $ lift $ base (\k v -> drawChildUpdateInt initialEnv markChildReady $ f k v) dm0 dm'
  let processChild k (_, _, _, sRef, _) = do
        readIORef sRef >>= \case
          ChildReadyStateInt_Ready -> return Nothing
          ChildReadyStateInt_Unready _ -> do
            writeIORef sRef $ ChildReadyStateInt_Unready $ Just k
            return $ Just sRef
  initialUnready <- liftIO $ IntMap.mapMaybe id <$> IntMap.traverseWithKey processChild children0
  liftIO $ if IntMap.null initialUnready
    then writeIORef haveEverBeenReady True
    else do
      modifyIORef' parentUnreadyChildren succ
      writeIORef pendingChange (initialUnready, mempty) -- The patch is always empty because it got applied implicitly when we ran the children the first time
  let result0 = IntMap.map (\(_, _, _, _, v) -> v) children0
      placeholders0 = fmap (\(_, _, ph, _, _) -> ph) children0
      delayed0 :: IntMap (HydrationRunnerT t m ())
      delayed0 = IntMap.map (\(delayed, _, _, _, _) -> delayed) children0
      delayed' :: Event t (p (HydrationRunnerT t m ()))
      delayed' = ffor children' $ fmap $ \(delayed, _, _, _, _) -> delayed
      result' = ffor children' $ fmap $ \(_, _, _, _, r) -> r
  doc <- askDocument
  let setFinalPlaceholder = do
        ph <- createTextNode doc ("" :: Text)
        insertAfterPreviousNode ph
        liftIO $ writeIORef lastPlaceholderRef ph
  addHydrationStepWithSetup (accumMaybeB (flip apply) delayed0 delayed') $ \delayed -> do
    _ <- join $ sample (sequence . IntMap.elems <$> delayed)
    setFinalPlaceholder
  liftIO $ writeIORef placeholders $! placeholders0
  getHydrationMode >>= \case
    HydrationMode_Immediate -> void $ IntMap.traverseWithKey (\_ (_, df, _, _, _) -> void $ append $ toNode df) children0
    _ -> pure ()
  requestDomAction_ $ ffor children' $ \p -> do
    (oldUnready, oldP) <- liftIO $ readIORef pendingChange
    newUnready <- liftIO $ updateChildUnreadiness p oldUnready
    let !newP = p <> oldP
    liftIO $ writeIORef pendingChange (newUnready, newP)
    when (IntMap.null newUnready) $ do
      applyDomUpdate newP
  return (result0, result')

insertAfterPreviousNode :: (Monad m, MonadJSM m) => DOM.IsNode node => node -> HydrationRunnerT t m ()
insertAfterPreviousNode node = do
  parent <- askParent
  nextNode <- maybe (Node.getFirstChild parent) Node.getNextSibling =<< getPreviousNode
  Node.insertBefore_ parent node nextNode
  setPreviousNode $ Just $ toNode node

data TraverseDMapChildImmediate k = TraverseDMapChildImmediate
  { _traverseDMapChildImmediate_fragment :: DOM.DocumentFragment
  -- ^ Child is appended to this fragment
  , _traverseDMapChildImmediate_placeholder :: DOM.Text
  -- ^ Placeholder reference
  , _traverseDMapChildImmediate_childReadyState :: IORef (ChildReadyState k)
  }

newtype TraverseDMapChildHydration t m = TraverseDMapChildHydration
  { _traverseDMapChildHydration_delayed :: HydrationRunnerT t m DOM.Text
  -- ^ Action to run at switchover, returns the placeholder
  }

data TraverseDMapChild t m k a = TraverseDMapChild
  { _traverseDMapChild_mode :: Either (TraverseDMapChildHydration t m) (TraverseDMapChildImmediate k)
  , _traverseDMapChild_result :: a
  }

{-# INLINABLE hoistTraverseWithKeyWithAdjust #-}
hoistTraverseWithKeyWithAdjust :: forall (k :: * -> *) v v' t m p.
  ( Adjustable t m
  , MonadHold t m
  , DMap.GCompare k
  , MonadIO m
  , MonadJSM m
  , PrimMonad m
  , MonadFix m
  , Patch (p k v)
  , Patch (p k (Constant Int))
  , PatchTarget (p k (Constant Int)) ~ DMap k (Constant Int)
  , Patch (p k (Compose (TraverseDMapChild t m k) v'))
  , PatchTarget (p k (Compose (TraverseDMapChild t m k) v')) ~ DMap k (Compose (TraverseDMapChild t m k) v')
  , Monoid (p k (Compose (TraverseDMapChild t m k) v'))
  )
  => (forall vv vv'.
         (forall a. k a -> vv a -> DomRenderHookT t m (vv' a))
      -> DMap k vv
      -> Event t (p k vv)
      -> DomRenderHookT t m (DMap k vv', Event t (p k vv'))
     )
  -- ^ The base monad's traversal
  -> (forall vv vv'. (forall a. vv a -> vv' a) -> p k vv -> p k vv')
  -- ^ A way of mapping over the patch type
  -> (p k (Compose (TraverseDMapChild t m k) v') -> DMap k (Constant (IORef (ChildReadyState k))) -> IO (DMap k (Constant (IORef (ChildReadyState k)))))
  -- ^ Given a patch for the children DOM elements, produce a patch for the childrens' unreadiness state
  -> (IORef (Map.Map (Some.Some k) DOM.Text) -> DOM.Text -> p k (Compose (TraverseDMapChild t m k) v') -> JSM ())
  -- ^ Apply a patch to the DOM
  -> (forall a. k a -> v a -> HydrationDomBuilderT t m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> HydrationDomBuilderT t m (DMap k v', Event t (p k v'))
hoistTraverseWithKeyWithAdjust base mapPatch updateChildUnreadiness applyDomUpdate_ (f :: forall a. k a -> v a -> HydrationDomBuilderT t m (v' a)) (dm0 :: DMap k v) dm' = do
  doc <- askDocument
  initialEnv <- HydrationDomBuilderT ask
  let parentUnreadyChildren = _hydrationDomBuilderEnv_unreadyChildren initialEnv
  pendingChange :: IORef (DMap k (Constant (IORef (ChildReadyState k))), p k (Compose (TraverseDMapChild t m k) v')) <- liftIO $ newIORef mempty
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
      markChildReady :: IORef (ChildReadyState k) -> JSM ()
      markChildReady childReadyState = do
        liftIO (readIORef childReadyState) >>= \case
          ChildReadyState_Ready -> return ()
          ChildReadyState_Unready countedAt -> do
            liftIO $ writeIORef childReadyState ChildReadyState_Ready
            case countedAt of
              Nothing -> return ()
              Just (Some.This k) -> do -- This child has been counted as unready, so we need to remove it from the unready set
                (oldUnready, p) <- liftIO $ readIORef pendingChange
                when (not $ DMap.null oldUnready) $ do -- This shouldn't actually ever be null
                  let newUnready = DMap.delete k oldUnready
                  liftIO $ writeIORef pendingChange (newUnready, p)
                  when (DMap.null newUnready) $ do
                    applyDomUpdate p
  (children0 :: DMap k (Compose (TraverseDMapChild t m k) v'), children' :: Event t (p k (Compose (TraverseDMapChild t m k) v')))
    <- HydrationDomBuilderT $ lift $ lift $ base (\k v -> drawChildUpdate initialEnv markChildReady $ f k v) dm0 dm'
  let processChild k (Compose (TraverseDMapChild e _)) = case e of
        Left _ -> pure $ ComposeMaybe Nothing
        Right immediate -> ComposeMaybe <$> do
          readIORef (_traverseDMapChildImmediate_childReadyState immediate) >>= \case
            ChildReadyState_Ready -> return Nothing
            ChildReadyState_Unready _ -> do
              writeIORef (_traverseDMapChildImmediate_childReadyState immediate) $ ChildReadyState_Unready $ Just $ Some.This k
              return $ Just $ Constant (_traverseDMapChildImmediate_childReadyState immediate)
  initialUnready <- liftIO $ DMap.mapMaybeWithKey (\_ -> getComposeMaybe) <$> DMap.traverseWithKey processChild children0
  liftIO $ if DMap.null initialUnready
    then writeIORef haveEverBeenReady True
    else do
      modifyIORef' parentUnreadyChildren succ
      writeIORef pendingChange (initialUnready, mempty) -- The patch is always empty because it got applied implicitly when we ran the children the first time
  getHydrationMode >>= \case
    HydrationMode_Hydrating -> addHydrationStepWithSetup (holdIncremental children0 children') $ \children -> do
      dm :: DMap k (Compose (TraverseDMapChild t m k) v') <- sample $ currentIncremental children
      phs <- traverse id $ weakenDMapWith (either _traverseDMapChildHydration_delayed (pure . _traverseDMapChildImmediate_placeholder) . _traverseDMapChild_mode . getCompose) dm
      liftIO $ writeIORef placeholders $! phs
      insertAfterPreviousNode lastPlaceholder
    HydrationMode_Immediate -> do
      let activate i = do
            append $ toNode $ _traverseDMapChildImmediate_fragment i
            pure $ _traverseDMapChildImmediate_placeholder i
      phs <- traverse id $ weakenDMapWith ((either (error "delayed") activate) . _traverseDMapChild_mode . getCompose) children0
      liftIO $ writeIORef placeholders $! phs
      append $ toNode lastPlaceholder

  requestDomAction_ $ ffor children' $ \p -> do
    (oldUnready, oldP) <- liftIO $ readIORef pendingChange
    newUnready <- liftIO $ updateChildUnreadiness p oldUnready
    let !newP = p <> oldP
    liftIO $ writeIORef pendingChange (newUnready, newP)
    when (DMap.null newUnready) $ do
      applyDomUpdate newP
  let result0 = DMap.map (_traverseDMapChild_result . getCompose) children0
      result' = ffor children' $ mapPatch $ _traverseDMapChild_result . getCompose
  return (result0, result')

{-# INLINABLE drawChildUpdate #-}
drawChildUpdate :: (MonadIO m, MonadJSM m, Reflex t)
  => HydrationDomBuilderEnv t
  -> (IORef (ChildReadyState k) -> JSM ()) -- This will NOT be called if the child is ready at initialization time; instead, the ChildReadyState return value will be ChildReadyState_Ready
  -> HydrationDomBuilderT t m (v' a)
  -> DomRenderHookT t m (Compose (TraverseDMapChild t m k) v' a)
drawChildUpdate initialEnv markReady child = liftIO (readIORef $ _hydrationDomBuilderEnv_hydrationMode initialEnv) >>= \case
  HydrationMode_Hydrating -> do
    let doc = _hydrationDomBuilderEnv_document initialEnv
    unreadyChildren <- liftIO $ newIORef 0
    (result, childDelayed) <- flip runStateT (pure ()) $ runReaderT (unHydrationDomBuilderT child) initialEnv
          { _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
          }
    let delayed = do
          placeholder <- createTextNode doc ("" :: Text)
          insertAfterPreviousNode placeholder
          pure placeholder
    return $ Compose $ TraverseDMapChild
      { _traverseDMapChild_result = result
      , _traverseDMapChild_mode = Left TraverseDMapChildHydration
        { _traverseDMapChildHydration_delayed = delayed <* childDelayed
        }
      }
  HydrationMode_Immediate -> do
    childReadyState <- liftIO $ newIORef $ ChildReadyState_Unready Nothing
    let doc = _hydrationDomBuilderEnv_document initialEnv
    unreadyChildren <- liftIO $ newIORef 0
    df <- createDocumentFragment doc
    placeholder <- createTextNode doc ("" :: Text)
    Node.appendChild_ df placeholder
    parentRef <- liftIO . newIORef $ toNode df
    (result, _s) <- flip runStateT (pure ()) $ runReaderT (unHydrationDomBuilderT child) initialEnv
          { _hydrationDomBuilderEnv_parent = parentRef
          , _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
          , _hydrationDomBuilderEnv_commitAction = markReady childReadyState
          }
    u <- liftIO $ readIORef unreadyChildren
    when (u == 0) $ liftIO $ writeIORef childReadyState ChildReadyState_Ready
    return $ Compose $ TraverseDMapChild
      { _traverseDMapChild_result = result
      , _traverseDMapChild_mode = Right TraverseDMapChildImmediate
        { _traverseDMapChildImmediate_fragment = df
        , _traverseDMapChildImmediate_placeholder = placeholder
        , _traverseDMapChildImmediate_childReadyState = childReadyState
        }
      }

{-# INLINABLE drawChildUpdateInt #-}
drawChildUpdateInt :: (MonadIO m, MonadJSM m, Reflex t)
  => HydrationDomBuilderEnv t
  -> (IORef ChildReadyStateInt -> JSM ()) -- This will NOT be called if the child is ready at initialization time; instead, the ChildReadyState return value will be ChildReadyState_Ready
  -> HydrationDomBuilderT t m v'
  -> DomRenderHookT t m (HydrationRunnerT t m (), DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v')
drawChildUpdateInt initialEnv markReady child = do
  childReadyState <- liftIO $ newIORef $ ChildReadyStateInt_Unready Nothing
  let doc = _hydrationDomBuilderEnv_document initialEnv
  unreadyChildren <- liftIO $ newIORef 0
  hydrationMode <- liftIO $ readIORef $ _hydrationDomBuilderEnv_hydrationMode initialEnv
  placeholder <- createTextNode doc ("" :: Text)
  (p, delayed) <- case hydrationMode of
    HydrationMode_Hydrating -> pure
      ( Left $ _hydrationDomBuilderEnv_parent initialEnv
      , insertAfterPreviousNode placeholder
      )
    HydrationMode_Immediate -> do
      df <- createDocumentFragment doc
      Node.appendChild_ df placeholder
      pure (Right df, pure ())
  p' <- either pure (liftIO . newIORef . toNode) p
  (result, finalState) <- flip runStateT (pure ()) $ runReaderT (unHydrationDomBuilderT child) initialEnv
        { _hydrationDomBuilderEnv_parent = p'
        , _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
        , _hydrationDomBuilderEnv_commitAction = markReady childReadyState
        }
  u <- liftIO $ readIORef unreadyChildren
  when (u == 0) $ liftIO $ writeIORef childReadyState ChildReadyStateInt_Ready
  return
    ( delayed >> finalState
    , either (const $ error "drawChildUpdateInt: Tried to use document fragment in hydration mode") id p
    , placeholder, childReadyState, result
    )

instance PerformEvent t m => PerformEvent t (HydrationDomBuilderT t m) where
  type Performable (HydrationDomBuilderT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance PostBuild t m => PostBuild t (HydrationDomBuilderT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (HydrationDomBuilderT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) => TriggerEvent t (HydrationDomBuilderT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = HydrationDomBuilderT . lift . lift $ newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = HydrationDomBuilderT . lift . lift $ newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = HydrationDomBuilderT . lift . lift $ newEventWithLazyTriggerWithOnComplete f

instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) => TriggerEvent t (DomRenderHookT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = DomRenderHookT . lift $ newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = DomRenderHookT . lift $ newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = DomRenderHookT . lift $ newEventWithLazyTriggerWithOnComplete f

instance HasJSContext m => HasJSContext (HydrationDomBuilderT t m) where
  type JSContextPhantom (HydrationDomBuilderT t m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance MonadRef m => MonadRef (HydrationDomBuilderT t m) where
  type Ref (HydrationDomBuilderT t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (HydrationDomBuilderT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance (HasJS x m, ReflexHost t) => HasJS x (HydrationDomBuilderT t m) where
  type JSX (HydrationDomBuilderT t m) = JSX m
  liftJS = lift . liftJS

instance MonadSample t m => MonadSample t (HydrationDomBuilderT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (HydrationDomBuilderT t m) where
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
