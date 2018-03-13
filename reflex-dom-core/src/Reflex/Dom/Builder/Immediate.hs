{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
{-# LANGUAGE StandaloneDeriving #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Reflex.Dom.Builder.Immediate
       ( EventTriggerRef (..)
       , ImmediateDomBuilderEnv (..)
       , ImmediateDomBuilderT (..)
       , runImmediateDomBuilderT
       , askParent
       , askEvents
       , append
       , textNodeInternal
       , deleteBetweenExclusive
       , extractBetweenExclusive
       , deleteUpTo
       , extractUpTo
       , SupportsImmediateDomBuilder
       , collectUpTo
       , collectUpToGivenParent
       , EventFilterTriggerRef (..)
       , wrap
       , makeElement
       , GhcjsDomHandler (..)
       , GhcjsDomHandler1 (..)
       , GhcjsDomEvent (..)
       , GhcjsDomSpace
       , GhcjsEventFilter (..)
       , Pair1 (..)
       , Maybe1 (..)
       , GhcjsEventSpec (..)
       , HasDocument (..)
       , ghcjsEventSpec_filters
       , ghcjsEventSpec_handler
       , GhcjsEventHandler (..)
#ifndef USE_TEMPLATE_HASKELL
       , phantom2
#endif
       , drawChildUpdate
       , ChildReadyState (..)
       , ChildReadyStateInt (..)
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
       -- * Internal
       , traverseDMapWithKeyWithAdjust'
       , hoistTraverseWithKeyWithAdjust
       , traverseIntMapWithKeyWithAdjust'
       , hoistTraverseIntMapWithKeyWithAdjust
       ) where

import Foreign.JavaScript.TH
import Reflex.Class as Reflex
import Reflex.Dom.Builder.Class
import Reflex.Dynamic
import Reflex.DynamicWriter (DynamicWriterT)
import Reflex.EventWriter (EventWriterT)
import Reflex.Host.Class
import qualified Reflex.Patch.DMap as PatchDMap
import qualified Reflex.Patch.DMapWithMove as PatchDMapWithMove
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Base
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Base hiding (askEvents)
import qualified Reflex.TriggerEvent.Base as TriggerEventT (askEvents)
import Reflex.TriggerEvent.Class
import Reflex.Query.Base (QueryT)

import Control.Concurrent
import Control.Lens hiding (element, ix)
import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State as Lazy (StateT)
#ifndef USE_TEMPLATE_HASKELL
import Data.Functor.Contravariant (phantom)
#endif
import Data.Bitraversable
import Data.Default
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Misc
import Data.Functor.Product
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid hiding (Product)
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM as DOM
import GHCJS.DOM.Debug (DomHasCallStack)
import GHCJS.DOM.RequestAnimationFrameCallback
import GHCJS.DOM.Document (Document, createDocumentFragment, createElement, createElementNS, createTextNode)
import GHCJS.DOM.Element (getScrollTop, removeAttribute, removeAttributeNS, setAttribute, setAttributeNS)
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.Event as Event
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.DocumentAndElementEventHandlers as Events
import GHCJS.DOM.EventM (EventM, event, on)
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.FileList as FileList
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.HTMLSelectElement as Select
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
import GHCJS.DOM.MouseEvent
import qualified GHCJS.DOM.Touch as Touch
import qualified GHCJS.DOM.TouchEvent as TouchEvent
import qualified GHCJS.DOM.TouchList as TouchList
import GHCJS.DOM.Node (appendChild_, getOwnerDocumentUnchecked, getParentNodeUnchecked, setNodeValue, toNode)
import qualified GHCJS.DOM.Node as DOM (insertBefore_)
import GHCJS.DOM.Types
       (liftJSM, askJSM, runJSM, JSM, MonadJSM,
        FocusEvent, IsElement, IsEvent, IsNode, KeyboardEvent, Node,
        ToDOMString, TouchEvent, WheelEvent, uncheckedCastTo, ClipboardEvent)
import qualified GHCJS.DOM.Types as DOM
import GHCJS.DOM.UIEvent
import GHCJS.DOM.KeyboardEvent as KeyboardEvent
import qualified GHCJS.DOM.Window as Window
import Language.Javascript.JSaddle (call, eval)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.FastMutableIntMap (PatchIntMap (..))
import qualified Data.FastMutableIntMap as FastMutableIntMap

import Reflex.Requester.Base
import Reflex.Requester.Class
import Foreign.JavaScript.Internal.Utils

#ifndef ghcjs_HOST_OS
import GHCJS.DOM.Types (MonadJSM (..))

instance MonadJSM m => MonadJSM (ImmediateDomBuilderT t m) where
    liftJSM' = ImmediateDomBuilderT . liftJSM'
#endif

data ImmediateDomBuilderEnv t
   = ImmediateDomBuilderEnv { _immediateDomBuilderEnv_document :: {-# UNPACK #-} !Document
                            , _immediateDomBuilderEnv_parent :: {-# UNPACK #-} !Node
                            , _immediateDomBuilderEnv_unreadyChildren :: {-# UNPACK #-} !(IORef Word) -- Number of children who still aren't fully rendered
                            , _immediateDomBuilderEnv_commitAction :: !(JSM ()) -- Action to take when all children are ready --TODO: we should probably get rid of this once we invoke it
                            }

newtype ImmediateDomBuilderT t m a = ImmediateDomBuilderT { unImmediateDomBuilderT :: ReaderT (ImmediateDomBuilderEnv t) (RequesterT t JSM Identity (TriggerEventT t m)) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
#if MIN_VERSION_base(4,9,1)
           , MonadAsyncException
#endif
           )

instance PrimMonad m => PrimMonad (ImmediateDomBuilderT x m) where
  type PrimState (ImmediateDomBuilderT x m) = PrimState m
  primitive = lift . primitive

instance MonadTrans (ImmediateDomBuilderT t) where
  lift = ImmediateDomBuilderT . lift . lift . lift

instance (Reflex t, MonadFix m) => DomRenderHook t (ImmediateDomBuilderT t m) where
  withRenderHook hook (ImmediateDomBuilderT a) = do
    e <- ImmediateDomBuilderT ask
    ImmediateDomBuilderT $ lift $ withRequesting $ \rsp -> do
      (x, req) <- lift $ runRequesterT (runReaderT a e) $ runIdentity <$> rsp
      return (ffor req $ \rm -> hook $ traverseRequesterData (\r -> Identity <$> r) rm, x)
  requestDomAction = ImmediateDomBuilderT . lift . requestingIdentity
  requestDomAction_ = ImmediateDomBuilderT . lift . requesting_

{-# INLINABLE runImmediateDomBuilderT #-}
runImmediateDomBuilderT
  :: ( MonadFix m
     , PerformEvent t m
     , MonadReflexCreateTrigger t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadRef m
     , Ref m ~ IORef
     )
  => ImmediateDomBuilderT t m a
  -> ImmediateDomBuilderEnv t
  -> Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -> m a
runImmediateDomBuilderT (ImmediateDomBuilderT a) env eventChan =
  flip runTriggerEventT eventChan $ do
    rec (x, req) <- runRequesterT (runReaderT a env) rsp
        rsp <- performEventAsync $ ffor req $ \rm f -> liftJSM $ runInAnimationFrame f $
          traverseRequesterData (\r -> Identity <$> r) rm
    return x
  where
    runInAnimationFrame f x = void . DOM.inAnimationFrame' $ \_ -> do
        v <- synchronously x
        void . liftIO $ f v

class Monad m => HasDocument m where
  askDocument :: m Document
  default askDocument :: (m ~ f m', MonadTrans f, Monad m', HasDocument m') => m Document
  askDocument = lift askDocument

instance HasDocument m => HasDocument (ReaderT r m)
instance HasDocument m => HasDocument (StateT s m)
instance HasDocument m => HasDocument (Lazy.StateT s m)
instance HasDocument m => HasDocument (EventWriterT t w m)
instance HasDocument m => HasDocument (DynamicWriterT t w m)
instance HasDocument m => HasDocument (PostBuildT t m)
instance HasDocument m => HasDocument (RequesterT t request response m)
instance HasDocument m => HasDocument (QueryT t q m)

instance Monad m => HasDocument (ImmediateDomBuilderT t m) where
  {-# INLINABLE askDocument #-}
  askDocument = ImmediateDomBuilderT $ asks _immediateDomBuilderEnv_document

{-# INLINABLE askParent #-}
askParent :: Monad m => ImmediateDomBuilderT t m Node
askParent = ImmediateDomBuilderT $ asks _immediateDomBuilderEnv_parent

{-# INLINABLE askEvents #-}
askEvents :: Monad m => ImmediateDomBuilderT t m (Chan [DSum (EventTriggerRef t) TriggerInvocation])
askEvents = ImmediateDomBuilderT . lift . lift $ TriggerEventT.askEvents

localEnv :: Monad m => (ImmediateDomBuilderEnv t -> ImmediateDomBuilderEnv t) -> ImmediateDomBuilderT t m a -> ImmediateDomBuilderT t m a
localEnv f = ImmediateDomBuilderT . local f . unImmediateDomBuilderT

{-# INLINABLE append #-}
append :: MonadJSM m => DOM.Node -> ImmediateDomBuilderT t m ()
append n = do
  p <- askParent
  liftJSM $ appendChild_ p n
  return ()

{-# INLINABLE textNodeInternal #-}
textNodeInternal :: (MonadJSM m, ToDOMString contents) => contents -> ImmediateDomBuilderT t m DOM.Text
textNodeInternal !t = do
  doc <- askDocument
  n <- liftJSM $ createTextNode doc t
  append $ toNode n
  return n

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

type SupportsImmediateDomBuilder t m = (Reflex t, MonadJSM m, MonadHold t m, MonadFix m, MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref JSM, Adjustable t m, PrimMonad m)

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

{-# INLINABLE wrap #-}
wrap :: forall m er t. (Reflex t, MonadFix m, MonadJSM m, MonadReflexCreateTrigger t m) => RawElement GhcjsDomSpace -> RawElementConfig er t GhcjsDomSpace -> ImmediateDomBuilderT t m (Element er GhcjsDomSpace t)
wrap e cfg = do
  events <- askEvents
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
  es <- do
    let h :: GhcjsEventHandler er
        !h = _ghcjsEventSpec_handler $ _rawElementConfig_eventSpec cfg -- Note: this needs to be done strictly and outside of the newFanEventWithTrigger, so that the newFanEventWithTrigger doesn't retain the entire cfg, which can cause a cyclic dependency that the GC won't be able to clean up
    ctx <- askJSM
    newFanEventWithTrigger $ \(WrapArg en) t ->
      case DMap.lookup en eventTriggerRefs of
        Just (EventFilterTriggerRef r) -> do
          writeIORef r $ Just t
          return $ do
            writeIORef r Nothing
        Nothing -> (`runJSM` ctx) <$> (`runJSM` ctx) (elementOnEventName en e $ do
          evt <- DOM.event
          mv <- lift $ unGhcjsEventHandler h (en, GhcjsDomEvent evt)
          case mv of
            Nothing -> return ()
            Just v -> liftIO $ do
              --TODO: I don't think this is quite right: if a new trigger is created between when this is enqueued and when it fires, this may not work quite right
              ref <- newIORef $ Just t
              writeChan events [EventTriggerRef ref :=> TriggerInvocation v (return ())])
  return $ Element
    { _element_events = es
    , _element_raw = e
    }

{-# INLINABLE makeElement #-}
makeElement :: forall er t m a. (MonadJSM m, MonadFix m, MonadReflexCreateTrigger t m, Adjustable t m, DomHasCallStack) => Text -> ElementConfig er t GhcjsDomSpace -> ImmediateDomBuilderT t m a -> ImmediateDomBuilderT t m ((Element er GhcjsDomSpace t, a), DOM.Element)
makeElement elementTag cfg child = do
  doc <- askDocument
  e <- liftJSM $ uncheckedCastTo DOM.Element <$> case cfg ^. namespace of
    Nothing -> createElement doc elementTag
    Just ens -> createElementNS doc (Just ens) elementTag
  ImmediateDomBuilderT $ iforM_ (cfg ^. initialAttributes) $ \(AttributeName mAttrNamespace n) v -> case mAttrNamespace of
    Nothing -> lift $ setAttribute e n v
    Just ans -> lift $ setAttributeNS e (Just ans) n v
  result <- flip localEnv child $ \env -> env
    { _immediateDomBuilderEnv_parent = toNode e
    }
  append $ toNode e
  wrapped <- wrap e $ extractRawElementConfig cfg
  return ((wrapped, result), e)

newtype GhcjsDomHandler a b = GhcjsDomHandler { unGhcjsDomHandler :: a -> JSM b }

newtype GhcjsDomHandler1 a b = GhcjsDomHandler1 { unGhcjsDomHandler1 :: forall (x :: EventTag). a x -> JSM (b x) }

newtype GhcjsDomEvent en = GhcjsDomEvent { unGhcjsDomEvent :: EventType en }

data GhcjsDomSpace

instance DomSpace GhcjsDomSpace where
  type EventSpec GhcjsDomSpace = GhcjsEventSpec
  type RawTextNode GhcjsDomSpace = DOM.Text
  type RawElement GhcjsDomSpace = DOM.Element
  type RawFile GhcjsDomSpace = DOM.File
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
phantom2 :: (Functor f, Contravariant f) => f a -> f b
phantom2 = phantom
{-# INLINE phantom2 #-}

ghcjsEventSpec_filters :: forall er . Lens' (GhcjsEventSpec er) (DMap EventName (GhcjsEventFilter er))
ghcjsEventSpec_filters f (GhcjsEventSpec a b) = (\a' -> GhcjsEventSpec a' b) <$> f a
{-# INLINE ghcjsEventSpec_filters #-}
ghcjsEventSpec_handler :: forall er en . Getter (GhcjsEventSpec er) ((EventName en, GhcjsDomEvent en) -> JSM (Maybe (er en)))
ghcjsEventSpec_handler f (GhcjsEventSpec _ (GhcjsEventHandler b)) = phantom2 (f b)
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

instance SupportsImmediateDomBuilder t m => NotReady t (ImmediateDomBuilderT t m) where
  notReadyUntil e = do
    eOnce <- headE e
    env <- ImmediateDomBuilderT ask
    let unreadyChildren = _immediateDomBuilderEnv_unreadyChildren env
    liftIO $ modifyIORef' unreadyChildren succ
    let ready = do
          old <- liftIO $ readIORef unreadyChildren
          let new = pred old
          liftIO $ writeIORef unreadyChildren $! new
          when (new == 0) $ _immediateDomBuilderEnv_commitAction env
    requestDomAction_ $ ready <$ eOnce
  notReady = do
    env <- ImmediateDomBuilderT ask
    let unreadyChildren = _immediateDomBuilderEnv_unreadyChildren env
    liftIO $ modifyIORef' unreadyChildren succ

instance SupportsImmediateDomBuilder t m => DomBuilder t (ImmediateDomBuilderT t m) where
  type DomBuilderSpace (ImmediateDomBuilderT t m) = GhcjsDomSpace
  {-# INLINABLE textNode #-}
  textNode (TextNodeConfig initialContents mSetContents) = do
    n <- textNodeInternal initialContents
    mapM_ (requestDomAction_ . fmap (setNodeValue n . Just)) mSetContents
    return $ TextNode n
  {-# INLINABLE element #-}
  element elementTag cfg child = fst <$> makeElement elementTag cfg child
  {-# INLINABLE inputElement #-}
  inputElement cfg = do
    ((e, _), domElement) <- makeElement "input" (cfg ^. inputElementConfig_elementConfig) $ return ()
    let domInputElement = uncheckedCastTo DOM.HTMLInputElement domElement
    Input.setValue domInputElement $ cfg ^. inputElementConfig_initialValue
    v0 <- Input.getValue domInputElement
    let getMyValue = Input.getValue domInputElement
    valueChangedByUI <- requestDomAction $ liftJSM getMyValue <$ Reflex.select (_element_events e) (WrapArg Input)
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
    let initialFocus = False --TODO: Is this correct?
    hasFocus <- holdDyn initialFocus $ leftmost
      [ False <$ Reflex.select (_element_events e) (WrapArg Blur)
      , True <$ Reflex.select (_element_events e) (WrapArg Focus)
      ]
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
  {-# INLINABLE textAreaElement #-}
  textAreaElement cfg = do --TODO
    ((e, _), domElement) <- makeElement "textarea" (cfg ^. textAreaElementConfig_elementConfig) $ return ()
    let domTextAreaElement = uncheckedCastTo DOM.HTMLTextAreaElement domElement
    TextArea.setValue domTextAreaElement $ cfg ^. textAreaElementConfig_initialValue
    v0 <- TextArea.getValue domTextAreaElement
    let getMyValue = TextArea.getValue domTextAreaElement
    valueChangedByUI <- requestDomAction $ liftJSM getMyValue <$ Reflex.select (_element_events e) (WrapArg Input)
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
  {-# INLINABLE selectElement #-}
  selectElement cfg child = do
    ((e, result), domElement) <- makeElement "select" (cfg ^. selectElementConfig_elementConfig) child
    let domSelectElement = uncheckedCastTo DOM.HTMLSelectElement domElement
    Select.setValue domSelectElement $ cfg ^. selectElementConfig_initialValue
    v0 <- Select.getValue domSelectElement
    let getMyValue = Select.getValue domSelectElement
    valueChangedByUI <- requestDomAction $ liftJSM getMyValue <$ Reflex.select (_element_events e) (WrapArg Change)
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
  placeRawElement = append . toNode
  wrapRawElement = wrap

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

instance SupportsImmediateDomBuilder t m => MountableDomBuilder t (ImmediateDomBuilderT t m) where
  type DomFragment (ImmediateDomBuilderT t m) = ImmediateDomFragment
  buildDomFragment w = do
    df <- createDocumentFragment =<< askDocument
    result <- flip localEnv w $ \env -> env
      { _immediateDomBuilderEnv_parent = toNode df
      }
    state <- liftIO $ newIORef FragmentState_Unmounted
    return (ImmediateDomFragment df state, result)
  mountDomFragment fragment setFragment = do
    parent <- askParent
    extractFragment fragment
    before <- textNodeInternal ("" :: Text)
    appendChild_ parent $ _immediateDomFragment_document fragment
    after <- textNodeInternal ("" :: Text)
    xs <- foldDyn (\new (previous, _) -> (new, Just previous)) (fragment, Nothing) setFragment
    requestDomAction_ $ ffor (updated xs) $ \(childFragment, Just previousFragment) -> do
      extractFragment previousFragment
      extractFragment childFragment
      insertBefore (_immediateDomFragment_document childFragment) after
      liftIO $ writeIORef (_immediateDomFragment_state childFragment) $ FragmentState_Mounted (before, after)
    liftIO $ writeIORef (_immediateDomFragment_state fragment) $ FragmentState_Mounted (before, after)

instance (Reflex t, Adjustable t m, MonadJSM m, MonadHold t m, MonadFix m, PrimMonad m) => Adjustable t (ImmediateDomBuilderT t m) where
  {-# INLINABLE runWithReplace #-}
  runWithReplace a0 a' = do
    initialEnv <- ImmediateDomBuilderT ask
    before <- textNodeInternal ("" :: Text)
    let parentUnreadyChildren = _immediateDomBuilderEnv_unreadyChildren initialEnv
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
              when (new == 0) $ _immediateDomBuilderEnv_commitAction initialEnv
    -- We draw 'after' in this roundabout way to avoid using MonadFix
    doc <- askDocument
    after <- createTextNode doc ("" :: Text)
    let drawInitialChild = do
          unreadyChildren <- liftIO $ newIORef 0
          let f = do
                result <- a0
                append $ toNode after
                return result
          result <- runReaderT (unImmediateDomBuilderT f) $ initialEnv
            { _immediateDomBuilderEnv_unreadyChildren = unreadyChildren
            , _immediateDomBuilderEnv_commitAction = myCommitAction
            }
          liftIO $ readIORef unreadyChildren >>= \case
            0 -> writeIORef haveEverBeenReady True
            _ -> modifyIORef' parentUnreadyChildren succ
          return result
    a'' <- numberOccurrences a'
    (result0, child') <- ImmediateDomBuilderT $ lift $ runWithReplace drawInitialChild $ ffor a'' $ \(cohortId, child) -> do
      df <- createDocumentFragment doc
      unreadyChildren <- liftIO $ newIORef 0
      let commitAction = do
            c <- liftIO $ readIORef currentCohort
            when (c <= cohortId) $ do -- If a newer cohort has already been committed, just ignore this
              deleteBetweenExclusive before after
              insertBefore df after
              liftIO $ writeIORef currentCohort cohortId
              myCommitAction
      result <- runReaderT (unImmediateDomBuilderT child) $ initialEnv
        { _immediateDomBuilderEnv_parent = toNode df
        , _immediateDomBuilderEnv_unreadyChildren = unreadyChildren
        , _immediateDomBuilderEnv_commitAction = commitAction
        }
      uc <- liftIO $ readIORef unreadyChildren
      let commitActionToRunNow = if uc == 0
            then Just commitAction
            else Nothing -- A child will run it when unreadyChildren is decremented to 0
      return (commitActionToRunNow, result)
    requestDomAction_ $ fmapMaybe fst child'
    return (result0, snd <$> child')
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust = traverseIntMapWithKeyWithAdjust'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust = traverseDMapWithKeyWithAdjust'
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove = do
    let updateChildUnreadiness (p :: PatchDMapWithMove k (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v')) old = do
          let new :: forall a. k a -> PatchDMapWithMove.NodeInfo k (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v') a -> IO (PatchDMapWithMove.NodeInfo k (Constant (IORef (ChildReadyState k))) a)
              new k = PatchDMapWithMove.nodeInfoMapFromM $ \case
                PatchDMapWithMove.From_Insert (Compose (_, _, sRef, _)) -> do
                  readIORef sRef >>= \case
                    ChildReadyState_Ready -> return PatchDMapWithMove.From_Delete
                    ChildReadyState_Unready _ -> do
                      writeIORef sRef $ ChildReadyState_Unready $ Just $ Some.This k
                      return $ PatchDMapWithMove.From_Insert $ Constant sRef
                PatchDMapWithMove.From_Delete -> return PatchDMapWithMove.From_Delete
                PatchDMapWithMove.From_Move fromKey -> return $ PatchDMapWithMove.From_Move fromKey
              deleteOrMove :: forall a. k a -> Product (Constant (IORef (ChildReadyState k))) (ComposeMaybe k) a -> IO (Constant () a)
              deleteOrMove _ (Pair (Constant sRef) (ComposeMaybe mToKey)) = do
                writeIORef sRef $ ChildReadyState_Unready $ Some.This <$> mToKey -- This will be Nothing if deleting, and Just if moving, so it works out in both cases
                return $ Constant ()
          p' <- fmap unsafePatchDMapWithMove $ DMap.traverseWithKey new $ unPatchDMapWithMove p
          _ <- DMap.traverseWithKey deleteOrMove $ PatchDMapWithMove.getDeletionsAndMoves p old
          return $ applyAlways p' old
    hoistTraverseWithKeyWithAdjust traverseDMapWithKeyWithAdjustWithMove mapPatchDMapWithMove updateChildUnreadiness $ \placeholders lastPlaceholderRef (p_ :: PatchDMapWithMove k (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v')) -> do
      let p = unPatchDMapWithMove p_
      phsBefore <- liftIO $ readIORef placeholders
      lastPlaceholder <- liftIO $ readIORef lastPlaceholderRef
      let collectIfMoved :: forall a. k a -> PatchDMapWithMove.NodeInfo k (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v') a -> JSM (Constant (Maybe DOM.DocumentFragment) a)
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
      let !phsAfter = fromMaybe phsBefore $ apply (weakenPatchDMapWithMoveWith (\(Compose (_, ph, _, _)) -> ph) p_) phsBefore --TODO: Don't recompute this
      let placeFragment :: forall a. k a -> PatchDMapWithMove.NodeInfo k (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v') a -> JSM (Constant () a)
          placeFragment k e = do
            let nextPlaceholder = maybe lastPlaceholder snd $ Map.lookupGT (Some.This k) phsAfter
            case PatchDMapWithMove._nodeInfo_from e of
              PatchDMapWithMove.From_Insert (Compose (df, _, _, _)) -> do
                df `insertBefore` nextPlaceholder
              PatchDMapWithMove.From_Delete -> do
                return ()
              PatchDMapWithMove.From_Move fromKey -> do
                Just (Constant mdf) <- return $ DMap.lookup fromKey collected
                mapM_ (`insertBefore` nextPlaceholder) mdf
            return $ Constant ()
      mapM_ (\(k :=> v) -> void $ placeFragment k v) $ DMap.toDescList p -- We need to go in reverse order here, to make sure the placeholders are in the right spot at the right time
      liftIO $ writeIORef placeholders $! phsAfter

{-# INLINABLE traverseDMapWithKeyWithAdjust' #-}
traverseDMapWithKeyWithAdjust' :: forall t m (k :: * -> *) v v'. (Adjustable t m, MonadHold t m, MonadFix m, MonadIO m, MonadJSM m, PrimMonad m, DMap.GCompare k) => (forall a. k a -> v a -> ImmediateDomBuilderT t m (v' a)) -> DMap k v -> Event t (PatchDMap k v) -> ImmediateDomBuilderT t m (DMap k v', Event t (PatchDMap k v'))
traverseDMapWithKeyWithAdjust' = do
  let updateChildUnreadiness (p :: PatchDMap k (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v')) old = do
        let new :: forall a. k a -> ComposeMaybe (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v') a -> IO (ComposeMaybe (Constant (IORef (ChildReadyState k))) a)
            new k (ComposeMaybe m) = ComposeMaybe <$> case m of
              Nothing -> return Nothing
              Just (Compose (_, _, sRef, _)) -> do
                readIORef sRef >>= \case
                  ChildReadyState_Ready -> return Nothing -- Delete this child, since it's ready
                  ChildReadyState_Unready _ -> do
                    writeIORef sRef $ ChildReadyState_Unready $ Just $ Some.This k
                    return $ Just $ Constant sRef
            delete _ (Constant sRef) = do
              writeIORef sRef $ ChildReadyState_Unready Nothing
              return $ Constant ()
        p' <- fmap PatchDMap $ DMap.traverseWithKey new $ unPatchDMap p
        _ <- DMap.traverseWithKey delete $ PatchDMap.getDeletions p old
        return $ applyAlways p' old
  hoistTraverseWithKeyWithAdjust traverseDMapWithKeyWithAdjust mapPatchDMap updateChildUnreadiness $ \placeholders lastPlaceholderRef (PatchDMap p) -> do
    phs <- liftIO $ readIORef placeholders
    forM_ (DMap.toList p) $ \(k :=> ComposeMaybe mv) -> do
      lastPlaceholder <- liftIO $ readIORef lastPlaceholderRef
      let nextPlaceholder = maybe lastPlaceholder snd $ Map.lookupGT (Some.This k) phs
      forM_ (Map.lookup (Some.This k) phs) $ \thisPlaceholder -> thisPlaceholder `deleteUpTo` nextPlaceholder
      forM_ mv $ \(Compose (df, _, _, _)) -> df `insertBefore` nextPlaceholder
    liftIO $ writeIORef placeholders $! fromMaybe phs $ apply (weakenPatchDMapWith (\(Compose (_, ph, _, _)) -> ph) $ PatchDMap p) phs

{-# INLINABLE traverseIntMapWithKeyWithAdjust' #-}
traverseIntMapWithKeyWithAdjust' :: forall t m v v'. (Adjustable t m, MonadHold t m, MonadFix m, MonadIO m, MonadJSM m, PrimMonad m) => (IntMap.Key -> v -> ImmediateDomBuilderT t m v') -> IntMap v -> Event t (PatchIntMap v) -> ImmediateDomBuilderT t m (IntMap v', Event t (PatchIntMap v'))
traverseIntMapWithKeyWithAdjust' = do
  let updateChildUnreadiness (p@(PatchIntMap pInner) :: PatchIntMap (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v')) old = do
        let new :: IntMap.Key -> Maybe (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v') -> IO (Maybe (IORef ChildReadyStateInt))
            new k m = case m of
              Nothing -> return Nothing
              Just (_, _, sRef, _) -> do
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
      forM_ mv $ \(df, _, _, _) -> df `insertBefore` nextPlaceholder
    liftIO $ writeIORef placeholders $! fromMaybe phs $ apply ((\(_, ph, _, _) -> ph) <$> PatchIntMap p) phs

#if MIN_VERSION_base(4,9,0)
data ChildReadyState k
#else
data ChildReadyState (k :: * -> *)
#endif
   = ChildReadyState_Ready
   | ChildReadyState_Unready !(Maybe (Some k))
   deriving (Show, Read, Eq, Ord)

data ChildReadyStateInt
   = ChildReadyStateInt_Ready
   | ChildReadyStateInt_Unready !(Maybe Int)
   deriving (Show, Read, Eq, Ord)

{-# INLINE hoistTraverseIntMapWithKeyWithAdjust #-}
hoistTraverseIntMapWithKeyWithAdjust :: forall v v' t m p.
  ( Adjustable t m
  , MonadIO m
  , MonadJSM m
  , MonadFix m
  , PrimMonad m
  , Monoid (p (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v'))
  , Functor p
  )
  => (   (IntMap.Key -> v -> RequesterT t JSM Identity (TriggerEventT t m) (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v'))
      -> IntMap v
      -> Event t (p v)
      -> RequesterT t JSM Identity (TriggerEventT t m) (IntMap (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v'), Event t (p (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v')))
     ) -- ^ The base monad's traversal
  -> (p (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v') -> IntMap (IORef ChildReadyStateInt) -> IO (IntMap (IORef ChildReadyStateInt))) -- ^ Given a patch for the children DOM elements, produce a patch for the childrens' unreadiness state
  -> (IORef (IntMap DOM.Text) -> IORef DOM.Text -> p (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v') -> JSM ()) -- ^ Apply a patch to the DOM
  -> (IntMap.Key -> v -> ImmediateDomBuilderT t m v')
  -> IntMap v
  -> Event t (p v)
  -> ImmediateDomBuilderT t m (IntMap v', Event t (p v'))
hoistTraverseIntMapWithKeyWithAdjust base updateChildUnreadiness applyDomUpdate_ f dm0 dm' = do
  initialEnv <- ImmediateDomBuilderT ask
  let parentUnreadyChildren = _immediateDomBuilderEnv_unreadyChildren initialEnv
  pendingChange :: IORef (IntMap (IORef ChildReadyStateInt), p (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v')) <- liftIO $ newIORef mempty
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
            when (new == 0) $ _immediateDomBuilderEnv_commitAction initialEnv
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
  (children0, children') <- ImmediateDomBuilderT $ lift $ base (\k v -> drawChildUpdateInt initialEnv markChildReady $ f k v) dm0 dm'
  let processChild k (_, _, sRef, _) = do
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
  let result0 = IntMap.map (\(_, _, _, v) -> v) children0
      placeholders0 = fmap (\(_, ph, _, _) -> ph) children0
      result' = ffor children' $ fmap $ \(_, _, _, r) -> r
  liftIO $ writeIORef placeholders $! placeholders0
  _ <- IntMap.traverseWithKey (\_ (df, _, _, _) -> void $ append $ toNode df) children0
  liftIO . writeIORef lastPlaceholderRef =<< textNodeInternal ("" :: Text)
  requestDomAction_ $ ffor children' $ \p -> do
    (oldUnready, oldP) <- liftIO $ readIORef pendingChange
    newUnready <- liftIO $ updateChildUnreadiness p oldUnready
    let !newP = p <> oldP
    liftIO $ writeIORef pendingChange (newUnready, newP)
    when (IntMap.null newUnready) $ do
      applyDomUpdate newP
  return (result0, result')

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
  , PatchTarget (p k (Constant Int)) ~ DMap k (Constant Int)
  , Monoid (p k (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v'))
  , Patch (p k (Constant Int))
  )
  => (forall vv vv'.
         (forall a. k a -> vv a -> RequesterT t JSM Identity (TriggerEventT t m) (vv' a))
      -> DMap k vv
      -> Event t (p k vv)
      -> RequesterT t JSM Identity (TriggerEventT t m) (DMap k vv', Event t (p k vv'))
     ) -- ^ The base monad's traversal
  -> (forall vv vv'. (forall a. vv a -> vv' a) -> p k vv -> p k vv') -- ^ A way of mapping over the patch type
  -> (p k (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v') -> DMap k (Constant (IORef (ChildReadyState k))) -> IO (DMap k (Constant (IORef (ChildReadyState k))))) -- ^ Given a patch for the children DOM elements, produce a patch for the childrens' unreadiness state
  -> (IORef (Map.Map (Some.Some k) DOM.Text) -> IORef DOM.Text -> p k (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v') -> JSM ()) -- ^ Apply a patch to the DOM
  -> (forall a. k a -> v a -> ImmediateDomBuilderT t m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> ImmediateDomBuilderT t m (DMap k v', Event t (p k v'))
hoistTraverseWithKeyWithAdjust base mapPatch updateChildUnreadiness applyDomUpdate_ (f :: forall a. k a -> v a -> ImmediateDomBuilderT t m (v' a)) (dm0 :: DMap k v) dm' = do
  initialEnv <- ImmediateDomBuilderT ask
  let parentUnreadyChildren = _immediateDomBuilderEnv_unreadyChildren initialEnv
  pendingChange :: IORef (DMap k (Constant (IORef (ChildReadyState k))), p k (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v')) <- liftIO $ newIORef mempty
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
            when (new == 0) $ _immediateDomBuilderEnv_commitAction initialEnv
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
  (children0, children') <- ImmediateDomBuilderT $ lift $ base (\k v -> drawChildUpdate initialEnv markChildReady $ f k v) dm0 dm'
  let processChild k (Compose (_, _, sRef, _)) = ComposeMaybe <$> do
        readIORef sRef >>= \case
          ChildReadyState_Ready -> return Nothing
          ChildReadyState_Unready _ -> do
            writeIORef sRef $ ChildReadyState_Unready $ Just $ Some.This k
            return $ Just $ Constant sRef
  initialUnready <- liftIO $ DMap.mapMaybeWithKey (\_ -> getComposeMaybe) <$> DMap.traverseWithKey processChild children0
  liftIO $ if DMap.null initialUnready
    then writeIORef haveEverBeenReady True
    else do
      modifyIORef' parentUnreadyChildren succ
      writeIORef pendingChange (initialUnready, mempty) -- The patch is always empty because it got applied implicitly when we ran the children the first time
  let result0 = DMap.map (\(Compose (_, _, _, v)) -> v) children0
      placeholders0 = weakenDMapWith (\(Compose (_, ph, _, _)) -> ph) children0
      result' = ffor children' $ mapPatch $ \(Compose (_, _, _, r)) -> r
  liftIO $ writeIORef placeholders $! placeholders0
  _ <- DMap.traverseWithKey (\_ (Compose (df, _, _, _)) -> Constant () <$ append (toNode df)) children0
  liftIO . writeIORef lastPlaceholderRef =<< textNodeInternal ("" :: Text)
  requestDomAction_ $ ffor children' $ \p -> do
    (oldUnready, oldP) <- liftIO $ readIORef pendingChange
    newUnready <- liftIO $ updateChildUnreadiness p oldUnready
    let !newP = p <> oldP
    liftIO $ writeIORef pendingChange (newUnready, newP)
    when (DMap.null newUnready) $ do
      applyDomUpdate newP
  return (result0, result')

{-# INLINABLE drawChildUpdate #-}
drawChildUpdate :: (MonadIO m, MonadJSM m)
  => ImmediateDomBuilderEnv t
  -> (IORef (ChildReadyState k) -> JSM ()) -- This will NOT be called if the child is ready at initialization time; instead, the ChildReadyState return value will be ChildReadyState_Ready
  -> ImmediateDomBuilderT t m (v' a)
  -> RequesterT t JSM Identity (TriggerEventT t m) (Compose ((,,,) DOM.DocumentFragment DOM.Text (IORef (ChildReadyState k))) v' a)
drawChildUpdate initialEnv markReady child = do
  childReadyState <- liftIO $ newIORef $ ChildReadyState_Unready Nothing
  unreadyChildren <- liftIO $ newIORef 0
  df <- createDocumentFragment $ _immediateDomBuilderEnv_document initialEnv
  (placeholder, result) <- runReaderT (unImmediateDomBuilderT $ (,) <$> textNodeInternal ("" :: Text) <*> child) $ initialEnv
    { _immediateDomBuilderEnv_parent = toNode df
    , _immediateDomBuilderEnv_unreadyChildren = unreadyChildren
    , _immediateDomBuilderEnv_commitAction = markReady childReadyState
    }
  u <- liftIO $ readIORef unreadyChildren
  when (u == 0) $ liftIO $ writeIORef childReadyState ChildReadyState_Ready
  return $ Compose (df, placeholder, childReadyState, result)

{-# INLINABLE drawChildUpdateInt #-}
drawChildUpdateInt :: (MonadIO m, MonadJSM m)
  => ImmediateDomBuilderEnv t
  -> (IORef ChildReadyStateInt -> JSM ()) -- This will NOT be called if the child is ready at initialization time; instead, the ChildReadyState return value will be ChildReadyState_Ready
  -> ImmediateDomBuilderT t m v'
  -> RequesterT t JSM Identity (TriggerEventT t m) (DOM.DocumentFragment, DOM.Text, IORef ChildReadyStateInt, v')
drawChildUpdateInt initialEnv markReady child = do
  childReadyState <- liftIO $ newIORef $ ChildReadyStateInt_Unready Nothing
  unreadyChildren <- liftIO $ newIORef 0
  df <- createDocumentFragment $ _immediateDomBuilderEnv_document initialEnv
  (placeholder, result) <- runReaderT (unImmediateDomBuilderT $ (,) <$> textNodeInternal ("" :: Text) <*> child) $ initialEnv
    { _immediateDomBuilderEnv_parent = toNode df
    , _immediateDomBuilderEnv_unreadyChildren = unreadyChildren
    , _immediateDomBuilderEnv_commitAction = markReady childReadyState
    }
  u <- liftIO $ readIORef unreadyChildren
  when (u == 0) $ liftIO $ writeIORef childReadyState ChildReadyStateInt_Ready
  return (df, placeholder, childReadyState, result)

mkHasFocus :: (MonadHold t m, Reflex t) => Element er d t -> m (Dynamic t Bool)
mkHasFocus e = do
  let initialFocus = False --TODO: Actually get the initial focus of the element
  holdDyn initialFocus $ leftmost
    [ False <$ Reflex.select (_element_events e) (WrapArg Blur)
    , True <$ Reflex.select (_element_events e) (WrapArg Focus)
    ]

insertBefore :: (MonadJSM m, IsNode new, IsNode existing) => new -> existing -> m ()
insertBefore new existing = do
  p <- getParentNodeUnchecked existing
  DOM.insertBefore_ p new (Just existing) -- If there's no parent, that means we've been removed from the DOM; this should not happen if the we're removing ourselves from the performEvent properly

instance PerformEvent t m => PerformEvent t (ImmediateDomBuilderT t m) where
  type Performable (ImmediateDomBuilderT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance PostBuild t m => PostBuild t (ImmediateDomBuilderT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ImmediateDomBuilderT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) => TriggerEvent t (ImmediateDomBuilderT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = ImmediateDomBuilderT . lift . lift $ newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = ImmediateDomBuilderT . lift . lift $ newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = ImmediateDomBuilderT . lift . lift $ newEventWithLazyTriggerWithOnComplete f

instance HasJSContext m => HasJSContext (ImmediateDomBuilderT t m) where
  type JSContextPhantom (ImmediateDomBuilderT t m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance MonadRef m => MonadRef (ImmediateDomBuilderT t m) where
  type Ref (ImmediateDomBuilderT t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (ImmediateDomBuilderT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance (HasJS x m, ReflexHost t) => HasJS x (ImmediateDomBuilderT t m) where
  type JSX (ImmediateDomBuilderT t m) = JSX m
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
  Paste -> return ()
  Reset -> return ()
  Search -> return ()
  Selectstart -> return ()
  Touchstart -> getTouchEvent
  Touchmove -> getTouchEvent
  Touchend -> getTouchEvent
  Touchcancel -> getTouchEvent
  Mousewheel -> return ()
  Wheel -> return ()

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
  Paste -> return ()
  Reset -> return ()
  Search -> return ()
  Selectstart -> return ()
  Touchstart -> getTouchEvent
  Touchmove -> getTouchEvent
  Touchend -> getTouchEvent
  Touchcancel -> getTouchEvent
  Mousewheel -> return ()
  Wheel -> return ()

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

instance MonadSample t m => MonadSample t (ImmediateDomBuilderT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (ImmediateDomBuilderT t m) where
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

wrapWindow :: (MonadJSM m, MonadReflexCreateTrigger t m) => DOM.Window -> WindowConfig t -> ImmediateDomBuilderT t m (Window t)
wrapWindow wv _ = do
  events <- wrapDomEventsMaybe wv (defaultDomWindowEventHandler wv) windowOnEventName
  return $ Window
    { _window_events = events
    , _window_raw = wv
    }

#ifdef USE_TEMPLATE_HASKELL
makeLenses ''GhcjsEventSpec
#endif
