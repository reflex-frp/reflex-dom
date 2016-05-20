{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, RecursiveDo #-}
module Reflex.Dom.DynamicWriter where

import Reflex
import Reflex.Host.Class
import Reflex.Dom.Class
import Reflex.Dom.Builder.Class
import Reflex.Dom.PerformEvent.Class
import Reflex.Dom.PostBuild.Class

import Control.Lens hiding (element)
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Exception

instance MonadTrans (DynamicWriterT t w) where
  lift = DynamicWriterT . lift

newtype DynamicWriterT t w m a = DynamicWriterT { unDynamicWriterT :: StateT [Dynamic t w] m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t, MonadSample t, MonadAsyncException, MonadException) -- The list is kept in reverse order

instance HasWebView m => HasWebView (DynamicWriterT t w m) where
  type WebViewPhantom (DynamicWriterT t w m) = WebViewPhantom m
  askWebView = lift askWebView

instance MonadRef m => MonadRef (DynamicWriterT t w m) where
  type Ref (DynamicWriterT t w m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (DynamicWriterT t w m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (DynamicWriterT t w m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

runDynamicWriterT :: (Reflex t, MonadHold t m, Monoid w) => DynamicWriterT t w m a -> m (a, Dynamic t w)
runDynamicWriterT (DynamicWriterT a) = do
  (result, ws) <- runStateT a []
  w <- mconcatDyn $ reverse ws
  return (result, w)

class Monad m => MonadDynamicWriter t w m | m -> t w where
  tellDyn :: Dynamic t w -> m ()

instance Monad m => MonadDynamicWriter t w (DynamicWriterT t w m) where
  tellDyn w = DynamicWriterT $ modify (w:)

instance MonadReader r m => MonadReader r (DynamicWriterT t w m) where
  ask = lift ask
  local f (DynamicWriterT a) = DynamicWriterT $ mapStateT (local f) a
  reader = lift . reader

liftDynamicWriterTThroughSync :: Monad m' => (m (a, [Dynamic t w]) -> m' (b, (a, [Dynamic t w]))) -> DynamicWriterT t w m a -> DynamicWriterT t w m' (b, a)
liftDynamicWriterTThroughSync f (DynamicWriterT child) = DynamicWriterT $ do
    s <- get
    (b, (a, newS)) <- lift $ f $ runStateT child s
    put newS
    return (b, a)

{-# INLINABLE liftDynamicWriterTElementConfig #-}
liftDynamicWriterTElementConfig :: ElementConfig er t (DynamicWriterT t w m) -> ElementConfig er t m
liftDynamicWriterTElementConfig cfg = cfg
  { _elementConfig_eventFilters = _elementConfig_eventFilters cfg
  , _elementConfig_eventHandler = _elementConfig_eventHandler cfg -- This requires PolyKinds, and will fail to unify types otherwise
  }

instance (DomBuilder t m, Monoid w, MonadHold t m, MonadFix m) => DomBuilder t (DynamicWriterT t w m) where
  type DomBuilderSpace (DynamicWriterT t w m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (DynamicWriterT child) = DynamicWriterT $ do
    s <- get
    let cfg' = cfg
          { _elementConfig_eventFilters = _elementConfig_eventFilters cfg
          , _elementConfig_eventHandler = _elementConfig_eventHandler cfg
          }
    (el, (a, newS)) <- lift $ element elementTag cfg' $ runStateT child s
    put newS
    return (el, a)
  fragment cfg (DynamicWriterT child) = do --TODO: Delete these things
    s <- DynamicWriterT get
    let cfg' = cfg
          { _fragmentConfig_insertAbove = fmap runDynamicWriterT $ _fragmentConfig_insertAbove cfg
          }
    rec children <- foldDyn (:) [] childOutputs
        tellDyn $ mconcat =<< children -- Needs to go above the child to preserve order
        (f, (a, newS)) <- DynamicWriterT $ lift $ fragment cfg' $ runStateT child s
        DynamicWriterT $ put newS
        let result = fmap fst $ _fragment_insertedAbove f
            childOutputs = fmap snd $ _fragment_insertedAbove f
    let f' = f
          { _fragment_insertedAbove = result
          }
    return (f', a)
  placeholder cfg = do --TODO: Delete these things
    let cfg' = cfg
          { _placeholderConfig_insertAbove = fmap runDynamicWriterT $ _placeholderConfig_insertAbove cfg
          }
    rec children <- foldDyn (:) [] childOutputs
        tellDyn $ mconcat =<< children -- Needs to go above the child to preserve order
        p <- DynamicWriterT $ lift $ placeholder cfg'
        let result = fmap fst $ _placeholder_insertedAbove p
            childOutputs = fmap snd $ _placeholder_insertedAbove p
    return $ p
      { _placeholder_insertedAbove = result
      }
  inputElement cfg = lift $ inputElement $ cfg & inputElementConfig_elementConfig %~ liftDynamicWriterTElementConfig
  textAreaElement cfg = lift $ textAreaElement $ cfg & textAreaElementConfig_elementConfig %~ liftDynamicWriterTElementConfig

instance (Deletable t m, Reflex t, Monoid w, MonadHold t m) => Deletable t (DynamicWriterT t w m) where
  deletable delete child = do
    (result, output) <- lift $ deletable delete $ runDynamicWriterT child
    output' <- holdDyn output $ constDyn mempty <$ delete
    tellDyn $ join output'
    return result

instance PerformEvent t m => PerformEvent t (DynamicWriterT t w m) where
  type Performable (DynamicWriterT t w m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance TriggerEvent t m => TriggerEvent t (DynamicWriterT t w m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance PostBuild t m => PostBuild t (DynamicWriterT t w m) where
  getPostBuild = lift getPostBuild

instance MonadDynamicWriter t w m => MonadDynamicWriter t w (ReaderT r m) where
  tellDyn = lift . tellDyn

instance MonadState s m => MonadState s (DynamicWriterT t w m) where
  get = lift get
  put = lift . put

instance HasJS x m => HasJS x (DynamicWriterT t w m) where
  type JSM (DynamicWriterT t w m) = JSM m
  liftJS = lift . liftJS
