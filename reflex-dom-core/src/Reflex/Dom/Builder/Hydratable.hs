{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Builder.Hydratable where

import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Coerce
import qualified Data.Map as Map
import Foreign.JavaScript.TH
#ifndef ghcjs_HOST_OS
import GHCJS.DOM.Types (MonadJSM (..))
#endif
import Reflex
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate (HasDocument (..), hydratableAttribute)
import Reflex.Host.Class

-- | A DomBuilder transformer that adds an attribute to all elements such that the
-- hydration builder knows which bits of DOM were added by us, and which were
-- added by external scripts.
newtype HydratableT m a = HydratableT { runHydratableT :: m a } deriving (Functor, Applicative, Monad, MonadAtomicRef, MonadFix, MonadIO)

#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (HydratableT m) where
    liftJSM' = HydratableT . liftJSM'
#endif

deriving instance MonadSample t m => MonadSample t (HydratableT m)
deriving instance MonadHold t m => MonadHold t (HydratableT m)

instance MonadTrans HydratableT where
  lift = HydratableT

instance MonadTransControl HydratableT where
  type StT HydratableT a = a
  liftWith f = HydratableT $ f runHydratableT
  restoreT = HydratableT

instance MonadRef m => MonadRef (HydratableT m) where
  type Ref (HydratableT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

instance PerformEvent t m => PerformEvent t (HydratableT m) where
  type Performable (HydratableT m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance PrimMonad m => PrimMonad (HydratableT m) where
  type PrimState (HydratableT m) = PrimState m
  primitive = lift . primitive

makeHydratable :: Reflex t => ElementConfig er t m -> ElementConfig er t m
makeHydratable cfg = cfg
  { _elementConfig_initialAttributes = Map.insert hydratableAttribute "" $ _elementConfig_initialAttributes cfg
  , _elementConfig_modifyAttributes = fmap (Map.delete hydratableAttribute) <$> _elementConfig_modifyAttributes cfg
  }

instance PostBuild t m => PostBuild t (HydratableT m) where
  getPostBuild = lift getPostBuild

deriving instance TriggerEvent t m => TriggerEvent t (HydratableT m)

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (HydratableT m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance Adjustable t m => Adjustable t (HydratableT m) where
  runWithReplace a0 a' = HydratableT $ runWithReplace (coerce a0) (coerceEvent a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = HydratableT $ traverseDMapWithKeyWithAdjust (\k v -> runHydratableT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = HydratableT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runHydratableT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseIntMapWithKeyWithAdjust f m0 m' = HydratableT $ traverseIntMapWithKeyWithAdjust (\k v -> runHydratableT $ f k v) (coerce m0) (coerceEvent m')

instance NotReady t m => NotReady t (HydratableT m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady

instance DomBuilder t m => DomBuilder t (HydratableT m) where
  type DomBuilderSpace (HydratableT m) = DomBuilderSpace m
  element t cfg = lift . element t (makeHydratable cfg) . runHydratableT
  inputElement cfg = lift $ inputElement $ cfg
    { _inputElementConfig_elementConfig = makeHydratable $ _inputElementConfig_elementConfig cfg
    }
  textAreaElement cfg = lift $ textAreaElement $ cfg
    { _textAreaElementConfig_elementConfig = makeHydratable $ _textAreaElementConfig_elementConfig cfg
    }
  selectElement cfg child = do
    let cfg' = cfg
          { _selectElementConfig_elementConfig = makeHydratable $ _selectElementConfig_elementConfig cfg
          }
    lift $ selectElement cfg' $ runHydratableT child

instance HasDocument m => HasDocument (HydratableT m)

instance HasJSContext m => HasJSContext (HydratableT m) where
  type JSContextPhantom (HydratableT m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance HasJS js m => HasJS js (HydratableT m) where
  type JSX (HydratableT m) = JSX m
  liftJS = lift . liftJS

instance DomRenderHook t m => DomRenderHook t (HydratableT m) where
  withRenderHook f = HydratableT . withRenderHook f . runHydratableT
  requestDomAction = HydratableT . requestDomAction
  requestDomAction_ = HydratableT . requestDomAction_
