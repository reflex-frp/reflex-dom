{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Builder.InputDisabled where

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
import Reflex.Dom.Builder.Immediate (HasDocument (..))
import Reflex.Host.Class

-- | A DomBuilder transformer that disables all 'inputElement's,
-- 'textAreaElement's, and 'selectElement's by adding the "disabled" HTML
-- attribute.  Note that 'element's that happen to have "input", "textarea", or
-- "select" as their tag will NOT be disabled.
newtype InputDisabledT m a = InputDisabledT { runInputDisabledT :: m a } deriving (Functor, Applicative, Monad, MonadAtomicRef, MonadFix, MonadIO)

#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (InputDisabledT m) where
    liftJSM' = InputDisabledT . liftJSM'
#endif

deriving instance MonadSample t m => MonadSample t (InputDisabledT m)
deriving instance MonadHold t m => MonadHold t (InputDisabledT m)

instance MonadTrans InputDisabledT where
  lift = InputDisabledT

instance MonadTransControl InputDisabledT where
  type StT InputDisabledT a = a
  liftWith f = InputDisabledT $ f runInputDisabledT
  restoreT = InputDisabledT

instance MonadRef m => MonadRef (InputDisabledT m) where
  type Ref (InputDisabledT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

instance PerformEvent t m => PerformEvent t (InputDisabledT m) where
  type Performable (InputDisabledT m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance PrimMonad m => PrimMonad (InputDisabledT m) where
  type PrimState (InputDisabledT m) = PrimState m
  primitive = lift . primitive

disableElementConfig :: Reflex t => ElementConfig er t m -> ElementConfig er t m
disableElementConfig cfg = cfg
  { _elementConfig_initialAttributes = Map.insert "disabled" "disabled" $ _elementConfig_initialAttributes cfg
  , _elementConfig_modifyAttributes = fmap (Map.delete "disabled") <$> _elementConfig_modifyAttributes cfg
  }

instance PostBuild t m => PostBuild t (InputDisabledT m) where
  getPostBuild = lift getPostBuild

deriving instance TriggerEvent t m => TriggerEvent t (InputDisabledT m)

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (InputDisabledT m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance Adjustable t m => Adjustable t (InputDisabledT m) where
  runWithReplace a0 a' = InputDisabledT $ runWithReplace (coerce a0) (coerceEvent a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = InputDisabledT $ traverseDMapWithKeyWithAdjust (\k v -> runInputDisabledT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = InputDisabledT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runInputDisabledT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseIntMapWithKeyWithAdjust f m0 m' = InputDisabledT $ traverseIntMapWithKeyWithAdjust (\k v -> runInputDisabledT $ f k v) (coerce m0) (coerceEvent m')

instance NotReady t m => NotReady t (InputDisabledT m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady

instance DomBuilder t m => DomBuilder t (InputDisabledT m) where
  type DomBuilderSpace (InputDisabledT m) = DomBuilderSpace m
  inputElement cfg = lift $ inputElement $ cfg
    { _inputElementConfig_elementConfig = disableElementConfig $ _inputElementConfig_elementConfig cfg
    }
  textAreaElement cfg = lift $ textAreaElement $ cfg
    { _textAreaElementConfig_elementConfig = disableElementConfig $ _textAreaElementConfig_elementConfig cfg
    }
  selectElement cfg child = do
    let cfg' = cfg
          { _selectElementConfig_elementConfig = disableElementConfig $ _selectElementConfig_elementConfig cfg
          }
    lift $ selectElement cfg' $ runInputDisabledT child

instance HasDocument m => HasDocument (InputDisabledT m)

instance HasJSContext m => HasJSContext (InputDisabledT m) where
  type JSContextPhantom (InputDisabledT m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance HasJS js m => HasJS js (InputDisabledT m) where
  type JSX (InputDisabledT m) = JSX m
  liftJS = lift . liftJS

instance DomRenderHook t m => DomRenderHook t (InputDisabledT m) where
  withRenderHook f = InputDisabledT . withRenderHook f . runInputDisabledT
  requestDomAction = InputDisabledT . requestDomAction
  requestDomAction_ = InputDisabledT . requestDomAction_
