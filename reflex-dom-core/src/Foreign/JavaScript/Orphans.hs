{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foreign.JavaScript.Orphans where

#ifndef ghcjs_HOST_OS

import Control.Monad.Trans.Class (lift)
import GHCJS.DOM.Types (MonadJSM (..))
import Reflex.DynamicWriter (DynamicWriterT)
import Reflex.Host.Class (HostFrame, ReflexHost)
import Reflex.PerformEvent.Base (PerformEventT (..))
import Reflex.PostBuild.Base (PostBuildT)
import Reflex.Requester.Base (RequesterT)
import Reflex.TriggerEvent.Base

instance (MonadJSM m, ReflexHost t) => MonadJSM (PostBuildT t m) where
  liftJSM' = lift . liftJSM'

instance (MonadJSM (HostFrame t), ReflexHost t) => MonadJSM (PerformEventT t m) where
  liftJSM' = PerformEventT . lift . liftJSM'

instance MonadJSM m => MonadJSM (DynamicWriterT t w m) where
  liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (RequesterT t request response m) where
  liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (TriggerEventT t m) where
  liftJSM' = lift . liftJSM'

#endif
