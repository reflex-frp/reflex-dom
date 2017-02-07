{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foreign.JavaScript.Orphans where

#ifndef __GHCJS__

import Reflex.Host.Class (ReflexHost, HostFrame)
import Control.Monad.Trans.Class (lift)
import Reflex.PostBuild.Base (PostBuildT)
import Reflex.PerformEvent.Base (PerformEventT(..))
import Reflex.DynamicWriter (DynamicWriterT)
import Reflex.Requester.Base (RequesterT)
import GHCJS.DOM.Types (MonadJSM(..))

instance (MonadJSM m, ReflexHost t) => MonadJSM (PostBuildT t m) where
  liftJSM' = lift . liftJSM'

instance (MonadJSM (HostFrame t), ReflexHost t) => MonadJSM (PerformEventT t m) where
  liftJSM' = PerformEventT . lift . liftJSM'

instance MonadJSM m => MonadJSM (DynamicWriterT t w m) where
  liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (RequesterT t request response m) where
  liftJSM' = lift . liftJSM'

#endif
