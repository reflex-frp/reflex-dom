{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.Dom.Orphans where

#ifndef ghcjs_HOST_OS
import Control.Monad.Trans
import GHCJS.DOM.Types (MonadJSM (..))
import Reflex.DynamicWriter
import Reflex.EventWriter
import Reflex.PostBuild.Base
import Reflex.Requester.Base

instance MonadJSM m => MonadJSM (DynamicWriterT t w m) where
  liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (EventWriterT t w m) where
  liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (PostBuildT t m) where
  liftJSM' = lift . liftJSM'

instance MonadJSM m => MonadJSM (RequesterT t request response m) where
  liftJSM' = lift . liftJSM'
#endif
