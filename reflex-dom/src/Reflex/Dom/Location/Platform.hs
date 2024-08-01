{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

#if defined(ANDROID)
module Reflex.Dom.Location.Platform where

import Reflex
import Control.Monad.IO.Class
import Reflex.Dom.Android.MainWidget

popHistoryState
  :: (PerformEvent t m, MonadIO (Performable m))
  => Event t ()
  -> m ()
popHistoryState evt = performEvent_ $ withGlobalJSExecutor goBack <$ evt
#else
module Reflex.Dom.Location.Platform (Reflex.Dom.Location.popHistoryState) where
import qualified Reflex.Dom.Location

#endif

