{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      :  Reflex.Dom.Location.Platform
-- Copyright   :  (c) Ryan Trinkle
-- License     :  BSD3
--
-- Maintainer  :  ryan.trinkle@gmail.com
--
-- Platform-specific browser history navigation.
--
--   On Android, 'popHistoryState' uses the JSaddle executor to trigger
--   the native back button.  On all other platforms it re-exports
--   'Reflex.Dom.Location.popHistoryState' from @reflex-dom-core@.
--
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

