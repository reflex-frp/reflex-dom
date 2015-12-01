module Reflex.Dom.Location (getLocationHost, getLocationProtocol) where

import Reflex.Dom.Class
import qualified Reflex.Dom.Internal.Foreign as F

import Control.Monad.IO.Class

getLocationHost :: (MonadIO m) => WebViewSingleton x -> m String
getLocationHost (WebViewSingleton wv) = liftIO $ F.getLocationHost wv

getLocationProtocol :: (MonadIO m) => WebViewSingleton x -> m String
getLocationProtocol (WebViewSingleton wv) = liftIO $ F.getLocationProtocol wv
