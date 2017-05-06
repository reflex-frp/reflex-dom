module Reflex.Dom.Location (getLocationHost, getLocationProtocol) where

import Data.Text (Text)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Location (getHost, getProtocol)
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.Window (getLocation)

getLocationHost :: (MonadJSM m) => m Text
getLocationHost = currentWindowUnchecked >>= getLocation >>= getHost

getLocationProtocol :: (MonadJSM m) => m Text
getLocationProtocol = currentWindowUnchecked >>= getLocation >>= getProtocol
