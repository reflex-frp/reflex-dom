module Reflex.Dom.Location (getLocationHost, getLocationProtocol) where

import Data.Text (Text)
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Window (getLocation)
import GHCJS.DOM.Location (getProtocol, getHost)

getLocationHost :: (MonadJSM m) => m Text
getLocationHost = currentWindowUnchecked >>= getLocation >>= getHost

getLocationProtocol :: (MonadJSM m) => m Text
getLocationProtocol = currentWindowUnchecked >>= getLocation >>= getProtocol
