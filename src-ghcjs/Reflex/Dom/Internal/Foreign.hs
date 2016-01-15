{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
module Reflex.Dom.Internal.Foreign ( module Reflex.Dom.Internal.Foreign
                                   , runWebGUI
                                   ) where

import Control.Monad
import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.Types

quitWebView :: WebView -> IO ()
quitWebView = error "quitWebView: unimplemented in GHCJS"

foreign import javascript unsafe "location['host']" getLocationHost_ :: IO JSString

getLocationHost :: FromJSString r => a -> IO r
getLocationHost _ = liftM fromJSString getLocationHost_

foreign import javascript unsafe "location['protocol']" getLocationProtocol_ :: IO JSString

getLocationProtocol :: FromJSString r => a -> IO r
getLocationProtocol _ = liftM fromJSString getLocationProtocol_
