{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP #-}

module Reflex.Dom.Internal.Foreign ( runWebGUI
                                   , quitWebView
                                   , module Reflex.Dom.Internal.Foreign
                                   ) where

import Control.Monad
import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.Types
import Data.Function
import GHCJS.Foreign

#define JS(name, js, type) foreign import javascript unsafe js name :: type

quitWebView :: WebView -> IO ()
quitWebView = error "quitWebView: unimplemented in GHCJS"

instance Eq Node where
  (==) = eqRef `on` unNode

JS(getLocationHost_, "location.host", IO JSString)

getLocationHost :: FromJSString r => a -> IO r
getLocationHost _ = liftM fromJSString getLocationHost_

JS(getLocationProtocol_, "location.protocol", IO JSString)

getLocationProtocol :: FromJSString r => a -> IO r
getLocationProtocol _ = liftM fromJSString getLocationProtocol_


