module Reflex.Dom.Internal.Foreign (runWebGUI, quitWebView) where

import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.Types
import Data.Function

quitWebView :: WebView -> IO ()
quitWebView = error "quitWebView: unimplemented in GHCJS"

instance Eq Node where
  (==) = eqRef `on` unNode
