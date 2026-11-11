module Foreign.JavaScript.Internal.Utils
  ( synchronously
  , freeRequestAnimationFrameCallback
  , js_dataView
  ) where

import GHCJS.DOM.Types (Callback (..), JSM, JSVal, FrameRequestCallback (..))
import Language.Javascript.JSaddle.Object (freeFunction, jsg, new)

synchronously :: JSM a -> JSM a
synchronously = id

freeRequestAnimationFrameCallback :: FrameRequestCallback -> JSM ()
freeRequestAnimationFrameCallback (FrameRequestCallback (Callback cb)) = freeFunction cb

js_dataView :: Int -> Int -> JSVal -> JSM JSVal
js_dataView off len ref = new (jsg "DataView") (ref, off, len)
