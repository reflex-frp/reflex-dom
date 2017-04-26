module Foreign.JavaScript.Internal.Utils
  ( synchronously
  , freeRequestAnimationFrameCallback
  , js_dataView
  ) where

import GHCJS.DOM.Types (JSM, JSVal, RequestAnimationFrameCallback (..), Callback (..))
import Language.Javascript.JSaddle.Object (freeFunction, new, jsg)

synchronously :: JSM a -> JSM a
synchronously = id

freeRequestAnimationFrameCallback :: RequestAnimationFrameCallback -> JSM ()
freeRequestAnimationFrameCallback (RequestAnimationFrameCallback (Callback cb)) = freeFunction cb

js_dataView :: Int -> Int -> JSVal -> JSM JSVal
js_dataView off len ref = new (jsg "DataView") (ref, off, len)
