module Foreign.JavaScript.Internal.Utils
  ( js_dataView
  ) where

import Language.Javascript.JSaddle.Types (JSVal, JSM)
import Language.Javascript.JSaddle.Object (new, jsg)

js_dataView :: Int -> Int -> JSVal -> JSM JSVal
js_dataView off len ref = new (jsg "DataView") (ref, off, len)
