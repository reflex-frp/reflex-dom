{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module Foreign.JavaScript.Internal.Utils
  ( js_dataView
  ) where

import Language.Javascript.JSaddle.Types (JSVal)

foreign import javascript safe "new DataView($3,$1,$2)"
  js_dataView :: Int -> Int -> JSVal -> IO JSVal
