{-# LANGUAGE CPP #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
#endif

module Foreign.JavaScript.Internal.Utils
  ( synchronously
  , freeRequestAnimationFrameCallback
  , js_dataView
  ) where

import GHCJS.Concurrent
import GHCJS.DOM.Types (JSM, JSVal, RequestAnimationFrameCallback (..))
#if __GLASGOW_HASKELL__ < 900
import GHCJS.Foreign.Callback (releaseCallback)
#else
import GHC.JS.Foreign.Callback (releaseCallback)
#endif

freeRequestAnimationFrameCallback :: RequestAnimationFrameCallback -> JSM ()
freeRequestAnimationFrameCallback (RequestAnimationFrameCallback cb) = releaseCallback cb

foreign import javascript safe "new DataView($3,$1,$2)"
  js_dataView :: Int -> Int -> JSVal -> IO JSVal
