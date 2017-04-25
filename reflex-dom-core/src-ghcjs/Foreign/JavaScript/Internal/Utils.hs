module Foreign.JavaScript.Internal.Utils
  ( synchronously
  , freeRequestAnimationFrameCallback
  ) where

import GHCJS.Concurrent
import GHCJS.DOM.Types (JSM, RequestAnimationFrameCallback (..))
import GHCJS.Foreign.Callback (releaseCallback)

freeRequestAnimationFrameCallback :: RequestAnimationFrameCallback -> JSM ()
freeRequestAnimationFrameCallback (RequestAnimationFrameCallback cb) = releaseCallback cb
