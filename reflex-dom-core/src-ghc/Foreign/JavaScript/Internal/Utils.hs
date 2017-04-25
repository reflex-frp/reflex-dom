module Foreign.JavaScript.Internal.Utils
  ( synchronously
  , freeRequestAnimationFrameCallback
  ) where

import GHCJS.DOM.Types (JSM, RequestAnimationFrameCallback (..), Callback (..))
import Language.Javascript.JSaddle.Object (freeFunction)

synchronously :: JSM a -> JSM a
synchronously = id

freeRequestAnimationFrameCallback :: RequestAnimationFrameCallback -> JSM ()
freeRequestAnimationFrameCallback (RequestAnimationFrameCallback (Callback cb)) = freeFunction cb
