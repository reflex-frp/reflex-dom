{-# LANGUAGE ScopedTypeVariables #-}
module Foreign.JavaScript.Utils
  ( bsFromMutableArrayBuffer
  , bsToArrayBuffer
  , jsonParse
  , safeJsonParse
  ) where

import Control.Exception
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign.JavaScript.Internal.Utils (js_dataView)
import qualified GHCJS.Buffer as JS
import GHCJS.DOM.Types (ArrayBuffer (..))
import qualified Language.Javascript.JSaddle as JS (catch, jsg, js1)
import qualified JavaScript.TypedArray.ArrayBuffer as JS
import Language.Javascript.JSaddle.Types (JSString, JSM, JSVal, MonadJSM, ghcjsPure, jsval, liftJSM)

{-# INLINABLE bsFromMutableArrayBuffer #-}
bsFromMutableArrayBuffer :: MonadJSM m => JS.MutableArrayBuffer -> m ByteString
bsFromMutableArrayBuffer ab = liftJSM $ JS.unsafeFreeze ab >>=
    ghcjsPure . JS.createFromArrayBuffer >>= ghcjsPure . JS.toByteString 0 Nothing

{-# INLINABLE bsToArrayBuffer #-}
bsToArrayBuffer :: MonadJSM m => ByteString -> m ArrayBuffer
bsToArrayBuffer bs = liftJSM $ do
  (b, off, len) <- ghcjsPure $ JS.fromByteString bs
  ArrayBuffer <$> if BS.length bs == 0 --TODO: remove this logic when https://github.com/ghcjs/ghcjs-base/issues/49 is fixed
                  then JS.create 0 >>= ghcjsPure . JS.getArrayBuffer >>= ghcjsPure . jsval
                  else do
                    ref <- ghcjsPure (JS.getArrayBuffer b) >>= ghcjsPure . jsval
                    js_dataView off len ref

safeJsonParse :: JSString -> JSM (Maybe JSVal)
safeJsonParse a = (Just <$> jsonParse a) `JS.catch` \(_ :: SomeException) -> return Nothing

jsonParse :: JSString -> JSM JSVal
jsonParse a = JS.jsg "JSON" ^. JS.js1 "parse" a
