{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Foreign.JavaScript.Utils
  ( bsFromMutableArrayBuffer
  , bsToArrayBuffer
  , jsonDecode
  , js_jsonParse
  ) where

import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign.JavaScript.Internal.Utils (js_dataView)
import qualified GHCJS.Buffer as JS
import GHCJS.DOM.Types (ArrayBuffer (..))
import GHCJS.Marshal ()
import Language.Javascript.JSaddle (jsg, js1)
import qualified JavaScript.TypedArray.ArrayBuffer as JS
import Language.Javascript.JSaddle.Types (JSString, JSM, JSVal, MonadJSM, ghcjsPure, jsval, liftJSM)
#ifdef ghcjs_HOST_OS
import Control.Exception (SomeException)
import Language.Javascript.JSaddle (fromJSVal, catch)
import System.IO.Unsafe
#else
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding
import Language.Javascript.JSaddle (textFromJSString)
#endif

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

jsonDecode :: FromJSON a => JSString -> Maybe a
#ifdef ghcjs_HOST_OS
jsonDecode t = do
  result <- unsafePerformIO $ (fromJSVal =<< js_jsonParse t) `catch` (\(_ :: SomeException) -> pure Nothing)
  case fromJSON result of
    Success a -> Just a
    Error _ -> Nothing
#else
jsonDecode = decode . LBS.fromStrict . encodeUtf8 . textFromJSString
#endif

js_jsonParse :: JSString -> JSM JSVal
js_jsonParse a = jsg "JSON" ^. js1 "parse" a
