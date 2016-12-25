{-# LANGUAGE CPP #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
{-# LANGUAGE LambdaCase #-}

module Reflex.Dom.WebSocket.Foreign
  ( module Reflex.Dom.WebSocket.Foreign
  , JSVal
  ) where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding
import GHCJS.Buffer
import GHCJS.DOM.CloseEvent
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.MessageEvent
import GHCJS.DOM.Types hiding (Text)
import qualified GHCJS.DOM.WebSocket as GD
import GHCJS.Foreign (JSType(..), jsTypeOf)
import GHCJS.Types
import JavaScript.TypedArray.ArrayBuffer as JS
import Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)
import Language.Javascript.JSaddle.Types (ghcjsPure)
#ifndef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Object (new, jsg)
#endif

data JSWebSocket = JSWebSocket { unWebSocket :: WebSocket }

class IsWebSocketMessage a where
  webSocketSend :: JSWebSocket -> a -> JSM ()

-- Use binary websocket communication for ByteString
-- Note: Binary websockets may not work correctly in IE 11 and below
instance IsWebSocketMessage ByteString where
  webSocketSend (JSWebSocket ws) bs = do
    (b, off, len) <- ghcjsPure $ fromByteString bs
    ab <- ArrayBuffer <$> if BS.length bs == 0 --TODO: remove this logic when https://github.com/ghcjs/ghcjs-base/issues/49 is fixed
                          then create 0 >>= ghcjsPure . getArrayBuffer >>= ghcjsPure . jsval
                          else do
                            ref <- ghcjsPure (getArrayBuffer b) >>= ghcjsPure . jsval
                            js_dataView off len ref -- new (jsg "DataView") (ref, off, len)
    GD.send ws $ Just ab

-- Use plaintext websocket communication for Text, and String
instance IsWebSocketMessage Text where
  webSocketSend (JSWebSocket ws) = GD.sendString ws . T.unpack

closeWebSocket :: JSWebSocket -> Word -> Text -> JSM ()
closeWebSocket (JSWebSocket ws) = GD.close ws

newWebSocket
  :: a
  -> Text -- url
  -> (Either ByteString JSVal -> JSM ()) -- onmessage
  -> JSM () -- onopen
  -> JSM () -- onerror
  -> ((Bool, Word, Text) -> JSM ()) -- onclose
  -> JSM JSWebSocket
newWebSocket _ url onMessage onOpen onError onClose = do
  ws <- GD.newWebSocket url (Just [] :: Maybe [Text])
  GD.setBinaryType ws "arraybuffer"
  _ <- on ws GD.open $ liftJSM onOpen
  _ <- on ws GD.error $ liftJSM onError
  _ <- on ws GD.closeEvent $ do
    e <- ask
    wasClean <- getWasClean e
    code <- getCode e
    reason <- getReason e
    liftJSM $ onClose (wasClean, code, reason)
  _ <- on ws GD.message $ do
    e <- ask
    d <- getData e
    liftJSM $ ghcjsPure (jsTypeOf d) >>= \case
      String -> onMessage $ Right d
      _ -> do
        ab <- mutableArrayBufferFromJSVal d >>= unsafeFreeze
        ghcjsPure (createFromArrayBuffer ab) >>= ghcjsPure . toByteString 0 Nothing >>= onMessage . Left
  return $ JSWebSocket ws

#ifdef ghcjs_HOST_OS
foreign import javascript safe "new DataView($3,$1,$2)" js_dataView :: Int -> Int -> JSVal -> IO JSVal
#else
js_dataView :: Int -> Int -> JSVal -> JSM JSVal
js_dataView off len ref = new (jsg "DataView") (ref, off, len)
#endif

onBSMessage :: Either ByteString JSVal -> JSM ByteString
onBSMessage = either return (\v -> encodeUtf8 <$> fromJSValUnchecked v)
