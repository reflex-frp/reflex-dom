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
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding
import Foreign.JavaScript.Utils (bsFromMutableArrayBuffer, bsToArrayBuffer)
import GHCJS.DOM.CloseEvent
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.MessageEvent
import GHCJS.DOM.Types hiding (Text)
import qualified GHCJS.DOM.WebSocket as GD
import GHCJS.Foreign (JSType(..), jsTypeOf)
import Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)
import Language.Javascript.JSaddle.Types (ghcjsPure)

data JSWebSocket = JSWebSocket { unWebSocket :: WebSocket }

class IsWebSocketMessage a where
  webSocketSend :: JSWebSocket -> a -> JSM ()

-- Use binary websocket communication for ByteString
-- Note: Binary websockets may not work correctly in IE 11 and below
instance IsWebSocketMessage ByteString where
  webSocketSend (JSWebSocket ws) bs = do
    ab <- bsToArrayBuffer bs
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
        ab <- mutableArrayBufferFromJSVal d
        bsFromMutableArrayBuffer ab >>= onMessage . Left
  return $ JSWebSocket ws

onBSMessage :: Either ByteString JSVal -> JSM ByteString
onBSMessage = either return (\v -> encodeUtf8 <$> fromJSValUnchecked v)
