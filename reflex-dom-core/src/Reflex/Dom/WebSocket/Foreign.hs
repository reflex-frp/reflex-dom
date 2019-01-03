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
import Data.Bifoldable
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding
import Foreign.JavaScript.Utils (bsFromMutableArrayBuffer, bsToArrayBuffer)
import GHCJS.DOM.CloseEvent
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.MessageEvent
import GHCJS.DOM.Types (JSM, JSVal, liftJSM, fromJSValUnchecked)
import GHCJS.DOM.WebSocket (WebSocket)
import qualified GHCJS.DOM.WebSocket as DOM
import GHCJS.Foreign (JSType(..), jsTypeOf)
import Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)
import Language.Javascript.JSaddle.Types (ghcjsPure)

newtype JSWebSocket = JSWebSocket { unWebSocket :: WebSocket }

class IsWebSocketMessage a where
  webSocketSend :: JSWebSocket -> a -> JSM ()

instance (IsWebSocketMessage a, IsWebSocketMessage b) => IsWebSocketMessage (Either a b) where
  webSocketSend jws = bitraverse_ (webSocketSend jws) (webSocketSend jws)

-- Use binary websocket communication for ByteString
-- Note: Binary websockets may not work correctly in IE 11 and below
instance IsWebSocketMessage ByteString where
  webSocketSend (JSWebSocket ws) bs = do
    ab <- bsToArrayBuffer bs
    DOM.send ws ab

instance IsWebSocketMessage LBS.ByteString where
  webSocketSend ws = webSocketSend ws . LBS.toStrict

-- Use plaintext websocket communication for Text, and String
instance IsWebSocketMessage Text where
  webSocketSend (JSWebSocket ws) = DOM.sendString ws

closeWebSocket :: JSWebSocket -> Word -> Text -> JSM ()
closeWebSocket (JSWebSocket ws) code reason = DOM.close ws (Just code) (Just reason)

newWebSocket
  :: a
  -> Text -- url
  -> [Text] -- protocols
  -> (Either ByteString JSVal -> JSM ()) -- onmessage
  -> JSM () -- onopen
  -> JSM () -- onerror
  -> ((Bool, Word, Text) -> JSM ()) -- onclose
  -> JSM JSWebSocket
newWebSocket _ url protocols onMessage onOpen onError onClose = do
  ws <- DOM.newWebSocket url protocols
  DOM.setBinaryType ws "arraybuffer"
  _ <- on ws DOM.open $ liftJSM onOpen
  _ <- on ws DOM.error $ liftJSM onError
  _ <- on ws DOM.closeEvent $ do
    e <- ask
    wasClean <- getWasClean e
    code <- getCode e
    reason <- getReason e
    liftJSM $ onClose (wasClean, code, reason)
  _ <- on ws DOM.message $ do
    e <- ask
    d <- getData e
    liftJSM $ ghcjsPure (jsTypeOf d) >>= \case
      String -> onMessage $ Right d
      _ -> do
        ab <- mutableArrayBufferFromJSVal d
        bsFromMutableArrayBuffer ab >>= onMessage . Left
  return $ JSWebSocket ws

onBSMessage :: Either ByteString JSVal -> JSM ByteString
onBSMessage = either return $ fmap encodeUtf8 . fromJSValUnchecked
