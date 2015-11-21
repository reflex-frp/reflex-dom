{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP #-}

module Reflex.Dom.WebSocket.Foreign where

import Prelude hiding (div, span, mapM, mapM_, concat, concatMap, all, sequence)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHCJS.Types
import GHCJS.DOM.WebSocket (message, closeEvent)
import qualified GHCJS.DOM.WebSocket as GD
import GHCJS.DOM.MessageEvent
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Types
import Control.Monad.IO.Class
import Control.Monad.Reader
import GHCJS.Buffer
import JavaScript.TypedArray.ArrayBuffer as JS
import GHCJS.Marshal.Pure

data JSWebSocket = JSWebSocket { unWebSocket :: WebSocket }

newWebSocket :: a -> String -> (ByteString -> IO ()) -> IO () -> IO JSWebSocket
newWebSocket _ url onMessage onClose = do
  ws <- GD.newWebSocket url (Just [] :: Maybe [String])
  GD.setBinaryType ws "arraybuffer"
  _ <- on ws message $ do
    e <- ask
    d <- getData e
    ab <- liftIO $ unsafeFreeze $ pFromJSVal d
    liftIO $ onMessage $ toByteString 0 Nothing $ createFromArrayBuffer ab
  _ <- on ws closeEvent $ liftIO onClose
  return $ JSWebSocket ws

webSocketSend :: JSWebSocket -> ByteString -> IO ()
webSocketSend (JSWebSocket ws) bs = do
  let (b, off, len) = fromByteString bs
  ab <- ArrayBuffer <$> if BS.length bs == 0 --TODO: remove this logic when https://github.com/ghcjs/ghcjs-base/issues/49 is fixed
                        then jsval . getArrayBuffer <$> create 0
                        else return $ js_dataView off len $ jsval $ getArrayBuffer b
  GD.send ws $ Just ab

foreign import javascript safe "new DataView($3,$1,$2)" js_dataView :: Int -> Int -> JSVal -> JSVal
