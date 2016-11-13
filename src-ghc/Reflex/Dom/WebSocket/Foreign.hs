{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.WebSocket.Foreign where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Control.Exception
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
import Graphics.UI.Gtk.WebKit.WebView

import Foreign.JavaScript.TH
import Reflex.Dom.Internal.Foreign

data JSWebSocket = JSWebSocket { wsValue :: JSValueRef
                               , wsContext :: JSContextRef
                               }

importJS Unsafe "this[0]['send'](this[1])" "_sendWSTextData" [t| forall x m. MonadJS x m => JSRef x -> String -> m () |]

sendWSTextData :: JSWebSocket -> String -> IO ()
sendWSTextData (JSWebSocket ws c) str = runWithJSContext (_sendWSTextData (JSRef_JavaScriptCore ws) str) c

class IsWebSocketMessage a where
  webSocketSend :: JSWebSocket -> a -> IO ()

-- Use binary websocket communication for ByteString
instance IsWebSocketMessage ByteString where
  webSocketSend (JSWebSocket ws c) bs = do
    elems <- forM (BS.unpack bs) $ \x -> jsvaluemakenumber c $ fromIntegral x
    let numElems = length elems
    bs' <- bracket (mallocArray numElems) free $ \elemsArr -> do
      pokeArray elemsArr elems
      a <- jsobjectmakearray c (fromIntegral numElems) elemsArr nullPtr
      newUint8Array <- jsstringcreatewithutf8cstring "new Uint8Array(this)"
      jsevaluatescript c newUint8Array a nullPtr 1 nullPtr
    send <- jsstringcreatewithutf8cstring ""
    sendArgs <- toJSObject c [ws, bs']
    _ <- jsevaluatescript c send sendArgs nullPtr 1 nullPtr
    return ()

-- Use plaintext websocket communication for Text
instance IsWebSocketMessage Text where
  webSocketSend jws t = sendWSTextData jws (T.unpack t)

importJS Unsafe "this[0]['close'](this[1], this[2])" "_closeWS" [t| forall x m. MonadJS x m => JSRef x -> Double -> String -> m () |]

closeWebSocket :: JSWebSocket -> Double -> Text -> IO ()
closeWebSocket (JSWebSocket ws c) code reason = do
  runWithJSContext (_closeWS (JSRef_JavaScriptCore ws) code (T.unpack reason)) c

newWebSocket
  :: WebView
  -> Text -- url
  -> (Either ByteString a -> IO ()) -- onmessage
  -> IO () -- onopen
  -> IO () -- onerror
  -> ((Bool, Word, Text) -> IO ()) -- onclose
  -> IO JSWebSocket
newWebSocket wv url onMessage onOpen onError onClose = withWebViewContext wv $ \c -> do
  url' <- jsvaluemakestring c =<< jsstringcreatewithutf8cstring (T.unpack url)
  newWSArgs <- toJSObject c [url']
  newWS <- jsstringcreatewithutf8cstring "(function(that) { var ws = new WebSocket(that[0]); ws['binaryType'] = 'arraybuffer'; return ws; })(this)"
  ws <- jsevaluatescript c newWS newWSArgs nullPtr 1 nullPtr
  onMessage' <- wrapper $ \_ _ _ _ args _ -> do
    e <- peekElemOff args 0
    dataProp <- jsstringcreatewithutf8cstring "data"
    msg <- jsobjectgetproperty c e dataProp nullPtr
    msg' <- fromJSStringMaybe c msg
    case msg' of
      Nothing -> return ()
      Just m -> onMessage $ Left $ encodeUtf8 m
    jsvaluemakeundefined c
  onMessageCb <- jsobjectmakefunctionwithcallback c nullPtr onMessage'
  onOpen' <- wrapper $ \_ _ _ _ _ _ -> do
    onOpen
    jsvaluemakeundefined c
  onOpenCb <- jsobjectmakefunctionwithcallback c nullPtr onOpen'
  onError' <- wrapper $ \_ _ _ _ _ _ -> do
    onError
    jsvaluemakeundefined c
  onErrorCb <- jsobjectmakefunctionwithcallback c nullPtr onError'
  onClose' <- wrapper $ \_ _ _ _ args _ -> do
    e <- peekElemOff args 0
    propWasClean <- jsstringcreatewithutf8cstring "wasClean"
    propCode <- jsstringcreatewithutf8cstring "code"
    propReason <- jsstringcreatewithutf8cstring "reason"
    wasClean <- fmap (fromMaybe False) $ fromJSBoolMaybe c
      =<< jsobjectgetproperty c e propWasClean nullPtr
    code <- fmap (fromMaybe 1000) $ fromJSNumMaybe c
      =<< jsobjectgetproperty c e propCode nullPtr
    reason <- fmap (fromMaybe mempty) $ fromJSStringMaybe c
      =<< jsobjectgetproperty c e propReason nullPtr
    onClose (wasClean, truncate code, reason)
    jsvaluemakeundefined c
  onCloseCb <- jsobjectmakefunctionwithcallback c nullPtr onClose'
  o <- toJSObject c [ws, onMessageCb, onOpenCb, onErrorCb, onCloseCb]
  addCbs <- jsstringcreatewithutf8cstring "this[0]['onmessage'] = this[1]; this[0]['onopen'] = this[2]; this[0]['onerror'] = this[3]; this[0]['onclose'] = this[4];"
  _ <- jsevaluatescript c addCbs o nullPtr 1 nullPtr
  return $ JSWebSocket ws c

onBSMessage :: Either ByteString b -> ByteString
onBSMessage = either id (error "onBSMessage: ghc env expects ByteString.")

type JSVal = ()
