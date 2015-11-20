{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances, PolyKinds, AllowAmbiguousTypes #-}

module Reflex.Dom.WebSocket.Foreign where

import Prelude hiding (div, span, mapM, mapM_, concat, concatMap, all, sequence)

import Control.Exception
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Text.Encoding
import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame
import Graphics.UI.Gtk.WebKit.WebView
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Reflex.Dom.Internal.Foreign

data JSWebSocket = JSWebSocket { wsValue :: JSValueRef
                               , wsContext :: JSContextRef
                               }

getLocationHost :: WebView -> IO String
getLocationHost wv = do
  c <- webFrameGetGlobalContext =<< webViewGetMainFrame wv
  script <- jsstringcreatewithutf8cstring "location.host"
  lh <- jsevaluatescript c script nullPtr nullPtr 1 nullPtr
  lh' <- fromJSStringMaybe c lh
  return $ maybe "" id lh'

getLocationProtocol :: WebView -> IO String
getLocationProtocol wv = do
  c <- webFrameGetGlobalContext =<< webViewGetMainFrame wv
  script <- jsstringcreatewithutf8cstring "location.protocol"
  lp <- jsevaluatescript c script nullPtr nullPtr 1 nullPtr
  lp' <- fromJSStringMaybe c lp
  return $ maybe "" id lp'

newWebSocket :: WebView -> String -> (ByteString -> IO ()) -> IO () -> IO JSWebSocket
newWebSocket wv url onMessage onClose = do
  c <- webFrameGetGlobalContext =<< webViewGetMainFrame wv
  url' <- jsvaluemakestring c =<< jsstringcreatewithutf8cstring url
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
      Just m -> onMessage $ encodeUtf8 $ T.pack m
    jsvaluemakeundefined c
  onMessageCb <- jsobjectmakefunctionwithcallback c nullPtr onMessage'
  onClose' <- wrapper $ \_ _ _ _ _ _ -> do
    onClose
    jsvaluemakeundefined c
  onCloseCb <- jsobjectmakefunctionwithcallback c nullPtr onClose'
  o <- toJSObject c [ws, onMessageCb, onCloseCb]
  addCbs <- jsstringcreatewithutf8cstring "this[0]['onmessage'] = this[1]; this[0]['onclose'] = this[2];"
  _ <- jsevaluatescript c addCbs o nullPtr 1 nullPtr
  return $ JSWebSocket ws c

webSocketSend :: JSWebSocket -> ByteString -> IO ()
webSocketSend (JSWebSocket ws c) bs = do
  elems <- forM (BS.unpack bs) $ \x -> jsvaluemakenumber c $ fromIntegral x
  let numElems = length elems
  bs' <- bracket (mallocArray numElems) free $ \elemsArr -> do
    pokeArray elemsArr elems
    a <- jsobjectmakearray c (fromIntegral numElems) elemsArr nullPtr
    newUint8Array <- jsstringcreatewithutf8cstring "new Uint8Array(this)"
    jsevaluatescript c newUint8Array a nullPtr 1 nullPtr
  send <- jsstringcreatewithutf8cstring "this[0]['send'](String['fromCharCode']['apply'](null, this[1]))"
  sendArgs <- toJSObject c [ws, bs']
  _ <- jsevaluatescript c send sendArgs nullPtr 1 nullPtr
  return ()

