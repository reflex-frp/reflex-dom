{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Reflex.Dom.Xhr.Foreign ( module Reflex.Dom.Xhr.Foreign
                              , XhrResponseType
                              ) where

import Control.Concurrent.MVar
import Control.Exception.Base
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM.File
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
import Graphics.UI.Gtk.WebKit.Types hiding (Text)
import Reflex.Dom.Xhr.Exception
import Reflex.Dom.Xhr.ResponseType
import System.Glib.FFI

import Reflex.Dom.Internal.Foreign

import Text.RawString.QQ

data XMLHttpRequest
   = XMLHttpRequest { xhrValue :: JSValueRef
                    , xhrContext :: JSContextRef --TODO: Get rid of this
                    }
   deriving (Eq, Ord)

class IsXhrPayload a where
  xmlHttpRequestSend :: XMLHttpRequest -> a -> IO ()

instance IsXhrPayload () where
  xmlHttpRequestSend xhr _ = xmlHttpRequestSendPayload xhr Nothing

instance IsXhrPayload Text where
  xmlHttpRequestSend xhr = xmlHttpRequestSendPayload xhr . Just

instance IsXhrPayload String where
  xmlHttpRequestSend xhr = xmlHttpRequestSendPayload xhr . Just . T.pack

instance IsXhrPayload File where
  xmlHttpRequestSend = error "xmlHttpRequestSend{File}: not implemented"

stringToJSValue :: JSContextRef -> Text -> IO JSValueRef
stringToJSValue ctx s = jsvaluemakestring ctx =<< jsstringcreatewithutf8cstring (T.unpack s)

fromResponseType :: XhrResponseType -> Text
fromResponseType XhrResponseType_Default = ""
fromResponseType XhrResponseType_ArrayBuffer = "arraybuffer"
fromResponseType XhrResponseType_Blob = "blob"
fromResponseType XhrResponseType_Text = "text"

toResponseType :: Text -> Maybe XhrResponseType
toResponseType "" = Just XhrResponseType_Default
toResponseType "arraybuffer" = Just XhrResponseType_ArrayBuffer
toResponseType "blob" = Just XhrResponseType_Blob
toResponseType "text" = Just XhrResponseType_Text
toResponseType _ = Nothing

xmlHttpRequestNew :: WebView -> IO XMLHttpRequest
xmlHttpRequestNew wv = withWebViewContext wv $ \jsContext -> do
  xhrScript <- jsstringcreatewithutf8cstring "new XMLHttpRequest()"
  xhr' <- jsevaluatescript jsContext xhrScript nullPtr nullPtr 1 nullPtr
  jsvalueprotect jsContext xhr'
  return $ XMLHttpRequest xhr' jsContext

xmlHttpRequestOpen :: XMLHttpRequest -> Text -> Text -> Bool -> Text -> Text -> IO ()
xmlHttpRequestOpen xhr method url async user password = do
  let c = xhrContext xhr
  method' <- stringToJSValue c method
  url' <- stringToJSValue c url
  async' <- jsvaluemakeboolean (xhrContext xhr) async
  user' <- stringToJSValue c user
  password' <- stringToJSValue c password
  o <- toJSObject c [xhrValue xhr, method', url', async', user', password']
  script <- jsstringcreatewithutf8cstring "this[0].open(this[1], this[2], this[3], this[4], this[5])"
  _ <- jsevaluatescript c script o nullPtr 1 nullPtr
  return ()

xmlHttpRequestOnreadystatechange :: XMLHttpRequest -> IO () -> IO ()
xmlHttpRequestOnreadystatechange xhr userCallback = do
  let c = xhrContext xhr
  fp <- wrapper $ \_ _ _ _ _ _ -> do
    userCallback
    jsvaluemakeundefined c
  cb <- jsobjectmakefunctionwithcallback c nullPtr fp
  o <- toJSObject c [xhrValue xhr, cb]
  script <- jsstringcreatewithutf8cstring "this[0].onreadystatechange=this[1]"
  _ <- jsevaluatescript c script o nullPtr 1 nullPtr
  return ()

xmlHttpRequestGetReadyState :: XMLHttpRequest -> IO Word
xmlHttpRequestGetReadyState xhr = do
  let c = xhrContext xhr
  script <- jsstringcreatewithutf8cstring "this.readyState"
  rs <- jsevaluatescript c script (xhrValue xhr) nullPtr 1 nullPtr
  d <- jsvaluetonumber c rs nullPtr
  return $ truncate d

xmlHttpRequestGetResponseType :: XMLHttpRequest -> IO (Maybe XhrResponseType)
xmlHttpRequestGetResponseType xhr = do
  script <- jsstringcreatewithutf8cstring "this.responseType"
  rt <- jsevaluatescript (xhrContext xhr) script (xhrValue xhr) nullPtr 1 nullPtr
  ms <- fromJSStringMaybe (xhrContext xhr) rt
  return $ join $ fmap toResponseType ms

xmlHttpRequestGetResponse :: XMLHttpRequest -> IO (Maybe XhrResponseBody)
xmlHttpRequestGetResponse xhr = do
  let c = xhrContext xhr
  mrt <- xmlHttpRequestGetResponseType xhr
  script <- jsstringcreatewithutf8cstring "this.response"
  t <- jsevaluatescript c script (xhrValue xhr) nullPtr 1 nullPtr
  isNull <- jsvalueisnull c t
  if isNull then return Nothing else case mrt of
    Just XhrResponseType_ArrayBuffer -> Just . XhrResponseBody_ArrayBuffer <$> bsFromArrayBuffer c t
    Just XhrResponseType_Blob -> Just . XhrResponseBody_Blob . Blob . castForeignPtr <$> newForeignPtr_ t
    Just XhrResponseType_Default -> fmap XhrResponseBody_Default <$> fromJSStringMaybe c t
    Just XhrResponseType_Text -> fmap XhrResponseBody_Text <$> fromJSStringMaybe c t
    _ -> return Nothing

xmlHttpRequestGetResponseText :: XMLHttpRequest -> IO (Maybe Text)
xmlHttpRequestGetResponseText xhr = do
  let c = xhrContext xhr
  script <- jsstringcreatewithutf8cstring "this.responseText"
  t <- jsevaluatescript c script (xhrValue xhr) nullPtr 1 nullPtr
  fromJSStringMaybe c t

xmlHttpRequestSendPayload :: XMLHttpRequest -> Maybe Text -> IO ()
xmlHttpRequestSendPayload xhr payload = do
  let c = xhrContext xhr
  result <- newEmptyMVar
  let wrapper' x = wrapper $ \_ _ _ _ _ _ -> putMVar result x >> jsvaluemakeundefined c
  bracket (wrapper' $ Just XhrException_Aborted) freeHaskellFunPtr $ \a -> do
    onAbort <- jsobjectmakefunctionwithcallback c nullPtr a
    bracket (wrapper' $ Just XhrException_Error) freeHaskellFunPtr $ \e -> do
      onError <- jsobjectmakefunctionwithcallback c nullPtr e
      bracket (wrapper' Nothing) freeHaskellFunPtr $ \l -> do
        onLoad <- jsobjectmakefunctionwithcallback c nullPtr l
        d <- case payload of
                  Nothing -> jsvaluemakeundefined c
                  Just payload' -> stringToJSValue c payload'
        o <- toJSObject c [xhrValue xhr, d, onError, onAbort, onLoad]
        s <- jsstringcreatewithutf8cstring send
        _ <- jsevaluatescript c s o nullPtr 1 nullPtr
        takeMVar result >>= mapM_ throwIO
  where
    send = [r|
      (function (xhr, d, onError, onAbort, onLoad) {
          var clear;
          var error = function () {
              clear(); onError();
          };
          var abort = function () {
              clear(); onAbort();
          };
          var load = function () {
              clear(); onLoad();
          };
          clear = function () {
              xhr.removeEventListener('error', error);
              xhr.removeEventListener('abort', abort);
              xhr.removeEventListener('load', load);
          }
          xhr.addEventListener('error', error);
          xhr.addEventListener('abort', abort);
          xhr.addEventListener('load', load);
          if(d) {
            xhr.send(d);
          } else {
            xhr.send();
          }
      })(this[0], this[1], this[2], this[3], this[4])
    |]

xmlHttpRequestSetRequestHeader :: XMLHttpRequest -> Text -> Text -> IO ()
xmlHttpRequestSetRequestHeader xhr header value = do
  let c = xhrContext xhr
  header' <- stringToJSValue c header
  value' <- stringToJSValue c value
  o <- toJSObject c [xhrValue xhr, header', value']
  script <- jsstringcreatewithutf8cstring "this[0].setRequestHeader(this[1], this[2])"
  _ <- jsevaluatescript c script o nullPtr 1 nullPtr
  return ()

xmlHttpRequestSetResponseType :: XMLHttpRequest -> Text -> IO ()
xmlHttpRequestSetResponseType xhr t = do
  let c = xhrContext xhr
  t' <- stringToJSValue c t
  o <- toJSObject c [xhrValue xhr, t']
  script <- jsstringcreatewithutf8cstring "this[0].responseType = this[1]"
  _ <- jsevaluatescript c script o nullPtr 1 nullPtr
  return ()

xmlHttpRequestGetStatus :: XMLHttpRequest -> IO Word
xmlHttpRequestGetStatus xhr = do
  let c = xhrContext xhr
  script <- jsstringcreatewithutf8cstring "this.status"
  s <- jsevaluatescript c script (xhrValue xhr) nullPtr 1 nullPtr
  d <- jsvaluetonumber c s nullPtr
  return $ truncate d

xmlHttpRequestGetStatusText :: XMLHttpRequest -> IO Text
xmlHttpRequestGetStatusText xhr =
  xmlAuxHttpRequestGetWithJS xhr "this.statusText"

xmlHttpRequestSetWithCredentials :: XMLHttpRequest -> Bool -> IO ()
xmlHttpRequestSetWithCredentials xhr b = do
  let c = xhrContext xhr
  b' <- jsvaluemakeboolean c b
  o <- toJSObject c [xhrValue xhr, b']
  script <- jsstringcreatewithutf8cstring "this[0].withCredentials = this[1]"
  _ <- jsevaluatescript c script o nullPtr 1 nullPtr
  return ()

xmlHttpRequestGetAllResponseHeaders :: XMLHttpRequest -> IO Text
xmlHttpRequestGetAllResponseHeaders xhr =
  xmlAuxHttpRequestGetWithJS xhr "this.getAllResponseHeaders()"

xmlHttpRequestGetResponseHeader :: XMLHttpRequest -> Text -> IO Text
xmlHttpRequestGetResponseHeader xhr hdr = do
  let c = xhrContext xhr
  t' <- stringToJSValue c hdr
  o <- toJSObject c [xhrValue xhr, t']
  script <- jsstringcreatewithutf8cstring "this[0].getResponseHeader(this[1])"
  t <- jsevaluatescript c script o nullPtr 1 nullPtr
  xmlAuxJsGetString c t

xmlAuxHttpRequestGetWithJS :: XMLHttpRequest -> String -> IO Text
xmlAuxHttpRequestGetWithJS xhr jsScript = do
  let c = xhrContext xhr
  script <- jsstringcreatewithutf8cstring jsScript
  t <- jsevaluatescript c script (xhrValue xhr) nullPtr 1 nullPtr
  xmlAuxJsGetString c t

xmlAuxJsGetString :: JSContextRef -> JSValueRef -> IO Text
xmlAuxJsGetString c t = do
  j <- jsvaluetostringcopy c t nullPtr
  l <- jsstringgetmaximumutf8cstringsize j
  s <- allocaBytes (fromIntegral l) $ \ps -> do
         _ <- jsstringgetutf8cstring'_ j ps (fromIntegral l)
         peekCString ps
  return $ T.pack s

