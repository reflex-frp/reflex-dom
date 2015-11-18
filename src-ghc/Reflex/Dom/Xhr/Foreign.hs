{-# LANGUAGE ForeignFunctionInterface #-}
module Reflex.Dom.Xhr.Foreign where

import Control.Lens.Indexed
import qualified Data.Text as T
import Data.Text (Text)
import System.Glib.FFI
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame

import Reflex.Dom.Internal.Foreign

data XMLHttpRequest
   = XMLHttpRequest { xhrValue :: JSValueRef
                    , xhrContext :: JSContextRef
                    }
   deriving (Eq, Ord)

responseTextToText :: Maybe String -> Maybe Text
responseTextToText = fmap T.pack

statusTextToText :: String -> Text
statusTextToText = T.pack

stringToJSValue :: JSContextRef -> String -> IO JSValueRef
stringToJSValue ctx s = jsvaluemakestring ctx =<< jsstringcreatewithutf8cstring s

toResponseType :: a -> a
toResponseType = id

xmlHttpRequestNew :: WebView -> IO XMLHttpRequest
xmlHttpRequestNew wv = do
  wf <- webViewGetMainFrame wv
  jsContext <- webFrameGetGlobalContext wf
  xhrScript <- jsstringcreatewithutf8cstring "new XMLHttpRequest()"
  xhr' <- jsevaluatescript jsContext xhrScript nullPtr nullPtr 1 nullPtr
  jsvalueprotect jsContext xhr'
  return $ XMLHttpRequest xhr' jsContext

xmlHttpRequestOpen :: XMLHttpRequest -> String -> String -> Bool -> String -> String -> IO ()
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

xmlHttpRequestGetResponseText :: XMLHttpRequest -> IO (Maybe String)
xmlHttpRequestGetResponseText xhr = do
  let c = xhrContext xhr
  script <- jsstringcreatewithutf8cstring "this.responseText"
  t <- jsevaluatescript c script (xhrValue xhr) nullPtr 1 nullPtr
  isNull <- jsvalueisnull c t
  case isNull of
       True -> return Nothing
       False -> do
         j <- jsvaluetostringcopy c t nullPtr
         l <- jsstringgetmaximumutf8cstringsize j
         s <- allocaBytes (fromIntegral l) $ \ps -> do
                _ <- jsstringgetutf8cstring'_ j ps (fromIntegral l)
                peekCString ps
         return $ Just s

xmlHttpRequestSend :: XMLHttpRequest -> Maybe String -> IO ()
xmlHttpRequestSend xhr payload = do
  let c = xhrContext xhr
  (o,s) <- case payload of
            Nothing -> do
              o <- toJSObject c [xhrValue xhr]
              s <- jsstringcreatewithutf8cstring "this[0].send();"
              return (o,s)
            Just payload' -> do
              d <- stringToJSValue c payload'
              o <- toJSObject c [xhrValue xhr, d]
              s <- jsstringcreatewithutf8cstring "this[0].send(this[1])"
              return (o,s)
  _ <- jsevaluatescript c s o nullPtr 1 nullPtr
  return ()
  
xmlHttpRequestSetRequestHeader :: XMLHttpRequest -> String -> String -> IO ()
xmlHttpRequestSetRequestHeader xhr header value = do
  let c = xhrContext xhr
  header' <- stringToJSValue c header
  value' <- stringToJSValue c value
  o <- toJSObject c [xhrValue xhr, header', value']
  script <- jsstringcreatewithutf8cstring "this[0].setRequestHeader(this[1], this[2])"
  _ <- jsevaluatescript c script o nullPtr 1 nullPtr
  return ()

xmlHttpRequestSetResponseType :: XMLHttpRequest -> String -> IO ()
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

xmlHttpRequestGetStatusText :: XMLHttpRequest -> IO String
xmlHttpRequestGetStatusText xhr = do
  let c = xhrContext xhr
  script <- jsstringcreatewithutf8cstring "this.statusText"
  t <- jsevaluatescript c script (xhrValue xhr) nullPtr 1 nullPtr
  j <- jsvaluetostringcopy c t nullPtr
  l <- jsstringgetmaximumutf8cstringsize j
  s <- allocaBytes (fromIntegral l) $ \ps -> do
         _ <- jsstringgetutf8cstring'_ j ps (fromIntegral l)
         peekCString ps
  return s
