{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Dom.Internal.Foreign where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Lens hiding (set)
import Control.Monad
import Control.Monad.State.Strict hiding (forM, forM_, get, mapM, mapM_, sequence, sequence_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.Marshal hiding (void)
import Foreign.Ptr
import GHCJS.DOM hiding (runWebGUI)
import GHCJS.DOM.Navigator
import GHCJS.DOM.Window
import Graphics.UI.Gtk hiding (Widget)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame
import Graphics.UI.Gtk.WebKit.Types hiding (Event, Text, Widget)
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import System.Directory
import System.Glib.FFI hiding (void)

#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

quitWebView :: WebView -> IO ()
quitWebView wv = postGUIAsync $ do w <- widgetGetToplevel wv --TODO: Shouldn't this be postGUISync?
                                   widgetDestroy w

installQuitHandler :: WebView -> IO ()
#ifdef mingw32_HOST_OS
installQuitHandler wv = return () -- TODO: Maybe figure something out here for Windows users.
#else
installQuitHandler wv = void $ installHandler keyboardSignal (Catch (quitWebView wv)) Nothing
#endif

makeDefaultWebView :: Text -> (WebView -> IO ()) -> IO ()
makeDefaultWebView userAgentKey main = do
  _ <- initGUI
  window <- windowNew
  _ <- timeoutAddFull (yield >> return True) priorityHigh 10
  windowSetDefaultSize window 900 600
  windowSetPosition window WinPosCenter
  scrollWin <- scrolledWindowNew Nothing Nothing
  webView <- webViewNew
  settings <- webViewGetWebSettings webView
  userAgent <- settings `get` webSettingsUserAgent
  settings `set` [ webSettingsUserAgent := userAgent <> " " <> userAgentKey
                 , webSettingsEnableUniversalAccessFromFileUris := True
                 , webSettingsEnableDeveloperExtras := True
                 ]
  webViewSetWebSettings webView settings
  window `containerAdd` scrollWin
  scrollWin `containerAdd` webView
  _ <- on window objectDestroy . liftIO $ mainQuit
  widgetShowAll window
  _ <- webView `on` loadFinished $ \_ -> do
    main webView --TODO: Should probably only do this once
  inspector <- webViewGetInspector webView
  _ <- inspector `on` inspectWebView $ \_ -> do
    inspectorWindow <- windowNew
    windowSetDefaultSize inspectorWindow 900 600
    inspectorScrollWin <- scrolledWindowNew Nothing Nothing
    inspectorWebView <- webViewNew
    inspectorWindow `containerAdd` inspectorScrollWin
    inspectorScrollWin `containerAdd` inspectorWebView
    widgetShowAll inspectorWindow
    return inspectorWebView
  wf <- webViewGetMainFrame webView
  pwd <- getCurrentDirectory
  webFrameLoadString wf "" Nothing $ "file://" <> pwd <> "/"
  installQuitHandler webView
  mainGUI

runWebGUI :: (WebView -> IO ()) -> IO ()
runWebGUI = runWebGUI' "GHCJS"

runWebGUI' :: Text -> (WebView -> IO ()) -> IO ()
runWebGUI' userAgentKey main = do
  -- Are we in a java script inside some kind of browser
  mbWindow <- currentWindow
  case mbWindow of
    Just window -> do
      -- Check if we are running in javascript inside the the native version
      Just n <- getNavigator window
      agent <- getUserAgent n
      unless ((" " <> userAgentKey) `T.isSuffixOf` agent) $ main (castToWebView window)
    Nothing -> do
      makeDefaultWebView userAgentKey main

foreign import ccall "wrapper"
  wrapper :: JSObjectCallAsFunctionCallback' -> IO JSObjectCallAsFunctionCallback

toJSObject :: JSContextRef -> [Ptr OpaqueJSValue] -> IO JSObjectRef
toJSObject ctx args = do
  o <- jsobjectmake ctx nullPtr nullPtr
  iforM_ args $ \n a -> do
    prop <- jsstringcreatewithutf8cstring $ show n
    jsobjectsetproperty ctx o prop a 1 nullPtr
  return o

fromJSStringMaybe :: JSContextRef -> JSValueRef -> IO (Maybe Text)
fromJSStringMaybe c t = do
  isNull <- jsvalueisnull c t
  if isNull then return Nothing else do
    j <- jsvaluetostringcopy c t nullPtr
    l <- jsstringgetmaximumutf8cstringsize j
    s <- allocaBytes (fromIntegral l) $ \ps -> do
           _ <- jsstringgetutf8cstring'_ j ps (fromIntegral l)
           peekCString ps
    return $ Just $ T.pack s

fromJSBoolMaybe :: JSContextRef -> JSValueRef -> IO (Maybe Bool)
fromJSBoolMaybe c t = do
  isBool <- jsvalueisboolean c t
  if not isBool then return Nothing else
    Just <$> jsvaluetoboolean c t

fromJSNumMaybe :: JSContextRef -> JSValueRef -> IO (Maybe Double)
fromJSNumMaybe c t = do
  isNum <- jsvalueisnumber c t
  if not isNum then return Nothing else
    Just <$> jsvaluetonumber c t nullPtr

getLocationHost :: WebView -> IO Text
getLocationHost wv = withWebViewContext wv $ \c -> do
  script <- jsstringcreatewithutf8cstring "location.host"
  lh <- jsevaluatescript c script nullPtr nullPtr 1 nullPtr
  lh' <- fromJSStringMaybe c lh
  return $ fromMaybe "" lh'

getLocationProtocol :: WebView -> IO Text
getLocationProtocol wv = withWebViewContext wv $ \c -> do
  script <- jsstringcreatewithutf8cstring "location.protocol"
  lp <- jsevaluatescript c script nullPtr nullPtr 1 nullPtr
  lp' <- fromJSStringMaybe c lp
  return $ fromMaybe "" lp'

bsToArrayBuffer :: JSContextRef -> ByteString -> IO JSValueRef
bsToArrayBuffer c bs = do
  elems <- forM (BS.unpack bs) $ \x -> jsvaluemakenumber c $ fromIntegral x
  let numElems = length elems
  bracket (mallocArray numElems) free $ \elemsArr -> do
    pokeArray elemsArr elems
    a <- jsobjectmakearray c (fromIntegral numElems) elemsArr nullPtr
    newUint8Array <- jsstringcreatewithutf8cstring "new Uint8Array(this)"
    jsevaluatescript c newUint8Array a nullPtr 1 nullPtr

bsFromArrayBuffer :: JSContextRef -> JSValueRef -> IO ByteString
bsFromArrayBuffer c a = do
  let getIntegral = fmap round . (\x -> jsvaluetonumber c x nullPtr)
  getByteLength <- jsstringcreatewithutf8cstring "this.byteLength"
  byteLength <- getIntegral =<< jsevaluatescript c getByteLength a nullPtr 1 nullPtr
  toUint8Array <- jsstringcreatewithutf8cstring "new Uint8Array(this)"
  uint8Array <- jsevaluatescript c toUint8Array a nullPtr 1 nullPtr
  getIx <- jsstringcreatewithutf8cstring "this[0][this[1]]"
  let arrayLookup i = do
        i' <- jsvaluemakenumber c (fromIntegral i)
        args <- toJSObject c [uint8Array, i']
        getIntegral =<< jsevaluatescript c getIx args nullPtr 1 nullPtr
  BS.pack <$> forM [0..byteLength-1] arrayLookup

withWebViewContext :: WebView -> (JSContextRef -> IO a) -> IO a
withWebViewContext wv f = f =<< webFrameGetGlobalContext =<< webViewGetMainFrame wv
