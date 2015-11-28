{-# LANGUAGE CPP, ForeignFunctionInterface, ScopedTypeVariables, LambdaCase #-}
module Reflex.Dom.Internal.Foreign where

import Control.Lens hiding (set)
import GHCJS.DOM hiding (runWebGUI)
import Control.Concurrent
import Control.Monad.State.Strict hiding (mapM, mapM_, forM, forM_, sequence, sequence_, get)
import Foreign.Ptr
import GHCJS.DOM.Navigator
import GHCJS.DOM.DOMWindow
import Graphics.UI.Gtk hiding (Widget)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.Types hiding (Event, Widget, unWidget)
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebInspector
import Data.List
import System.Directory
import System.Glib.FFI hiding (void)

#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

quitWebView :: WebView -> IO ()
quitWebView wv = postGUIAsync $ do w <- widgetGetToplevel wv
                                   widgetDestroy w

installQuitHandler :: WebView -> IO ()
#ifdef mingw32_HOST_OS
installQuitHandler wv = return () -- TODO: Maybe figure something out here for Windows users.
#else
installQuitHandler wv = installHandler keyboardSignal (Catch (quitWebView wv)) Nothing >> return ()
#endif

makeDefaultWebView :: String -> (WebView -> IO ()) -> IO ()
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
  settings `set` [ webSettingsUserAgent := userAgent ++ " " ++ userAgentKey
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
  webFrameLoadString wf "" Nothing $ "file://" ++ pwd ++ "/"
  installQuitHandler webView
  mainGUI

runWebGUI :: (WebView -> IO ()) -> IO ()
runWebGUI = runWebGUI' "GHCJS"

runWebGUI' :: String -> (WebView -> IO ()) -> IO ()
runWebGUI' userAgentKey main =
  do mbWindow <- currentWindow -- Are we in a javascript inside some kind of browser?
     case mbWindow of
       Just window -> do
         -- Check if we are running in javascript inside the the native version
         Just n <- domWindowGetNavigator window
         agent <- navigatorGetUserAgent n
         unless ((" " ++ userAgentKey) `isSuffixOf` agent) $
           main (castToWebView window)
       Nothing -> do -- this is the branch initially taken in a compiled program or in ghci
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

fromJSStringMaybe :: JSContextRef -> JSValueRef -> IO (Maybe String)
fromJSStringMaybe c t = do
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

