{-# LANGUAGE ForeignFunctionInterface #-}
module Reflex.Dom.Internal.Foreign where

import GHCJS.DOM hiding (runWebGUI)
import Control.Concurrent
import Control.Monad.State.Strict hiding (mapM, mapM_, forM, forM_, sequence, sequence_, get)
import GHCJS.DOM.Navigator
import GHCJS.DOM.DOMWindow
import Graphics.UI.Gtk hiding (Widget)
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.Types hiding (Event, Widget, unWidget)
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebInspector
import Data.List
import Data.String
import System.Directory

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
  _ <- inspector `on` inspectWebView $ \wv -> do
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
  mainGUI

runWebGUI :: (WebView -> IO ()) -> IO ()
runWebGUI = runWebGUI' "GHCJS"

runWebGUI' :: String -> (WebView -> IO ()) -> IO ()
runWebGUI' userAgentKey main = do
  -- Are we in a java script inside some kind of browser
  mbWindow <- currentWindow
  case mbWindow of
    Just window -> do
      -- Check if we are running in javascript inside the the native version
      Just n <- domWindowGetNavigator window
      agent <- navigatorGetUserAgent n
      unless ((" " ++ userAgentKey) `isSuffixOf` agent) $ main (castToWebView window)
    Nothing -> do
      makeDefaultWebView userAgentKey main
