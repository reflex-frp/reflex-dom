{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
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
import GHCJS.DOM.Types (runJSM, askJSM, JSContextRef(..))
import GHCJS.DOM.Navigator
import GHCJS.DOM.Window
import System.Directory
import qualified GI.Gtk.Functions as Gtk (init, main)
import GI.WebKit2
       (webViewLoadUri, webViewLoadHtml, webViewGetInspector,
        onWebViewLoadChanged, webViewSetSettings,
        settingsSetEnableDeveloperExtras, settingsSetUserAgent,
        settingsGetUserAgent, webViewGetSettings, webViewNew, WebView)

#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif
import GI.GLib.Functions (timeoutAdd, idleAdd)
import GI.GLib.Constants (pattern PRIORITY_DEFAULT, pattern PRIORITY_HIGH)
import GI.Gtk.Objects.Widget
       (widgetShowAll, onWidgetDestroy, widgetDestroy, widgetGetToplevel)
import GI.Gtk.Objects.Window
       (windowSetPosition, windowSetDefaultSize, windowNew)
import GI.Gtk.Enums (WindowType(..), WindowPosition(..))
import GI.Gtk.Objects.ScrolledWindow (scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Functions (mainQuit)
import Control.Monad.Trans.Reader (ask)

quitWebView :: WebView -> IO ()
quitWebView wv = void . idleAdd PRIORITY_DEFAULT $ do  --TODO: Shouldn't this be Sync?
    w <- widgetGetToplevel wv
    widgetDestroy w
    return False

installQuitHandler :: WebView -> IO ()
#ifdef mingw32_HOST_OS
installQuitHandler wv = return () -- TODO: Maybe figure something out here for Windows users.
#else
installQuitHandler wv = void $ installHandler keyboardSignal (Catch (quitWebView wv)) Nothing
#endif

runWebGUI :: ((WebView, JSContextRef) -> IO ()) -> IO ()
runWebGUI main = do
  _ <- Gtk.init Nothing
  window <- windowNew WindowTypeToplevel
  _ <- timeoutAdd PRIORITY_HIGH 10 (yield >> return True)
  windowSetDefaultSize window 900 600
  windowSetPosition window WindowPositionCenter
  scrollWin <- scrolledWindowNew noAdjustment noAdjustment
  webView <- webViewNew
  putStrLn "forking server!"
  _ <- forkIO $ run 3706 $ do
    ctx <- askJSM
    liftIO $ forkIO $ forever $ do
        threadDelay 1000000
        runJSM syncPoint ctx
--    liftIO $ threadDelay 20000000
--    idleAdd PRIORITY_DEFAULT $ do
    liftIO $ main (webView, ctx)
    liftIO $ putStrLn "main exited"
   --     return False
    forever $ do
        liftIO $ threadDelay 100000
  settings <- webViewGetSettings webView
  -- TODO settingsSetEnableUniversalAccessFromFileUris settings True
  settingsSetEnableDeveloperExtras settings True
  webViewSetSettings webView settings
  window `containerAdd` scrollWin
  scrollWin `containerAdd` webView
  _ <- onWidgetDestroy window mainQuit
  widgetShowAll window
--  _ <- onWebViewLoadChanged webView $ \_ ->
--    -- TODO run jsaddle.js when load is finished
--    return ()
  inspector <- webViewGetInspector webView
--  _ <- inspector `on` inspectWebView $ \_ -> do
--    inspectorWindow <- windowNew
--    windowSetDefaultSize inspectorWindow 900 600
--    inspectorScrollWin <- scrolledWindowNew noAdjustment noAdjustment
--    inspectorWebView <- webViewNew
--    inspectorWindow `containerAdd` inspectorScrollWin
--    inspectorScrollWin `containerAdd` inspectorWebView
--    widgetShowAll inspectorWindow
--    return inspectorWebView
  pwd <- getCurrentDirectory
--  webViewLoadHtml webView "" . Just $ "file://" <> T.pack pwd <> "/"
  webViewLoadUri webView "http://localhost:3706/"
  installQuitHandler webView
  Gtk.main

--foreign import ccall "wrapper"
--  wrapper :: JSObjectCallAsFunctionCallback' -> IO JSObjectCallAsFunctionCallback
--
--toJSObject :: JSContextRef -> [Ptr OpaqueJSValue] -> IO JSObjectRef
--toJSObject ctx args = do
--  o <- jsobjectmake ctx nullPtr nullPtr
--  iforM_ args $ \n a -> do
--    prop <- jsstringcreatewithutf8cstring $ show n
--    jsobjectsetproperty ctx o prop a 1 nullPtr
--  return o
--
--fromJSStringMaybe :: JSContextRef -> JSValueRef -> IO (Maybe Text)
--fromJSStringMaybe c t = do
--  isNull <- jsvalueisnull c t
--  if isNull then return Nothing else do
--    j <- jsvaluetostringcopy c t nullPtr
--    l <- jsstringgetmaximumutf8cstringsize j
--    s <- allocaBytes (fromIntegral l) $ \ps -> do
--           _ <- jsstringgetutf8cstring'_ j ps (fromIntegral l)
--           peekCString ps
--    return $ Just $ T.pack s
--
--getLocationHost :: WebView -> IO Text
--getLocationHost wv = withWebViewContext wv $ \c -> do
--  script <- jsstringcreatewithutf8cstring "location.host"
--  lh <- jsevaluatescript c script nullPtr nullPtr 1 nullPtr
--  lh' <- fromJSStringMaybe c lh
--  return $ fromMaybe "" lh'
--
--getLocationProtocol :: WebView -> IO Text
--getLocationProtocol wv = withWebViewContext wv $ \c -> do
--  script <- jsstringcreatewithutf8cstring "location.protocol"
--  lp <- jsevaluatescript c script nullPtr nullPtr 1 nullPtr
--  lp' <- fromJSStringMaybe c lp
--  return $ fromMaybe "" lp'
--
--bsToArrayBuffer :: JSContextRef -> ByteString -> IO JSValueRef
--bsToArrayBuffer c bs = do
--  elems <- forM (BS.unpack bs) $ \x -> jsvaluemakenumber c $ fromIntegral x
--  let numElems = length elems
--  bracket (mallocArray numElems) free $ \elemsArr -> do
--    pokeArray elemsArr elems
--    a <- jsobjectmakearray c (fromIntegral numElems) elemsArr nullPtr
--    newUint8Array <- jsstringcreatewithutf8cstring "new Uint8Array(this)"
--    jsevaluatescript c newUint8Array a nullPtr 1 nullPtr
--
--bsFromArrayBuffer :: JSContextRef -> JSValueRef -> IO ByteString
--bsFromArrayBuffer c a = do
--  let getIntegral = fmap round . (\x -> jsvaluetonumber c x nullPtr)
--  getByteLength <- jsstringcreatewithutf8cstring "this.byteLength"
--  byteLength <- getIntegral =<< jsevaluatescript c getByteLength a nullPtr 1 nullPtr
--  toUint8Array <- jsstringcreatewithutf8cstring "new Uint8Array(this)"
--  uint8Array <- jsevaluatescript c toUint8Array a nullPtr 1 nullPtr
--  getIx <- jsstringcreatewithutf8cstring "this[0][this[1]]"
--  let arrayLookup i = do
--        i' <- jsvaluemakenumber c (fromIntegral i)
--        args <- toJSObject c [uint8Array, i']
--        getIntegral =<< jsevaluatescript c getIx args nullPtr 1 nullPtr
--  BS.pack <$> forM [0..byteLength-1] arrayLookup
--
--withWebViewContext :: WebView -> (JSContextRef -> IO a) -> IO a
--withWebViewContext wv f = f =<< webFrameGetGlobalContext =<< webViewGetMainFrame wv
