{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Dom.Internal
  ( module Main
  , run
  , mainWidget
  , mainWidgetWithHead, mainWidgetWithCss, mainWidgetWithHead', mainWidgetInElementById, runApp'
  , mainHydrationWidgetWithHead, mainHydrationWidgetWithHead'
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Reflex.Dom.Core (Widget)
import Reflex.Dom.Main as Main hiding
       (mainWidget, mainWidgetWithHead, mainWidgetWithCss,
        mainWidgetWithHead', mainWidgetInElementById, runApp',
        mainHydrationWidgetWithHead, mainHydrationWidgetWithHead')
import qualified Reflex.Dom.Main as Main
       (mainWidget, mainWidgetWithHead, mainWidgetWithCss,
        mainWidgetWithHead', mainWidgetInElementById, runApp',
        mainHydrationWidgetWithHead, mainHydrationWidgetWithHead')

#if defined(ghcjs_HOST_OS)
run :: a -> a
run = id
#elif defined(MIN_VERSION_jsaddle_warp)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Language.Javascript.JSaddle (JSM)
import qualified Language.Javascript.JSaddle.Warp as JW
import System.Environment (lookupEnv)

run :: JSM () -> IO()
run jsm = do
  port <- maybe 3003 read <$> lookupEnv "JSADDLE_WARP_PORT"
  putStrLn $ "Running jsaddle-warp server on port " <> show port
  JW.run port jsm
#elif defined(MIN_VERSION_jsaddle_wkwebview)
#if defined(ios_HOST_OS)
import Data.Default
import Data.Monoid ((<>))
import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.WKWebView (run', mainBundleResourcePath)
import Language.Javascript.JSaddle.WKWebView.Internal (jsaddleMainHTMLWithBaseURL)

-- TODO: upstream to jsaddle-wkwebview
run :: JSM () -> IO ()
run jsm = do
  let indexHtml = "<!DOCTYPE html><html><head></head><body></body></html>"
  baseUrl <- mainBundleResourcePath >>= \case
    Nothing -> do
      putStrLn "Reflex.Dom.run: unable to find main bundle resource path. Assets may not load properly."
      return ""
    Just p -> return $ "file://" <> p <> "/index.html"
  run' def $ jsaddleMainHTMLWithBaseURL indexHtml baseUrl jsm
#else
import Language.Javascript.JSaddle.WKWebView (run)
#endif
#elif defined(ANDROID)
import Android.HaskellActivity
import Control.Monad
import Control.Concurrent
import Data.Default
import Data.String
import Reflex.Dom.Android.MainWidget
import System.IO
import Language.Javascript.JSaddle (JSM)

run :: JSM () -> IO ()
run jsm = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  continueWithCallbacks $ def
    { _activityCallbacks_onCreate = \_ -> do
        a <- getHaskellActivity
        let startPage = fromString "file:///android_asset/index.html"
        startMainWidget a startPage jsm
    }
  forever $ threadDelay 1000000000
#elif defined(wasm32_HOST_ARCH)
import qualified Language.Javascript.JSaddle.Wasm as Wasm (run)
import Language.Javascript.JSaddle (JSM)
run :: JSM () -> IO ()
run = Wasm.run 0
#else
import Language.Javascript.JSaddle.WebKitGTK (run)
#endif

mainWidget :: (forall x. Widget x ()) -> IO ()
mainWidget w = run $ Main.mainWidget w
{-# INLINE mainWidget #-}

mainWidgetWithHead :: (forall x. Widget x ()) -> (forall x. Widget x ()) -> IO ()
mainWidgetWithHead h b = run $ Main.mainWidgetWithHead h b
{-# INLINE mainWidgetWithHead #-}

mainWidgetWithCss :: ByteString -> (forall x. Widget x ()) -> IO ()
mainWidgetWithCss css w = run $ Main.mainWidgetWithCss css w
{-# INLINE mainWidgetWithCss #-}

mainWidgetWithHead' :: (a -> Widget () b, b -> Widget () a) -> IO ()
mainWidgetWithHead' w = run $ Main.mainWidgetWithHead' w
{-# INLINE mainWidgetWithHead' #-}

mainWidgetInElementById :: Text -> (forall x. Widget x ()) -> IO ()
mainWidgetInElementById eid w = run $ Main.mainWidgetInElementById eid w
{-# INLINE mainWidgetInElementById #-}

runApp' :: (forall x. AppInput DomTimeline -> Widget x (AppOutput DomTimeline)) -> IO ()
runApp' app = run $ Main.runApp' app
{-# INLINE runApp' #-}

mainHydrationWidgetWithHead :: (forall x. HydrationWidget x ()) -> (forall x. HydrationWidget x ()) -> IO ()
mainHydrationWidgetWithHead h b = run $ Main.mainHydrationWidgetWithHead h b
{-# INLINE mainHydrationWidgetWithHead #-}

mainHydrationWidgetWithHead' :: HydrationWidget () () -> HydrationWidget () () -> IO ()
mainHydrationWidgetWithHead' h b = run $ Main.mainHydrationWidgetWithHead' h b
{-# INLINE mainHydrationWidgetWithHead' #-}
