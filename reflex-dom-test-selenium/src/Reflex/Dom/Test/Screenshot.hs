{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reflex.Dom.Test.Screenshot
  ( TestWidget
  , TestWidgetConfig (..)
  , testWidget, testWidget', testWidgetStatic, testWidgetHydrated
  , testWithSelenium
  ) where

import Control.Concurrent (threadDelay, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (when, void)
import Control.Monad.Catch (MonadMask, handle, finally)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
import Network.HTTP.Types (status200)
import Network.Socket (PortNumber)
import Network.Wai (responseLBS)
import Network.WebSockets (defaultConnectionOptions)
import Reflex.Dom.Core
import System.IO (stderr)
import System.IO.Silently (hSilence)
import System.Process (std_in, std_out, std_err, createProcess, proc, StdStream (..), terminateProcess)
import qualified Data.Text as T

import qualified Control.Concurrent.Async as Async
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai.Handler.Warp as Warp

import Test.Hspec.WebDriver (SpecWith, WdTestSession, hspec, sessionWith, using)
import Test.Util.ChromeFlags
import Test.Util.UnshareNetwork
import Test.WebDriver (WD)
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Capabilities as WD

deriving instance MonadMask WD

testWithSelenium
  :: FilePath
  -- ^ Path to the chromium executable
  -> Bool
  -- ^ True means that the browser is invoked in headless mode, False means that
  -- the user can see the interaction happening on the browser (useful for debugging).
  -> SpecWith (WdTestSession a)
  -- ^ The tests we want to write in the selenium session
  -> IO ()
testWithSelenium chromium isHeadless actualTests = do
  handle (\(_ :: IOError) -> return ()) $ unshareNetork -- If we run into an exception with sandboxing, just don't bother
  withSandboxedChromeFlags isHeadless $ \chromeFlags -> do
    withSeleniumServer $ \selenium -> do
      let browserPath = T.strip $ T.pack chromium
      when (T.null browserPath) $ fail "No browser found"
      let wdConfig = WD.defaultConfig { WD.wdPort = fromIntegral $ _selenium_portNumber selenium }
          chromeCaps' = WD.getCaps $ chromeConfig browserPath chromeFlags
      hspec (sessionWith wdConfig "" . using [(chromeCaps', "")] $ actualTests) `finally` _selenium_stopServer selenium

seleniumPort, jsaddlePort :: PortNumber
seleniumPort = 8000
jsaddlePort = 8001

data Selenium = Selenium
  { _selenium_portNumber :: PortNumber
  , _selenium_stopServer :: IO ()
  }

startSeleniumServer :: PortNumber -> IO (IO ())
startSeleniumServer port = do
  (_,_,_,ph) <- createProcess $ (proc "selenium-server" ["-port", show port])
    { std_in = NoStream
    , std_out = NoStream
    , std_err = NoStream
    }
  return $ terminateProcess ph

withSeleniumServer :: (Selenium -> IO ()) -> IO ()
withSeleniumServer f = do
  stopServer <- startSeleniumServer seleniumPort
  threadDelay $ 1000 * 1000 * 2 -- TODO poll or wait on a a signal to block on
  f $ Selenium
    { _selenium_portNumber = seleniumPort
    , _selenium_stopServer = stopServer
    }

chromeConfig :: Text -> [Text] -> WD.WDConfig
chromeConfig fp flags =
  WD.useBrowser
    (WD.chrome { WD.chromeBinary = Just $ T.unpack fp, WD.chromeOptions = T.unpack <$> flags })
    WD.defaultConfig

--------------------------------------------------------------------------------
-- Function to test widgets
--------------------------------------------------------------------------------

type TestWidget js t m =
  (DomBuilder t m, MonadHold t m, PostBuild t m
  , Prerender js t m, PerformEvent t m, TriggerEvent t m
  , MonadFix m, MonadIO (Performable m), MonadIO m)

data TestWidgetConfig = TestWidgetConfig
  { testWidgetConfig_debug :: Bool
  -- ^ If this flag is set to True, during the test we will emit debug messages
  --   giving insight on our progress in the hydratation progress
  , testWidgetConfig_headWidget :: (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ We can add widgets here that will be included in the head of the page
  --   (useful for example to include external js libraries in the tests)
  }

testWidget
  :: TestWidgetConfig
  -> WD a
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> WD b
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> WD b
testWidget cfg before after widget = testWidget' cfg before (const after) widget

testWidgetStatic
  :: TestWidgetConfig
  -> WD ()
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> WD ()
testWidgetStatic cfg before widget = testWidget' cfg (void before) (const $ pure ()) widget

testWidgetHydrated
  :: TestWidgetConfig
  -> WD b
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> WD b
testWidgetHydrated cfg after widget = testWidget' cfg (pure ()) (const after) widget

testWidget'
  :: TestWidgetConfig
  -> WD a
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> (a -> WD b)
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> WD b
testWidget' (TestWidgetConfig withDebugging headWidget) beforeJS afterSwitchover bodyWidget = do
  let putStrLnDebug :: MonadIO m => Text -> m ()
      putStrLnDebug m = when withDebugging $ liftIO $ putStrLn $ T.unpack m
      staticApp = do
        el "head" $ headWidget
        el "body" $ do
          bodyWidget
          el "script" $ text $ TE.decodeUtf8 $ LBS.toStrict $ jsaddleJs False
  putStrLnDebug "rendering static"
  ((), html) <- liftIO $ renderStatic $ runHydratableT staticApp
  putStrLnDebug "rendered static"
  waitBeforeJS <- liftIO newEmptyMVar -- Empty until JS should be run
  waitUntilSwitchover <- liftIO newEmptyMVar -- Empty until switchover
  let entryPoint = do
        putStrLnDebug "taking waitBeforeJS"
        liftIO $ takeMVar waitBeforeJS
        let switchOverAction = do
              putStrLnDebug "switchover syncPoint"
              syncPoint
              putStrLnDebug "putting waitUntilSwitchover"
              liftIO $ putMVar waitUntilSwitchover ()
              putStrLnDebug "put waitUntilSwitchover"
        putStrLnDebug "running mainHydrationWidgetWithSwitchoverAction"
        mainHydrationWidgetWithSwitchoverAction switchOverAction blank bodyWidget
        putStrLnDebug "syncPoint after mainHydrationWidgetWithSwitchoverAction"
        syncPoint
  application <- liftIO $ jsaddleOr defaultConnectionOptions entryPoint $ \_ sendResponse -> do
    putStrLnDebug "sending response"
    r <- sendResponse $ responseLBS status200 [] $ "<!doctype html>\n" <> LBS.fromStrict html
    putStrLnDebug "sent response"
    return r
  waitJSaddle <- liftIO newEmptyMVar
  let settings = foldr ($) Warp.defaultSettings
        [ Warp.setPort $ fromIntegral $ toInteger jsaddlePort
        , Warp.setBeforeMainLoop $ do
            putStrLnDebug "putting waitJSaddle"
            putMVar waitJSaddle ()
            putStrLnDebug "put waitJSaddle"
        ]
      -- hSilence to get rid of ConnectionClosed logs
      silenceIfDebug = if withDebugging then id else hSilence [stderr]
      jsaddleWarp = silenceIfDebug $ Warp.runSettings settings application
  withAsync' jsaddleWarp $ do
    putStrLnDebug "taking waitJSaddle"
    liftIO $ takeMVar waitJSaddle
    putStrLnDebug "opening page"
    WD.openPage $ "http://localhost:" <> show jsaddlePort
    putStrLnDebug "running beforeJS"
    a <- beforeJS
    putStrLnDebug "putting waitBeforeJS"
    liftIO $ putMVar waitBeforeJS ()
    putStrLnDebug "taking waitUntilSwitchover"
    liftIO $ takeMVar waitUntilSwitchover
    putStrLnDebug "running afterSwitchover"
    afterSwitchover a

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

withAsync' :: (MonadIO m, MonadMask m) => IO a -> m b -> m b
withAsync' f g = bracket
  (liftIO $ Async.async f)
  (liftIO . Async.uninterruptibleCancel)
  (const g)

--------------------------------------------------------------------------------

{- Note: a nicer interface?

This would have lead to a niced interface, but I don't think this solution is
expressible with the library

data SessionData = SessionData
  { sessionData_wdConfig :: WD.WDConfig
  , sessionData_capabilities :: [Capabilities]
  , sessionData_selenium :: Selenium
  }

testSpec = around bracketSelenium $ do
  describe "createRecipe" $ _what

bracketSelenium :: (SessionData -> IO ()) -> IO ()
bracketSelenium action = do
  handle (\(_ :: IOError) -> return ()) $ unshareNetork -- If we run into an exception with sandboxing, just don't bother
  isHeadless <- pure True -- (== Nothing) <$> lookupEnv "NO_HEADLESS"
  withSandboxedChromeFlags isHeadless $ \chromeFlags -> do
    withSeleniumServer $ \selenium -> do
      let browserPath = T.strip $ T.pack chromium
      when (T.null browserPath) $ fail "No browser found"
      withDebugging <- isNothing <$> lookupEnv "NO_DEBUG"
      let
        wdConfig = WD.defaultConfig { WD.wdPort = fromIntegral $ _selenium_portNumber selenium }
        chromeCaps' = WD.getCaps $ chromeConfig browserPath chromeFlags
      action (SessionData wdConfig [chromeCaps'] selenium) `finally` _selenium_stopServer selenium
-}
