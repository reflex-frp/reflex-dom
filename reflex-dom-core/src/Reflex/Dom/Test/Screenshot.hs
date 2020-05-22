{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}


module Reflex.Dom.Test.Screenshot
  (testWithSelenium, testWidgetDebug'WithHead)

  where


import Control.Concurrent
import Control.Exception ()
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core
-- import System.Exit
import System.Process
import Control.Monad.Catch

import Test.Util.ChromeFlags
import Test.Util.UnshareNetwork

import Test.WebDriver
-- import Test.WebDriver.Commands
-- import Control.Concurrent.Async
import qualified Control.Concurrent.Async as Async
import Network.Socket
import System.Environment
import System.Which (staticWhich)
import Data.Maybe
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets

import Network.Wai
import Network.HTTP.Types (status200)
import System.IO.Silently
import System.IO

import Test.Hspec.WebDriver hiding (runWD, click, uploadFile, WD)
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Capabilities as WD

deriving instance MonadMask WD

chromium :: FilePath
chromium = $(staticWhich "chromium")

myWDConfig :: WDConfig
myWDConfig = useBrowser chrome $ defaultConfig { wdPort = 4444 }

data SessionData = SessionData
  { sessionData_wdConfig :: WD.WDConfig
  , sessionData_capabilities :: [Capabilities]
  , sessionData_selenium :: Selenium
  }

{- Note: a nicer interface?

This would have lead to a niced interface, but I don't think this solution is
expressible with the library

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

testWithSelenium :: SpecWith (WdTestSession a) -> IO ()
testWithSelenium actualTests = do
  handle (\(_ :: IOError) -> return ()) $ unshareNetork -- If we run into an exception with sandboxing, just don't bother
  isHeadless <- pure True -- (== Nothing) <$> lookupEnv "NO_HEADLESS"
  withSandboxedChromeFlags isHeadless $ \chromeFlags -> do
    withSeleniumServer $ \selenium -> do
      let browserPath = T.strip $ T.pack chromium
      when (T.null browserPath) $ fail "No browser found"
      withDebugging <- isNothing <$> lookupEnv "NO_DEBUG"
      let wdConfig = WD.defaultConfig { WD.wdPort = fromIntegral $ _selenium_portNumber selenium }
          chromeCaps' = WD.getCaps $ chromeConfig browserPath chromeFlags
      hspec (sessionWith wdConfig "" . using [(chromeCaps', "")] $ actualTests) `finally` _selenium_stopServer selenium

-- testWidgetStatic :: WD b -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ()) -> WD b
-- testWidgetStatic = testWidgetStaticDebug withDebugging
-- testWidget :: WD () -> WD b -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ()) -> WD b
-- testWidget = testWidgetDebug withDebugging
-- testWidget' :: WD a -> (a -> WD b) -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ()) -> WD b
-- testWidget' = testWidgetDebug' withDebugging

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
chromeConfig fp flags = WD.useBrowser (WD.chrome { WD.chromeBinary = Just $ T.unpack fp, WD.chromeOptions = T.unpack <$> flags }) WD.defaultConfig

type TestWidget js t m = (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js t m, PerformEvent t m, TriggerEvent t m, MonadFix m, MonadIO (Performable m), MonadIO m)

-- | TODO: do something about JSExceptions not causing tests to fail
testWidgetDebug
  :: Bool
  -> WD ()
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> WD b
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing
  -> WD b
testWidgetDebug withDebugging beforeJS afterSwitchover =
  testWidgetDebug' withDebugging beforeJS (const afterSwitchover)

testWidgetDebug'WithHead
  :: Bool
  -> WD a
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> (a -> WD b)
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget that we want to put in the head additionally
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> WD b
testWidgetDebug'WithHead withDebugging beforeJS afterSwitchover headWidget bodyWidget = do
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
  application <- liftIO $ jsaddleOr defaultConnectionOptions entryPoint $ \req sendResponse -> do
    putStrLnDebug "sending response"
    putStrLnDebug $ "The request was: " <> T.pack (show req)
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

-- | TODO: do something about JSExceptions not causing tests to fail
testWidgetDebug'
  :: Bool
  -> WD a
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> (a -> WD b)
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> WD b
testWidgetDebug' withDebugging beforeJS afterSwitchover bodyWidget = do
  let putStrLnDebug :: MonadIO m => Text -> m ()
      putStrLnDebug m = when withDebugging $ liftIO $ putStrLn $ T.unpack m
      staticApp = do
        el "head" $ pure ()
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

seleniumPort, jsaddlePort :: PortNumber
seleniumPort = 8000
jsaddlePort = 8001

withAsync' :: (MonadIO m, MonadMask m) => IO a -> m b -> m b
withAsync' f g = bracket
  (liftIO $ Async.async f)
  (liftIO . Async.uninterruptibleCancel)
  (const g)

testWidgetStaticDebug
  :: Bool
  -> WD b
  -- ^ Webdriver commands to run before JS runs and after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing
  -> WD b
testWidgetStaticDebug withDebugging w = testWidgetDebug withDebugging (void w) w

--------------------------------------------------------------------------------
-- Tasty
--------------------------------------------------------------------------------
