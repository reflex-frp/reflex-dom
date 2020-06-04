{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utilities for testing reflex-dom widgets with Selenium.
--
-- Any uses of the 'testWidget' functions will start a jsaddle warp server and
-- serve the given widget in an otherwise empty page. Webdriver commands can run
-- on the statically rendered page, and on the hydrated page.
module Reflex.Dom.Test.Selenium
  ( TestWidget
  , TestWidgetConfig (..)
  , testWidget, testWidget', testWidgetStatic, testWidgetHydrated
  , withSeleniumSpec
  , embedSpec
  , SeleniumSetupConfig (..)
  ) where

import Control.Concurrent (threadDelay, newEmptyMVar, takeMVar, putMVar)
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask, handle)
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
import Test.Hspec.WebDriver (Spec, SpecWith, WdTestSession, sessionWith, using)
import Test.Util.ChromeFlags
import Test.Util.UnshareNetwork
import Test.WebDriver (WD)

import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.Hspec.Core.Spec as Hspec
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Capabilities as WD

deriving instance MonadMask WD

data SeleniumSetupConfig = SeleniumSetupConfig
  { _seleniumSetupConfig_chromiumPath :: FilePath
  -- ^ Path to the chromium executable
  , _seleniumSetupConfig_headless :: Bool
  -- ^ True means that the browser is invoked in headless mode, False means that
  -- the user can see the interaction happening on the browser (useful for debugging).
  , _seleniumSetupConfig_seleniumPort :: PortNumber
  -- ^ The port number used by selenium
  }

-- | Setup a selenium server and use it to run some hspec tests.
--
-- withSeleniumSpec config $ \runSession -> hspec $ do
--  describe "tests using webdriver session" $ runSession $ do
--    webdriver tests here
withSeleniumSpec :: SeleniumSetupConfig -> ((forall multi. SpecWith (WdTestSession multi) -> Spec) -> IO a) -> IO a
withSeleniumSpec config runSpec = do
  liftIO $ handle (\(_ :: IOError) -> return ()) $ unshareNetork -- If we run into an exception with sandboxing, just don't bother
  withSandboxedChromeFlags (_seleniumSetupConfig_headless config) $ \chromeFlags -> do
    withSeleniumServer (_seleniumSetupConfig_seleniumPort config) $ do
      let browserPath = T.strip $ T.pack $ _seleniumSetupConfig_chromiumPath config
      when (T.null browserPath) $ fail "No browser found"
      let wdConfig = WD.defaultConfig { WD.wdPort = fromIntegral $ _seleniumSetupConfig_seleniumPort config }
          chromeCaps' = WD.getCaps $ chromeConfig browserPath chromeFlags
      runSpec $ sessionWith wdConfig "" . using [(chromeCaps', "chrome")]

startSeleniumServer :: PortNumber -> IO (IO ())
startSeleniumServer port = do
  (_,_,_,ph) <- createProcess $ (proc "selenium-server" ["-port", show port])
    { std_in = NoStream
    , std_out = NoStream
    , std_err = NoStream
    }
  return $ terminateProcess ph

withSeleniumServer :: PortNumber -> IO a -> IO a
withSeleniumServer port f = bracket (startSeleniumServer port) id $ \_ -> do
  threadDelay $ 1000 * 1000 * 2 -- TODO poll or wait on a a signal to block on
  f

chromeConfig :: Text -> [Text] -> WD.WDConfig
chromeConfig fp flags =
  WD.useBrowser
    (WD.chrome { WD.chromeBinary = Just $ T.unpack fp, WD.chromeOptions = T.unpack <$> flags })
    WD.defaultConfig

--------------------------------------------------------------------------------
-- Function to test widgets
--------------------------------------------------------------------------------

-- | The environment in which your widgets can be tested
type TestWidget js t m =
  (DomBuilder t m, MonadHold t m, PostBuild t m
  , Prerender js t m, PerformEvent t m, TriggerEvent t m
  , MonadFix m, MonadIO (Performable m), MonadIO m)

-- | Configuration of individual tests
data TestWidgetConfig = TestWidgetConfig
  { _testWidgetConfig_debug :: Bool
  -- ^ If this flag is set to True, during the test we will emit debug messages
  , _testWidgetConfig_headWidget :: (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ We can add widgets here that will be included in the head of the page
  --   (useful for example to include external js libraries in the tests)
  , _testWidgetConfig_jsaddlePort :: PortNumber
  -- ^ Port used by the jsaddle server
  }

-- | Test a widget by running some webdriver commands before and after
-- hydration.
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

-- | Test a widget by running some webdriver commands before hydration (on the
-- statically rendered page).
testWidgetStatic
  :: TestWidgetConfig
  -> WD a
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> WD a
testWidgetStatic cfg before widget = testWidget' cfg before pure widget

-- | Test a widget by running some webdriver commands after hydration.
testWidgetHydrated
  :: TestWidgetConfig
  -> WD b
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> WD b
testWidgetHydrated cfg after widget = testWidget' cfg (pure ()) (const after) widget

-- | Test a widget by running some webdriver commands before and after
-- hydration. Like 'testWidget', but you can use the result of the webdriver
-- commands from before hydration in the post hydration test.
testWidget'
  :: TestWidgetConfig
  -> WD a
  -- ^ Webdriver commands to run before the JS runs (i.e. on the statically rendered page)
  -> (a -> WD b)
  -- ^ Webdriver commands to run after hydration switchover
  -> (forall m js. TestWidget js (SpiderTimeline Global) m => m ())
  -- ^ Widget we are testing (contents of body)
  -> WD b
testWidget' (TestWidgetConfig withDebugging headWidget jsaddlePort) beforeJS afterSwitchover bodyWidget = do
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
      silenceIfNotDebugging = if withDebugging then id else hSilence [stderr]
      jsaddleWarp = silenceIfNotDebugging $ Warp.runSettings settings application
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

-- | Embed a different spec type by evaluating it directly.
embedSpec :: (Hspec.Example a, MonadIO m) => Hspec.Arg a -> a -> m ()
embedSpec arg spec = liftIO $ do
  Hspec.Result _info status <- Hspec.evaluateExample
    spec
    Hspec.defaultParams
    (\actionWith -> actionWith arg)
    (\_ -> pure ())
  case status of
    Hspec.Success -> pure ()
    Hspec.Pending _ _ -> pure () -- should not happen
    failure -> throwIO failure

withAsync' :: (MonadIO m, MonadMask m) => IO a -> m b -> m b
withAsync' f g = bracket
  (liftIO $ Async.async f)
  (liftIO . Async.uninterruptibleCancel)
  (const g)
