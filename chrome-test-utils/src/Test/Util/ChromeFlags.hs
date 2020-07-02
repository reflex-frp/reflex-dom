{-# LANGUAGE OverloadedStrings #-}
module Test.Util.ChromeFlags where

import Data.Semigroup
import Data.Text
import System.IO.Temp

withSandboxedChromeFlags :: Bool -> ([Text] -> IO a) -> IO a
withSandboxedChromeFlags headless action =
  withSystemTempDirectory "reflex-dom-core_test" $ \tmp ->
    action
      [ if headless then "--headless" else "--auto-open-devtools-for-tabs"
      , "--disable-gpu"
      , "--no-sandbox"
      , "--remote-debugging-port=9222"
      , "--user-data-dir=" <> pack tmp
      ]
