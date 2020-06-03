{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import Data.Text as T
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core
import Reflex.Time
import System.Exit
import System.Mem
import System.Process

import Test.Util.ChromeFlags
import Test.Util.UnshareNetwork

#if MIN_VERSION_base(4,11,0)
import GHC.Stats (getRTSStatsEnabled, getRTSStats, RTSStats(..), gcdetails_live_bytes, gc)
currentBytesUsed :: RTSStats -> Int64
currentBytesUsed = fromIntegral . gcdetails_live_bytes . gc
#else
import GHC.Stats (getGCStats, GCStats(..))
getRTSStats = getGCStats
#endif

-- In initial testing, the minimum live bytes count was 233128 and maximum was
-- 363712.  Going over the maximum means the test has actually failed - we
-- probably have a memory leak; going under the minimum doesn't indicate a
-- memory leak, but may mean the test needs to be updated.
minBytesAllowed, resetThreshold, maxBytesAllowed :: Int64
(minBytesAllowed, resetThreshold, maxBytesAllowed) = (200000, 400000, 600000)

-- Some times the memory usage might flair up and then then return to normal
-- this probably indicates an issue, but if you are trying to fix a slow consistent
-- leak it can confuse the results by making the tests fail when the slow leak is
-- fixed.
-- Set this limit to say how many failures (currentBytesUsed > maxBytesAllowed)
-- want to ignore.  If the memory usage goes back under resetThreshold
-- the failure count is reset to 0.
failureLimit :: Int
failureLimit = 0

main :: IO ()
main = do
  handle (\(_ :: IOError) -> return ()) $ unshareNetwork -- If we run into an exception with sandboxing, just don't bother
  withSandboxedChromeFlags True $ \chromeFlags -> do
    mainThread <- myThreadId
    browserProcess <- spawnCommand $ mconcat
      [ "echo 'Starting Chromium' ; chromium "
      , unpack $ T.unwords chromeFlags
      , " http://localhost:3911 ; echo 'Chromium exited'"
      ]
    let finishTest result = do
          interruptProcessGroupOf browserProcess
          throwTo mainThread result
    putStrLn "About to start the server"
    run 3911 $ do
      -- enableLogging True
      liftIO $ putStrLn "Running..."
      mainWidget $ do
        let w = do
              rec let modifyAttrs = flip pushAlways (updated d) $ \_ -> sample $ current d
                  (e, _) <- element "button" (def & modifyAttributes .~ modifyAttrs) blank
                  d <- holdDyn mempty $ mempty <$ domEvent Click e
              return ()
        postBuild <- getPostBuild
        let f (!failures, !n) = liftIO $ if n < 3000
              then do performMajorGC
                      threadDelay 5000 -- Wait a bit to allow requestAnimationFrame to call its callback sometimes; this value was experimentally determined
                      gcStats <- getRTSStats
                      print $ currentBytesUsed gcStats
                      when (currentBytesUsed gcStats < minBytesAllowed) $ do
                        putStrLn "FAILED: currentBytesUsed < minBytesAllowed"
                        finishTest $ ExitFailure 2
                      let overMax = currentBytesUsed gcStats > maxBytesAllowed
                          underReset = currentBytesUsed gcStats < resetThreshold
                      when (overMax && failures >= failureLimit) $ do
                        putStrLn "FAILED: currentBytesUsed > maxBytesAllowed"
                        finishTest $ ExitFailure 1
                      return $ Just (
                          if overMax
                              then succ failures
                              else (if underReset then 0 else failures), succ n)
              else do putStrLn "SUCCEEDED"
                      finishTest ExitSuccess
                      return Nothing
        rec redraw <- performEvent <=< delay 0 $ f <$> leftmost
              [ (0 :: Int, 0 :: Int) <$ postBuild
              , fmapMaybe id redraw
              ]
        _ <- widgetHold w $ w <$ redraw
        return ()
      liftIO $ forever $ threadDelay 1000000000
