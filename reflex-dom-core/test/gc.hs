{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import GHC.Stats
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core
import System.Exit
import System.IO.Temp
import System.Mem
import System.Process

-- In initial testing, the minimum live bytes count was 233128 and maximum was
-- 363712.  Going over the maximum means the test has actually failed - we
-- probably have a memory leak; going under the minimum doesn't indicate a
-- memory leak, but may mean the test needs to be updated.
minBytesAllowed, maxBytesAllowed :: Int64
(minBytesAllowed, maxBytesAllowed) = (200000, 400000)

main :: IO ()
main = do
  mainThread <- myThreadId
  withSystemTempDirectory "reflex-dom-core_test_gc" $ \tmp -> do
    browserProcess <- spawnCommand $ "xvfb-run -a chromium --user-data-dir=" ++ tmp ++ " http://localhost:3911"
    let finishTest result = do
          interruptProcessGroupOf browserProcess
          throwTo mainThread result
    run 3911 $ do
      liftIO $ putStrLn "Running..."
      mainWidget $ do
        let w = do
              rec let modifyAttrs = flip pushAlways (updated d) $ \_ -> sample $ current d
                  (e, _) <- element "button" (def & modifyAttributes .~ modifyAttrs) blank
                  d <- holdDyn mempty $ mempty <$ domEvent Click e
              return ()
        postBuild <- getPostBuild
        let f n = liftIO $ if n < 3000
              then do performMajorGC
                      gcStats <- getGCStats
                      print $ currentBytesUsed gcStats
                      when (currentBytesUsed gcStats < minBytesAllowed) $ do
                        putStrLn "FAILED: currentBytesUsed < minBytesAllowed"
                        finishTest $ ExitFailure 2
                      when (currentBytesUsed gcStats > maxBytesAllowed) $ do
                        putStrLn "FAILED: currentBytesUsed > maxBytesAllowed"
                        finishTest $ ExitFailure 1
                      return $ Just $ succ n
              else do putStrLn "SUCCEEDED"
                      finishTest ExitSuccess
                      return Nothing
        rec redraw <- performEvent <=< delay 0 $ f <$> leftmost
              [ (0 :: Int) <$ postBuild
              , fmapMaybe id redraw
              ]
        _ <- widgetHold w $ w <$ redraw
        return ()
      liftIO $ forever $ threadDelay 1000000000
