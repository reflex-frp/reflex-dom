module Reflex.Dom.Time where

import Reflex
import Reflex.Dom.Class

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Fixed
import Data.Time.Clock

-- | Send events over time with the given basis time and interval
--   If the system starts running behind, occurrences will be dropped rather than buffered
--   Each occurrence of the resulting event will contain the index of the current interval, with 0 representing the basis time
tickLossy :: MonadWidget t m => NominalDiffTime -> UTCTime -> m (Event t Integer)
tickLossy dt t0 = performEventAsync . fmap callAtNextInterval =<< getPostBuild
  where callAtNextInterval _ cb = void $ liftIO $ forkIO $ forever $ do
          t <- getCurrentTime
          let offset = t `diffUTCTime` t0
              (n, alreadyElapsed) = offset `divMod'` dt
          threadDelay $ ceiling $ (dt - alreadyElapsed) * 1000000
          cb n
