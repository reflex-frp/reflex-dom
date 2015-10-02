{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Reflex.Dom.Time where

import Reflex
import Reflex.Dom.Class

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Fixed
import Data.Time.Clock
import Data.Typeable

data TickInfo
  = TickInfo { _tickInfo_lastUTC :: UTCTime
             -- ^ UTC time immediately after the last tick.
             , _tickInfo_n :: Integer
             -- ^ Number of time periods since t0
             , _tickInfo_alreadyElapsed :: NominalDiffTime
             -- ^ Amount of time already elapsed in the current tick period.
             }
  deriving (Eq, Ord, Typeable)

-- | Special case of tickLossyFrom that uses the post-build event to start the
--   tick thread.
tickLossy :: MonadWidget t m => NominalDiffTime -> UTCTime -> m (Event t TickInfo)
tickLossy dt t0 = tickLossyFrom dt t0 =<< getPostBuild

-- | Send events over time with the given basis time and interval
--   If the system starts running behind, occurrences will be dropped rather than buffered
--   Each occurrence of the resulting event will contain the index of the current interval, with 0 representing the basis time
tickLossyFrom
    :: MonadWidget t m
    => NominalDiffTime
    -> UTCTime
    -> Event t a
    -- ^ Event that starts a tick generation thread.  Usually you want this to
    -- be something like the result of getPostBuild that only fires once.  But
    -- there could be uses for starting multiple timer threads.
    -> m (Event t TickInfo)
tickLossyFrom dt t0 e = performEventAsync $ fmap callAtNextInterval e
  where callAtNextInterval _ cb = void $ liftIO $ forkIO $ forever $ do
          tick <- getCurrentTick dt t0
          threadDelay $ ceiling $ (dt - _tickInfo_alreadyElapsed tick) * 1000000
          cb tick

clockLossy :: MonadWidget t m => NominalDiffTime -> UTCTime -> m (Dynamic t TickInfo)
clockLossy dt t0 = do
  initial <- liftIO $ getCurrentTick dt t0
  e <- tickLossy dt t0
  holdDyn initial e

getCurrentTick :: NominalDiffTime -> UTCTime -> IO TickInfo
getCurrentTick dt t0 = do
  t <- getCurrentTime
  let offset = t `diffUTCTime` t0
      (n, alreadyElapsed) = offset `divMod'` dt
  return $ TickInfo t n alreadyElapsed

-- | Delay an Event's occurrences by a given amount in seconds.
delay :: MonadWidget t m => NominalDiffTime -> Event t a -> m (Event t a)
delay dt e = performEventAsync $ ffor e $ \a cb -> liftIO $ void $ forkIO $ do
  threadDelay $ ceiling $ dt * 1000000
  cb a

-- | Block occurrences of an Event until th given number of seconds elapses without
--   the Event firing, at which point the last occurrence of the Event will fire.
debounce :: MonadWidget t m => NominalDiffTime -> Event t a -> m (Event t a)
debounce dt e = do
  n :: Dynamic t Integer <- count e
  let tagged = attachDynWith (,) n e
  delayed <- delay dt tagged
  return $ attachWithMaybe (\n' (t, v) -> if n' == t then Just v else Nothing) (current n) delayed
