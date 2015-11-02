{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TypeFamilies #-}

module Reflex.Dom.Time where

import Reflex
import Reflex.Dom.Class

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Fixed
import Data.Time.Clock
import System.Random
import Data.Typeable

data TickInfo
  = TickInfo { _tickInfo_lastUTC :: UTCTime
             -- ^ UTC time immediately after the last tick.
             , _tickInfo_n :: Integer
             -- ^ Number of time periods since t0
             , _tickInfo_alreadyElapsed :: NominalDiffTime
             -- ^ Amount of time already elapsed in the current tick period.
             }
  deriving (Eq, Ord, Show, Typeable)

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

-- | Send events with Poisson timing with the given basis and rate
--   Each occurence of the resulting event will contain the index of
--   the current interval, with 0 representing the basis time
poissonLossyFrom
  :: (RandomGen g, MonadWidget t m)
  => g
  -> Double
  -- ^ Poisson event rate (Hz)
  -> UTCTime
  -- ^ Baseline time for events
  -> Event t a
  -- ^ Event that starts a tick generation thread. Usually you want this to
  -- be something like the result of getPostBuild that only fires once. But
  -- there could be uses for starting multiple timer threads.
  -- Start sending events in response to the event parameter.
  -> m (Event t TickInfo)
poissonLossyFrom rnd rate t0 t =
  inhomogeneousPoissonFrom rnd (current $ constDyn rate) rate t0 t


-- | Send events with Poisson timing with the given basis and rate
--   Each occurence of the resulting event will contain the index of
--   the current interval, with 0 representing the basis time.
--   Automatically begin sending events when the DOM is built
poissonLossy
  :: (RandomGen g, MonadWidget t m)
  => g
  -> Double
  -- ^ Poisson event rate (Hz)
  -> UTCTime
  -- ^ Baseline time for events
  -> m (Event t TickInfo)
poissonLossy rnd rate t0 = poissonLossyFrom rnd rate t0 =<< getPostBuild

-- | Send events with inhomogeneous Poisson timing with the given basis
--   and variable rate. Provide a maxRate that you expect to support.
inhomogeneousPoissonFrom
  :: (RandomGen g, MonadWidget t m)
  => g
  -> Behavior t Double
  -> Double
  -> UTCTime
  -> Event t a
  -> m (Event t TickInfo)
inhomogeneousPoissonFrom rnd rate maxRate t0 e = do

  -- Create a thread for producing homogeneous poisson events
  -- along with random Doubles (usage of Double's explained below)
  ticksWithRateRand <- performEventAsync $
                       fmap callAtNextInterval e

  -- Filter homogeneous events according to associated
  -- random values and the current rate parameter
  return $ attachWithMaybe filterFun rate ticksWithRateRand

  where

    -- Inhomogeneous poisson processes are built from faster
    -- homogeneous ones by randomly dropping events from the
    -- fast process. For each fast homogeneous event, choose
    -- a uniform random sample from (0, rMax). If the
    -- inhomogeneous rate at this moment is greater than the
    -- random sample, then keep this event, otherwise drop it
    filterFun :: Double -> (TickInfo, Double) -> Maybe TickInfo
    filterFun r (tInfo, p)
      | r >= p    = Just tInfo
      | otherwise = Nothing

    callAtNextInterval _ cb = void $ liftIO $ forkIO $ go t0 rnd cb 0

    go tTargetLast lastGen cb lastN = do
      t <- getCurrentTime

      -- Generate random numbers for this poisson interval (u)
      -- and sample-retention likelihood (p)
      let (u, nextGen)            = randomR (0,1) lastGen
          (p :: Double, nextGen') = randomR (0,maxRate) nextGen

      -- Inter-event interval is drawn from exponential
      -- distribution accourding to u
      let dt             = realToFrac $ -1 * log(u)/maxRate :: NominalDiffTime
          offset         = t `diffUTCTime` t0
          nEvents        = lastN + 1
          alreadyElapsed = diffUTCTime t tTargetLast
          tTarget        = addUTCTime dt tTargetLast
          thisDelay      = realToFrac $ diffUTCTime tTarget t :: Double
      threadDelay $ ceiling $ thisDelay * 1000000
      void $ cb $ (TickInfo t nEvents alreadyElapsed, p)
      go tTarget nextGen' cb nEvents

-- | Send events with inhomogeneous Poisson timing with the given basis
--   and variable rate. Provide a maxRate that you expect to support
inhomogeneousPoisson
  :: (RandomGen g, MonadWidget t m)
  => g
  -> Behavior t Double
  -> Double
  -> UTCTime
  -> m (Event t TickInfo)
inhomogeneousPoisson rnd rate maxRate t0 =
  inhomogeneousPoissonFrom rnd rate maxRate t0 =<< getPostBuild
