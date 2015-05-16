{-# LANGUAGe ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Dom.Time where

import Reflex
import Reflex.Dom.Class

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Fixed
import Data.Time.Clock
import System.Random

data TickInfo
  = TickInfo { _tickInfo_lastUTC :: UTCTime
             -- ^ UTC time immediately after the last tick.
             , _tickInfo_n :: Integer
             -- ^ Number of time periods since t0
             , _tickInfo_alreadyElapsed :: NominalDiffTime
             -- ^ Amount of time already elapsed in the current tick period.
             }
  deriving (Eq)

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
          t <- getCurrentTime
          let offset = t `diffUTCTime` t0
              (n, alreadyElapsed) = offset `divMod'` dt
          threadDelay $ ceiling $ (dt - alreadyElapsed) * 1000000
          cb $ TickInfo t n alreadyElapsed

delay :: MonadWidget t m => NominalDiffTime -> Event t a -> m (Event t a)
delay dt e = performEventAsync $ ffor e $ \a cb -> liftIO $ void $ forkIO $ do
  threadDelay $ ceiling $ dt * 1000000
  cb a

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
  ticksWithRateRand <- performEventAsync $
                       fmap callAtNextInterval e
  return $ attachWithMaybe filterFun rate ticksWithRateRand

  where

    filterFun :: Double -> (TickInfo, Double) -> Maybe TickInfo
    filterFun r (tInfo, p)
      | r >= p    = Just tInfo
      | otherwise = Nothing

    callAtNextInterval _ cb = void $ liftIO $ forkIO $ go rnd cb

    go lastGen cb = do
      t <- getCurrentTime
      let (u, nextGen)  = randomR (0,1) lastGen
          (p :: Double, nextGen') = randomR (0,maxRate) nextGen
          dt = realToFrac $ -1 * log(u)/maxRate :: NominalDiffTime
          offset = t `diffUTCTime` t0
          (n, alreadyElapsed) = offset `divMod'` dt
      threadDelay $ ceiling $ (dt - alreadyElapsed) * 1000000
      void $ cb $ (TickInfo t n alreadyElapsed, p)
      go nextGen' cb

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
