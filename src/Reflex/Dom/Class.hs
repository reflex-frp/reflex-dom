module Reflex.Dom.Class ( module Reflex.Dom.Class
                        , module Foreign.JavaScript.TH
                        ) where

import Reflex.Class
import Reflex.Dom.PostBuild.Class
import Reflex.Dom.PerformEvent.Class

import Prelude hiding (mapM, mapM_, sequence, concat)

import Foreign.JavaScript.TH

import Data.Map (Map)
import qualified Data.Map as Map

-- | Alias for Data.Map.singleton
(=:) :: k -> a -> Map k a
(=:) = Map.singleton

keycodeEnter :: Int
keycodeEnter = 13

keycodeEscape :: Int
keycodeEscape = 27

{-# INLINABLE holdOnStartup #-}
holdOnStartup :: (PostBuild t m, PerformEvent t m, MonadHold t m) => a -> Performable m a -> m (Behavior t a)
holdOnStartup a0 ma = do
  hold a0 =<< performEvent . (ma <$) =<< getPostBuild
