{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Dom.Class ( module Reflex.Dom.Class
                        , module Foreign.JavaScript.TH
                        ) where

import Reflex.Class
import Reflex.Dom.PerformEvent.Class
import Reflex.Dom.PostBuild.Class

import Prelude hiding (concat, mapM, mapM_, sequence)

import Foreign.JavaScript.TH

import Data.Map (Map)
import qualified Data.Map as Map

-- | Alias for Data.Map.singleton
(=:) :: k -> a -> Map k a
(=:) = Map.singleton
infixr 7 =: -- Ought to bind tighter than <>, which is infixr 6

keycodeEnter :: Int
keycodeEnter = 13

keycodeEscape :: Int
keycodeEscape = 27

{-# INLINABLE holdOnStartup #-}
holdOnStartup :: (PostBuild t m, PerformEvent t m, MonadHold t m) => a -> Performable m a -> m (Behavior t a)
holdOnStartup a0 ma = do
  hold a0 =<< performEvent . (ma <$) =<< getPostBuild
