{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Dom.Class ( module Reflex.Dom.Class
                        , module Foreign.JavaScript.TH
                        , module Web.KeyCode
                        ) where

import Control.Lens
import Reflex.Class
import Web.KeyCode

import Foreign.JavaScript.TH
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class

-- | Previously an alias for 'Data.Map.singleton', but now generalised to 'At'
(=:) :: (At m, Monoid m) => Index m -> IxValue m -> m
k =: a = at k ?~ a $ mempty
infixr 7 =: -- Ought to bind tighter than <>, which is infixr 6

{-# DEPRECATED keycodeEnter "Instead of `x == keycodeEnter`, use `keyCodeLookup x == Enter`" #-}
keycodeEnter :: Int
keycodeEnter = 13

{-# DEPRECATED keycodeEscape "Instead of `x == keycodeEscape`, use `keyCodeLookup x == Escape`" #-}
keycodeEscape :: Int
keycodeEscape = 27

{-# INLINABLE holdOnStartup #-}
holdOnStartup :: (PostBuild t m, PerformEvent t m, MonadHold t m) => a -> Performable m a -> m (Behavior t a)
holdOnStartup a0 ma = do
  hold a0 =<< performEvent . (ma <$) =<< getPostBuild
