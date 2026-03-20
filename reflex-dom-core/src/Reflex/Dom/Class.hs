{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module: Reflex.Dom.Class
--
-- Utility re-exports and convenience functions. The most important export is
-- the '(=:)' operator for building single-entry 'Map's (commonly used for
-- HTML attributes):
--
-- @
-- elAttr \"a\" (\"href\" =: \"\/about\" \<\> \"class\" =: \"link\") $ text \"About\"
-- @
--
-- Also re-exports 'HasJSContext' from "Foreign.JavaScript.TH" and
-- 'keyCodeLookup' from "Web.KeyCode".
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

-- | Create a single-entry container. Generalised to 'At', but typically used
-- with @Map Text Text@ for HTML attributes. Combine multiple attributes with
-- @(\<\>)@:
--
-- @
-- elAttr \"input\" (\"type\" =: \"text\" \<\> \"placeholder\" =: \"Name\" \<\> \"class\" =: \"input\") blank
-- @
(=:) :: (At m, Monoid m) => Index m -> IxValue m -> m
k =: a = at k ?~ a $ mempty
infixr 7 =: -- Ought to bind tighter than <>, which is infixr 6

{-# DEPRECATED keycodeEnter "Instead of `x == keycodeEnter`, use `keyCodeLookup x == Enter`" #-}
keycodeEnter :: Int
keycodeEnter = 13

{-# DEPRECATED keycodeEscape "Instead of `x == keycodeEscape`, use `keyCodeLookup x == Escape`" #-}
keycodeEscape :: Int
keycodeEscape = 27

-- | Run an action at post-build time and hold its result as a 'Behavior'.
-- Useful for fetching an initial value from the environment (e.g. current
-- time, config) that the widget needs as a 'Behavior'.
--
-- @
-- now <- holdOnStartup (UTCTime (ModifiedJulianDay 0) 0) getCurrentTime
-- @
{-# INLINABLE holdOnStartup #-}
holdOnStartup :: (PostBuild t m, PerformEvent t m, MonadHold t m) => a -> Performable m a -> m (Behavior t a)
holdOnStartup a0 ma = do
  hold a0 =<< performEvent . (ma <$) =<< getPostBuild
