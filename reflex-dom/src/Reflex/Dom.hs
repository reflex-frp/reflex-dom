-- Disable haddocks on this module due to a bug on haddocks when selectively
-- reexporting on ghc > 8.2.
-- https://github.com/haskell/haddock/issues/979
{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}
module Reflex.Dom (module X) where

import Foreign.JavaScript.Orphans ()
import Reflex.Dom.Core as X hiding (mainWidget, mainWidgetInElementById, mainWidgetWithCss,
                             mainWidgetWithHead, mainWidgetWithHead', runApp',
                             mainHydrationWidgetWithHead, mainHydrationWidgetWithHead')
import Reflex.Dom.Internal as X
