-- Disable haddocks on this module due to a bug on haddocks when selectively
-- reexporting on ghc > 8.2.
-- https://github.com/haskell/haddock/issues/979
{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}
module Reflex.Dom.Core (module X) where

import Reflex as X hiding (askEvents)
import Reflex.Dom.Builder.Class as X
import Reflex.Dom.Builder.Hydratable as X
import Reflex.Dom.Builder.Immediate as X
import Reflex.Dom.Builder.InputDisabled as X
import Reflex.Dom.Builder.Static as X
import Reflex.Dom.Class as X
import Reflex.Dom.Location as X
import Reflex.Dom.Main as X
import Reflex.Dom.Modals.Class as X
import Reflex.Dom.Old as X
import Reflex.Dom.Prerender as X
import Reflex.Dom.WebSocket as X
import Reflex.Dom.Widget as X
import Reflex.Dom.Xhr as X
import Reflex.Dom.Xhr.FormData as X
