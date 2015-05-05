module Reflex.Dom.Internal.Foreign (runWebGUI) where

import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.Types
import Data.Function

instance Eq Node where
  (==) = eqRef `on` unNode
