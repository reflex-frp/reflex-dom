-----------------------------------------------------------------------------
-- |
-- Module      :  Reflex.Dom
-- Copyright   :  (c) Ryan Trinkle
-- License     :  BSD3
--
-- Maintainer  :  ryan.trinkle@gmail.com
--
-- Main public API for @reflex-dom@.
--
--   This module re-exports everything from "Reflex.Dom.Core" (the
--   platform-independent implementation) plus the platform-specific
--   'run', 'mainWidget', 'mainWidgetWithCss', etc. from
--   "Reflex.Dom.Internal".  Executables should depend on @reflex-dom@;
--   libraries should depend on @reflex-dom-core@.
--
--   Haddock rendering is disabled due to
--   <https://github.com/haskell/haddock/issues/979 haddock #979>.
--
-----------------------------------------------------------------------------
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
