{-# LANGUAGE ScopedTypeVariables, Rank2Types, ConstraintKinds #-}
module Data.Constraint.Possibly
       ( Possibly (..)
       , ifPossible
       ) where

import Data.Constraint

class Possibly c where
  getPossibly :: Maybe (Dict c)

ifPossible :: forall proxy c r. Possibly c => proxy c -> (c => r) -> r -> r
ifPossible _ with without = case getPossibly :: Maybe (Dict c) of
  Just Dict -> with
  Nothing -> without
