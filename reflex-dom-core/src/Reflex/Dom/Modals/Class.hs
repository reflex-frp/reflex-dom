{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Dom.Modals.Class where

import Reflex

class ModalOpener t m where
  requestingModal :: Event t (m (Event t a)) -> m (Event t (Maybe a))
