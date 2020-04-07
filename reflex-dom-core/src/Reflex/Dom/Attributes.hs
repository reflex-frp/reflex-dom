{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Reflex DOM supports both coarse and fine grained control over element attributes.
--
-- The coarse API is in this module, whereby you can add or remove attributes
-- using 'Text'. This is similar to the old Reflex DOM API which often exposed
-- attributes as 'Data.Map.Map' 'Text' 'Text'.
--
-- The fine grained API is spread across submodules which encapsulate a
-- particular attribute. For implementing your own fine grained control over
-- attributes, see "Reflex.Dom.Attributes.Types".
module Reflex.Dom.Attributes
  (
  -- * Base types
    Namespace
  , AttributeName(..)
  , AttributePatch
  -- * Functions for coarse control over attributes
  , setAttribute
  , removeAttribute
  , setAttributeMap
  , mapKeysToAttributeName
  -- * Modules for fine grained control over particular attributes
  , module FineGrained
  ) where

import Reflex.Dom.Attributes.Types

import Reflex.Dom.Attributes.Class as FineGrained
import Reflex.Dom.Attributes.Style as FineGrained
