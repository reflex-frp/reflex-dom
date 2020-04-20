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
-- using 'Data.Text.Text'. This is similar to the old Reflex DOM API which often
-- exposed attributes as 'Data.Map.Map' 'Data.Text.Text' 'Data.Text.Text'.
--
-- The fine grained API is spread across submodules which encapsulate a
-- particular attribute. These functions are for carefully updating
-- /parts/ of attributes rather than just setting them as a whole. This
-- behaviour is particularly useful when working with external JS libraries
-- which set certain properties (often adding classes or setting styles) without
-- wiping them when reflex updates attributes.
--
-- For implementing your own fine grained control over attributes, see 'IsAttribute'.
module Reflex.Dom.Attributes
  (
  -- * Base types
    Namespace
  , AttributeName(..)
  , DeclareAttrs
  , ModifyAttrs
  -- * Functions for coarse control over attributes
  , declareAttribute
  , setAttribute
  , removeAttribute
  , declareAttributeMap
  , mapKeysToAttributeName
  -- * Modules for fine grained control over particular attributes
  , module FineGrained
  -- * Implementing new fine grained attributes
  , IsAttribute(..)
  , PatchSettings(..)
  , singleAttribute
  , singleModifyAttribute
  ) where

import Reflex.Dom.Attributes.Types

import Reflex.Dom.Attributes.Class as FineGrained
import Reflex.Dom.Attributes.Style as FineGrained
