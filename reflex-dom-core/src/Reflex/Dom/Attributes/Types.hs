{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types for creating or interacting with attributes.
module Reflex.Dom.Attributes.Types
  (
  -- * Base types
    Namespace
  , AttributeName(..)

  -- * Text based attribute functions
  , declareAttribute
  , setAttribute
  , removeAttribute
  , declareAttributeMap
  , mapKeysToAttributeName

  -- * Attributes
  , DeclareAttrs
  , attributesPatchIsEmpty
  , removeAttrFromDeclareAttrs
  , lookupAttrInPatch
  , ModifyAttrs
  , removeAttrFromModifyAttrs
  , toPatch

  -- * Implementing new attributes
  , IsAttribute(..)
  , PatchSettings(..)
  , singleAttribute
  , singleModifyAttribute
  ) where

import Control.Lens
import Control.Monad (when, void)
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum(..))
import Data.Foldable (traverse_)
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.Map (Map)
import Data.Map.Misc (diffMap)
import Data.Patch
import Data.Proxy
import Data.String (IsString(..))
import Data.Text
import GHCJS.DOM.Types (JSM)
import Type.Reflection
import qualified Data.Dependent.Map as DMap
import qualified Data.Dependent.Map.Lens as DMap
import qualified Data.Map as Map
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.Types as DOM

type Namespace = Text

-- | Attribute names which may or may not have namespaces
data AttributeName = AttributeName !(Maybe Namespace) !Text deriving (Show, Read, Eq, Ord)

-- | By default, AttributeNames are unnamespaced
instance IsString AttributeName where
  fromString = AttributeName Nothing . fromString

-- | Used for controlling 'applyAttrPatchDOM'.
data PatchSettings
  = PatchSettings_CheckBeforePatching
  -- ^ Be careful to minimise the patch. This is used for checking attrs during
  -- hydration, to prevent touching the DOM when unnecessary. In particular,
  -- always patching during hydration can result in CSS animations running more
  -- than once.
  | PatchSettings_PatchAlways
  -- ^ Always run the patch. Alternatively: checking before patching is
  -- optional, and can be omitted to avoid an extra roundtrip.

-- | Define new attributes by creating instances of this class.
-- This is defined around 'Patch' so we can use 'holdIncremental'.
-- The 'PatchTarget' is used in declarative attributes (i.e. static and
-- 'Dynamic' based attributes), and the patch itself is used in modify
-- attributes ('Event' based attributes).
class (Monoid (PatchTarget a), Semigroup a, Patch a) => IsAttribute a where
  -- | Apply an attribute patch to the given DOM element.
  applyAttrPatchDOM :: PatchSettings -> DOM.Element -> a -> JSM ()
  -- | Produce a map representing the attributes as text.
  staticAttrMap :: Proxy a -> PatchTarget a -> Map AttributeName Text -- TODO: is there a reason why we can't make PatchTarget injective? I'd like to get rid of 'Proxy'.
  -- | Produce a patch by diffing two 'PatchTarget' values. The primary use of
  -- this is to produce patches for dynamic attributes.
  diffAttr :: PatchTarget a -> PatchTarget a -> Maybe a

-- | Convert attributes to an attribute patch (by setting all attributes).
toPatch :: IsAttribute a => PatchTarget a -> Maybe a
toPatch = diffAttr mempty

-- | The purpose of this is to ensure the ordering of traversal is predictable.
-- We want to apply the basic attributes before the others such that the basic
-- ones don't overwrite the finer ones.
data AttrKey a where
  AttrKey_BasicAttributes :: AttrKey ModBasicAttributes
  AttrKey_Other :: IsAttribute a => TypeRep a -> AttrKey a

-- | Does the same job as ArgDict but without actually needing to use ArgDict
withAttr :: AttrKey a -> (IsAttribute a => b) -> b
withAttr = \case
  AttrKey_BasicAttributes -> id
  AttrKey_Other _ -> id

-- | This is just to apply the 'PatchTarget' type family in DMap values
newtype PT a = PT { _unPT :: PatchTarget a }
instance Semigroup (PatchTarget a) => Semigroup (PT a) where
  PT a <> PT b = PT (a <> b)
instance Monoid (PatchTarget a) => Monoid (PT a) where
  mempty = PT mempty

-- | A collection of individual attributes.
-- Attributes of the same `IsAttribute` type are merged using their 'Semigroup'
-- instance.
newtype DeclareAttrs = DeclareAttrs
  { unDeclareAttrs :: DMap AttrKey PT
  }

-- Unions the items together and filters them out if they are 'mempty'.
instance Semigroup DeclareAttrs where
  DeclareAttrs a <> DeclareAttrs b = DeclareAttrs
    $ DMap.unionWithKey (\k (PT x) (PT y) -> withAttr k $ PT (x <> y)) a b

instance Monoid DeclareAttrs where
  mempty = DeclareAttrs DMap.empty

-- | Create 'DeclareAttrs' from a single attribute.
singleAttribute :: forall a. (IsAttribute a, Typeable a) => Proxy a -> PatchTarget a -> DeclareAttrs
singleAttribute Proxy = DeclareAttrs . DMap.singleton (AttrKey_Other $ typeRep @a) . PT

-- | A collection of individual attribute patches.
-- Attribute patches of the same 'IsAttribute' type are merged by using their
-- 'Semigroup' instance.
newtype ModifyAttrs = ModifyAttrs
  { unModifyAttrs :: DMap AttrKey Identity
  }

instance Semigroup ModifyAttrs where
  ModifyAttrs a <> ModifyAttrs b = ModifyAttrs $ DMap.unionWithKey (`withAttr` (<>)) a b

instance Monoid ModifyAttrs where
  mempty = ModifyAttrs DMap.empty

instance IsAttribute ModifyAttrs where
  applyAttrPatchDOM s e = DOM.liftJSM . traverse_ (\(k :=> Identity a) -> withAttr k $ applyAttrPatchDOM s e a) . DMap.toList . unModifyAttrs
  staticAttrMap Proxy = foldMap foldAttrs . DMap.toList . unDeclareAttrs
    where
      foldAttrs = \case
        AttrKey_BasicAttributes :=> PT a -> staticAttrMap (Proxy :: Proxy ModBasicAttributes) a
        AttrKey_Other _ :=> (PT a :: PT a) -> staticAttrMap (Proxy :: Proxy a) a
  diffAttr (DeclareAttrs olds) (DeclareAttrs news) = Just $ ModifyAttrs $ DMap.merge
    (DMap.mapMaybeMissing $ \k (PT o) -> withAttr k $ Identity <$> diffAttr o mempty)
    (DMap.mapMaybeMissing $ \k (PT n) -> withAttr k $ Identity <$> diffAttr mempty n)
    (DMap.zipWithMaybeMatched $ \k (PT a) (PT b) -> withAttr k $ Identity <$> diffAttr a b)
    olds
    news

instance Patch ModifyAttrs where
  type PatchTarget ModifyAttrs = DeclareAttrs
  apply (ModifyAttrs p) (DeclareAttrs m) = Just $ DeclareAttrs $ DMap.merge
    (DMap.mapMaybeMissing $ \k (Identity mods) -> withAttr k $ PT <$> apply mods mempty)
    (DMap.mapMissing $ \_k decs -> decs)
    (DMap.zipWithMaybeMatched $ \k (Identity a) (PT b) -> withAttr k $ PT <$> apply a b)
    p m

-- | Create 'ModifyAttrs' from a single attribute patch.
singleModifyAttribute :: forall a. (IsAttribute a, Typeable a) => a -> ModifyAttrs
singleModifyAttribute = ModifyAttrs . DMap.singleton (AttrKey_Other $ typeRep @a) . Identity

-- | Check if the attribute patch is empty.
attributesPatchIsEmpty :: ModifyAttrs -> Bool
attributesPatchIsEmpty = DMap.null . unModifyAttrs

-- | A cheap way of supporting existing code which doesn't need fine grained
-- control over the values of attributes. This allows us to keep (=:) for
-- setting attributes (although it is no longer generalised to 'Map').
newtype BasicAttributes = BasicAttributes
  { unBasicAttributes :: Map AttributeName Text
  } deriving (Semigroup, Monoid)

newtype ModBasicAttributes = ModBasicAttributes
  { unModBasicAttributes :: PatchMap AttributeName Text
  } deriving (Semigroup, Monoid)

instance Patch ModBasicAttributes where
  type PatchTarget ModBasicAttributes = BasicAttributes
  apply (ModBasicAttributes p) (BasicAttributes m) = BasicAttributes <$> apply p m

instance IsAttribute ModBasicAttributes where
  applyAttrPatchDOM s e = void . Map.traverseWithKey (\k -> applyAttrPatchMaybe s k e) . unPatchMap . unModBasicAttributes
  staticAttrMap Proxy = unBasicAttributes
  diffAttr (BasicAttributes m) (BasicAttributes m') = Just $ ModBasicAttributes $ PatchMap $ diffMap m m'

-- | Set or remove an attribute
applyAttrPatchMaybe :: PatchSettings -> AttributeName -> DOM.Element -> Maybe Text -> JSM ()
applyAttrPatchMaybe settings (AttributeName mns k) e = \case
  Just a -> case mns of
    Nothing -> do
      shouldPatch <- case settings of
        PatchSettings_PatchAlways -> pure True
        PatchSettings_CheckBeforePatching -> do
          ma <- Element.getAttribute e k
          pure $ maybe True (a /=) ma
      when shouldPatch $ Element.setAttribute e k a
    Just ns -> do
      shouldPatch <- case settings of
        PatchSettings_PatchAlways -> pure True
        PatchSettings_CheckBeforePatching -> do
          ma <- Element.getAttributeNS e (Just ns) k
          pure $ maybe True (a /=) ma
      when shouldPatch $ Element.setAttributeNS e (Just ns) k a
  Nothing -> case mns of
    Nothing -> Element.removeAttribute e k
    Just ns -> Element.removeAttributeNS e (Just ns) k

-- | Declare an attribute with a value.
declareAttribute :: AttributeName -> Text -> DeclareAttrs
declareAttribute k = DeclareAttrs . DMap.singleton AttrKey_BasicAttributes . PT . BasicAttributes . Map.singleton k

-- | Set an attribute using the given 'Map'.
declareAttributeMap :: Map AttributeName Text -> DeclareAttrs
declareAttributeMap = DeclareAttrs . DMap.singleton AttrKey_BasicAttributes . PT . BasicAttributes

-- | Utility function for creating un-namespaced attribute maps
mapKeysToAttributeName :: Map Text Text -> Map AttributeName Text
mapKeysToAttributeName = Map.mapKeysMonotonic (AttributeName Nothing)

lookupAttrInPatch :: AttributeName -> DeclareAttrs -> Maybe Text
lookupAttrInPatch k (DeclareAttrs dm) = case DMap.lookup AttrKey_BasicAttributes dm of
  Nothing -> Nothing
  Just (PT (BasicAttributes m)) -> Map.lookup k m

-- | Set an attribute to the given 'Text'.
setAttribute :: AttributeName -> Text -> ModifyAttrs
setAttribute k = ModifyAttrs . DMap.singleton AttrKey_BasicAttributes . Identity . ModBasicAttributes . PatchMap . Map.singleton k . Just

-- | Remove an attribute.
removeAttribute :: AttributeName -> ModifyAttrs
removeAttribute k = ModifyAttrs . DMap.singleton AttrKey_BasicAttributes $ Identity $ ModBasicAttributes $ PatchMap $ Map.singleton k Nothing

-- TODO we can't really remove anything except basic attributes, so some
-- widgets which want total control over an attribute are possibly breakable
removeAttrFromModifyAttrs :: AttributeName -> ModifyAttrs -> ModifyAttrs
removeAttrFromModifyAttrs k = ModifyAttrs . DMap.alter f AttrKey_BasicAttributes . unModifyAttrs
  where f = \case
          Nothing -> Nothing
          Just (Identity (ModBasicAttributes (PatchMap m))) -> Just $ Identity $ ModBasicAttributes $ PatchMap $ Map.delete k m

-- TODO see comment for removeAttrFromModifyAttrs
removeAttrFromDeclareAttrs :: AttributeName -> DeclareAttrs -> DeclareAttrs
removeAttrFromDeclareAttrs k = DeclareAttrs . DMap.alter f AttrKey_BasicAttributes . unDeclareAttrs
  where f = \case
          Nothing -> Nothing
          Just (PT (BasicAttributes m)) -> Just $ PT $ BasicAttributes $ Map.delete k m

_PT :: Iso' (PT a) (PatchTarget a)
_PT = iso (\(PT a) -> a) PT

_BasicAttributes :: Iso' BasicAttributes (Map AttributeName Text)
_BasicAttributes = iso unBasicAttributes BasicAttributes

_DeclareAttrs :: Iso' DeclareAttrs (DMap AttrKey PT)
_DeclareAttrs = iso unDeclareAttrs DeclareAttrs

type instance Index DeclareAttrs = AttributeName
type instance IxValue DeclareAttrs = Text
instance Ixed DeclareAttrs where
  ix k = _DeclareAttrs . DMap.dmix AttrKey_BasicAttributes . _PT . _BasicAttributes . ix k
instance At DeclareAttrs where
  at k f = _DeclareAttrs $ DMap.dmat AttrKey_BasicAttributes g
    where g (Just (PT (BasicAttributes m))) = Just . PT . BasicAttributes <$> at k f m
          g Nothing = Just . PT . BasicAttributes <$> at k f mempty

_ModBasicAttributes :: Iso' ModBasicAttributes (Map AttributeName (Maybe Text))
_ModBasicAttributes = iso (unPatchMap . unModBasicAttributes) (ModBasicAttributes . PatchMap)

_ModifyAttrs :: Iso' ModifyAttrs (DMap AttrKey Identity)
_ModifyAttrs = iso unModifyAttrs ModifyAttrs

type instance Index ModifyAttrs = AttributeName
type instance IxValue ModifyAttrs = Maybe Text
instance Ixed ModifyAttrs where
  ix k = _ModifyAttrs . DMap.dmix AttrKey_BasicAttributes . _Wrapped . _ModBasicAttributes . ix k
instance At ModifyAttrs where
  at k f = _ModifyAttrs $ DMap.dmat AttrKey_BasicAttributes g
    where g (Just (Identity (ModBasicAttributes (PatchMap m)))) = Just . Identity . ModBasicAttributes . PatchMap <$> at k f m
          g Nothing = Just . Identity . ModBasicAttributes . PatchMap <$> at k f mempty

deriveGEq ''AttrKey
deriveGCompare ''AttrKey
