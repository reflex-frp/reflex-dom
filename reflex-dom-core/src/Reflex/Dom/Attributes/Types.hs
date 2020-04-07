{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types for creating or interacting with attributes.
module Reflex.Dom.Attributes.Types
  (
  -- * Base types
    Namespace
  , AttributeName(..)

  -- * Text based attribute functions
  , setAttribute
  , removeAttribute
  , setAttributeMap
  , mapKeysToAttributeName

  -- * AttributePatch
  , AttributePatch
  , traverseAttributePatch_
  , foldMapAttributePatch
  , attributesPatchIsEmpty
  , removeAttrFromPatch
  , lookupAttrInPatch

  -- * Implementing new attributes
  , IsAttribute(..)
  , PatchSettings(..)
  , singleAttribute

  -- * Utility types and functions for implementing new attributes #attribute_utility_types#
  -- ** SignedSet
  , SignedSet
  , signedSetFromList
  , positiveSetFromList
  , negativeSetFromList
  , splitSignedSet
  -- ** SignedList
  , SignedList (FirstPositive, Zero, FirstNegative)
  , leftmostPositive
  , leftmostPositiveMap
  , patchAttrDOMSignedText
  -- ** GroupMap
  , GroupMap
  , singletonGroupMap
  , groupMapFromMap
  , groupMapToMap
  ) where

import Control.Monad (when, void)
import Data.Dependent.Map (DMap, DSum(..))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.String (IsString(..))
import Data.Text
import GHCJS.DOM.Types (JSM)
import Reflex.Patch
import Reflex.Query.Class (SelectedCount)
import Type.Reflection
import qualified Data.Dependent.Map as DMap
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.Types as DOM

type Namespace = Text

-- | Attribute names which may or may not have namespaces
data AttributeName = AttributeName !(Maybe Namespace) !Text deriving (Show, Read, Eq, Ord)

-- | By default, AttributeNames are unnamespaced
instance IsString AttributeName where
  fromString = AttributeName Nothing . fromString

-- | Used for controlling 'patchAttrDOM'.
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
-- You probably want to implement your attributes as newtypes over types from
-- [here](#attribute_utility_types) to ensure that they are law abiding.
class (Eq a, Monoid a, Group a) => IsAttribute a where
  -- | Patch the attributes in DOM directly
  patchAttrDOM :: PatchSettings -> DOM.Element -> a -> JSM ()
  -- | Produce a map representing the attributes as text.
  staticAttrMap :: a -> Map AttributeName Text

-- | A wrapper which reveals 'IsAttribute'.
data AnAttribute a where
  AnAttribute :: IsAttribute a => a -> AnAttribute a

-- | 'AttributePatch' is a collection of individual attribute patches.
-- Patches of the same `IsAttribute` type are merged using their 'Semigroup'
-- instance.
newtype AttributePatch = AttributePatch
  { unAttributePatch :: DMap TypeRep AnAttribute
-- TODO: Consider TypeRepMap.
-- + May be faster
-- - Marked as broken in nixpkgs
-- - Does not build on GHCJS yet
-- - Does not have a way of filtering (but could it?)
  }

-- | Check if the attribute patch is empty.
attributesPatchIsEmpty :: AttributePatch -> Bool
attributesPatchIsEmpty = DMap.null . unAttributePatch

-- | Create 'AttributePatch' from a single attribute patch.
singleAttribute :: forall a. (IsAttribute a, Typeable a) => a -> AttributePatch
singleAttribute = AttributePatch . DMap.singleton (typeRep @a) . AnAttribute

-- | Traverse each attribute in the patch.
traverseAttributePatch_ :: Applicative m => (forall a. IsAttribute a => a -> m ()) -> AttributePatch -> m ()
traverseAttributePatch_ f = traverse_ (\(_ :=> AnAttribute a) -> f a) . DMap.toList . unAttributePatch

-- | Fold attributes to some value.
foldMapAttributePatch :: Monoid m => (forall a. IsAttribute a => a -> m) -> AttributePatch -> m
foldMapAttributePatch f = foldMap (\(_ :=> AnAttribute a) -> f a) . DMap.toList . unAttributePatch

-- Unions the items together and filters them out if they are 'mempty'.
instance Semigroup AttributePatch where
  AttributePatch a <> AttributePatch b = AttributePatch
    $ DMap.filterWithKey (\_ (AnAttribute x) -> x /= mempty)
    $ DMap.unionWithKey (\_ (AnAttribute x) (AnAttribute y) -> AnAttribute (x <> y)) a b
instance Monoid AttributePatch where
  mempty = AttributePatch DMap.empty
instance Group AttributePatch where
  negateG = AttributePatch . DMap.map (\(AnAttribute a) -> AnAttribute $ negateG a) . unAttributePatch
instance Patch AttributePatch where
  type PatchTarget AttributePatch = AttributePatch
  apply a b = Just $ a <> b

-- | Maintain an invertible set of elements.
newtype SignedSet a = SignedSet { _unSignedSet :: GroupMap a SelectedCount } deriving (Eq, Semigroup, Monoid, Group)

-- | Create a 'SignedSet' from a list which is augmented with the count already.
signedSetFromList :: Ord a => [(a, SelectedCount)] -> SignedSet a
signedSetFromList = SignedSet . GroupMap . Map.fromList

-- | Create a 'SignedSet' with these positive elements.
positiveSetFromList :: Ord a => [a] -> SignedSet a
positiveSetFromList = signedSetFromList . fmap (\a -> (a, 1))

-- | Create a 'SignedSet' with these negative elements.
negativeSetFromList :: Ord a => [a] -> SignedSet a
negativeSetFromList = signedSetFromList . fmap (\a -> (a, -1))

-- | Split the signed set into the negative and positive parts.
splitSignedSet :: SignedSet a -> ([a], [a])
splitSignedSet (SignedSet (GroupMap m)) = (Map.keys negatives, Map.keys positives)
  where (negatives, positives) = Map.partition (<0) m

-- | Order preserving version of 'SignedSet'. The intention of this is to
-- provide something akin to 'Maybe' which is a good group.
data SignedList a = SignedList
  { _signedList_positive :: [a]
  , _signedList_negative :: [a]
  } deriving Eq
instance Eq a => Semigroup (SignedList a) where
  SignedList p1 n1 <> SignedList p2 n2 = SignedList
    { _signedList_positive = (p1 L.\\ n2) <> (p2 L.\\ n1)
    , _signedList_negative = (n1 L.\\ p2) <> (n2 L.\\ p1)
    }
instance Eq a => Monoid (SignedList a) where
  mempty = SignedList [] []
instance Eq a => Group (SignedList a) where
  negateG (SignedList l r) = SignedList r l

{-# COMPLETE FirstPositive, Zero, FirstNegative #-}

-- | The first positive element
pattern FirstPositive :: a -> SignedList a
pattern FirstPositive a <- SignedList (a:_) _
  where FirstPositive a = SignedList [a] []

-- | The first negative element
pattern FirstNegative :: a -> SignedList a
pattern FirstNegative a <- SignedList _ (a:_)
  where FirstNegative a = SignedList [] [a]

-- | The zero element
pattern Zero :: SignedList a
pattern Zero = SignedList [] []

-- | Helper function for implementing 'patchAttrDOM' for your type using
-- 'SignedList'. Only the first positive or negative element is used in the patch.
patchAttrDOMSignedText :: PatchSettings -> AttributeName -> DOM.Element -> SignedList Text -> JSM ()
patchAttrDOMSignedText settings (AttributeName mns k) e = \case
  FirstPositive a -> case mns of
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
  FirstNegative _ -> case mns of
    Nothing -> Element.removeAttribute e k
    Just ns -> Element.removeAttributeNS e (Just ns) k
  Zero -> pure ()

-- | Get the first positive element, if it exists.
leftmostPositive :: SignedList a -> Maybe a
leftmostPositive = \case
  FirstPositive a -> Just a
  _ -> Nothing

-- | Like 'leftmostPositive', but returns an empty or singleton 'Map'. Useful
-- for implementing 'staticAttrMap'.
leftmostPositiveMap :: k -> SignedList a -> Map k a
leftmostPositiveMap k = maybe Map.empty (Map.singleton k) . leftmostPositive

-- | Like 'MonoidalMap', but prunes the leaves when they become 'mempty'.
newtype GroupMap k a = GroupMap { unGroupMap :: Map k a } deriving Eq
instance (Ord k, Eq a, Monoid a) => Semigroup (GroupMap k a) where
  GroupMap a <> GroupMap b = GroupMap $ Map.merge lefts rights both a b
    where lefts = Map.preserveMissing
          rights = Map.preserveMissing
          both = Map.zipWithMaybeMatched $ \_ x y ->
            let m = x <> y in if m == mempty then Nothing else Just m
instance (Ord k, Eq a, Monoid a) => Monoid (GroupMap k a) where
  mempty = GroupMap Map.empty
instance (Ord k, Eq a, Group a) => Group (GroupMap k a) where
  negateG = GroupMap . fmap negateG . unGroupMap

-- | Create a 'GroupMap' from a 'Map'.
groupMapFromMap :: Map k a -> GroupMap k a
groupMapFromMap = GroupMap

-- | Obtain a 'Map' from a 'GroupMap'.
groupMapToMap :: GroupMap k a -> Map k a
groupMapToMap = unGroupMap

-- | Create a 'GroupMap' with a single member.
singletonGroupMap :: k -> a -> GroupMap k a
singletonGroupMap k = GroupMap . Map.singleton k

groupMapDelete :: Ord k => k -> GroupMap k a -> GroupMap k a
groupMapDelete k = GroupMap . Map.delete k . unGroupMap

-- | A cheap way of supporting existing code which doesn't need fine grained
-- control over the values of attributes. This allows us to keep (=:) for
-- setting attributes (although it is no longer generalised to 'Map').
newtype BasicAttributes = BasicAttributes
  { _unBasicAttributes :: GroupMap AttributeName (SignedList Text)
  } deriving (Eq, Semigroup, Monoid, Group)
instance IsAttribute BasicAttributes where
  patchAttrDOM s e (BasicAttributes m) = void $ Map.traverseWithKey (\k -> patchAttrDOMSignedText s k e) $ groupMapToMap m
  staticAttrMap (BasicAttributes m) = Map.mapMaybe leftmostPositive $ groupMapToMap m

-- | Set an attribute to the given 'Text'.
setAttribute :: AttributeName -> Text -> AttributePatch
setAttribute k = singleAttribute . BasicAttributes . singletonGroupMap k . FirstPositive

-- | Remove an attribute.
removeAttribute :: AttributeName -> AttributePatch
removeAttribute k = singleAttribute $ BasicAttributes $ singletonGroupMap k $ FirstNegative ""

-- | Set an attribute using the given 'Map'.
setAttributeMap :: Map AttributeName Text -> AttributePatch
setAttributeMap = singleAttribute . BasicAttributes . groupMapFromMap . fmap FirstPositive

-- | Utility function for creating un-namespaced attribute maps
mapKeysToAttributeName :: Map Text Text -> Map AttributeName Text
mapKeysToAttributeName = Map.mapKeysMonotonic (AttributeName Nothing)

lookupAttrInPatch :: AttributeName -> AttributePatch -> Maybe Text
lookupAttrInPatch k (AttributePatch dm) = case DMap.lookup (typeRep @BasicAttributes) dm of
  Nothing -> Nothing
  Just (AnAttribute (BasicAttributes (GroupMap gm))) -> (\case FirstPositive a -> Just a; _ -> Nothing) =<< Map.lookup k gm

removeAttrFromPatch :: AttributeName -> AttributePatch -> AttributePatch
removeAttrFromPatch k (AttributePatch m) = AttributePatch $ DMap.alter f (typeRep @BasicAttributes) m
  where f = \case
          Nothing -> Nothing
          Just (AnAttribute (BasicAttributes gm)) -> Just $ AnAttribute $ BasicAttributes $ groupMapDelete k gm
