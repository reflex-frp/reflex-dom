{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions for adjusting parts of the class attribute.
module Reflex.Dom.Attributes.Class
  (
  -- * Declaratively specifying classes (for static or dynamic attributes)
    declareClass
  , declareClasses
  -- * Adding or removing classes (for modifying attributes)
  , addClass
  , addClasses
  , removeClass
  , removeClasses
  ) where

import Control.Monad (when)
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import Data.Patch
import Data.Proxy
import Data.Set (Set)
import Data.Text (Text)
import Reflex.Dom.Attributes.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHCJS.DOM.DOMTokenList as DOMTokenList
import qualified GHCJS.DOM.Element as Element

newtype Class = Class { _unClass :: Set Text } deriving (Eq, Semigroup, Monoid)
-- A patch for classes is represented by a map from classname to bool, where
-- false = delete class and true = add class.
newtype ModifyClass = ModifyClass { _unModifyClass :: Map Text Bool } deriving (Eq, Semigroup, Monoid)

instance Patch ModifyClass where
  type PatchTarget ModifyClass = Class
  apply (ModifyClass m) (Class old)
    | Map.null m = Nothing
    | otherwise = Just $ Class $ Map.keysSet additions <> (old Set.\\ Map.keysSet deletions)
    where (additions, deletions) = Map.partition id m

instance IsAttribute ModifyClass where
  applyAttrPatchDOM settings e (ModifyClass s) = do
    l <- Element.getClassList e
    let (additions, deletions) = Map.partition id s
    -- This traversal seems like it shouldn't be necessary, but something is
    -- wrong with ghcjs-dom's implementation of the add function. add [1,2,3]
    -- calls add([1,2,3]), not add(1,2,3), so you end up with "1,2,3" as the
    -- class. https://github.com/ghcjs/ghcjs-dom/issues/80
    -- Also, you can't just call add('1 2 3'), since the strings must be free of
    -- spaces.
    for_ (Map.keys additions) $ \c -> do
      shouldAdd <- case settings of
        PatchSettings_PatchAlways -> pure True
        PatchSettings_CheckBeforePatching -> not <$> DOMTokenList.contains l c
      when shouldAdd $ DOMTokenList.add l [c]
    traverse_ (DOMTokenList.remove l . pure) (Map.keys deletions)
  staticAttrMap _ (Class s) = case Set.null s of
    True -> Map.empty
    False -> Map.singleton "class" $ T.unwords $ Set.toList s
  diffAttr (Class old) (Class new)
    | Set.null additions && Set.null deletions = Nothing
    | otherwise = Just $ ModifyClass $ Map.fromSet (const True) additions <> Map.fromSet (const False) deletions
    where
      additions = Set.difference new old
      deletions = Set.difference old new

-- | Declaratively specify classes from a single 'Text'.
--
-- > declareClass "some_class"
-- > declareClass "first_class second_class"
declareClass :: Text -> DeclareAttrs
declareClass = singleAttribute (Proxy :: Proxy ModifyClass) . Class . Set.fromList . T.words

-- | Declaratively specify classes from a list.
--
-- > declareClasses ["first_class", "second_class"]
declareClasses :: [Text] -> DeclareAttrs
declareClasses = singleAttribute (Proxy :: Proxy ModifyClass) . Class . Set.fromList . concatMap T.words

-- | Add classes from a single 'Text'.
--
-- > addClass "some_class"
-- > addClass "first_class second_class"
addClass :: Text -> ModifyAttrs
addClass = singleModifyAttribute . ModifyClass . Map.fromList . fmap (, True) . T.words

-- | Add classes from a list.
--
-- > addClasses ["first_class", "second_class"]
addClasses :: [Text] -> ModifyAttrs
addClasses = singleModifyAttribute . ModifyClass . Map.fromList . fmap (, True) . concatMap T.words

-- | Remove classes in the given 'Text'.
--
-- > removeClass "some_class"
-- > removeClass "first_class second_class"
removeClass :: Text -> ModifyAttrs
removeClass = singleModifyAttribute . ModifyClass . Map.fromList . fmap (, False) . T.words

-- | Remove classes in the list.
--
-- > removeClasses ["first_class", "second_class"]
removeClasses :: [Text] -> ModifyAttrs
removeClasses = singleModifyAttribute . ModifyClass . Map.fromList . fmap (, False) . concatMap T.words
