{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions for adjusting parts of the class attribute.
module Reflex.Dom.Attributes.Class
  ( addClass
  , addClasses
  , removeClass
  , removeClasses
  ) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Reflex.Dom.Attributes.Types
import Reflex.Patch
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified GHCJS.DOM.DOMTokenList as DOMTokenList
import qualified GHCJS.DOM.Element as Element

-- A patch for classes is represented by a map from classname to operation count.
newtype Class = Class { _unClass :: SignedSet Text } deriving (Eq, Semigroup, Monoid, Group)
instance IsAttribute Class where
  patchAttrDOM settings e (Class s) = do
    l <- Element.getClassList e
    let (deletions, additions) = splitSignedSet s
    liftIO $ putStrLn $ "Patching class, additions: " <> show additions <> ", deletions: " <> show deletions
    -- This traversal seems like it shouldn't be necessary, but something is
    -- wrong with ghcjs-dom's implementation of the add function. add [1,2,3]
    -- calls add([1,2,3]), not add(1,2,3), so you end up with "1,2,3" as the
    -- class. https://github.com/ghcjs/ghcjs-dom/issues/80
    -- Also, you can't just call add('1 2 3'), since the strings must be free of
    -- spaces.
    flip traverse_ additions $ \c -> do
      shouldAdd <- case settings of
        PatchSettings_PatchAlways -> pure True
        PatchSettings_CheckBeforePatching -> not <$> DOMTokenList.contains l c
      when shouldAdd $ DOMTokenList.add l [c]
    traverse_ (DOMTokenList.remove l . pure) deletions
  staticAttrMap (Class s) =
    let (_, additions) = splitSignedSet s
    in case null additions of
      True -> Map.empty
      False -> Map.singleton "class" $ T.unwords additions

-- | Add classes from a single 'Text'.
--
-- > addClass "some_class"
-- > addClass "first_class second_class"
addClass :: Text -> AttributePatch
addClass c = singleAttribute $ Class $ positiveSetFromList $ T.words c

-- | Add classes from a list.
--
-- > addClasses ["first_class", "second_class"]
addClasses :: [Text] -> AttributePatch
addClasses cs = singleAttribute $ Class $ positiveSetFromList $ concatMap T.words cs

-- | Remove classes in the given 'Text'.
--
-- > removeClass "some_class"
-- > removeClass "first_class second_class"
removeClass :: Text -> AttributePatch
removeClass c = singleAttribute $ Class $ negativeSetFromList $ T.words c

-- | Remove classes in the list.
--
-- > removeClasses ["first_class", "second_class"]
removeClasses :: [Text] -> AttributePatch
removeClasses cs = singleAttribute $ Class $ negativeSetFromList $ concatMap T.words cs
