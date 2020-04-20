{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions for adjusting parts of the style attribute.
module Reflex.Dom.Attributes.Style
  (
  -- * Declaratively specifying styles (for static or dynamic attributes)
    declareStyle
  , declareStyleWithPriority
  -- * Adding or removing styles (for modifying attributes)
  , addStyle
  , addStyleWithPriority
  , removeStyle
  ) where

import Data.Map.Misc (diffMap)
import Data.Proxy
import Control.Lens (iforM_)
import Control.Monad (void, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Reflex.Dom.Attributes.Types
import Data.Patch
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified GHCJS.DOM.CSSStyleDeclaration as CSSStyleDeclaration
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.ElementCSSInlineStyle as ElementCSSInlineStyle
import qualified GHCJS.DOM.Types as DOM

data StyleValue = StyleValue
  { _styleValue_value :: Text
  , _styleValue_priority :: Maybe Text
  } deriving Eq

styleValueText :: StyleValue -> Text
styleValueText (StyleValue v mp) = maybe id (\p a -> a <> " !" <> p) mp $ v

newtype Style = Style
  { _unStyle :: Map Text StyleValue
  } deriving (Eq, Semigroup, Monoid)

newtype ModifyStyle = ModifyStyle
  { _unModifyStyle :: Map Text (Maybe StyleValue)
  } deriving (Eq, Semigroup, Monoid)

instance Patch ModifyStyle where
  type PatchTarget ModifyStyle = Style
  apply (ModifyStyle p) (Style a) = Style <$> apply (PatchMap p) a

instance IsAttribute ModifyStyle where
  applyAttrPatchDOM settings e (ModifyStyle props) = void . runMaybeT $ do
    -- We cast the element to a HTMLElement because it has an instance for
    -- IsElementCSSInlineStyle.
    htmlEl <- MaybeT $ DOM.castTo DOM.HTMLElement e
    -- We use uncheckedCastTo here because mixins have a dreadful bug:
    -- https://github.com/ghcjs/ghcjs-dom/issues/96
    -- By coercing straight to ElementCSSInlineStyle, we don't actually try to
    -- instantiate the mixin, and thereby avoid the bug in the above ticket.
    let es = DOM.uncheckedCastTo DOM.ElementCSSInlineStyle htmlEl
    style <- lift $ ElementCSSInlineStyle.getStyle es
    lift $ iforM_ props $ \p -> \case
      Just (StyleValue value prio) -> do
        shouldPatch <- case settings of
          PatchSettings_PatchAlways -> pure True
          PatchSettings_CheckBeforePatching -> do
            value' <- CSSStyleDeclaration.getPropertyValue style p
            prio' <- CSSStyleDeclaration.getPropertyPriority style p
            pure $ value /= value' || prio /= prio'
        when shouldPatch $ CSSStyleDeclaration.setProperty style p value prio
      Nothing -> do
        CSSStyleDeclaration.removeProperty_ style p
  staticAttrMap _ (Style m) = case Map.null m of
    True -> Map.empty
    False -> Map.singleton "style" $ T.intercalate ";" $ fmap (\(k, s) -> k <> ":" <> styleValueText s) $ Map.toList m
  diffAttr (Style old) (Style new) = case diffMap old new of
    m | Map.null m -> Nothing
      | otherwise -> Just $ ModifyStyle m

-- | Declaratively specify a style property.
--
-- > declareStyle "color" "red"
declareStyle :: Text -> Text -> DeclareAttrs
declareStyle p v = singleAttribute (Proxy :: Proxy ModifyStyle) . Style . Map.singleton p $ StyleValue v Nothing

-- | Declaratively specify a style property with priority.
--
-- > declareStyleWithPriority "color" "red" "important"
declareStyleWithPriority :: Text -> Text -> Text -> DeclareAttrs
declareStyleWithPriority p v prio = singleAttribute (Proxy :: Proxy ModifyStyle) . Style . Map.singleton p $ StyleValue v (Just prio)

-- | Add a style property.
--
-- > addStyle "color" "red"
addStyle :: Text -> Text -> ModifyAttrs
addStyle p v = singleModifyAttribute . ModifyStyle . Map.singleton p $ Just $ StyleValue v Nothing

-- | Add a style property with priority.
--
-- > addStyleWithPriority "color" "red" "important"
addStyleWithPriority :: Text -> Text -> Text -> ModifyAttrs
addStyleWithPriority p v prio = singleModifyAttribute . ModifyStyle . Map.singleton p $ Just $ StyleValue v (Just prio)

-- | Remove a style property.
--
-- > removeStyle "color"
removeStyle :: Text -> ModifyAttrs
removeStyle p = singleModifyAttribute $ ModifyStyle $ Map.singleton p Nothing
