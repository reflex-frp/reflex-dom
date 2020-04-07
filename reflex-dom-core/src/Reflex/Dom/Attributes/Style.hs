{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions for adjusting parts of the style attribute.
module Reflex.Dom.Attributes.Style
  ( addStyle
  , addStyleWithPriority
  , removeStyle
  ) where

import Control.Lens (iforM_)
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Reflex.Dom.Attributes.Types
import Reflex.Patch
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
  { _unStyle :: GroupMap Text (SignedList StyleValue)
  } deriving (Eq, Semigroup, Monoid, Group)

instance IsAttribute Style where
  patchAttrDOM settings e (Style props) = void . runMaybeT $ do
    -- We cast the element to a HTMLElement because it has an instance for
    -- IsElementCSSInlineStyle.
    htmlEl <- MaybeT $ DOM.castTo DOM.HTMLElement e
    -- We use uncheckedCastTo here because mixins have a dreadful bug:
    -- https://github.com/ghcjs/ghcjs-dom/issues/96
    -- By coercing straight to ElementCSSInlineStyle, we don't actually try to
    -- instantiate the mixin, and thereby avoid the bug in the above ticket.
    let es = DOM.uncheckedCastTo DOM.ElementCSSInlineStyle htmlEl
    style <- lift $ ElementCSSInlineStyle.getStyle es
    lift $ iforM_ (groupMapToMap props) $ \p -> \case
      -- Prefer additions
      FirstPositive (StyleValue value prio) -> do
        liftIO $ T.putStrLn $ "Setting style property: " <> p <> ": " <> value
        shouldPatch <- case settings of
          PatchSettings_PatchAlways -> pure True
          PatchSettings_CheckBeforePatching -> do
            value' <- CSSStyleDeclaration.getPropertyValue style p
            prio' <- CSSStyleDeclaration.getPropertyPriority style p
            pure $ value /= value' || prio /= prio'
        when shouldPatch $ CSSStyleDeclaration.setProperty style p value (Nothing :: Maybe Text) -- TODO priority
      -- Then deletions
      FirstNegative _ -> do
        liftIO $ T.putStrLn $ "Removing style property: " <> p
        CSSStyleDeclaration.removeProperty_ style p
      -- Or do nothing
      Zero -> pure ()
  staticAttrMap (Style cs) = let additions = Map.mapMaybe leftmostPositive (groupMapToMap cs) in case Map.null additions of
    True -> Map.empty
    False -> Map.singleton "style" $ T.intercalate ";" $ fmap (\(k, s) -> k <> ":" <> styleValueText s) $ Map.toList additions

-- | Add a style property.
--
-- > addStyle "color" "red"
addStyle :: Text -> Text -> AttributePatch
addStyle p v = singleAttribute . Style . singletonGroupMap p $ FirstPositive $ StyleValue v Nothing

-- | Add a style property with priority.
--
-- > addStyleWithPriority "color" "red" "important"
addStyleWithPriority :: Text -> Text -> Text -> AttributePatch
addStyleWithPriority p v prio = singleAttribute . Style . singletonGroupMap p $ FirstPositive $ StyleValue v (Just prio)

-- | Remove a style property.
--
-- > removeStyle "color"
removeStyle :: Text -> AttributePatch
removeStyle p = singleAttribute $ Style $ singletonGroupMap p $ FirstNegative $ StyleValue "" Nothing
