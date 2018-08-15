{-# LANGUAGE OverloadedStrings     #-}

module Reflex.Dom.Contrib.Vanishing where

------------------------------------------------------------------------------
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Enumerates the visibility patterns provided by the DOM and easily
-- controllable with dynamic attributes.
--
-- * DisplayNone takes the element out of the document flow
-- * VisibilityHidden leaves the element in the flow, which allows you to do
--   height calculations even while the element is not visible.
data ElementVisibility
  = DisplayNone
  | VisibilityHidden
  | Visible
  deriving (Eq,Ord,Show,Read,Enum,Bounded)


------------------------------------------------------------------------------
-- | Returns attributes representing the specified visibility.
visibilityAttrs :: ElementVisibility -> Map Text Text
visibilityAttrs DisplayNone = "style" =: "display: none;"
visibilityAttrs VisibilityHidden = "style" =: "visibility: hidden;"
visibilityAttrs Visible = mempty


------------------------------------------------------------------------------
-- | Returns VisibilityHidden when argument is True.  Visible otherwise.
hiddenWhen :: Bool -> ElementVisibility
hiddenWhen True = VisibilityHidden
hiddenWhen False = Visible


------------------------------------------------------------------------------
-- | Returns DisplayNone when argument is True.  Visible otherwise.
displayNoneWhen :: Bool -> ElementVisibility
displayNoneWhen True = DisplayNone
displayNoneWhen False = Visible


------------------------------------------------------------------------------
-- | Convenient helper for showing a vanishing div.
vanishingDiv :: MonadWidget t m => Dynamic t ElementVisibility -> m a -> m a
vanishingDiv = vanishing "div"


------------------------------------------------------------------------------
-- | Convenient helper for showing a vanishing span.
vanishingSpan :: MonadWidget t m => Dynamic t ElementVisibility -> m a -> m a
vanishingSpan = vanishing "span"


------------------------------------------------------------------------------
-- | General function for showing a vanishing element.
vanishing
    :: MonadWidget t m
    => Text
    -> Dynamic t ElementVisibility
    -> m a
    -> m a
vanishing tagName vis = elDynAttr tagName (visibilityAttrs <$> vis)


------------------------------------------------------------------------------
-- | General function for showing a vanishing element.
vanishingAttr
    :: MonadWidget t m
    => Text
    -> Map Text Text
    -> Dynamic t ElementVisibility
    -> m a
    -> m a
vanishingAttr tagName attrs vis =
    elDynAttr tagName ((\v -> visibilityAttrs v <> attrs) <$> vis)


------------------------------------------------------------------------------
-- | General function for showing a vanishing element.
vanishingDynAttr
    :: MonadWidget t m
    => Text
    -> Dynamic t (Map Text Text)
    -> Dynamic t ElementVisibility
    -> m a
    -> m a
vanishingDynAttr tagName attrs vis =
    elDynAttr tagName ((\v as -> visibilityAttrs v <> as) <$> vis <*> attrs)


------------------------------------------------------------------------------
-- | Explicitly name data structure for controlling whether a widget is in the
-- DOM or not.
data DomExistence = InDom | NotInDom


------------------------------------------------------------------------------
-- | This function is a little different than the others.  Instead of setting
-- a display:none or visibility:hidden style when the element is invisible,
-- this function removes the widget from the DOM completely.
goneWhen
    :: MonadWidget t m
    => m a
    -> Dynamic t DomExistence
    -> m a
    -> m (Event t a)
goneWhen defWidget existence widget = dyn (dontShow <$> existence)
  where
    dontShow NotInDom = defWidget
    dontShow InDom = widget


------------------------------------------------------------------------------
-- | This function uses a Maybe value to control whether a widget gets placed
-- in the DOM or not.
maybeGone
    :: MonadWidget t m
    => m b
    -> (a -> m b)
    -> Dynamic t (Maybe a)
    -> m (Event t b)
maybeGone defWidget widget mayVal = dyn (maybe defWidget widget <$> mayVal)
