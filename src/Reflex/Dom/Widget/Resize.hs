{-# LANGUAGE RecursiveDo #-}
module Reflex.Dom.Widget.Resize where

import Reflex
import Reflex.Dom.Class
import Reflex.Dom.Widget.Basic

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import GHCJS.DOM.Element

-- | A widget that wraps the given widget in a div and fires an event when resized.
--   Adapted from github.com/marcj/css-element-queries
resizeDetector :: MonadWidget t m => m a -> m (Event t (), a)
resizeDetector = resizeDetectorWithStyle ""

resizeDetectorWithStyle :: MonadWidget t m
  => String -- ^ A css style string. Warning: It should not contain the "position" style attribute.
  -> m a -- ^ The embedded widget
  -> m (Event t (), a) -- ^ An 'Event' that fires on resize, and the result of the embedded widget
resizeDetectorWithStyle styleString w = do
  let childStyle = "position: absolute; left: 0; top: 0;"
      containerAttrs = "style" =: "position: absolute; left: 0; top: 0; right: 0; bottom: 0; overflow: scroll; z-index: -1; visibility: hidden;"
  (parent, (expand, expandChild, shrink, w')) <- elAttr' "div" ("style" =: ("position: relative;" <> styleString)) $ do
    w' <- w
    elAttr "div" containerAttrs $ do
      (expand, (expandChild, _)) <- elAttr' "div" containerAttrs $ elAttr' "div" ("style" =: childStyle) $ return ()
      (shrink, _) <- elAttr' "div" containerAttrs $ elAttr "div" ("style" =: (childStyle <> "width: 200%; height: 200%;")) $ return ()
      return (expand, expandChild, shrink, w')
  let reset = do
        let e = _el_element expand
            s = _el_element shrink
        eow <- elementGetOffsetWidth e
        eoh <- elementGetOffsetHeight e
        let ecw = eow + 10
            ech = eoh + 10
        elementSetAttribute (_el_element expandChild) "style" (childStyle <> "width: " <> show ecw <> "px;" <> "height: " <> show ech <> "px;")
        esw <- elementGetScrollWidth e
        elementSetScrollLeft e esw
        esh <- elementGetScrollHeight e
        elementSetScrollTop e esh
        ssw <- elementGetScrollWidth s
        elementSetScrollLeft s ssw
        ssh <- elementGetScrollHeight s
        elementSetScrollTop s ssh
        lastWidth <- elementGetOffsetWidth (_el_element parent)
        lastHeight <- elementGetOffsetHeight (_el_element parent)
        return (Just lastWidth, Just lastHeight)
      resetIfChanged ds = do
        pow <- elementGetOffsetWidth (_el_element parent)
        poh <- elementGetOffsetHeight (_el_element parent)
        if ds == (Just pow, Just poh)
           then return Nothing
           else liftM Just reset
  pb <- getPostBuild
  expandScroll <- wrapDomEvent (_el_element expand) elementOnscroll $ return ()
  shrinkScroll <- wrapDomEvent (_el_element shrink) elementOnscroll $ return ()
  size0 <- performEvent $ fmap (const $ liftIO reset) pb
  rec resize <- performEventAsync $ fmap (\d cb -> liftIO $ cb =<< resetIfChanged d) $ tag (current dimensions) $ leftmost [expandScroll, shrinkScroll]
      dimensions <- holdDyn (Nothing, Nothing) $ leftmost [ size0, fmapMaybe id resize ]
  return (fmap (const ()) $ fmapMaybe id resize, w')
