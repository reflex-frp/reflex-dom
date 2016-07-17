{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module Reflex.Dom.Widget.Resize where

import Reflex
import Reflex.Dom.PostBuild.Class
import Reflex.Dom.PerformEvent.Class
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Class
import Reflex.Dom.Old
import Reflex.Dom.Widget.Basic

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import GHCJS.DOM.Element hiding (reset)
import GHCJS.DOM.EventM (on)
import qualified Data.Map as Map

-- | A widget that wraps the given widget in a div and fires an event when resized.
--   Adapted from github.com/marcj/css-element-queries
resizeDetector :: (DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadIO (Performable m), MonadIO m, MonadFix m) => m a -> m (Event t (), a)
resizeDetector = resizeDetectorWithStyle ""

resizeDetectorWithStyle :: (DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadIO (Performable m), MonadIO m, MonadFix m)
  => Text -- ^ A css style string. Warning: It should not contain the "position" style attribute.
  -> m a -- ^ The embedded widget
  -> m (Event t (), a) -- ^ An 'Event' that fires on resize, and the result of the embedded widget
resizeDetectorWithStyle styleString = resizeDetectorWithAttrs ("style" =: styleString)

resizeDetectorWithAttrs :: (DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadIO (Performable m), MonadIO m, MonadFix m)
  => Map Text Text -- ^ A map of attributes. Warning: It should not modify the "position" style attribute.
  -> m a -- ^ The embedded widget
  -> m (Event t (), a) -- ^ An 'Event' that fires on resize, and the result of the embedded widget
resizeDetectorWithAttrs attrs w = do
  let childStyle = "position: absolute; left: 0; top: 0;"
      containerAttrs = "style" =: "position: absolute; left: 0; top: 0; right: 0; bottom: 0; overflow: scroll; z-index: -1; visibility: hidden;"
  (parent, (expand, expandChild, shrink, w')) <- elAttr' "div" (Map.unionWith (<>) attrs ("style" =: "position: relative;")) $ do
    w' <- w
    elAttr "div" containerAttrs $ do
      (expand, (expandChild, _)) <- elAttr' "div" containerAttrs $ elAttr' "div" ("style" =: childStyle) $ return ()
      (shrink, _) <- elAttr' "div" containerAttrs $ elAttr "div" ("style" =: (childStyle <> "width: 200%; height: 200%;")) $ return ()
      return (expand, expandChild, shrink, w')
  let reset = do
        let e = _el_element expand
            s = _el_element shrink
        eow <- getOffsetWidth e
        eoh <- getOffsetHeight e
        let ecw = eow + 10
            ech = eoh + 10
        setAttribute (_el_element expandChild) ("style" :: Text) (childStyle <> "width: " <> T.pack (show ecw) <> "px;" <> "height: " <> T.pack (show ech) <> "px;")
        esw <- getScrollWidth e
        setScrollLeft e esw
        esh <- getScrollHeight e
        setScrollTop e esh
        ssw <- getScrollWidth s
        setScrollLeft s ssw
        ssh <- getScrollHeight s
        setScrollTop s ssh
        lastWidth <- getOffsetWidth (_el_element parent)
        lastHeight <- getOffsetHeight (_el_element parent)
        return (Just lastWidth, Just lastHeight)
      resetIfChanged ds = do
        pow <- getOffsetWidth (_el_element parent)
        poh <- getOffsetHeight (_el_element parent)
        if ds == (Just pow, Just poh)
          then return Nothing
          else fmap Just reset
  pb <- getPostBuild
  expandScroll <- wrapDomEvent (_el_element expand) (`on` scroll) $ return ()
  shrinkScroll <- wrapDomEvent (_el_element shrink) (`on` scroll) $ return ()
  size0 <- performEvent $ fmap (const $ liftIO reset) pb
  rec resize <- performEventAsync $ fmap (\d cb -> liftIO $ cb =<< resetIfChanged d) $ tag (current dimensions) $ leftmost [expandScroll, shrinkScroll]
      dimensions <- holdDyn (Nothing, Nothing) $ leftmost [ size0, fmapMaybe id resize ]
  return (fmapMaybe void resize, w')
