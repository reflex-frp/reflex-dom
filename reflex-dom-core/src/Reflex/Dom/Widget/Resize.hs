{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Dom.Widget.Resize where

import Reflex.Class
import Reflex.Time
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Class
import Reflex.Dom.Widget.Basic
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM.Element
import GHCJS.DOM.EventM (on)
import qualified GHCJS.DOM.GlobalEventHandlers as Events (scroll)
import GHCJS.DOM.Types (MonadJSM, liftJSM, uncheckedCastTo, HTMLElement(..))
import GHCJS.DOM.HTMLElement (getOffsetWidth, getOffsetHeight)
import qualified GHCJS.DOM.Types as DOM

-- | A widget that wraps the given widget in a div and fires an event when resized.
--   Adapted from @github.com\/marcj\/css-element-queries@
--
-- This function can cause strange scrollbars to appear in some circumstances.
-- These can be hidden with pseudo selectors, for example, in webkit browsers:
-- .wrapper *::-webkit-scrollbar { width: 0px; background: transparent; }
resizeDetector :: (MonadJSM m, DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m), MonadFix m) => m a -> m (Event t (Maybe Double, Maybe Double), a)
resizeDetector = resizeDetectorWithStyle ""

resizeDetectorWithStyle :: (MonadJSM m, DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m), MonadFix m)
  => Text -- ^ A css style string. Warning: It should not contain the "position" style attribute.
  -> m a -- ^ The embedded widget
  -> m (Event t (Maybe Double, Maybe Double), a) -- ^ An 'Event' that fires on resize, and the result of the embedded widget
resizeDetectorWithStyle styleString = resizeDetectorWithAttrs ("style" =: styleString)

resizeDetectorWithAttrs :: (MonadJSM m, DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m), MonadFix m)
  => Map Text Text -- ^ A map of attributes. Warning: It should not modify the "position" style attribute.
  -> m a -- ^ The embedded widget
  -> m (Event t (Maybe Double, Maybe Double), a) -- ^ An 'Event' that fires on resize, and the result of the embedded widget
resizeDetectorWithAttrs attrs w = do
  let childStyle = "position: absolute; left: 0; top: 0;"
      containerAttrs = "style" =: "position: absolute; left: 0; top: 0; right: 0; bottom: 0; overflow: scroll; z-index: -1; visibility: hidden;"
  (parent, (expand, expandChild, shrink, w')) <- elAttr' "div" (Map.unionWith (<>) attrs ("style" =: "position: relative;")) $ do
    w' <- w
    elAttr "div" containerAttrs $ do
      (expand, (expandChild, _)) <- elAttr' "div" containerAttrs $ elAttr' "div" ("style" =: childStyle) $ return ()
      (shrink, _) <- elAttr' "div" containerAttrs $ elAttr "div" ("style" =: (childStyle <> "width: 200%; height: 200%;")) $ return ()
      return (expand, expandChild, shrink, w')
  let p = uncheckedCastTo HTMLElement $ _element_raw parent
      reset = do
        let e = uncheckedCastTo HTMLElement $ _element_raw expand
            s = _element_raw shrink
        eow <- getOffsetWidth e
        eoh <- getOffsetHeight e
        let ecw = eow + 10
            ech = eoh + 10
        setAttribute (_element_raw expandChild) ("style" :: Text) (childStyle <> "width: " <> T.pack (show ecw) <> "px;" <> "height: " <> T.pack (show ech) <> "px;")
        esw <- getScrollWidth e
        setScrollLeft e esw
        esh <- getScrollHeight e
        setScrollTop e esh
        ssw <- getScrollWidth s
        setScrollLeft s ssw
        ssh <- getScrollHeight s
        setScrollTop s ssh
        lastWidth <- getOffsetWidth p
        lastHeight <- getOffsetHeight p
        return (Just lastWidth, Just lastHeight)
      resetIfChanged ds = do
        pow <- getOffsetWidth p
        poh <- getOffsetHeight p
        if ds == (Just pow, Just poh)
          then return Nothing
          else fmap Just reset
  pb <- delay 0 =<< getPostBuild
  expandScroll <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw expand) (`on` Events.scroll) $ return ()
  shrinkScroll <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw shrink) (`on` Events.scroll) $ return ()
  size0 <- performEvent $ fmap (const $ liftJSM reset) pb
  rec resize <- performEventAsync $ fmap (\d cb -> (liftIO . cb) =<< liftJSM (resetIfChanged d)) $ tag (current dimensions) $ leftmost [expandScroll, shrinkScroll]
      dimensions <- holdDyn (Nothing, Nothing) $ leftmost [ size0, fmapMaybe id resize ]
  return (updated dimensions, w')
