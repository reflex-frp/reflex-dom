{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}

module Reflex.Dom.Widget.Lazy where

import Reflex
import Reflex.Dom.Class
import Reflex.Dom.Widget.Basic

import Control.Monad.IO.Class
import Data.Fixed
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import GHCJS.DOM.Element

-- |A list view for long lists. Creates a scrollable element and only renders child row elements near the current scroll position.
virtualListWithSelection :: forall t m k v. (MonadWidget t m, Ord k)
  => Int -- ^ The height of the visible region in pixels
  -> Int -- ^ The height of each row in pixels
  -> Dynamic t Int -- ^ The total number of items
  -> Int -- ^ The index of the row to scroll to on initialization
  -> Event t Int -- ^ An 'Event' containing a row index. Used to scroll to the given index.
  -> String -- ^ The element tag for the list
  -> Dynamic t (Map String String) -- ^ The attributes of the list
  -> String -- ^ The element tag for a row
  -> Dynamic t (Map String String) -- ^ The attributes of each row
  -> (k -> Dynamic t v -> m ()) -- ^ The row child element builder
  -> Dynamic t (Map k v) -- ^ The 'Map' of items
  -> m (Dynamic t (Int, Int), Event t k) -- ^ A tuple containing: a 'Dynamic' of the index (based on the current scroll position) and number of items currently being rendered, and an 'Event' of the selected key
virtualListWithSelection heightPx rowPx maxIndex i0 setI listTag listAttrs rowTag rowAttrs itemBuilder items = do
  totalHeightStyle <- mapDyn (toHeightStyle . (*) rowPx) maxIndex
  rec (container, itemList) <- elAttr "div" outerStyle $ elAttr' "div" containerStyle $ do
        currentTop <- mapDyn (listWrapperStyle . fst) window
        (_, lis) <- elDynAttr "div" totalHeightStyle $ tagWrapper listTag listAttrs currentTop $ listWithKey itemsInWindow $ \k v -> do
            (li,_) <- tagWrapper rowTag rowAttrs (constDyn $ toHeightStyle rowPx) $ itemBuilder k v
            return $ fmap (const k) (domEvent Click li)
        return lis
      scrollPosition <- holdDyn 0 $ domEvent Scroll container
      window <- mapDyn (findWindow heightPx rowPx) scrollPosition
      itemsInWindow <- combineDyn (\(_,(idx,num)) is -> Map.fromList $ take num $ drop idx $ Map.toList is) window items
  postBuild <- getPostBuild
  performEvent_ $ fmap (\i -> liftIO $ elementSetScrollTop (_el_element container) (i * rowPx)) $ leftmost [setI, fmap (const i0) postBuild]
  indexAndLength <- mapDyn snd window
  sel <- mapDyn (leftmost . Map.elems) itemList
  return (indexAndLength, switch $ current sel)
  where
    toStyleAttr m = "style" =: (Map.foldWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m)
    outerStyle = toStyleAttr $ "position" =: "relative" <>
                               "height" =: (show heightPx <> "px")
    containerStyle = toStyleAttr $ "overflow" =: "auto" <>
                                   "position" =: "absolute" <>
                                   "left" =: "0" <>
                                   "right" =: "0" <>
                                   "height" =: (show heightPx <> "px")
    listWrapperStyle t = toStyleAttr $ "position" =: "relative" <>
                                       "top" =: (show t <> "px")
    toHeightStyle h = toStyleAttr ("height" =: (show h <> "px"))
    tagWrapper elTag attrs attrsOverride c = do
      attrs' <- combineDyn Map.union attrsOverride attrs
      elDynAttr' elTag attrs' c
    findWindow windowSize sizeIncrement startingPosition =
      let (startingIndex, topOffsetPx) = startingPosition `divMod'` sizeIncrement
          topPx = startingPosition - topOffsetPx
          numItems = windowSize `div` sizeIncrement + 1
          preItems = min startingIndex numItems
      in (topPx - preItems * sizeIncrement, (startingIndex - preItems, preItems + numItems * 2))

