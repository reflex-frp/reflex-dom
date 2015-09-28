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
  => Dynamic t Int -- ^ The height of the visible region in pixels
  -> Int -- ^ The height of each row in pixels
  -> Dynamic t Int -- ^ The total number of items
  -> Int -- ^ The index of the row to scroll to on initialization
  -> Event t Int -- ^ An 'Event' containing a row index. Used to scroll to the given index.
  -> String -- ^ The element tag for the list
  -> Dynamic t (Map String String) -- ^ The attributes of the list
  -> String -- ^ The element tag for a row
  -> Dynamic t (Map String String) -- ^ The attributes of each row
  -> (k -> Dynamic t (Maybe v) -> Dynamic t Bool -> m ()) -- ^ The row child element builder
  -> Dynamic t (Map k v) -- ^ The 'Map' of items
  -> (Int -> k) -- ^ Index to Key function, used to determine position of Map elements
  -> m (Dynamic t (Int, Int), Event t k) -- ^ A tuple containing: a 'Dynamic' of the index (based on the current scroll position) and number of items currently being rendered, and an 'Event' of the selected key
virtualListWithSelection heightPx rowPx maxIndex i0 setI listTag listAttrs rowTag rowAttrs itemBuilder items indexToKey = do
  totalHeightStyle <- mapDyn (toHeightStyle . (*) rowPx) maxIndex
  containerStyle <- mapDyn toContainer heightPx
  viewportStyle <- mapDyn toViewport heightPx
  rec (container, sel) <- elDynAttr "div" containerStyle $ elDynAttr' "div" viewportStyle $ do
        currentTop <- mapDyn (listWrapperStyle . fst) window
        (_, lis) <- elDynAttr "div" totalHeightStyle $ tagWrapper listTag listAttrs currentTop $ selectViewListWithKey_ selected itemsInWindow $ \k v s -> do
            (li,_) <- tagWrapper rowTag rowAttrs (constDyn $ toHeightStyle rowPx) $ itemBuilder k v s
            return $ fmap (const k) (domEvent Click li)
        return lis
      selected <- holdDyn (indexToKey i0) sel
      pb <- getPostBuild
      scrollPosition <- holdDyn 0 $ leftmost [ domEvent Scroll container
                                             , fmap (const (i0 * rowPx)) pb
                                             ]
      window <- combineDyn (\h -> findWindow h rowPx) heightPx scrollPosition
      itemsInWindow <- combineDyn (\(_,(idx,num)) is -> Map.fromList $ map (\i -> let ix = indexToKey i in (ix, Map.lookup ix is)) [idx .. idx + num]) window items
  postBuild <- getPostBuild
  performEvent_ $ fmap (\i -> liftIO $ elementSetScrollTop (_el_element container) (i * rowPx)) $ leftmost [setI, fmap (const i0) postBuild]
  indexAndLength <- mapDyn snd window
  return (indexAndLength, sel)
  where
    toStyleAttr m = "style" =: (Map.foldWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m)
    toViewport h = toStyleAttr $ "overflow" =: "auto" <> "position" =: "absolute" <>
                                 "left" =: "0" <> "right" =: "0" <> "height" =: (show h <> "px")
    toContainer h = toStyleAttr $ "position" =: "relative" <> "height" =: (show h <> "px")
    listWrapperStyle t = toStyleAttr $ "position" =: "relative" <>
                                       "top" =: (show t <> "px")
    toHeightStyle h = toStyleAttr ("height" =: (show h <> "px") <> "overflow" =: "hidden")
    tagWrapper elTag attrs attrsOverride c = do
      attrs' <- combineDyn Map.union attrsOverride attrs
      elDynAttr' elTag attrs' c
    findWindow windowSize sizeIncrement startingPosition =
      let (startingIndex, topOffsetPx) = startingPosition `divMod'` sizeIncrement
          topPx = startingPosition - topOffsetPx
          numItems = windowSize `div` sizeIncrement + 1
          preItems = min startingIndex numItems
      in (topPx - preItems * sizeIncrement, (startingIndex - preItems, preItems + numItems * 2))


