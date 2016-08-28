{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Dom.Widget.Lazy where

import Reflex.Class
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Class
import Reflex.Dom.Widget.Basic
import Reflex.Dynamic
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Fixed
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM.Element

-- |A list view for long lists. Creates a scrollable element and only renders child row elements near the current scroll position.
virtualListWithSelection :: forall t m k v. (DomBuilder t m, PostBuild t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m), DomBuilderSpace m ~ GhcjsDomSpace, MonadFix m, Ord k)
  => Dynamic t Int -- ^ The height of the visible region in pixels
  -> Int -- ^ The height of each row in pixels
  -> Dynamic t Int -- ^ The total number of items
  -> Int -- ^ The index of the row to scroll to on initialization
  -> Event t Int -- ^ An 'Event' containing a row index. Used to scroll to the given index.
  -> Text -- ^ The element tag for the list
  -> Dynamic t (Map Text Text) -- ^ The attributes of the list
  -> Text -- ^ The element tag for a row
  -> Dynamic t (Map Text Text) -- ^ The attributes of each row
  -> (k -> Dynamic t (Maybe v) -> Dynamic t Bool -> m ()) -- ^ The row child element builder
  -> Dynamic t (Map k v) -- ^ The 'Map' of items
  -> (Int -> k) -- ^ Index to Key function, used to determine position of Map elements
  -> m (Dynamic t (Int, Int), Event t k) -- ^ A tuple containing: a 'Dynamic' of the index (based on the current scroll position) and number of items currently being rendered, and an 'Event' of the selected key
virtualListWithSelection heightPx rowPx maxIndex i0 setI listTag listAttrs rowTag rowAttrs itemBuilder items indexToKey = do
  let totalHeightStyle = fmap (toHeightStyle . (*) rowPx) maxIndex
      containerStyle = fmap toContainer heightPx
      viewportStyle = fmap toViewport heightPx
  rec (container, sel) <- elDynAttr "div" containerStyle $ elDynAttr' "div" viewportStyle $ do
        let currentTop = fmap (listWrapperStyle . fst) window
        (_, lis) <- elDynAttr "div" totalHeightStyle $ tagWrapper listTag listAttrs currentTop $ selectViewListWithKey_ selected itemsInWindow $ \k v s -> do
            (li,_) <- tagWrapper rowTag rowAttrs (constDyn $ toHeightStyle rowPx) $ itemBuilder k v s
            return $ fmap (const k) (domEvent Click li)
        return lis
      selected <- holdDyn (indexToKey i0) sel
      pb <- getPostBuild
      scrollPosition <- holdDyn 0 $ leftmost [ domEvent Scroll container
                                             , fmap (const (i0 * rowPx)) pb
                                             ]
      let window = zipDynWith (findWindow rowPx) heightPx scrollPosition
          itemsInWindow = zipDynWith (\(_,(idx,num)) is -> Map.fromList $ map (\i -> let ix = indexToKey i in (ix, Map.lookup ix is)) [idx .. idx + num]) window items
  postBuild <- getPostBuild
  performEvent_ $ ffor (leftmost [setI, i0 <$ postBuild]) $ \i -> do
    liftIO $ setScrollTop (_element_raw container) (i * rowPx)
  let indexAndLength = fmap snd window
  return (indexAndLength, sel)
  where
    toStyleAttr m = "style" =: Map.foldWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m
    toViewport h = toStyleAttr $ "overflow" =: "auto" <> "position" =: "absolute" <>
                                 "left" =: "0" <> "right" =: "0" <> "height" =: (T.pack (show h) <> "px")
    toContainer h = toStyleAttr $ "position" =: "relative" <> "height" =: (T.pack (show h) <> "px")
    listWrapperStyle t = toStyleAttr $ "position" =: "relative" <>
                                       "top" =: (T.pack (show t) <> "px")
    toHeightStyle h = toStyleAttr ("height" =: (T.pack (show h) <> "px") <> "overflow" =: "hidden")
    tagWrapper elTag attrs attrsOverride c = do
      let attrs' = zipDynWith Map.union attrsOverride attrs
      elDynAttr' elTag attrs' c
    findWindow sizeIncrement windowSize startingPosition =
      let (startingIndex, topOffsetPx) = startingPosition `divMod'` sizeIncrement
          topPx = startingPosition - topOffsetPx
          numItems = windowSize `div` sizeIncrement + 1
          preItems = min startingIndex numItems
      in (topPx - preItems * sizeIncrement, (startingIndex - preItems, preItems + numItems * 2))

virtualList :: forall t m k v a. (DomBuilder t m, PostBuild t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m), DomBuilderSpace m ~ GhcjsDomSpace, MonadFix m, Ord k)
  => Dynamic t Int -- ^ A 'Dynamic' of the visible region's height in pixels
  -> Int -- ^ The fixed height of each row in pixels
  -> Dynamic t Int -- ^ A 'Dynamic' of the total number of items
  -> Int -- ^ The index of the row to scroll to on initialization
  -> Event t Int -- ^ An 'Event' containing a row index. Used to scroll to the given index.
  -> (k -> Int) -- ^ Key to Index function, used to position items.
  -> Map k v -- ^ The initial 'Map' of items
  -> Event t (Map k (Maybe v)) -- ^ The update 'Event'. Nothing values are removed from the list and Just values are added or updated.
  -> (k -> v -> Event t v -> m a) -- ^ The row child element builder.
  -> m (Dynamic t (Int, Int), Dynamic t (Map k a)) -- ^ A tuple containing: a 'Dynamic' of the index (based on the current scroll position) and number of items currently being rendered, and the 'Dynamic' list result
virtualList heightPx rowPx maxIndex i0 setI keyToIndex items0 itemsUpdate itemBuilder = do
  let virtualH = fmap (mkVirtualHeight . (*) rowPx) maxIndex
      containerStyle = fmap mkContainer heightPx
      viewportStyle = fmap mkViewport heightPx
  pb <- getPostBuild
  rec (viewport, result) <- elDynAttr "div" containerStyle $ elDynAttr' "div" viewportStyle $ elDynAttr "div" virtualH $
        listWithKeyShallowDiff items0 itemsUpdate $ \k v e -> elAttr "div" (mkRow k) $ itemBuilder k v e
      scrollPosition <- holdDyn 0 $ leftmost [ domEvent Scroll viewport
                                             , fmap (const (i0 * rowPx)) pb
                                             ]
      let window = zipDynWith (findWindow rowPx) heightPx scrollPosition
  performEvent_ $ ffor (leftmost [setI, i0 <$ pb]) $ \i -> do
    liftIO $ setScrollTop (_element_raw viewport) (i * rowPx)
  return (uniqDyn window, result)
  where
    toStyleAttr m = "style" =: Map.foldWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m
    mkViewport h = toStyleAttr $ "overflow" =: "auto" <> "position" =: "absolute" <>
                                 "left" =: "0" <> "right" =: "0" <> "height" =: (T.pack (show h) <> "px")
    mkContainer h = toStyleAttr $ "position" =: "relative" <> "height" =: (T.pack (show h) <> "px")
    mkVirtualHeight h = let h' = h * rowPx --TODO: test the use of this
                        in toStyleAttr $ "height" =: (T.pack (show h') <> "px") <>
                                         "overflow" =: "hidden" <>
                                         "position" =: "relative"
    mkRow k = toStyleAttr $ "height" =: (T.pack (show rowPx) <> "px") <>
                            "top" =: ((<>"px") $ T.pack $ show $ keyToIndex k * rowPx) <>
                            "position" =: "absolute" <>
                            "width" =: "100%"
    findWindow sizeIncrement windowSize startingPosition =
      let (startingIndex, _) = startingPosition `divMod'` sizeIncrement
          numItems = (windowSize + sizeIncrement - 1) `div` sizeIncrement
      in (startingIndex, numItems)

virtualListBuffered
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m), DomBuilderSpace m ~ GhcjsDomSpace, MonadFix m, Ord k)
  => Int
  -> Dynamic t Int
  -> Int
  -> Dynamic t Int
  -> Int
  -> Event t Int
  -> (k -> Int)
  -> Map k v
  -> Event t (Map k (Maybe v))
  -> (k -> v -> Event t v -> m a)
  -> m (Event t (Int, Int), Dynamic t (Map k a))
virtualListBuffered buffer heightPx rowPx maxIndex i0 setI keyToIndex items0 itemsUpdate itemBuilder = do
    (win, m) <- virtualList heightPx rowPx maxIndex i0 setI keyToIndex items0 itemsUpdate itemBuilder
    pb <- getPostBuild
    let extendWin o l = (max 0 (o - l * (buffer-1) `div` 2), l * buffer)
    rec let winHitEdge = fmapMaybe id $ attachWith (\(oldOffset, oldLimit) (winOffset, winLimit) ->
              if winOffset > oldOffset && winOffset + winLimit < oldOffset + oldLimit
                 then Nothing
                 else Just (extendWin winOffset winLimit)) (current winBuffered) (updated win)
        winBuffered <- holdDyn (0, 0) $ leftmost [ winHitEdge
                                                 , attachPromptlyDynWith (\(x, y) _ -> extendWin x y) win pb
                                                 ]
    return (updated winBuffered, m)
