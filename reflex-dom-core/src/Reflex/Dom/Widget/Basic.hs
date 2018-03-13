{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Dom.Widget.Basic
  ( partitionMapBySetLT
  , ChildResult (..)

  -- * Displaying Values
  , text
  , dynText
  , display
  , button
  , dyn
  , dyn_
  , widgetHold
  , widgetHold_

  -- * Creating DOM Elements
  , el
  , elAttr
  , elClass
  , elDynAttr
  , elDynClass
  , elDynAttrNS

  -- ** With Element Results
  , el'
  , elAttr'
  , elClass'
  , elDynAttr'
  , elDynClass'
  , elDynAttrNS'
  , dynamicAttributesToModifyAttributes
  , dynamicAttributesToModifyAttributesWithInitial

  -- * Specific DOM Elements
  , Link (..)
  , linkClass
  , link
  , divClass
  , dtdd
  , blank

  -- * Tables and Lists
  , tableDynAttr
  , tabDisplay

  , HasAttributes (..)
  , module Data.Map.Misc
  , module Reflex.Collection
  , module Reflex.Workflow
  ) where

import Reflex.Class
import Reflex.Collection
import Reflex.Dom.Builder.Class
import Reflex.Dom.Class
import Reflex.Dynamic
import Reflex.Network
import Reflex.NotReady.Class
import Reflex.PostBuild.Class
import Reflex.Workflow
import GHCJS.DOM.Debug (DomHasCallStack)

import Control.Arrow
import Control.Lens hiding (children, element)
import Control.Monad.Reader hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Data.Align
import Data.Default
import Data.Either
import Data.Foldable
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Misc
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.These
import Data.Traversable
import Prelude hiding (mapM, mapM_, sequence, sequence_)

-- | Breaks the given Map into pieces based on the given Set.  Each piece will contain only keys that are less than the key of the piece, and greater than or equal to the key of the piece with the next-smaller key.  There will be one additional piece containing all keys from the original Map that are larger or equal to the largest key in the Set.
-- Either k () is used instead of Maybe k so that the resulting map of pieces is sorted so that the additional piece has the largest key.
-- No empty pieces will be included in the output.

--TODO: This can probably be done more efficiently by dividing and conquering, re-using the structure of the Set instead of going through the Set linearally
partitionMapBySetLT :: forall k v. Ord k => Set k -> Map k v -> Map (Either k ()) (Map k v)
partitionMapBySetLT s m0 = Map.fromDistinctAscList $ go (Set.toAscList s) m0
  where go :: [k] -> Map k v -> [(Either k (), Map k v)]
        go [] m = if Map.null m
                  then []
                  else [(Right (), m)]
        go (h:t) m = let (lt, eq, gt) = Map.splitLookup h m
                         geq = maybe id (Map.insert h) eq gt
                     in if Map.null lt
                        then go t geq
                        else (Left h, lt) : go t geq

newtype ChildResult t k a = ChildResult { unChildResult :: (a, Event t (Map k (Maybe (ChildResult t k a)))) }

text :: (DomBuilder t m, DomHasCallStack) => Text -> m ()
text t = void $ textNode $ def & textNodeConfig_initialContents .~ t

{-# INLINABLE dynText #-}
dynText :: forall t m. (PostBuild t m, DomBuilder t m, DomHasCallStack) => Dynamic t Text -> m ()
dynText t = do
  postBuild <- getPostBuild
  void $ textNode $ (def :: TextNodeConfig t) & textNodeConfig_setContents .~ leftmost
    [ updated t
    , tag (current t) postBuild
    ]
  notReadyUntil postBuild

display :: (PostBuild t m, DomBuilder t m, Show a, DomHasCallStack) => Dynamic t a -> m ()
display = dynText . fmap (T.pack . show)

button :: (DomBuilder t m, DomHasCallStack) => Text -> m (Event t ())
button t = do
  (e, _) <- element "button" def $ text t
  return $ domEvent Click e

--TODO: Should this be renamed to 'widgetView' for consistency with 'widgetHold'?
-- | Given a Dynamic of widget-creating actions, create a widget that is recreated whenever the Dynamic updates.
--   The returned Event of widget results occurs when the Dynamic does.
--   Note:  Often, the type 'a' is an Event, in which case the return value is an Event-of-Events that would typically be flattened (via 'switchPromptly').
dyn :: (DomBuilder t m, PostBuild t m) => Dynamic t (m a) -> m (Event t a)
dyn = networkView

-- | Like 'dyn' but discards result.
dyn_ :: (DomBuilder t m, PostBuild t m) => Dynamic t (m a) -> m ()
dyn_ = void . dyn

-- | Given an initial widget and an Event of widget-creating actions, create a widget that is recreated whenever the Event fires.
--   The returned Dynamic of widget results occurs when the Event does.
--   Note:  Often, the type 'a' is an Event, in which case the return value is a Dynamic-of-Events that would typically be flattened.
widgetHold :: (DomBuilder t m, MonadHold t m) => m a -> Event t (m a) -> m (Dynamic t a)
widgetHold = networkHold

-- | Like 'widgetHold' but discards result.
widgetHold_ :: (DomBuilder t m, MonadHold t m) => m a -> Event t (m a) -> m ()
widgetHold_ z = void . widgetHold z

-- | Create a DOM element
-- > el "div" (text "Hello World")
-- <div>Hello World</div>
{-# INLINABLE el #-}
el :: forall t m a. (DomBuilder t m, DomHasCallStack) => Text -> m a -> m a
el elementTag child = snd <$> el' elementTag child

-- | Create a DOM element with attributes
-- > elAttr "a" ("href" =: "http://google.com") (text "Google!")
-- <a href="http://google.com">Google!</a>
{-# INLINABLE elAttr #-}
elAttr :: forall t m a. (DomBuilder t m, DomHasCallStack) => Text -> Map Text Text -> m a -> m a
elAttr elementTag attrs child = snd <$> elAttr' elementTag attrs child

-- | Create a DOM element with classes
-- > elClass "div" "row" (return ())
-- <div class="row"></div>
{-# INLINABLE elClass #-}
elClass :: forall t m a. (DomBuilder t m, DomHasCallStack) => Text -> Text -> m a -> m a
elClass elementTag c child = snd <$> elClass' elementTag c child

-- | Create a DOM element with Dynamic Attributes
-- > elClass "div" (constDyn ("class" =: "row")) (return ())
-- <div class="row"></div>
{-# INLINABLE elDynAttr #-}
elDynAttr :: forall t m a. (DomBuilder t m, PostBuild t m, DomHasCallStack) => Text -> Dynamic t (Map Text Text) -> m a -> m a
elDynAttr elementTag attrs child = snd <$> elDynAttr' elementTag attrs child

-- | Create a DOM element with a Dynamic Class
-- > elDynClass "div" (constDyn "row") (return ())
-- <div class="row"></div>
{-# INLINABLE elDynClass #-}
elDynClass :: forall t m a. (DomBuilder t m, PostBuild t m, DomHasCallStack) => Text -> Dynamic t Text -> m a -> m a
elDynClass elementTag c child = snd <$> elDynClass' elementTag c child

-- | Create a DOM element and return the element
-- > do (e, _) <- el' "div" (text "Click")
-- >    return $ domEvent Click e
{-# INLINABLE el' #-}
el' :: forall t m a. (DomBuilder t m, DomHasCallStack) => Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
el' elementTag = element elementTag def

-- | Create a DOM element with attributes and return the element
{-# INLINABLE elAttr' #-}
elAttr' :: forall t m a. (DomBuilder t m, DomHasCallStack) => Text -> Map Text Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elAttr' elementTag attrs = element elementTag $ def
  & initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs

-- | Create a DOM element with a class and return the element
{-# INLINABLE elClass' #-}
elClass' :: forall t m a. (DomBuilder t m, DomHasCallStack) => Text -> Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elClass' elementTag c = elAttr' elementTag ("class" =: c)

-- | Create a DOM element with Dynamic Attributes and return the element
{-# INLINABLE elDynAttr' #-}
elDynAttr' :: forall t m a. (DomBuilder t m, PostBuild t m, DomHasCallStack) => Text -> Dynamic t (Map Text Text) -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynAttr' = elDynAttrNS' Nothing

-- | Create a DOM element with a Dynamic class and return the element
{-# INLINABLE elDynClass' #-}
elDynClass' :: forall t m a. (DomBuilder t m, PostBuild t m, DomHasCallStack) => Text -> Dynamic t Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynClass' elementTag c = elDynAttr' elementTag (fmap ("class" =:) c)

{-# INLINABLE elDynAttrNS' #-}
elDynAttrNS' :: forall t m a. (DomBuilder t m, PostBuild t m, DomHasCallStack) => Maybe Text -> Text -> Dynamic t (Map Text Text) -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynAttrNS' mns elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg = def
        & elementConfig_namespace .~ mns
        & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
  result <- element elementTag cfg child
  postBuild <- getPostBuild
  notReadyUntil postBuild
  return result

{-# INLINABLE elDynAttrNS #-}
elDynAttrNS :: forall t m a. (DomBuilder t m, PostBuild t m, DomHasCallStack) => Maybe Text -> Text -> Dynamic t (Map Text Text) -> m a -> m a
elDynAttrNS mns elementTag attrs child = fmap snd $ elDynAttrNS' mns elementTag attrs child

dynamicAttributesToModifyAttributes :: (Ord k, PostBuild t m) => Dynamic t (Map k Text) -> m (Event t (Map k (Maybe Text)))
dynamicAttributesToModifyAttributes = dynamicAttributesToModifyAttributesWithInitial mempty

dynamicAttributesToModifyAttributesWithInitial :: (Ord k, PostBuild t m) => Map k Text -> Dynamic t (Map k Text) -> m (Event t (Map k (Maybe Text)))
dynamicAttributesToModifyAttributesWithInitial attrs0 d = do
  postBuild <- getPostBuild
  let modificationsNeeded = flip push (align postBuild $ updated d) $ \x -> do
        p <- case x of
          This () -> do
            new <- sample $ current d
            return $ diffMap attrs0 new
          These () new -> return $ diffMap attrs0 new
          That new -> do
            old <- sample $ current d
            return $ diffMap old new
        return $ if Map.null p then Nothing else Just p
  return modificationsNeeded

--------------------------------------------------------------------------------
-- Copied and pasted from Reflex.Widget.Class
--------------------------------------------------------------------------------

{-
schedulePostBuild x = performEvent_ . (x <$) =<< getPostBuild

elDynHtml' :: DomBuilder t m => Text -> Dynamic t Text -> m (El t)
elDynHtml' elementTag html = do
  e <- buildEmptyElement elementTag (Map.empty :: Map Text Text)
  schedulePostBuild $ setInnerHTML e . Just =<< sample (current html)
  performEvent_ $ fmap (setInnerHTML e . Just) $ updated html
  wrapElement defaultDomEventHandler e

elDynHtmlAttr' :: DomBuilder t m => Text -> Map Text Text -> Dynamic t Text -> m (El t)
elDynHtmlAttr' elementTag attrs html = do
  e <- buildEmptyElement elementTag attrs
  schedulePostBuild $ setInnerHTML e . Just =<< sample (current html)
  performEvent_ $ fmap (setInnerHTML e . Just) $ updated html
  wrapElement defaultDomEventHandler e
-}

newtype Link t
  = Link { _link_clicked :: Event t ()
         }

linkClass :: (DomBuilder t m, DomHasCallStack) => Text -> Text -> m (Link t)
linkClass s c = do
  (l,_) <- elAttr' "a" ("class" =: c) $ text s
  return $ Link $ domEvent Click l

link :: (DomBuilder t m, DomHasCallStack) => Text -> m (Link t)
link s = linkClass s ""

divClass :: forall t m a. (DomBuilder t m, DomHasCallStack) => Text -> m a -> m a
divClass = elClass "div"

dtdd :: forall t m a. (DomBuilder t m, DomHasCallStack) => Text -> m a -> m a
dtdd h w = do
  el "dt" $ text h
  el "dd" w

blank :: forall m. Monad m => m ()
blank = return ()

-- | A widget to display a table with static columns and dynamic rows.
tableDynAttr :: forall t m r k v. (Ord k, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, DomHasCallStack)
  => Text                                   -- ^ Class applied to <table> element
  -> [(Text, k -> Dynamic t r -> m v)]      -- ^ Columns of (header, row key -> row value -> child widget)
  -> Dynamic t (Map k r)                      -- ^ Map from row key to row value
  -> (k -> m (Dynamic t (Map Text Text))) -- ^ Function to compute <tr> element attributes from row key
  -> m (Dynamic t (Map k (Element EventResult (DomBuilderSpace m) t, [v])))        -- ^ Map from row key to (El, list of widget return values)
tableDynAttr klass cols dRows rowAttrs = elAttr "div" (Map.singleton "style" "zoom: 1; overflow: auto; background: white;") $
    elAttr "table" (Map.singleton "class" klass) $ do
      el "thead" $ el "tr" $
        mapM_ (\(h, _) -> el "th" $ text h) cols
      el "tbody" $
        listWithKey dRows (\k r -> do
          dAttrs <- rowAttrs k
          elDynAttr' "tr" dAttrs $ mapM (\x -> el "td" $ snd x k r) cols)

-- | A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
tabDisplay :: forall t m k. (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, Ord k, DomHasCallStack)
  => Text               -- ^ Class applied to <ul> element
  -> Text               -- ^ Class applied to currently active <li> element
  -> Map k (Text, m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
tabDisplay ulClass activeClass tabItems = do
  let t0 = listToMaybe $ Map.keys tabItems
  rec currentTab :: Demux t (Maybe k) <- elAttr "ul" ("class" =: ulClass) $ do
        tabClicksList :: [Event t k] <- Map.elems <$> imapM (\k (s,_) -> headerBarLink s k $ demuxed currentTab (Just k)) tabItems
        let eTabClicks :: Event t k = leftmost tabClicksList
        fmap demux $ holdDyn t0 $ fmap Just eTabClicks
  el "div" $ do
    iforM_ tabItems $ \k (_, w) -> do
      let isSelected = demuxed currentTab $ Just k
          attrs = ffor isSelected $ \s -> if s then Map.empty else Map.singleton "style" "display:none;"
      elDynAttr "div" attrs w
    return ()
  where
    headerBarLink :: Text -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k isSelected = do
      let attrs = fmap (\b -> if b then Map.singleton "class" activeClass else Map.empty) isSelected
      elDynAttr "li" attrs $ do
        a <- link x
        return $ fmap (const k) (_link_clicked a)

class HasAttributes a where
  type Attrs a :: *
  attributes :: Lens' a (Attrs a)
