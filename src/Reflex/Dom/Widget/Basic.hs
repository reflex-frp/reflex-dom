{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Dom.Widget.Basic
  ( partitionMapBySetLT
  , listHoldWithKey
  , ChildResult (..)

  -- * Displaying Values
  , text
  , dynText
  , display
  , button
  , dyn
  , widgetHold

  -- * Working with Maps
  , diffMapNoEq
  , diffMap
  , applyMap
  , mapPartitionEithers
  , applyMapKeysSet

  -- * Widgets on Collections
  , listWithKey
  , listWithKey'
  , listWithKeyShallowDiff
  , listViewWithKey
  , selectViewListWithKey
  , selectViewListWithKey_

  -- * Creating DOM Elements
  , el
  , elAttr
  , elClass
  , elDynAttr
  , elDynClass

  -- ** With Element Results
  , el'
  , elAttr'
  , elClass'
  , elDynAttr'
  , elDynClass'
  , elDynAttrNS'
  , dynamicAttributesToModifyAttributes

  -- * List Utils
  , list
  , simpleList

  -- * Specific DOM Elements
  , Link (..)
  , linkClass
  , link
  , divClass
  , dtdd
  , blank

  -- * Workflows
  , Workflow (..)
  , workflow
  , workflowView
  , mapWorkflow

  -- * Tables and Lists
  , tableDynAttr
  , tabDisplay

  , HasAttributes (..)
  ) where

import Reflex.Class
import Reflex.Dom.Builder.Class
import Reflex.Dom.Class
import Reflex.Dom.Internal.Foreign ()
import Reflex.Dynamic
import Reflex.PostBuild.Class

import Control.Arrow
import Control.Lens hiding (children, element)
import Control.Monad.Reader hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Data.Align
import Data.Default
import Data.Either
import Data.Foldable
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.These
import Data.Traversable
import Prelude hiding (mapM, mapM_, sequence, sequence_)

widgetHoldInternal :: forall t m a b. DomBuilder t m => m a -> Event t (m b) -> m (a, Event t b)
widgetHoldInternal = runWithReplace

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

listHoldWithKey :: forall t m k v a. (Ord k, DomBuilder t m, MonadHold t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (Dynamic t (Map k a))
listHoldWithKey m0 m' f = do
  let dm0 = mapWithFunctorToDMap $ Map.mapWithKey f m0
      dm' = fmap (PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k v -> ComposeMaybe $ fmap (f k) v)) m'
  (a0, a') <- sequenceDMapWithAdjust dm0 dm'
  fmap dmapToMap . incrementalToDynamic <$> holdIncremental a0 a' --TODO: Move the dmapToMap to the righthand side so it doesn't get fully redone every time

text :: DomBuilder t m => Text -> m ()
text t = void $ textNode $ def & textNodeConfig_initialContents .~ t

dynText :: forall t m. (PostBuild t m, DomBuilder t m) => Dynamic t Text -> m ()
dynText t = do
  postBuild <- getPostBuild
  void $ textNode $ (def :: TextNodeConfig t) & textNodeConfig_setContents .~ leftmost
    [ updated t
    , tag (current t) postBuild
    ]

display :: (PostBuild t m, DomBuilder t m, Show a) => Dynamic t a -> m ()
display = dynText . fmap (T.pack . show)

button :: DomBuilder t m => Text -> m (Event t ())
button t = do
  (e, _) <- element "button" def $ text t
  return $ domEvent Click e

--TODO: Should this be renamed to 'widgetView' for consistency with 'widgetHold'?
-- | Given a Dynamic of widget-creating actions, create a widget that is recreated whenever the Dynamic updates.
--   The returned Event of widget results occurs when the Dynamic does.
--   Note:  Often, the type 'a' is an Event, in which case the return value is an Event-of-Events that would typically be flattened (via 'switchPromptly').
dyn :: (DomBuilder t m, PostBuild t m) => Dynamic t (m a) -> m (Event t a)
dyn child = do
  postBuild <- getPostBuild
  let newChild = leftmost [updated child, tag (current child) postBuild]
  snd <$> widgetHoldInternal (return ()) newChild

-- | Given an initial widget and an Event of widget-creating actions, create a widget that is recreated whenever the Event fires.
--   The returned Dynamic of widget results occurs when the Event does.
--   Note:  Often, the type 'a' is an Event, in which case the return value is a Dynamic-of-Events that would typically be flattened.
widgetHold :: (DomBuilder t m, MonadHold t m) => m a -> Event t (m a) -> m (Dynamic t a)
widgetHold child0 newChild = do
  (result0, newResult) <- widgetHoldInternal child0 newChild
  holdDyn result0 newResult

diffMapNoEq :: (Ord k) => Map k v -> Map k v -> Map k (Maybe v)
diffMapNoEq olds news = flip Map.mapMaybe (align olds news) $ \case
  This _ -> Just Nothing
  These _ new -> Just $ Just new
  That new -> Just $ Just new

diffMap :: (Ord k, Eq v) => Map k v -> Map k v -> Map k (Maybe v)
diffMap olds news = flip Map.mapMaybe (align olds news) $ \case
  This _ -> Just Nothing
  These old new
    | old == new -> Nothing
    | otherwise -> Just $ Just new
  That new -> Just $ Just new

applyMap :: Ord k => Map k (Maybe v) -> Map k v -> Map k v
applyMap patch old = insertions `Map.union` (old `Map.difference` deletions)
  where (deletions, insertions) = mapPartitionEithers $ maybeToEither <$> patch
        maybeToEither = \case
          Nothing -> Left ()
          Just r -> Right r

mapPartitionEithers :: Map k (Either a b) -> (Map k a, Map k b)
mapPartitionEithers m = (fromLeft <$> ls, fromRight <$> rs)
  where (ls, rs) = Map.partition isLeft m
        fromLeft (Left l) = l
        fromLeft _ = error "mapPartitionEithers: fromLeft received a Right value; this should be impossible"
        fromRight (Right r) = r
        fromRight _ = error "mapPartitionEithers: fromRight received a Left value; this should be impossible"

-- | Apply a map patch to a set
-- > applyMapKeysSet patch (Map.keysSet m) == Map.keysSet (applyMap patch m)
applyMapKeysSet :: Ord k => Map k (Maybe v) -> Set k -> Set k
applyMapKeysSet patch old = Map.keysSet insertions `Set.union` (old `Set.difference` Map.keysSet deletions)
  where (insertions, deletions) = Map.partition isJust patch

--TODO: Something better than Dynamic t (Map k v) - we want something where the Events carry diffs, not the whole value
listWithKey :: forall t k v m a. (Ord k, DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
listWithKey vals mkChild = do
  postBuild <- getPostBuild
  let childValChangedSelector = fanMap $ updated vals
  rec sentVals :: Dynamic t (Map k v) <- foldDyn applyMap Map.empty changeVals
      let changeVals :: Event t (Map k (Maybe v))
          changeVals = attachWith diffMapNoEq (current sentVals) $ leftmost
                         [ updated vals
                         , tag (current vals) postBuild --TODO: This should probably be added to the attachWith, not to the updated; if we were using diffMap instead of diffMapNoEq, I think it might not work
                         ]
  listHoldWithKey Map.empty changeVals $ \k v ->
    mkChild k =<< holdDyn v (select childValChangedSelector $ Const2 k)

{-# DEPRECATED listWithKey' "listWithKey' has been renamed to listWithKeyShallowDiff; also, its behavior has changed to fix a bug where children were always rebuilt (never updated)" #-}
listWithKey' :: (Ord k, DomBuilder t m, MonadFix m, MonadHold t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))
listWithKey' = listWithKeyShallowDiff

-- | Display the given map of items (in key order) using the builder function provided, and update it with the given event.  'Nothing' update entries will delete the corresponding children, and 'Just' entries will create them if they do not exist or send an update event to them if they do.
listWithKeyShallowDiff :: (Ord k, DomBuilder t m, MonadFix m, MonadHold t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))
listWithKeyShallowDiff initialVals valsChanged mkChild = do
  let childValChangedSelector = fanMap $ fmap (Map.mapMaybe id) valsChanged
  sentVals <- foldDyn applyMap Map.empty $ fmap (fmap void) valsChanged
  let relevantPatch patch _ = case patch of
        Nothing -> Just Nothing -- Even if we let a Nothing through when the element doesn't already exist, this doesn't cause a problem because it is ignored
        Just _ -> Nothing -- We don't want to let spurious re-creations of items through
  listHoldWithKey initialVals (attachWith (flip (Map.differenceWith relevantPatch)) (current sentVals) valsChanged) $ \k v ->
    mkChild k v $ select childValChangedSelector $ Const2 k

--TODO: Something better than Dynamic t (Map k v) - we want something where the Events carry diffs, not the whole value
-- | Create a dynamically-changing set of Event-valued widgets.
--   This is like listWithKey, specialized for widgets returning (Event t a).  listWithKey would return 'Dynamic t (Map k (Event t a))' in this scenario, but listViewWithKey flattens this to 'Event t (Map k a)' via 'switch'.
listViewWithKey :: (Ord k, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m (Event t a)) -> m (Event t (Map k a))
listViewWithKey vals mkChild = switch . fmap mergeMap <$> listViewWithKey' vals mkChild

listViewWithKey' :: (Ord k, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Behavior t (Map k a))
listViewWithKey' vals mkChild = current <$> listWithKey vals mkChild

-- | Create a dynamically-changing set of widgets, one of which is selected at any time.
selectViewListWithKey :: forall t m k v a. (DomBuilder t m, Ord k, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t k          -- ^ Current selection key
  -> Dynamic t (Map k v)  -- ^ Dynamic key/value map
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -- ^ Function to create a widget for a given key from Dynamic value and Dynamic Bool indicating if this widget is currently selected
  -> m (Event t (k, a))        -- ^ Event that fires when any child's return Event fires.  Contains key of an arbitrary firing widget.
selectViewListWithKey selection vals mkChild = do
  let selectionDemux = demux selection -- For good performance, this value must be shared across all children
  selectChild <- listWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux k
    selectSelf <- mkChild k v selected
    return $ fmap ((,) k) selectSelf
  return $ switchPromptlyDyn $ leftmost . Map.elems <$> selectChild

selectViewListWithKey_ :: forall t m k v a. (DomBuilder t m, Ord k, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t k          -- ^ Current selection key
  -> Dynamic t (Map k v)  -- ^ Dynamic key/value map
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -- ^ Function to create a widget for a given key from Dynamic value and Dynamic Bool indicating if this widget is currently selected
  -> m (Event t k)        -- ^ Event that fires when any child's return Event fires.  Contains key of an arbitrary firing widget.
selectViewListWithKey_ selection vals mkChild = fmap fst <$> selectViewListWithKey selection vals mkChild

-- | Create a DOM element
-- > el "div" (text "Hello World")
-- <div>Hello World</div>
{-# INLINABLE el #-}
el :: forall t m a. DomBuilder t m => Text -> m a -> m a
el elementTag child = snd <$> el' elementTag child

-- | Create a DOM element with attributes
-- > elAttr "a" ("href" =: "http://google.com") (text "Google!")
-- <a href="http://google.com">Google!</a>
{-# INLINABLE elAttr #-}
elAttr :: forall t m a. DomBuilder t m => Text -> Map Text Text -> m a -> m a
elAttr elementTag attrs child = snd <$> elAttr' elementTag attrs child

-- | Create a DOM element with classes
-- > elClass "div" "row" (return ())
-- <div class="row"></div>
{-# INLINABLE elClass #-}
elClass :: forall t m a. DomBuilder t m => Text -> Text -> m a -> m a
elClass elementTag c child = snd <$> elClass' elementTag c child

-- | Create a DOM element with Dynamic Attributes
-- > elClass "div" (constDyn ("class" =: "row")) (return ())
-- <div class="row"></div>
{-# INLINABLE elDynAttr #-}
elDynAttr :: forall t m a. (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Map Text Text) -> m a -> m a
elDynAttr elementTag attrs child = snd <$> elDynAttr' elementTag attrs child

-- | Create a DOM element with a Dynamic Class
-- > elDynClass "div" (constDyn "row") (return ())
-- <div class="row"></div>
{-# INLINABLE elDynClass #-}
elDynClass :: forall t m a. (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Text -> m a -> m a
elDynClass elementTag c child = snd <$> elDynClass' elementTag c child

-- | Create a DOM element and return the element
-- > do (e, _) <- el' "div" (text "Click")
-- >    return $ domEvent Click e
{-# INLINABLE el' #-}
el' :: forall t m a. DomBuilder t m => Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
el' elementTag = element elementTag def

-- | Create a DOM element with attributes and return the element
{-# INLINABLE elAttr' #-}
elAttr' :: forall t m a. DomBuilder t m => Text -> Map Text Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elAttr' elementTag attrs = element elementTag $ def
  & initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs

-- | Create a DOM element with a class and return the element
{-# INLINABLE elClass' #-}
elClass' :: forall t m a. DomBuilder t m => Text -> Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elClass' elementTag c = elAttr' elementTag ("class" =: c)

-- | Create a DOM element with Dynamic Attributes and return the element
{-# INLINABLE elDynAttr' #-}
elDynAttr' :: forall t m a. (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Map Text Text) -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynAttr' = elDynAttrNS' Nothing

-- | Create a DOM element with a Dynamic class and return the element
{-# INLINABLE elDynClass' #-}
elDynClass' :: forall t m a. (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynClass' elementTag c = elDynAttr' elementTag (fmap ("class" =:) c)

{-# INLINABLE elDynAttrNS' #-}
elDynAttrNS' :: forall t m a. (DomBuilder t m, PostBuild t m) => Maybe Text -> Text -> Dynamic t (Map Text Text) -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynAttrNS' mns elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg = def
        & elementConfig_namespace .~ mns
        & modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
  element elementTag cfg child

dynamicAttributesToModifyAttributes :: (Ord k, PostBuild t m) => Dynamic t (Map k Text) -> m (Event t (Map k (Maybe Text)))
dynamicAttributesToModifyAttributes = dynamicAttributesToModifyAttributesWithInitial mempty

dynamicAttributesToModifyAttributesWithInitial :: (Ord k, PostBuild t m) => Map k Text -> Dynamic t (Map k Text) -> m (Event t (Map k (Maybe Text)))
dynamicAttributesToModifyAttributesWithInitial attrs0 d = do
  postBuild <- getPostBuild
  let modificationsNeeded = flip pushAlways (align postBuild $ updated d) $ \case
        This () -> do
          new <- sample $ current d
          return $ diffMap attrs0 new
        These () new -> return $ diffMap attrs0 new
        That new -> do
          old <- sample $ current d
          return $ diffMap old new
  return modificationsNeeded

--------------------------------------------------------------------------------
-- Copied and pasted from Reflex.Widget.Class
--------------------------------------------------------------------------------

-- | Create a dynamically-changing set of widgets from a Dynamic key/value map.
--   Unlike the 'withKey' variants, the child widgets are insensitive to which key they're associated with.
list :: (Ord k, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m) => Dynamic t (Map k v) -> (Dynamic t v -> m a) -> m (Dynamic t (Map k a))
list dm mkChild = listWithKey dm (\_ dv -> mkChild dv)

-- | Create a dynamically-changing set of widgets from a Dynamic list.
simpleList :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m) => Dynamic t [v] -> (Dynamic t v -> m a) -> m (Dynamic t [a])
simpleList xs mkChild = fmap (fmap (map snd . Map.toList)) $ flip list mkChild $ fmap (Map.fromList . zip [(1::Int)..]) xs

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

data Link t
  = Link { _link_clicked :: Event t ()
         }

linkClass :: DomBuilder t m => Text -> Text -> m (Link t)
linkClass s c = do
  (l,_) <- elAttr' "a" ("class" =: c) $ text s
  return $ Link $ domEvent Click l

link :: DomBuilder t m => Text -> m (Link t)
link s = linkClass s ""

divClass :: forall t m a. DomBuilder t m => Text -> m a -> m a
divClass = elClass "div"

dtdd :: forall t m a. DomBuilder t m => Text -> m a -> m a
dtdd h w = do
  el "dt" $ text h
  el "dd" w

blank :: forall m. Monad m => m ()
blank = return ()

newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

workflow :: forall t m a. (DomBuilder t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Dynamic t a)
workflow w0 = do
  rec eResult <- widgetHold (unWorkflow w0) $ fmap unWorkflow $ switch $ snd <$> current eResult
  return $ fmap fst eResult

workflowView :: forall t m a. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Workflow t m a -> m (Event t a)
workflowView w0 = do
  rec eResult <- dyn . fmap unWorkflow =<< holdDyn w0 eReplace
      eReplace <- fmap switch $ hold never $ fmap snd eResult
  return $ fmap fst eResult

mapWorkflow :: (DomBuilder t m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflow f (Workflow x) = Workflow (fmap (f *** fmap (mapWorkflow f)) x)

-- | A widget to display a table with static columns and dynamic rows.
tableDynAttr :: forall t m r k v. (Ord k, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text                                   -- ^ Class applied to <table> element
  -> [(Text, k -> Dynamic t r -> m v)]      -- ^ Columns of (header, row key -> row value -> child widget)
  -> Dynamic t (Map k r)                      -- ^ Map from row key to row value
  -> (k -> m (Dynamic t (Map Text Text))) -- ^ Function to compute <tr> element attributes from row key
  -> m (Dynamic t (Map k (Element EventResult (DomBuilderSpace m) t, [v])))        -- ^ Map from row key to (El, list of widget return values)
tableDynAttr klass cols dRows rowAttrs = elAttr "div" (Map.singleton "style" "zoom: 1; overflow: auto; background: white;") $ do
    elAttr "table" (Map.singleton "class" klass) $ do
      el "thead" $ el "tr" $ do
        mapM_ (\(h, _) -> el "th" $ text h) cols
      el "tbody" $ do
        listWithKey dRows (\k r -> do
          dAttrs <- rowAttrs k
          elDynAttr' "tr" dAttrs $ mapM (\x -> el "td" $ snd x k r) cols)

-- | A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
tabDisplay :: forall t m k. (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, Ord k)
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
