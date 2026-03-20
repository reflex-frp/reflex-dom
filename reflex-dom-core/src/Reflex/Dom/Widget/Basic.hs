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
-- |
-- Module: Reflex.Dom.Widget.Basic
--
-- High-level widget combinators for building DOM. These are the functions you
-- use in day-to-day reflex-dom code. They are all built on the low-level
-- 'DomBuilder' methods from "Reflex.Dom.Builder.Class".
--
-- == Constraint Guide
--
-- Functions here require different constraint sets depending on what they do:
--
-- [@DomBuilder t m@] Static element creation: 'el', 'elAttr', 'elClass',
--   'text', 'blank', 'button', 'divClass'.
--
-- [@(DomBuilder t m, PostBuild t m)@] Dynamic content: 'elDynAttr',
--   'elDynClass', 'dynText', 'display', 'dyn', 'dyn_'.
--
-- [@(Adjustable t m, MonadHold t m)@] Stateful widget replacement:
--   'widgetHold', 'widgetHold_'.
--
-- All of these work in both static ('StaticDomBuilderT') and GHCJS contexts.
-- In static rendering, dynamic content is rendered with its initial value and
-- never updates. Events (e.g. from 'button') are 'never'.
--
-- == Primed Variants
--
-- Functions ending in @\'@ (e.g. 'el\'', 'elAttr\'') return an 'Element'
-- handle alongside the child result. Use 'domEvent' to extract events:
--
-- @
-- (e, _) <- el\' \"button\" $ text \"Click\"
-- let click = domEvent Click e  -- Event t ()
-- @
--
-- Non-primed variants discard the element handle for convenience.
module Reflex.Dom.Widget.Basic
  (
  -- * Displaying Values
    text
  , dynText
  , comment
  , dynComment
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
  , partitionMapBySetLT
  ) where

import Reflex.Adjustable.Class
import Reflex.Class
import Reflex.Collection
import Reflex.Dom.Builder.Class
import Reflex.Dom.Class
import Reflex.Dynamic
import Reflex.Network
import Reflex.PostBuild.Class
import Reflex.Workflow

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
{-# DEPRECATED partitionMapBySetLT "This will be removed in future releases." #-}
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

-- | Create a static text node. This is the most basic way to put text on screen.
--
-- @
-- el \"p\" $ text \"Hello, world!\"
-- @
--
-- Multiple 'text' calls produce adjacent text nodes:
--
-- @
-- el \"p\" $ do
--   text \"Hello, \"
--   text \"world!\"
-- -- renders: \<p\>Hello, world!\<\/p\>
-- @
{-# INLINABLE text #-}
text :: DomBuilder t m => Text -> m ()
text t = void $ textNode $ def & textNodeConfig_initialContents .~ t

-- | Create a text node whose content changes over time.
--
-- Requires 'PostBuild' to sample the initial value of the 'Dynamic'.
-- In static rendering, only the initial value is used.
--
-- @
-- nameDyn <- holdDyn \"Anonymous\" nameUpdateEvt
-- dynText nameDyn
-- @
{-# INLINABLE dynText #-}
dynText :: forall t m. (PostBuild t m, DomBuilder t m) => Dynamic t Text -> m ()
dynText t = do
  postBuild <- getPostBuild
  void $ textNode $ (def :: TextNodeConfig t) & textNodeConfig_setContents .~ leftmost
    [ updated t
    , tag (current t) postBuild
    ]
  notReadyUntil postBuild

comment :: DomBuilder t m => Text -> m ()
comment t = void $ commentNode $ def & commentNodeConfig_initialContents .~ t

{-# INLINABLE dynComment #-}
dynComment :: forall t m. (PostBuild t m, DomBuilder t m) => Dynamic t Text -> m ()
dynComment t = do
  postBuild <- getPostBuild
  void $ commentNode $ (def :: CommentNodeConfig t) & commentNodeConfig_setContents .~ leftmost
    [ updated t
    , tag (current t) postBuild
    ]
  notReadyUntil postBuild

-- | Display a 'Show'-able 'Dynamic' value as text. Equivalent to
-- @dynText . fmap (T.pack . show)@.
--
-- @
-- counter <- foldDyn (+) (0 :: Int) (1 \<$ clickEvt)
-- el \"p\" $ do
--   text \"Count: \"
--   display counter
-- @
display :: (PostBuild t m, DomBuilder t m, Show a) => Dynamic t a -> m ()
display = dynText . fmap (T.pack . show)

-- | Create a @\<button\>@ element with the given label, returning a click 'Event'.
--
-- Internally uses 'domEvent' 'Click' on the created element.
--
-- In 'StaticDomSpace', the returned event is 'never'.
-- In 'GhcjsDomSpace', it fires @()@ on each click.
--
-- @
-- clickEvt <- button \"Submit\"
-- @
button :: DomBuilder t m => Text -> m (Event t ())
button t = do
  (e, _) <- element "button" def $ text t
  return $ domEvent Click e

-- | Rebuild a widget entirely whenever the 'Dynamic' changes. Each time the
-- 'Dynamic' updates, the previous widget is destroyed and a new one is built.
--
-- Requires 'Adjustable' (from the @reflex@ package) for dynamic widget
-- replacement. See also 'widgetHold' for an event-driven variant.
--
-- The returned 'Event' fires with the new widget's result each time the widget
-- is rebuilt (including once at post-build time).
--
-- __Warning:__ If @a@ is itself an 'Event', you get an @Event t (Event t b)@.
-- Flatten with 'switchHold' or 'switchDyn'.
--
-- @
-- dyn $ ffor currentPage $ \\case
--   HomePage  -> homeWidget
--   AboutPage -> aboutWidget
-- @
dyn :: (Adjustable t m, NotReady t m, PostBuild t m) => Dynamic t (m a) -> m (Event t a)
dyn = networkView

-- | Like 'dyn' but discards the result.
dyn_ :: (Adjustable t m, NotReady t m, PostBuild t m) => Dynamic t (m a) -> m ()
dyn_ = void . dyn

-- | Hold a widget, replacing it whenever the 'Event' fires with a new widget
-- action. Returns a 'Dynamic' that always holds the latest widget's result.
--
-- Requires 'Adjustable' (from the @reflex@ package) for dynamic widget
-- replacement. See also 'dyn' for a 'Dynamic'-driven variant.
--
-- Unlike 'dyn', this takes an initial widget and an update 'Event' rather than
-- a 'Dynamic'. Use when the widget changes in response to discrete events
-- (e.g. a server response), not continuous state.
--
-- @
-- widgetHold (text \"Loading...\") (buildResult \<$\> responseEvt)
-- @
widgetHold :: (Adjustable t m, MonadHold t m) => m a -> Event t (m a) -> m (Dynamic t a)
widgetHold = networkHold

-- | Like 'widgetHold' but discards the result.
widgetHold_ :: (Adjustable t m, MonadHold t m) => m a -> Event t (m a) -> m ()
widgetHold_ z = void . widgetHold z

-- | Create a DOM element
--
-- >>> el "div" (text "Hello World")
-- <div>Hello World</div>
{-# INLINABLE el #-}
el :: forall t m a. DomBuilder t m => Text -> m a -> m a
el elementTag child = snd <$> el' elementTag child

-- | Create a DOM element with attributes
--
-- >>> elAttr "a" ("href" =: "https://reflex-frp.org") (text "Reflex-FRP!")
-- <a href="https://reflex-frp.org">Reflex-FRP!</a>
{-# INLINABLE elAttr #-}
elAttr :: forall t m a. DomBuilder t m => Text -> Map Text Text -> m a -> m a
elAttr elementTag attrs child = snd <$> elAttr' elementTag attrs child

-- | Create a DOM element with classes
--
-- >>> elClass "div" "row" (return ())
-- <div class="row"></div>
{-# INLINABLE elClass #-}
elClass :: forall t m a. DomBuilder t m => Text -> Text -> m a -> m a
elClass elementTag c child = snd <$> elClass' elementTag c child

-- | Create a DOM element with Dynamic Attributes.
--
-- Attribute changes are applied as incremental patches to the real DOM;
-- attributes that do not change between updates are left untouched.
--
-- @
-- visibleDyn <- toggle True clickEvt
-- let attrsDyn = ffor visibleDyn $ \\vis ->
--       if vis then (\"class\" =: \"visible\")
--              else (\"class\" =: \"hidden\")
-- elDynAttr \"div\" attrsDyn $ text \"Toggle me\"
-- @
{-# INLINABLE elDynAttr #-}
elDynAttr :: forall t m a. (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Map Text Text) -> m a -> m a
elDynAttr elementTag attrs child = snd <$> elDynAttr' elementTag attrs child

-- | Create a DOM element with a Dynamic class string.
--
-- When the 'Dynamic' updates, the @class@ attribute is patched on the real
-- DOM element. Equivalent to @elDynAttr tag (fmap (\"class\" =:) classDyn)@.
--
-- @
-- isActive <- toggle False clickEvt
-- let classDyn = ffor isActive $ \\a -> if a then \"tab active\" else \"tab\"
-- elDynClass \"div\" classDyn $ text \"Tab content\"
-- @
{-# INLINABLE elDynClass #-}
elDynClass :: forall t m a. (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Text -> m a -> m a
elDynClass elementTag c child = snd <$> elDynClass' elementTag c child

-- | Create a DOM element and return the element
--
-- @
--  do (e, _) <- el' "div" (text "Click")
--     return $ domEvent Click e
-- @
{-# INLINABLE el' #-}
el' :: forall t m a. DomBuilder t m => Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
el' elementTag = element elementTag def

-- | Create a DOM element with attributes and return the element.
--
-- @
-- (imgEl, _) <- elAttr' \"img\" (\"src\" =: \"logo.png\" \<> \"alt\" =: \"Logo\") blank
-- let mouseEvt = domEvent Mouseover imgEl
-- @
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
        & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
  result <- element elementTag cfg child
  postBuild <- getPostBuild
  notReadyUntil postBuild
  return result

{-# INLINABLE elDynAttrNS #-}
elDynAttrNS :: forall t m a. (DomBuilder t m, PostBuild t m) => Maybe Text -> Text -> Dynamic t (Map Text Text) -> m a -> m a
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

newtype Link t
  = Link { _link_clicked :: Event t ()
         }

-- | Create a clickable @\<a\>@ element with text and a CSS class.
--
-- @
-- lnk <- linkClass \"Sign up\" \"cta-link\"
-- performEvent_ $ redirect \"\/signup\" \<$ _link_clicked lnk
-- @
linkClass :: DomBuilder t m => Text -> Text -> m (Link t)
linkClass s c = do
  (l,_) <- elAttr' "a" ("class" =: c) $ text s
  return $ Link $ domEvent Click l

-- | Create a clickable @\<a\>@ element with no CSS class. See 'linkClass'.
link :: DomBuilder t m => Text -> m (Link t)
link s = linkClass s ""

-- | Shorthand for @elClass \"div\"@. Creates a @\<div\>@ with the given class.
--
-- @
-- divClass \"container\" $ do
--   divClass \"header\" $ text \"Title\"
--   divClass \"body\"   $ text \"Content\"
-- @
divClass :: forall t m a. DomBuilder t m => Text -> m a -> m a
divClass = elClass "div"

-- | Render a definition list term\/description pair.
--
-- @
-- el \"dl\" $ do
--   dtdd \"Name\" $ text \"Alice\"
--   dtdd \"Age\"  $ text \"30\"
-- -- renders: \<dt\>Name\<\/dt\>\<dd\>Alice\<\/dd\>\<dt\>Age\<\/dt\>\<dd\>30\<\/dd\>
-- @
dtdd :: forall t m a. DomBuilder t m => Text -> m a -> m a
dtdd h w = do
  el "dt" $ text h
  el "dd" w

-- | Do nothing. Useful as a no-op child widget.
--
-- @
-- elAttr \"img\" (\"src\" =: \"logo.png\") blank   -- self-closing-style element
-- elAttr \"hr\" mempty blank                    -- no child content needed
-- @
blank :: forall m. Monad m => m ()
blank = return ()

-- TODO: Move to an example project.
-- | A widget to display a table with static columns and dynamic rows.
tableDynAttr :: forall t m r k v. (Ord k, Eq r, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
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

-- TODO: Move to an example project.
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
