{-# LANGUAGE ScopedTypeVariables, LambdaCase, ConstraintKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, RecursiveDo, GADTs, DataKinds, RankNTypes, TemplateHaskell #-}

module Reflex.Dom.Widget.Basic where

import Reflex.Dom.Class
import Reflex.Dom.Internal.Foreign ()

import Prelude hiding (mapM, mapM_, sequence, sequence_)
import Reflex
import Reflex.Host.Class
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Dependent.Sum (DSum (..))
import Data.Foldable
import Data.Traversable
import Control.Monad.Trans
import Control.Monad.Reader hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.State hiding (state, mapM, mapM_, forM, forM_, sequence, sequence_)
import GHCJS.DOM.Node
import GHCJS.DOM.UIEvent
import GHCJS.DOM.EventM (event, EventM)
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Types hiding (Widget (..), unWidget, Event)
import GHCJS.DOM.NamedNodeMap
import Control.Lens hiding (element, children)
import Data.These
import Data.Align
import Data.Maybe
import Data.GADT.Compare.TH
import Data.Bitraversable
import GHCJS.DOM.MouseEvent

type AttributeMap = Map String String

data El t
  = El { _el_element :: HTMLElement
       , _el_events :: EventSelector t (WrapArg EventResult EventName)
       }

class Attributes m a where
  addAttributes :: IsElement e => a -> e -> m ()

instance MonadIO m => Attributes m AttributeMap where
  addAttributes curAttrs e = liftIO $ imapM_ (elementSetAttribute e) curAttrs

instance MonadWidget t m => Attributes m (Dynamic t AttributeMap) where
  addAttributes attrs e = do
    schedulePostBuild $ do
      curAttrs <- sample $ current attrs
      liftIO $ imapM_ (elementSetAttribute e) curAttrs
    addVoidAction $ flip fmap (updated attrs) $ \newAttrs -> liftIO $ do
      oldAttrs <- maybe (return Set.empty) namedNodeMapGetNames =<< elementGetAttributes e
      forM_ (Set.toList $ oldAttrs `Set.difference` Map.keysSet newAttrs) $ elementRemoveAttribute e
      imapM_ (elementSetAttribute e) newAttrs --TODO: avoid re-setting unchanged attributes; possibly do the compare using Align in haskell

buildEmptyElement :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m HTMLElement
buildEmptyElement elementTag attrs = do
  doc <- askDocument
  p <- askParent
  Just e <- liftIO $ documentCreateElement doc elementTag
  addAttributes attrs e
  _ <- liftIO $ nodeAppendChild p $ Just e
  return $ castToHTMLElement e

-- We need to decide what type of attrs we've got statically, because it will often be a recursively defined value, in which case inspecting it will lead to a cycle
buildElement :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m a -> m (HTMLElement, a)
buildElement elementTag attrs child = do
  e <- buildEmptyElement elementTag attrs
  result <- subWidget (toNode e) child
  return (e, result)

namedNodeMapGetNames :: IsNamedNodeMap self => self -> IO (Set String)
namedNodeMapGetNames self = do
  l <- namedNodeMapGetLength self
  let locations = if l == 0 then [] else [0..l-1] -- Can't use 0..l-1 if l is 0 because l is unsigned and will wrap around
  liftM Set.fromList $ forM locations $ \i -> do
    Just n <- namedNodeMapItem self i
    nodeGetNodeName n

text :: MonadWidget t m => String -> m ()
text = void . text'

--TODO: Wrap the result
text' :: MonadWidget t m => String -> m Text
text' s = do
  doc <- askDocument
  p <- askParent
  Just n <- liftIO $ documentCreateTextNode doc s
  _ <- liftIO $ nodeAppendChild p $ Just n
  return n

dynText :: MonadWidget t m => Dynamic t String -> m ()
dynText s = do
  n <- text' ""
  schedulePostBuild $ do
    curS <- sample $ current s
    liftIO $ nodeSetNodeValue n curS
  addVoidAction $ fmap (liftIO . nodeSetNodeValue n) $ updated s

display :: (MonadWidget t m, Show a) => Dynamic t a -> m ()
display a = dynText =<< mapDyn show a

--TODO: Should this be renamed to 'widgetView' for consistency with 'widgetHold'?
-- | Given a Dynamic of widget-creating actions, create a widget that is recreated whenever the Dynamic updates.
--   The returned Event of widget results occurs when the Dynamic does.
--   Note:  Often, the type 'a' is an Event, in which case the return value is an Event-of-Events that would typically be flattened.
dyn :: MonadWidget t m => Dynamic t (m a) -> m (Event t a)
dyn child = do
  postBuild <- getPostBuild
  let newChild = leftmost [updated child, tag (current child) postBuild]
  liftM snd $ widgetHoldInternal (return ()) newChild

-- | Given an initial widget and an Event of widget-creating actions, create a widget that is recreated whenever the Event fires.
--   The returned Dynamic of widget results occurs when the Event does.
--   Note:  Often, the type 'a' is an Event, in which case the return value is a Dynamic-of-Events that would typically be flattened.
widgetHold :: MonadWidget t m => m a -> Event t (m a) -> m (Dynamic t a)
widgetHold child0 newChild = do
  (result0, newResult) <- widgetHoldInternal child0 newChild
  holdDyn result0 newResult

widgetHoldInternal :: MonadWidget t m => m a -> Event t (m b) -> m (a, Event t b)
widgetHoldInternal child0 newChild = do
  startPlaceholder <- text' ""
  p <- askParent
  (result0, childVoidAction0) <- subWidgetWithVoidActions p child0
  endPlaceholder <- text' ""
  (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
  performEvent_ $ fmap (const $ return ()) newChildBuilt --TODO: Get rid of this hack
  childVoidAction <- hold childVoidAction0 $ fmap snd newChildBuilt
  addVoidAction $ switch childVoidAction --TODO: Should this be a switchPromptly?
  doc <- askDocument
  runWidget <- getRunWidget
  let build c = do
        Just df <- liftIO $ documentCreateDocumentFragment doc
        (result, postBuild, voidActions) <- runWidget df c
        runFrameWithTriggerRef newChildBuiltTriggerRef (result, voidActions)
        postBuild
        mp <- liftIO $ nodeGetParentNode endPlaceholder
        case mp of
          Nothing -> return () --TODO: Is this right?
          Just p -> do
            _ <- liftIO $ nodeInsertBefore p (Just df) (Just endPlaceholder)
            return ()
        return ()
  addVoidAction $ ffor newChild $ \c -> do
    liftIO $ deleteBetweenExclusive startPlaceholder endPlaceholder
    build c
  return (result0, fmap fst newChildBuilt)

--TODO: Something better than Dynamic t (Map k v) - we want something where the Events carry diffs, not the whole value
-- | Create a dynamically-changing set of widgets from a Dynamic key/value map.
listWithKey :: (Ord k, MonadWidget t m)
  => Dynamic t (Map k v)         -- ^ Dynamic key/value map
  -> (k -> Dynamic t v -> m a)   -- ^ Function to create a widget for a given key from Dynamic value
  -> m (Dynamic t (Map k a))     -- ^ Dynamic map from keys to widget results
listWithKey vals mkChild = do
  doc <- askDocument
  endPlaceholder <- text' ""
  (newChildren, newChildrenTriggerRef) <- newEventWithTriggerRef
  performEvent_ $ fmap (const $ return ()) newChildren --TODO: Get rid of this hack
  children <- hold Map.empty  newChildren
  addVoidAction $ switch $ fmap (mergeWith (>>) . map snd . Map.elems) children
  runWidget <- getRunWidget
  let buildChild df k v = runWidget df $ do
        childStart <- text' ""
        result <- mkChild k =<< holdDyn v (fmapMaybe (Map.lookup k) (updated vals))
        childEnd <- text' ""
        return (result, (childStart, childEnd))
  schedulePostBuild $ do
    Just df <- liftIO $ documentCreateDocumentFragment doc
    curVals <- sample $ current vals
    initialState <- iforM curVals $ \k v -> do
      (result, postBuild, voidAction) <- buildChild df k v
      return ((result, voidAction), postBuild)
    runFrameWithTriggerRef newChildrenTriggerRef $ fmap fst initialState --TODO: Do all these in a single runFrame
    sequence_ $ fmap snd initialState
    Just p <- liftIO $ nodeGetParentNode endPlaceholder
    _ <- liftIO $ nodeInsertBefore p (Just df) (Just endPlaceholder)
    return ()
  addVoidAction $ flip fmap (updated vals) $ \newVals -> do
    curState <- sample children
    --TODO: Should we remove the parent from the DOM first to avoid reflows?
    (newState, postBuild) <- flip runStateT (return ()) $ liftM (Map.mapMaybe id) $ iforM (align curState newVals) $ \k -> \case
      This ((_, (start, end)), _) -> do
        liftIO $ deleteBetweenInclusive start end
        return Nothing
      That v -> do
        Just df <- liftIO $ documentCreateDocumentFragment doc
        (childResult, childPostBuild, childVoidAction) <- lift $ buildChild df k v
        let s = (childResult, childVoidAction)
        modify (>>childPostBuild)
        let placeholder = case Map.lookupGT k curState of
              Nothing -> endPlaceholder
              Just (_, ((_, (start, _)), _)) -> start
        Just p <- liftIO $ nodeGetParentNode placeholder
        _ <- liftIO $ nodeInsertBefore p (Just df) (Just placeholder)
        return $ Just s
      These state _ -> do
        return $ Just state
    runFrameWithTriggerRef newChildrenTriggerRef newState
    postBuild
  holdDyn Map.empty $ fmap (fmap (fst . fst)) newChildren

-- | Create a dynamically-changing set of widgets from a key/value map with update Events.
listWithKey' :: forall t m k v a. (Ord k, MonadWidget t m)
  => Map k v                       -- ^ Initial key/value map
  -> Event t (Map k (Maybe v))     -- ^ Event of map updates
  -> (k -> v -> Event t v -> m a)  -- ^ Function to create a widget for a given key from initial value and update Event
  -> m (Dynamic t (Map k a))       -- ^ Dynamic map from keys to widget results
listWithKey' initialVals valsChanged mkChild = do
  doc <- askDocument
  endPlaceholder <- text' ""
  (newChildren, newChildrenTriggerRef) <- newEventWithTriggerRef
--  performEvent_ $ fmap (const $ return ()) newChildren --TODO: Get rid of this hack
  runWidget <- getRunWidget
  let childValChangedSelector :: EventSelector t (Const2 k v)
      childValChangedSelector = fanMap $ fmap (Map.mapMaybe id) valsChanged
      buildChild df k v = runWidget df $ wrapChild k v
      wrapChild k v = do
        childStart <- text' ""
        result <- mkChild k v $ select childValChangedSelector $ Const2 k
        childEnd <- text' ""
        return (result, (childStart, childEnd))
  Just dfOrig <- liftIO $ documentCreateDocumentFragment doc
  initialState <- iforM initialVals $ \k v -> subWidgetWithVoidActions (toNode dfOrig) $ wrapChild k v --Note: we have to use subWidgetWithVoidActions rather than runWidget here, because running post-build actions during build can cause not-yet-constructed values to be read
  children <- holdDyn initialState newChildren
  addVoidAction $ switch $ fmap (mergeWith (>>) . map snd . Map.elems) $ current children
  Just pOrig <- liftIO $ nodeGetParentNode endPlaceholder
  _ <- liftIO $ nodeInsertBefore pOrig (Just dfOrig) (Just endPlaceholder)
  addVoidAction $ flip fmap valsChanged $ \newVals -> do
    curState <- sample $ current children
    --TODO: Should we remove the parent from the DOM first to avoid reflows?
    (newState, postBuild) <- flip runStateT (return ()) $ liftM (Map.mapMaybe id) $ iforM (align curState newVals) $ \k -> \case
      These ((_, (start, end)), _) Nothing -> do -- Deleting child
        liftIO $ deleteBetweenInclusive start end
        return Nothing
      These ((_, (start, end)), _) (Just v) -> do -- Replacing existing child
        liftIO $ deleteBetweenExclusive start end
        Just df <- liftIO $ documentCreateDocumentFragment doc
        (childResult, childPostBuild, childVoidAction) <- lift $ buildChild df k v
        let s = (childResult, childVoidAction)
        modify (>>childPostBuild)
        Just p <- liftIO $ nodeGetParentNode end
        _ <- liftIO $ nodeInsertBefore p (Just df) (Just end)
        return $ Just s
      That Nothing -> return Nothing -- Deleting non-existent child
      That (Just v) -> do -- Creating new child
        Just df <- liftIO $ documentCreateDocumentFragment doc
        (childResult, childPostBuild, childVoidAction) <- lift $ buildChild df k v
        let s = (childResult, childVoidAction)
        modify (>>childPostBuild)
        let placeholder = case Map.lookupGT k curState of
              Nothing -> endPlaceholder
              Just (_, ((_, (start, _)), _)) -> start
        Just p <- liftIO $ nodeGetParentNode placeholder
        _ <- liftIO $ nodeInsertBefore p (Just df) (Just placeholder)
        return $ Just s
      This state -> do -- No change
        return $ Just state
    runFrameWithTriggerRef newChildrenTriggerRef newState
    postBuild
  mapDyn (fmap (fst . fst)) children

--TODO: Something better than Dynamic t (Map k v) - we want something where the Events carry diffs, not the whole value
-- | Create a dynamically-changing set of Event-valued widgets.
--   This is like listWithKey, specialized for widgets returning (Event t a).  listWithKey would return 'Dynamic t (Map k (Event t a))' in this scenario, but listViewWithKey flattens this to 'Event t (Map k a)' via 'switch'.
listViewWithKey :: (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m (Event t a)) -> m (Event t (Map k a))
listViewWithKey vals mkChild = liftM (switch . fmap mergeMap) $ listViewWithKey' vals mkChild

listViewWithKey' :: (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Behavior t (Map k a))
listViewWithKey' vals mkChild = do
  doc <- askDocument
  endPlaceholder <- text' ""
  (newChildren, newChildrenTriggerRef) <- newEventWithTriggerRef
  performEvent_ $ fmap (const $ return ()) newChildren --TODO: Get rid of this hack
  children <- hold Map.empty newChildren
  addVoidAction $ switch $ fmap (mergeWith (>>) . map snd . Map.elems) children
  runWidget <- getRunWidget
  let buildChild df k v = runWidget df $ do
        childStart <- text' ""
        result <- mkChild k =<< holdDyn v (fmapMaybe (Map.lookup k) (updated vals))
        childEnd <- text' ""
        return (result, (childStart, childEnd))
  schedulePostBuild $ do
    Just df <- liftIO $ documentCreateDocumentFragment doc
    curVals <- sample $ current vals
    initialState <- iforM curVals $ \k v -> do
      (result, postBuild, voidAction) <- buildChild df k v
      return ((result, voidAction), postBuild)
    runFrameWithTriggerRef newChildrenTriggerRef $ fmap fst initialState --TODO: Do all these in a single runFrame
    sequence_ $ fmap snd initialState
    Just p <- liftIO $ nodeGetParentNode endPlaceholder
    _ <- liftIO $ nodeInsertBefore p (Just df) (Just endPlaceholder)
    return ()
  addVoidAction $ flip fmap (updated vals) $ \newVals -> do
    curState <- sample children
    --TODO: Should we remove the parent from the DOM first to avoid reflows?
    (newState, postBuild) <- flip runStateT (return ()) $ liftM (Map.mapMaybe id) $ iforM (align curState newVals) $ \k -> \case
      This ((_, (start, end)), _) -> do
        liftIO $ deleteBetweenInclusive start end
        return Nothing
      That v -> do
        Just df <- liftIO $ documentCreateDocumentFragment doc
        (childResult, childPostBuild, childVoidAction) <- lift $ buildChild df k v
        let s = (childResult, childVoidAction)
        modify (>>childPostBuild)
        let placeholder = case Map.lookupGT k curState of
              Nothing -> endPlaceholder
              Just (_, ((_, (start, _)), _)) -> start
        mp <- liftIO $ nodeGetParentNode placeholder
        forM_ mp $ \p -> liftIO $ nodeInsertBefore p (Just df) (Just placeholder) --TODO: Should this really be ignored if mp is Nothing, or should we ensure that mp is never Nothing
        return $ Just s
      These state _ -> do
        return $ Just state
    runFrameWithTriggerRef newChildrenTriggerRef newState
    postBuild
  return $ fmap (fmap (fst . fst)) children

--TODO: Deduplicate the various list*WithKey* implementations

-- | Create a dynamically-changing set of widgets, one of which is selected at any time.
selectViewListWithKey_ :: forall t m k v a. (MonadWidget t m, Ord k)
  => Dynamic t k          -- ^ Current selection key
  -> Dynamic t (Map k v)  -- ^ Dynamic key/value map
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -- ^ Function to create a widget for a given key from Dynamic value and Dynamic Bool indicating if this widget is currently selected
  -> m (Event t k)        -- ^ Event that fires when any child's return Event fires.  Contains key of an arbitrary firing widget.
selectViewListWithKey_ selection vals mkChild = do
  let selectionDemux = demux selection -- For good performance, this value must be shared across all children
  selectChild <- listWithKey vals $ \k v -> do
    selected <- getDemuxed selectionDemux k
    selectSelf <- mkChild k v selected
    return $ fmap (const k) selectSelf
  liftM switchPromptlyDyn $ mapDyn (leftmost . Map.elems) selectChild

--------------------------------------------------------------------------------
-- Basic DOM manipulation helpers
--------------------------------------------------------------------------------

-- | s and e must both be children of the same node and s must precede e
deleteBetweenExclusive :: (IsNode start, IsNode end) => start -> end -> IO ()
deleteBetweenExclusive s e = do
  mCurrentParent <- nodeGetParentNode e -- May be different than it was at initial construction, e.g., because the parent may have dumped us in from a DocumentFragment
  case mCurrentParent of
    Nothing -> return () --TODO: Is this the right behavior?
    Just currentParent -> do
      let go = do
            Just x <- nodeGetPreviousSibling e -- This can't be Nothing because we should hit 's' first
            when (toNode s /= toNode x) $ do
              _ <- nodeRemoveChild currentParent $ Just x
              go
      go

-- | s and e must both be children of the same node and s must precede e
deleteBetweenInclusive :: (IsNode start, IsNode end) => start -> end -> IO ()
deleteBetweenInclusive s e = do
  mCurrentParent <- nodeGetParentNode e -- May be different than it was at initial construction, e.g., because the parent may have dumped us in from a DocumentFragment
  case mCurrentParent of
    Nothing -> return () --TODO: Is this the right behavior?
    Just currentParent -> do
      let go = do
            Just x <- nodeGetPreviousSibling e -- This can't be Nothing because we should hit 's' first
            _ <- nodeRemoveChild currentParent $ Just x
            when (toNode s /= toNode x) go
      go
      _ <- nodeRemoveChild currentParent $ Just e
      return ()

--------------------------------------------------------------------------------
-- Adapters
--------------------------------------------------------------------------------

wrapDomEvent :: (Functor (Event t), MonadIO m, MonadSample t m, MonadReflexCreateTrigger t m, Reflex t, HasPostGui t h m) => e -> (e -> EventM event e () -> IO (IO ())) -> EventM event e a -> m (Event t a)
wrapDomEvent element elementOnevent getValue = wrapDomEventMaybe element elementOnevent $ liftM Just getValue

wrapDomEventMaybe :: (Functor (Event t), MonadIO m, MonadSample t m, MonadReflexCreateTrigger t m, Reflex t, HasPostGui t h m) => e -> (e -> EventM event e () -> IO (IO ())) -> EventM event e (Maybe a) -> m (Event t a)
wrapDomEventMaybe element elementOnevent getValue = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  e <- newEventWithTrigger $ \et -> do
        unsubscribe <- {-# SCC "a" #-} liftIO $ {-# SCC "b" #-} elementOnevent element $ {-# SCC "c" #-} do
          mv <- {-# SCC "d" #-} getValue
          forM_ mv $ \v -> liftIO $ postGui $ runWithActions [et :=> v]
        return $ liftIO $ do
          {-# SCC "e" #-} unsubscribe
  return $! {-# SCC "f" #-} e

data EventTag
   = AbortTag
   | BlurTag
   | ChangeTag
   | ClickTag
   | ContextmenuTag
   | DblclickTag
   | DragTag
   | DragendTag
   | DragenterTag
   | DragleaveTag
   | DragoverTag
   | DragstartTag
   | DropTag
   | ErrorTag
   | FocusTag
   | InputTag
   | InvalidTag
   | KeydownTag
   | KeypressTag
   | KeyupTag
   | LoadTag
   | MousedownTag
   | MouseenterTag
   | MouseleaveTag
   | MousemoveTag
   | MouseoutTag
   | MouseoverTag
   | MouseupTag
--   | MousewheelTag -- webkitgtk only provides elementOnmousewheel (not elementOnwheel), but firefox does not support the mousewheel event; we should provide wheel (the equivalent, standard event), but we will need to make sure webkitgtk supports it first
   | ScrollTag
   | SelectTag
   | SubmitTag
--   | WheelTag -- See MousewheelTag
   | BeforecutTag
   | CutTag
   | BeforecopyTag
   | CopyTag
   | BeforepasteTag
   | PasteTag
   | ResetTag
   | SearchTag
   | SelectstartTag
   | TouchstartTag
   | TouchmoveTag
   | TouchendTag
   | TouchcancelTag

data EventName :: EventTag -> * where
  Abort :: EventName 'AbortTag
  Blur :: EventName 'BlurTag
  Change :: EventName 'ChangeTag
  Click :: EventName 'ClickTag
  Contextmenu :: EventName 'ContextmenuTag
  Dblclick :: EventName 'DblclickTag
  Drag :: EventName 'DragTag
  Dragend :: EventName 'DragendTag
  Dragenter :: EventName 'DragenterTag
  Dragleave :: EventName 'DragleaveTag
  Dragover :: EventName 'DragoverTag
  Dragstart :: EventName 'DragstartTag
  Drop :: EventName 'DropTag
  Error :: EventName 'ErrorTag
  Focus :: EventName 'FocusTag
  Input :: EventName 'InputTag
  Invalid :: EventName 'InvalidTag
  Keydown :: EventName 'KeydownTag
  Keypress :: EventName 'KeypressTag
  Keyup :: EventName 'KeyupTag
  Load :: EventName 'LoadTag
  Mousedown :: EventName 'MousedownTag
  Mouseenter :: EventName 'MouseenterTag
  Mouseleave :: EventName 'MouseleaveTag
  Mousemove :: EventName 'MousemoveTag
  Mouseout :: EventName 'MouseoutTag
  Mouseover :: EventName 'MouseoverTag
  Mouseup :: EventName 'MouseupTag
  --Mousewheel :: EventName 'MousewheelTag
  Scroll :: EventName 'ScrollTag
  Select :: EventName 'SelectTag
  Submit :: EventName 'SubmitTag
  --Wheel :: EventName 'WheelTag
  Beforecut :: EventName 'BeforecutTag
  Cut :: EventName 'CutTag
  Beforecopy :: EventName 'BeforecopyTag
  Copy :: EventName 'CopyTag
  Beforepaste :: EventName 'BeforepasteTag
  Paste :: EventName 'PasteTag
  Reset :: EventName 'ResetTag
  Search :: EventName 'SearchTag
  Selectstart :: EventName 'SelectstartTag
  Touchstart :: EventName 'TouchstartTag
  Touchmove :: EventName 'TouchmoveTag
  Touchend :: EventName 'TouchendTag
  Touchcancel :: EventName 'TouchcancelTag

type family EventType en where
  EventType 'AbortTag = UIEvent
  EventType 'BlurTag = UIEvent
  EventType 'ChangeTag = UIEvent
  EventType 'ClickTag = MouseEvent
  EventType 'ContextmenuTag = MouseEvent
  EventType 'DblclickTag = MouseEvent
  EventType 'DragTag = MouseEvent
  EventType 'DragendTag = MouseEvent
  EventType 'DragenterTag = MouseEvent
  EventType 'DragleaveTag = MouseEvent
  EventType 'DragoverTag = MouseEvent
  EventType 'DragstartTag = MouseEvent
  EventType 'DropTag = MouseEvent
  EventType 'ErrorTag = UIEvent
  EventType 'FocusTag = UIEvent
  EventType 'InputTag = UIEvent
  EventType 'InvalidTag = UIEvent
  EventType 'KeydownTag = UIEvent
  EventType 'KeypressTag = UIEvent
  EventType 'KeyupTag = UIEvent
  EventType 'LoadTag = UIEvent
  EventType 'MousedownTag = MouseEvent
  EventType 'MouseenterTag = UIEvent
  EventType 'MouseleaveTag = UIEvent
  EventType 'MousemoveTag = MouseEvent
  EventType 'MouseoutTag = MouseEvent
  EventType 'MouseoverTag = MouseEvent
  EventType 'MouseupTag = MouseEvent
  --EventType 'MousewheelTag = MouseEvent
  EventType 'ScrollTag = UIEvent
  EventType 'SelectTag = UIEvent
  EventType 'SubmitTag = UIEvent
  --EventType 'WheelTag = UIEvent
  EventType 'BeforecutTag = UIEvent
  EventType 'CutTag = UIEvent
  EventType 'BeforecopyTag = UIEvent
  EventType 'CopyTag = UIEvent
  EventType 'BeforepasteTag = UIEvent
  EventType 'PasteTag = UIEvent
  EventType 'ResetTag = UIEvent
  EventType 'SearchTag = UIEvent
  EventType 'SelectstartTag = UIEvent
  EventType 'TouchstartTag = UIEvent
  EventType 'TouchmoveTag = UIEvent
  EventType 'TouchendTag = UIEvent
  EventType 'TouchcancelTag = UIEvent

onEventName :: IsElement e => EventName en -> e -> EventM (EventType en) e () -> IO (IO ())
onEventName en = case en of
  Abort -> elementOnabort
  Blur -> elementOnblur
  Change -> elementOnchange
  Click -> elementOnclick
  Contextmenu -> elementOncontextmenu
  Dblclick -> elementOndblclick
  Drag -> elementOndrag
  Dragend -> elementOndragend
  Dragenter -> elementOndragenter
  Dragleave -> elementOndragleave
  Dragover -> elementOndragover
  Dragstart -> elementOndragstart
  Drop -> elementOndrop
  Error -> elementOnerror
  Focus -> elementOnfocus
  Input -> elementOninput
  Invalid -> elementOninvalid
  Keydown -> elementOnkeydown
  Keypress -> elementOnkeypress
  Keyup -> elementOnkeyup
  Load -> elementOnload
  Mousedown -> elementOnmousedown
  Mouseenter -> elementOnmouseenter
  Mouseleave -> elementOnmouseleave
  Mousemove -> elementOnmousemove
  Mouseout -> elementOnmouseout
  Mouseover -> elementOnmouseover
  Mouseup -> elementOnmouseup
  --Mousewheel -> elementOnmousewheel
  Scroll -> elementOnscroll
  Select -> elementOnselect
  Submit -> elementOnsubmit
  --Wheel -> elementOnwheel
  Beforecut -> elementOnbeforecut
  Cut -> elementOncut
  Beforecopy -> elementOnbeforecopy
  Copy -> elementOncopy
  Beforepaste -> elementOnbeforepaste
  Paste -> elementOnpaste
  Reset -> elementOnreset
  Search -> elementOnsearch
  Selectstart -> elementOnselectstart
  Touchstart -> elementOntouchstart
  Touchmove -> elementOntouchmove
  Touchend -> elementOntouchend
  Touchcancel -> elementOntouchcancel

newtype EventResult en = EventResult { unEventResult :: EventResultType en }

type family EventResultType (en :: EventTag) :: * where
  EventResultType 'ClickTag = ()
  EventResultType 'DblclickTag = ()
  EventResultType 'KeypressTag = Int
  EventResultType 'ScrollTag = Int
  EventResultType 'MousemoveTag = (Int, Int)
  EventResultType 'MousedownTag = (Int, Int)
  EventResultType 'MouseupTag = (Int, Int)
  EventResultType 'MouseenterTag = ()
  EventResultType 'MouseleaveTag = ()
  EventResultType 'FocusTag = ()
  EventResultType 'BlurTag = ()
  EventResultType 'ChangeTag = ()

wrapDomEventsMaybe :: (Functor (Event t), IsElement e, MonadIO m, MonadSample t m, MonadReflexCreateTrigger t m, Reflex t, HasPostGui t h m) => e -> (forall en. EventName en -> EventM (EventType en) e (Maybe (f en))) -> m (EventSelector t (WrapArg f EventName))
wrapDomEventsMaybe element handlers = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  e <- newFanEventWithTrigger $ \(WrapArg en) et -> do
        unsubscribe <- liftIO $ (onEventName en) element $ do
          mv <- handlers en
          forM_ mv $ \v -> liftIO $ postGui $ runWithActions [et :=> v]
        return $ liftIO $ do
          unsubscribe
  return $! e

getKeyEvent :: EventM UIEvent e Int
getKeyEvent = do
  e <- event
  liftIO $ do
    which <- uiEventGetWhich e
    if which /= 0 then return which else do
      charCode <- uiEventGetCharCode e
      if charCode /= 0 then return charCode else
        uiEventGetKeyCode e

getMouseEventCoords :: EventM MouseEvent e (Int, Int)
getMouseEventCoords = do
  e <- event
  liftIO $ bisequence (mouseEventGetX e, mouseEventGetY e)

defaultDomEventHandler :: IsElement e => e -> EventName en -> EventM (EventType en) e (Maybe (EventResult en))
defaultDomEventHandler e evt = liftM (Just . EventResult) $ case evt of
  Click -> return ()
  Dblclick -> return ()
  Keypress -> getKeyEvent
  Scroll -> liftIO $ elementGetScrollTop e
  Mousemove -> getMouseEventCoords
  Mouseup -> getMouseEventCoords
  Mousedown -> getMouseEventCoords
  Mouseenter -> return ()
  Mouseleave -> return ()
  Focus -> return ()
  Blur -> return ()
  Change -> return ()

wrapElement :: forall t h m. (Functor (Event t), MonadIO m, MonadSample t m, MonadReflexCreateTrigger t m, Reflex t, HasPostGui t h m) => HTMLElement -> m (El t)
wrapElement e = do
  es <- wrapDomEventsMaybe e $ defaultDomEventHandler e
  return $ El e es

elDynAttr' :: forall t m a. MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m (El t, a)
elDynAttr' elementTag attrs child = do
  (e, result) <- buildElement elementTag attrs child
  e' <- wrapElement e
  return (e', result)

{-# INLINABLE elAttr #-}
elAttr :: forall t m a. MonadWidget t m => String -> Map String String -> m a -> m a
elAttr elementTag attrs child = do
  (_, result) <- buildElement elementTag attrs child
  return result

{-# INLINABLE el' #-}
el' :: forall t m a. MonadWidget t m => String -> m a -> m (El t, a)
el' elementTag child = elAttr' elementTag (Map.empty :: AttributeMap) child

{-# INLINABLE elAttr' #-}
elAttr' :: forall t m a. MonadWidget t m => String -> Map String String -> m a -> m (El t, a)
elAttr' elementTag attrs child = do
  (e, result) <- buildElement elementTag attrs child
  e' <- wrapElement e
  return (e', result)

{-# INLINABLE elDynAttr #-}
elDynAttr :: forall t m a. MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m a
elDynAttr elementTag attrs child = do
  (_, result) <- buildElement elementTag attrs child
  return result

{-# INLINABLE el #-}
el :: forall t m a. MonadWidget t m => String -> m a -> m a
el elementTag child = elAttr elementTag Map.empty child

elClass :: forall t m a. MonadWidget t m => String -> String -> m a -> m a
elClass elementTag c child = elAttr elementTag ("class" =: c) child

--------------------------------------------------------------------------------
-- Copied and pasted from Reflex.Widget.Class
--------------------------------------------------------------------------------

-- | Create a dynamically-changing set of widgets from a Dynamic key/value map.
--   Unlike the 'withKey' variants, the child widgets are insensitive to which key they're associated with.
list :: (MonadWidget t m, Ord k) => Dynamic t (Map k v) -> (Dynamic t v -> m a) -> m (Dynamic t (Map k a))
list dm mkChild = listWithKey dm (\_ dv -> mkChild dv)

-- | Create a dynamically-changing set of widgets from a Dynamic list.
simpleList :: MonadWidget t m => Dynamic t [v] -> (Dynamic t v -> m a) -> m (Dynamic t [a])
simpleList xs mkChild = mapDyn (map snd . Map.toList) =<< flip list mkChild =<< mapDyn (Map.fromList . zip [(1::Int)..]) xs

elDynHtml' :: MonadWidget t m => String -> Dynamic t String -> m (El t)
elDynHtml' elementTag html = do
  e <- buildEmptyElement elementTag (Map.empty :: Map String String)
  schedulePostBuild $ liftIO . htmlElementSetInnerHTML e =<< sample (current html)
  addVoidAction $ fmap (liftIO . htmlElementSetInnerHTML e) $ updated html
  wrapElement e

elDynHtmlAttr' :: MonadWidget t m => String -> Map String String -> Dynamic t String -> m (El t)
elDynHtmlAttr' elementTag attrs html = do
  e <- buildEmptyElement elementTag attrs
  schedulePostBuild $ liftIO . htmlElementSetInnerHTML e =<< sample (current html)
  addVoidAction $ fmap (liftIO . htmlElementSetInnerHTML e) $ updated html
  wrapElement e

{-

--TODO: Update dynamically
{-# INLINABLE dynHtml #-}
dynHtml :: MonadWidget t m => Dynamic t String -> m ()
dynHtml ds = do
  let mkSelf h = do
        doc <- askDocument
        Just e <- liftIO $ liftM (fmap castToHTMLElement) $ documentCreateElement doc "div"
        liftIO $ htmlElementSetInnerHTML e h
        return e
  eCreated <- performEvent . fmap mkSelf . tagDyn ds =<< getEInit
  putEChildren $ fmap ((:[]) . toNode) eCreated

-}

data Link t
  = Link { _link_clicked :: Event t ()
         }

class HasDomEvent t a where
  domEvent :: EventName en -> a -> Event t (EventResultType en)

instance Reflex t => HasDomEvent t (El t) where
  domEvent en el = fmap unEventResult $ select (_el_events el) (WrapArg en)

linkClass :: MonadWidget t m => String -> String -> m (Link t)
linkClass s c = do
  (l,_) <- elAttr' "a" ("class" =: c) $ text s
  return $ Link $ domEvent Click l

link :: MonadWidget t m => String -> m (Link t)
link s = linkClass s ""

button :: MonadWidget t m => String -> m (Event t ())
button s = do
  (e, _) <- el' "button" $ text s
  return $ domEvent Click e

newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

workflow :: forall t m a. MonadWidget t m => Workflow t m a -> m (Dynamic t a)
workflow w0 = do
  rec eResult <- widgetHold (unWorkflow w0) $ fmap unWorkflow $ switch $ fmap snd $ current eResult
  mapDyn fst eResult

workflowView :: forall t m a. MonadWidget t m => Workflow t m a -> m (Event t a)
workflowView w0 = do
  rec eResult <- dyn =<< mapDyn unWorkflow =<< holdDyn w0 eReplace
      eReplace <- liftM switch $ hold never $ fmap snd eResult
  return $ fmap fst eResult

divClass :: forall t m a. MonadWidget t m => String -> m a -> m a
divClass = elClass "div"

dtdd :: forall t m a. MonadWidget t m => String -> m a -> m a
dtdd h w = do
  el "dt" $ text h
  el "dd" $ w

blank :: forall t m. MonadWidget t m => m ()
blank = return ()

-- | A widget to display a table with static columns and dynamic rows.
tableDynAttr :: forall t m r k v. (MonadWidget t m, Show k, Ord k)
  => String                                   -- ^ Class applied to <table> element
  -> [(String, k -> Dynamic t r -> m v)]      -- ^ Columns of (header, row key -> row value -> child widget)
  -> Dynamic t (Map k r)                      -- ^ Map from row key to row value
  -> (k -> m (Dynamic t (Map String String))) -- ^ Function to compute <tr> element attributes from row key
  -> m (Dynamic t (Map k (El t, [v])))        -- ^ Map from row key to (El, list of widget return values)
tableDynAttr klass cols dRows rowAttrs = elAttr "div" (Map.singleton "style" "zoom: 1; overflow: auto; background: white;") $ do
    elAttr "table" (Map.singleton "class" klass) $ do
      el "thead" $ el "tr" $ do
        mapM_ (\(h, _) -> el "th" $ text h) cols
      el "tbody" $ do
        listWithKey dRows (\k r -> do
          dAttrs <- rowAttrs k
          elDynAttr' "tr" dAttrs $ mapM (\x -> el "td" $ snd x k r) cols)

--TODO preselect a tab on open
-- | A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
tabDisplay :: forall t m k. (MonadFix m, MonadWidget t m, Show k, Ord k)
  => String               -- ^ Class applied to <ul> element
  -> String               -- ^ Class applied to currently active <li> element
  -> Map k (String, m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
tabDisplay ulClass activeClass tabItems = do
  rec dCurrentTab <- holdDyn Nothing (updated dTabClicks)
      dTabClicks :: Dynamic t (Maybe k) <- elAttr "ul" (Map.singleton "class" ulClass) $ do
        tabClicksList :: [Event t k] <- (liftM Map.elems) $ imapM (\k (s,_) -> headerBarLink s k =<< mapDyn (== (Just k)) dCurrentTab) tabItems
        let eTabClicks :: Event t k = leftmost tabClicksList
        holdDyn Nothing $ fmap Just eTabClicks :: m (Dynamic t (Maybe k))
  divClass "" $ do
    let dTabs :: Dynamic t (Map k (String, m ())) = constDyn tabItems
    _ <- listWithKey dTabs (\k dTab -> do
      dAttrs <- mapDyn (\sel -> do
        let t1 = listToMaybe $ Map.keys tabItems
        if sel == Just k || (sel == Nothing && t1 == Just k) then Map.empty else Map.singleton "style" "display:none;") dCurrentTab 
      elDynAttr "div" dAttrs $ dyn =<< mapDyn snd dTab)
    return ()
  where
    headerBarLink :: (MonadWidget t m, Ord k) => String -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k dBool = do
      dAttributes <- mapDyn (\b -> if b then Map.singleton "class" activeClass else Map.empty) dBool
      elDynAttr "li" dAttributes $ do
        a <- link x
        return $ fmap (const k) (_link_clicked a)

-- | Place an element into the DOM and wrap it with Reflex event handlers.  Note: undefined behavior may result if the element is placed multiple times, removed from the DOM after being placed, or in other situations.  Don't use this unless you understand the internals of MonadWidget.
unsafePlaceElement :: MonadWidget t m => HTMLElement -> m (El t)
unsafePlaceElement e = do
  p <- askParent
  _ <- liftIO $ nodeAppendChild p $ Just e
  wrapElement e

deriveGEq ''EventName
deriveGCompare ''EventName

--------------------------------------------------------------------------------
-- Deprecated functions

{-# DEPRECATED _el_clicked "Use `domEvent Click` instead" #-}
_el_clicked :: Reflex t => El t -> Event t ()
_el_clicked = domEvent Click

{-# DEPRECATED _el_keypress "Use `domEvent Keypress` instead" #-}
_el_keypress :: Reflex t => El t -> Event t Int
_el_keypress = domEvent Keypress

{-# DEPRECATED _el_scrolled "Use `domEvent Scroll` instead" #-}
_el_scrolled :: Reflex t => El t -> Event t Int
_el_scrolled = domEvent Scroll
