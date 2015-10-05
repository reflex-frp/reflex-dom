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
import GHCJS.DOM.EventM (on, event, EventM, stopPropagation)
import GHCJS.DOM.Document
import GHCJS.DOM.Element as E
import GHCJS.DOM.Types hiding (Event)
import qualified GHCJS.DOM.Types as DOM (Event)
import GHCJS.DOM.NamedNodeMap as NNM
import Control.Lens hiding (element, children)
import Data.These
import Data.Align
import Data.Maybe
import Data.GADT.Compare.TH
import Data.Bitraversable
import GHCJS.DOM.MouseEvent
import Data.IORef
import Data.Default

type AttributeMap = Map String String

data ElConfig attrs
  = ElConfig { _elConfig_namespace :: Maybe String
             , _elConfig_attributes :: attrs
             }

makeLenses ''ElConfig

instance (attrs ~ Map String String) => Default (ElConfig attrs) where
  def = ElConfig { _elConfig_namespace = Nothing
                 , _elConfig_attributes = Map.empty
                 }

data El t
  = El { _el_element :: Element
       , _el_events :: EventSelector t (WrapArg EventResult EventName)
       }

class Attributes m a where
  addAttributes :: IsElement e => a -> e -> m ()

instance MonadIO m => Attributes m AttributeMap where
  addAttributes curAttrs e = imapM_ (setAttribute e) curAttrs

instance MonadWidget t m => Attributes m (Dynamic t AttributeMap) where
  addAttributes attrs e = do
    schedulePostBuild $ do
      curAttrs <- sample $ current attrs
      imapM_ (setAttribute e) curAttrs
    addVoidAction $ flip fmap (updated attrs) $ \newAttrs -> liftIO $ do
      oldAttrs <- maybe (return Set.empty) namedNodeMapGetNames =<< getAttributes e
      forM_ (Set.toList $ oldAttrs `Set.difference` Map.keysSet newAttrs) $ removeAttribute e
      imapM_ (setAttribute e) newAttrs --TODO: avoid re-setting unchanged attributes; possibly do the compare using Align in haskell

buildEmptyElementNS :: (MonadWidget t m, Attributes m attrs) => Maybe String -> String -> attrs -> m Element
buildEmptyElementNS mns elementTag attrs = do
  doc <- askDocument
  p <- askParent
  Just e <- liftIO $ case mns of
    Nothing -> createElement doc (Just elementTag)
    Just ns -> createElementNS doc (Just ns) (Just elementTag)
  addAttributes attrs e
  _ <- appendChild p $ Just e
  return $ castToElement e

buildEmptyElement :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m Element
buildEmptyElement = buildEmptyElementNS Nothing

-- We need to decide what type of attrs we've got statically, because it will often be a recursively defined value, in which case inspecting it will lead to a cycle
buildElementNS :: (MonadWidget t m, Attributes m attrs) => Maybe String -> String -> attrs -> m a -> m (Element, a)
buildElementNS mns elementTag attrs child = do
  e <- buildEmptyElementNS mns elementTag attrs
  result <- subWidget (toNode e) child
  return (e, result)

buildElement :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m a -> m (Element, a)
buildElement = buildElementNS Nothing

namedNodeMapGetNames :: NamedNodeMap -> IO (Set String)
namedNodeMapGetNames self = do
  l <- NNM.getLength self
  let locations = if l == 0 then [] else [0..l-1] -- Can't use 0..l-1 if l is 0 because l is unsigned and will wrap around
  liftM (Set.fromList . catMaybes) $ forM locations $ \i -> do
    Just n <- NNM.item self i
    getNodeName n

text :: MonadWidget t m => String -> m ()
text = void . text'

--TODO: Wrap the result
text' :: MonadWidget t m => String -> m Text
text' s = do
  doc <- askDocument
  p <- askParent
  Just n <- createTextNode doc s
  _ <- appendChild p $ Just n
  return n

dynText :: MonadWidget t m => Dynamic t String -> m ()
dynText s = do
  n <- text' ""
  schedulePostBuild $ do
    curS <- sample $ current s
    setNodeValue n $ Just curS
  addVoidAction $ fmap (setNodeValue n . Just) $ updated s

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
  (result0, childVoidAction0) <- do
    p <- askParent
    subWidgetWithVoidActions p child0
  endPlaceholder <- text' ""
  (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
  performEvent_ $ fmap (const $ return ()) newChildBuilt --TODO: Get rid of this hack
  childVoidAction <- hold childVoidAction0 $ fmap snd newChildBuilt
  addVoidAction $ switch childVoidAction --TODO: Should this be a switchPromptly?
  doc <- askDocument
  runWidget <- getRunWidget
  let build c = do
        Just df <- createDocumentFragment doc
        (result, postBuild, voidActions) <- runWidget df c
        runFrameWithTriggerRef newChildBuiltTriggerRef (result, voidActions)
        postBuild
        mp' <- getParentNode endPlaceholder
        forM_ mp' $ \p' -> insertBefore p' (Just df) (Just endPlaceholder)
        return ()
  addVoidAction $ ffor newChild $ \c -> do
    liftIO $ deleteBetweenExclusive startPlaceholder endPlaceholder
    build c
  return (result0, fmap fst newChildBuilt)

diffMapNoEq :: (Ord k) => Map k v -> Map k v -> Map k (Maybe v)
diffMapNoEq olds news = flip Map.mapMaybe (align olds news) $ \case
  This _ -> Just Nothing
  These _ new -> Just $ Just new
  That new -> Just $ Just new

applyMap :: Ord k => Map k v -> Map k (Maybe v) -> Map k v
applyMap olds diffs = flip Map.mapMaybe (align olds diffs) $ \case
  This old -> Just old
  These _ new -> new
  That new -> new

--TODO: Something better than Dynamic t (Map k v) - we want something where the Events carry diffs, not the whole value
listWithKey :: forall t k v m a. (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
listWithKey vals mkChild = do
  postBuild <- getPostBuild
  rec sentVals :: Dynamic t (Map k v) <- foldDyn (flip applyMap) Map.empty changeVals
      let changeVals :: Event t (Map k (Maybe v))
          changeVals = attachWith diffMapNoEq (current sentVals) $ leftmost
                         [ updated vals
                         , tag (current vals) postBuild
                         ]
  listWithKeyShallowDiff Map.empty changeVals $ \k v0 dv -> do
    mkChild k =<< holdDyn v0 dv

{-# DEPRECATED listWithKey' "listWithKey' has been renamed to listWithKeyShallowDiff; also, its behavior has changed to fix a bug where children were always rebuilt (never updated)" #-}
listWithKey' :: (Ord k, MonadWidget t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))
listWithKey' = listWithKeyShallowDiff

-- | Display the given map of items (in key order) using the builder function provided, and update it with the given event.  'Nothing' update entries will delete the corresponding children, and 'Just' entries will create them if they do not exist or send an update event to them if they do.
listWithKeyShallowDiff :: (Ord k, MonadWidget t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))
listWithKeyShallowDiff initialVals valsChanged mkChild = do
  let childValChangedSelector = fanMap $ fmap (Map.mapMaybe id) valsChanged
  sentVals <- foldDyn (flip applyMap) Map.empty $ fmap (fmap (fmap (\_ -> ()))) valsChanged
  let relevantDiff diff _ = case diff of
        Nothing -> Just Nothing -- Even if we let a Nothing through when the element doesn't already exist, this doesn't cause a problem because it is ignored
        Just _ -> Nothing -- We don't want to let spurious re-creations of items through
  listHoldWithKey initialVals (attachWith (flip (Map.differenceWith relevantDiff)) (current sentVals) valsChanged) $ \k v ->
    mkChild k v $ Reflex.select childValChangedSelector $ Const2 k

-- | Display the given map of items using the builder function provided, and update it with the given event.  'Nothing' entries will delete the corresponding children, and 'Just' entries will create or replace them.  Since child events do not take any signal arguments, they are always rebuilt.  To update a child without rebuilding, either embed signals in the map's values, or refer to them directly in the builder function.
listHoldWithKey :: (Ord k, MonadWidget t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (Dynamic t (Map k a))
listHoldWithKey initialVals valsChanged mkChild = do
  doc <- askDocument
  endPlaceholder <- text' ""
  (newChildren, newChildrenTriggerRef) <- newEventWithTriggerRef
--  performEvent_ $ fmap (const $ return ()) newChildren --TODO: Get rid of this hack
  runWidget <- getRunWidget
  let buildChild df k v = runWidget df $ wrapChild k v
      wrapChild k v = do
        childStart <- text' ""
        result <- mkChild k v
        childEnd <- text' ""
        return (result, (childStart, childEnd))
  Just dfOrig <- createDocumentFragment doc
  initialState <- iforM initialVals $ \k v -> subWidgetWithVoidActions (toNode dfOrig) $ wrapChild k v --Note: we have to use subWidgetWithVoidActions rather than runWidget here, because running post-build actions during build can cause not-yet-constructed values to be read
  stateRef <- liftIO $ newIORef initialState
  children <- holdDyn initialState newChildren
  addVoidAction $ switch $ fmap (mergeWith (>>) . map snd . Map.elems) $ current children
  mpOrig <- getParentNode endPlaceholder
  forM_ mpOrig $ \pOrig -> insertBefore pOrig (Just dfOrig) (Just endPlaceholder)
  addVoidAction $ flip fmap valsChanged $ \newVals -> do
    curState <- liftIO $ readIORef stateRef
    --TODO: Should we remove the parent from the DOM first to avoid reflows?
    (newState, postBuild) <- flip runStateT (return ()) $ liftM (Map.mapMaybe id) $ iforM (align curState newVals) $ \k -> \case
      These ((_, (start, end)), _) Nothing -> do -- Deleting child
        liftIO $ deleteBetweenInclusive start end
        return Nothing
      These ((_, (start, end)), _) (Just v) -> do -- Replacing existing child
        liftIO $ deleteBetweenExclusive start end
        Just df <- createDocumentFragment doc
        (childResult, childPostBuild, childVoidAction) <- lift $ buildChild df k v
        let s = (childResult, childVoidAction)
        modify (>>childPostBuild)
        mp <- getParentNode end
        forM_ mp $ \p -> insertBefore p (Just df) (Just end)
        return $ Just s
      That Nothing -> return Nothing -- Deleting non-existent child
      That (Just v) -> do -- Creating new child
        Just df <- createDocumentFragment doc
        (childResult, childPostBuild, childVoidAction) <- lift $ buildChild df k v
        let s = (childResult, childVoidAction)
        modify (>>childPostBuild)
        let placeholder = case Map.lookupGT k curState of
              Nothing -> endPlaceholder
              Just (_, ((_, (start, _)), _)) -> start
        mp <- getParentNode placeholder
        forM_ mp $ \p -> insertBefore p (Just df) (Just placeholder)
        return $ Just s
      This state -> do -- No change
        return $ Just state
    liftIO $ writeIORef stateRef newState
    runFrameWithTriggerRef newChildrenTriggerRef newState
    postBuild
  mapDyn (fmap (fst . fst)) children

--TODO: Something better than Dynamic t (Map k v) - we want something where the Events carry diffs, not the whole value
-- | Create a dynamically-changing set of Event-valued widgets.
--   This is like listWithKey, specialized for widgets returning (Event t a).  listWithKey would return 'Dynamic t (Map k (Event t a))' in this scenario, but listViewWithKey flattens this to 'Event t (Map k a)' via 'switch'.
listViewWithKey :: (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m (Event t a)) -> m (Event t (Map k a))
listViewWithKey vals mkChild = liftM (switch . fmap mergeMap) $ listViewWithKey' vals mkChild

listViewWithKey' :: (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Behavior t (Map k a))
listViewWithKey' vals mkChild = liftM current $ listWithKey vals mkChild

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
  mCurrentParent <- getParentNode e -- May be different than it was at initial construction, e.g., because the parent may have dumped us in from a DocumentFragment
  case mCurrentParent of
    Nothing -> return () --TODO: Is this the right behavior?
    Just currentParent -> do
      let go = do
            Just x <- getPreviousSibling e -- This can't be Nothing because we should hit 's' first
            when (toNode s /= toNode x) $ do
              _ <- removeChild currentParent $ Just x
              go
      go

-- | s and e must both be children of the same node and s must precede e
deleteBetweenInclusive :: (IsNode start, IsNode end) => start -> end -> IO ()
deleteBetweenInclusive s e = do
  mCurrentParent <- getParentNode e -- May be different than it was at initial construction, e.g., because the parent may have dumped us in from a DocumentFragment
  case mCurrentParent of
    Nothing -> return () --TODO: Is this the right behavior?
    Just currentParent -> do
      let go = do
            Just x <- getPreviousSibling e -- This can't be Nothing because we should hit 's' first
            _ <- removeChild currentParent $ Just x
            when (toNode s /= toNode x) go
      go
      _ <- removeChild currentParent $ Just e
      return ()

--------------------------------------------------------------------------------
-- Adapters
--------------------------------------------------------------------------------

wrapDomEvent :: (Functor (Event t), MonadIO m, MonadSample t m, MonadReflexCreateTrigger t m, Reflex t, HasPostGui t h m) => e -> (e -> EventM e event () -> IO (IO ())) -> EventM e event a -> m (Event t a)
wrapDomEvent element elementOnevent getValue = wrapDomEventMaybe element elementOnevent $ liftM Just getValue

wrapDomEventMaybe :: (Functor (Event t), MonadIO m, MonadSample t m, MonadReflexCreateTrigger t m, Reflex t, HasPostGui t h m) => e -> (e -> EventM e event () -> IO (IO ())) -> EventM e event (Maybe a) -> m (Event t a)
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
   | MousewheelTag
   | ScrollTag
   | SelectTag
   | SubmitTag
   | WheelTag
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
  Mousewheel :: EventName 'MousewheelTag
  Scroll :: EventName 'ScrollTag
  Select :: EventName 'SelectTag
  Submit :: EventName 'SubmitTag
  Wheel :: EventName 'WheelTag
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
  EventType 'BlurTag = FocusEvent
  EventType 'ChangeTag = DOM.Event
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
  EventType 'FocusTag = FocusEvent
  EventType 'InputTag = DOM.Event
  EventType 'InvalidTag = DOM.Event
  EventType 'KeydownTag = KeyboardEvent
  EventType 'KeypressTag = KeyboardEvent
  EventType 'KeyupTag = KeyboardEvent
  EventType 'LoadTag = UIEvent
  EventType 'MousedownTag = MouseEvent
  EventType 'MouseenterTag = MouseEvent
  EventType 'MouseleaveTag = MouseEvent
  EventType 'MousemoveTag = MouseEvent
  EventType 'MouseoutTag = MouseEvent
  EventType 'MouseoverTag = MouseEvent
  EventType 'MouseupTag = MouseEvent
  EventType 'MousewheelTag = MouseEvent
  EventType 'ScrollTag = UIEvent
  EventType 'SelectTag = UIEvent
  EventType 'SubmitTag = DOM.Event
  EventType 'WheelTag = WheelEvent
  EventType 'BeforecutTag = DOM.Event
  EventType 'CutTag = DOM.Event
  EventType 'BeforecopyTag = DOM.Event
  EventType 'CopyTag = DOM.Event
  EventType 'BeforepasteTag = DOM.Event
  EventType 'PasteTag = DOM.Event
  EventType 'ResetTag = DOM.Event
  EventType 'SearchTag = DOM.Event
  EventType 'SelectstartTag = DOM.Event
  EventType 'TouchstartTag = TouchEvent
  EventType 'TouchmoveTag = TouchEvent
  EventType 'TouchendTag = TouchEvent
  EventType 'TouchcancelTag = TouchEvent

onEventName :: IsElement e => EventName en -> e -> EventM e (EventType en) () -> IO (IO ())
onEventName en e = case en of
  Abort -> on e E.abort
  Blur -> on e E.blurEvent
  Change -> on e E.change
  Click -> on e E.click
  Contextmenu -> on e E.contextMenu
  Dblclick -> on e E.dblClick
  Drag -> on e E.drag
  Dragend -> on e E.dragEnd
  Dragenter -> on e E.dragEnter
  Dragleave -> on e E.dragLeave
  Dragover -> on e E.dragOver
  Dragstart -> on e E.dragStart
  Drop -> on e E.drop
  Error -> on e E.error
  Focus -> on e E.focusEvent
  Input -> on e E.input
  Invalid -> on e E.invalid
  Keydown -> on e E.keyDown
  Keypress -> on e E.keyPress
  Keyup -> on e E.keyUp
  Load -> on e E.load
  Mousedown -> on e E.mouseDown
  Mouseenter -> on e E.mouseEnter
  Mouseleave -> on e E.mouseLeave
  Mousemove -> on e E.mouseMove
  Mouseout -> on e E.mouseOut
  Mouseover -> on e E.mouseOver
  Mouseup -> on e E.mouseUp
  Mousewheel -> on e E.mouseWheel
  Scroll -> on e E.scroll
  Select -> on e E.select
  Submit -> on e E.submit
  Wheel -> on e E.wheel
  Beforecut -> on e E.beforeCut
  Cut -> on e E.cut
  Beforecopy -> on e E.beforeCopy
  Copy -> on e E.copy
  Beforepaste -> on e E.beforePaste
  Paste -> on e E.paste
  Reset -> on e E.reset
  Search -> on e E.search
  Selectstart -> on e E.selectStart
  Touchstart -> on e E.touchStart
  Touchmove -> on e E.touchMove
  Touchend -> on e E.touchEnd
  Touchcancel -> on e E.touchCancel

newtype EventResult en = EventResult { unEventResult :: EventResultType en }

type family EventResultType (en :: EventTag) :: * where
  EventResultType 'ClickTag = ()
  EventResultType 'DblclickTag = ()
  EventResultType 'KeypressTag = Int
  EventResultType 'KeydownTag = Int
  EventResultType 'KeyupTag = Int
  EventResultType 'ScrollTag = Int
  EventResultType 'MousemoveTag = (Int, Int)
  EventResultType 'MousedownTag = (Int, Int)
  EventResultType 'MouseupTag = (Int, Int)
  EventResultType 'MouseenterTag = ()
  EventResultType 'MouseleaveTag = ()
  EventResultType 'FocusTag = ()
  EventResultType 'BlurTag = ()
  EventResultType 'ChangeTag = ()
  EventResultType 'DragTag = ()
  EventResultType 'DragendTag = ()
  EventResultType 'DragenterTag = ()
  EventResultType 'DragleaveTag = ()
  EventResultType 'DragoverTag = ()
  EventResultType 'DragstartTag = ()
  EventResultType 'DropTag = ()
  EventResultType 'AbortTag = ()
  EventResultType 'ContextmenuTag = ()
  EventResultType 'ErrorTag = ()
  EventResultType 'InputTag = ()
  EventResultType 'InvalidTag = ()
  EventResultType 'LoadTag = ()
  EventResultType 'MouseoutTag = ()
  EventResultType 'MouseoverTag = ()
  EventResultType 'SelectTag = ()
  EventResultType 'SubmitTag = ()
  EventResultType 'BeforecutTag = ()
  EventResultType 'CutTag = ()
  EventResultType 'BeforecopyTag = ()
  EventResultType 'CopyTag = ()
  EventResultType 'BeforepasteTag = ()
  EventResultType 'PasteTag = ()
  EventResultType 'ResetTag = ()
  EventResultType 'SearchTag = ()
  EventResultType 'SelectstartTag = ()
  EventResultType 'TouchstartTag = ()
  EventResultType 'TouchmoveTag = ()
  EventResultType 'TouchendTag = ()
  EventResultType 'TouchcancelTag = ()

wrapDomEventsMaybe :: (Functor (Event t), IsElement e, MonadIO m, MonadSample t m, MonadReflexCreateTrigger t m, Reflex t, HasPostGui t h m) => e -> (forall en. EventName en -> EventM e (EventType en) (Maybe (f en))) -> m (EventSelector t (WrapArg f EventName))
wrapDomEventsMaybe element handlers = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  e <- newFanEventWithTrigger $ \(WrapArg en) et -> do
        unsubscribe <- onEventName en element $ do
          mv <- handlers en
          forM_ mv $ \v -> liftIO $ postGui $ runWithActions [et :=> v]
        return $ liftIO $ do
          unsubscribe
  return $! e

getKeyEvent :: EventM e KeyboardEvent Int
getKeyEvent = do
  e <- event
  which <- getWhich e
  if which /= 0 then return which else do
    charCode <- getCharCode e
    if charCode /= 0 then return charCode else
      getKeyCode e

getMouseEventCoords :: EventM e MouseEvent (Int, Int)
getMouseEventCoords = do
  e <- event
  bisequence (getX e, getY e)

defaultDomEventHandler :: IsElement e => e -> EventName en -> EventM e (EventType en) (Maybe (EventResult en))
defaultDomEventHandler e evt = liftM (Just . EventResult) $ case evt of
  Click -> return ()
  Dblclick -> return ()
  Keypress -> getKeyEvent
  Scroll -> getScrollTop e
  Keydown -> getKeyEvent
  Keyup -> getKeyEvent
  Mousemove -> getMouseEventCoords
  Mouseup -> getMouseEventCoords
  Mousedown -> getMouseEventCoords
  Mouseenter -> return ()
  Mouseleave -> return ()
  Focus -> return ()
  Blur -> return ()
  Change -> return ()
  Drag -> return ()
  Dragend -> return ()
  Dragenter -> return ()
  Dragleave -> return ()
  Dragover -> return ()
  Dragstart -> return ()
  Drop -> return ()
  Abort -> return ()
  Contextmenu -> return ()
  Error -> return ()
  Input -> return ()
  Invalid -> return ()
  Load -> return ()
  Mouseout -> return ()
  Mouseover -> return ()
  Select -> return ()
  Submit -> return ()
  Beforecut -> return ()
  Cut -> return ()
  Beforecopy -> return ()
  Copy -> return ()
  Beforepaste -> return ()
  Paste -> return ()
  Reset -> return ()
  Search -> return ()
  Selectstart -> return ()
  Touchstart -> return ()
  Touchmove -> return ()
  Touchend -> return ()
  Touchcancel -> return ()

wrapElement :: forall t h m. (Functor (Event t), MonadIO m, MonadSample t m, MonadReflexCreateTrigger t m, Reflex t, HasPostGui t h m) => (forall en. Element -> EventName en -> EventM Element (EventType en) (Maybe (EventResult en))) -> Element -> m (El t)
wrapElement eh e = do
  es <- wrapDomEventsMaybe e $ eh e
  return $ El e es

{-# INLINABLE elStopPropagationNS #-}
elStopPropagationNS :: (MonadWidget t m, IsEvent (EventType en)) => Maybe String -> String -> EventName en -> m a -> m a
elStopPropagationNS mns elementTag evt child = do
  (e, result) <- buildElementNS mns elementTag (Map.empty :: Map String String) child
  _ <- liftIO $ onEventName evt e stopPropagation
  return result

{-# INLINABLE elWith #-}
elWith :: (MonadWidget t m, Attributes m attrs) => String -> ElConfig attrs -> m a -> m a
elWith elementTag cfg child = do
  (_, result) <- buildElementNS (cfg ^. namespace) elementTag (cfg ^. attributes) child
  return result

{-# INLINABLE elWith' #-}
elWith' :: (MonadWidget t m, Attributes m attrs) => String -> ElConfig attrs -> m a -> m (El t, a)
elWith' elementTag cfg child = do
  (e, result) <- buildElementNS (cfg ^. namespace) elementTag (cfg ^. attributes) child
  e' <- wrapElement defaultDomEventHandler e
  return (e', result)

{-# INLINABLE emptyElWith #-}
emptyElWith :: (MonadWidget t m, Attributes m attrs) => String -> ElConfig attrs -> m ()
emptyElWith elementTag cfg = do
  _ <- buildEmptyElementNS (cfg ^. namespace) elementTag (cfg ^. attributes)
  return ()

{-# INLINABLE emptyElWith' #-}
emptyElWith' :: (MonadWidget t m, Attributes m attrs) => String -> ElConfig attrs -> m (El t)
emptyElWith' elementTag cfg = do
  wrapElement defaultDomEventHandler =<< buildEmptyElementNS (cfg ^. namespace) elementTag (cfg ^. attributes)

{-# INLINABLE elDynAttrNS' #-}
elDynAttrNS' :: forall t m a. MonadWidget t m => Maybe String -> String -> Dynamic t (Map String String) -> m a -> m (El t, a)
elDynAttrNS' mns elementTag attrs = elWith' elementTag $
  def & namespace .~ mns
      & elConfig_attributes .~ attrs

{-# INLINABLE elDynAttr' #-}
elDynAttr' :: forall t m a. MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m (El t, a)
elDynAttr' elementTag attrs = elWith' elementTag $ def & elConfig_attributes .~ attrs

{-# INLINABLE elAttr #-}
elAttr :: forall t m a. MonadWidget t m => String -> Map String String -> m a -> m a
elAttr elementTag attrs = elWith elementTag $ def & attributes .~ attrs

{-# INLINABLE el' #-}
el' :: forall t m a. MonadWidget t m => String -> m a -> m (El t, a)
el' elementTag = elWith' elementTag def

{-# INLINABLE elAttr' #-}
elAttr' :: forall t m a. MonadWidget t m => String -> Map String String -> m a -> m (El t, a)
elAttr' elementTag attrs = elWith' elementTag $ def & attributes .~ attrs

{-# INLINABLE elDynAttr #-}
elDynAttr :: forall t m a. MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m a
elDynAttr elementTag attrs = elWith elementTag $ def & elConfig_attributes .~ attrs

{-# INLINABLE el #-}
el :: forall t m a. MonadWidget t m => String -> m a -> m a
el elementTag = elWith elementTag def

elClass :: forall t m a. MonadWidget t m => String -> String -> m a -> m a
elClass elementTag c = elWith elementTag $ def & attributes .~ "class" =: c

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
  schedulePostBuild $ setInnerHTML e . Just =<< sample (current html)
  addVoidAction $ fmap (setInnerHTML e . Just) $ updated html
  wrapElement defaultDomEventHandler e

elDynHtmlAttr' :: MonadWidget t m => String -> Map String String -> Dynamic t String -> m (El t)
elDynHtmlAttr' elementTag attrs html = do
  e <- buildEmptyElement elementTag attrs
  schedulePostBuild $ setInnerHTML e . Just =<< sample (current html)
  addVoidAction $ fmap (setInnerHTML e . Just) $ updated html
  wrapElement defaultDomEventHandler e

data Link t
  = Link { _link_clicked :: Event t ()
         }

class HasAttributes a where
  type Attrs a :: *
  attributes :: Lens' a (Attrs a)

instance HasAttributes (ElConfig attrs) where
  type Attrs (ElConfig attrs) = attrs
  attributes = elConfig_attributes

class HasNamespace a where
  namespace :: Lens' a (Maybe String)

instance HasNamespace (ElConfig attrs) where
  namespace = elConfig_namespace

class HasDomEvent t a where
  domEvent :: EventName en -> a -> Event t (EventResultType en)

instance Reflex t => HasDomEvent t (El t) where
  domEvent en e = fmap unEventResult $ Reflex.select (_el_events e) (WrapArg en)

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
unsafePlaceElement :: MonadWidget t m => Element -> m (El t)
unsafePlaceElement e = do
  p <- askParent
  _ <- appendChild p $ Just e
  wrapElement defaultDomEventHandler e

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
