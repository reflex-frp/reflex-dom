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
import GHCJS.DOM.EventM (on, event, EventM)
import GHCJS.DOM.Document
import GHCJS.DOM.Element as E
import GHCJS.DOM.HTMLElement
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

type AttributeMap = Map String String

data El t
  = El { _el_element :: HTMLElement
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

buildEmptyElement :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m HTMLElement
buildEmptyElement elementTag attrs = do
  doc <- askDocument
  p <- askParent
  Just e <- createElement doc (Just elementTag)
  addAttributes attrs e
  _ <- appendChild p $ Just e
  return $ castToHTMLElement e

-- We need to decide what type of attrs we've got statically, because it will often be a recursively defined value, in which case inspecting it will lead to a cycle
buildElement :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m a -> m (HTMLElement, a)
buildElement elementTag attrs child = do
  e <- buildEmptyElement elementTag attrs
  result <- subWidget (toNode e) child
  return (e, result)

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
dyn :: MonadWidget t m => Dynamic t (m a) -> m (Event t a)
dyn child = do
  startPlaceholder <- text' ""
  endPlaceholder <- text' ""
  (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
  let e = fmap snd newChildBuilt --TODO: Get rid of this hack
  childVoidAction <- hold never e
  performEvent_ $ fmap (const $ return ()) e --TODO: Get rid of this hack
  addVoidAction $ switch childVoidAction
  doc <- askDocument
  runWidget <- getRunWidget
  let build c = do
        Just df <- createDocumentFragment doc
        (result, postBuild, voidActions) <- runWidget df c
        runFrameWithTriggerRef newChildBuiltTriggerRef (result, voidActions)
        postBuild
        Just p <- getParentNode endPlaceholder
        _ <- insertBefore p (Just df) (Just endPlaceholder)
        return ()
  schedulePostBuild $ do
    c <- sample $ current child
    build c
  addVoidAction $ ffor (updated child) $ \newChild -> do
    liftIO $ deleteBetweenExclusive startPlaceholder endPlaceholder
    build newChild
  return $ fmap fst newChildBuilt

widgetHold :: MonadWidget t m => m a -> Event t (m a) -> m (Dynamic t a)
widgetHold child0 newChild = do
  startPlaceholder <- text' ""
  result0 <- child0 -- I'm pretty sure this is wrong; the void actions should get removed when the child is swapped out
  endPlaceholder <- text' ""
  (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
  performEvent_ $ fmap (const $ return ()) newChildBuilt --TODO: Get rid of this hack
  childVoidAction <- hold never $ fmap snd newChildBuilt
  addVoidAction $ switch childVoidAction --TODO: Should this be a switchPromptly?
  doc <- askDocument
  runWidget <- getRunWidget
  let build c = do
        Just df <- createDocumentFragment doc
        (result, postBuild, voidActions) <- runWidget df c
        runFrameWithTriggerRef newChildBuiltTriggerRef (result, voidActions)
        postBuild
        mp <- getParentNode endPlaceholder
        case mp of
          Nothing -> return () --TODO: Is this right?
          Just p -> do
            _ <- insertBefore p (Just df) (Just endPlaceholder)
            return ()
        return ()
  addVoidAction $ ffor newChild $ \c -> do
    liftIO $ deleteBetweenExclusive startPlaceholder endPlaceholder
    build c
  holdDyn result0 $ fmap fst newChildBuilt

--TODO: Something better than Dynamic t (Map k v) - we want something where the Events carry diffs, not the whole value
listWithKey :: (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
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
    Just df <- createDocumentFragment doc
    curVals <- sample $ current vals
    initialState <- iforM curVals $ \k v -> do
      (result, postBuild, voidAction) <- buildChild df k v
      return ((result, voidAction), postBuild)
    runFrameWithTriggerRef newChildrenTriggerRef $ fmap fst initialState --TODO: Do all these in a single runFrame
    sequence_ $ fmap snd initialState
    Just p <- getParentNode endPlaceholder
    _ <- insertBefore p (Just df) (Just endPlaceholder)
    return ()
  addVoidAction $ flip fmap (updated vals) $ \newVals -> do
    curState <- sample children
    --TODO: Should we remove the parent from the DOM first to avoid reflows?
    (newState, postBuild) <- flip runStateT (return ()) $ liftM (Map.mapMaybe id) $ iforM (align curState newVals) $ \k -> \case
      This ((_, (start, end)), _) -> do
        liftIO $ deleteBetweenInclusive start end
        return Nothing
      That v -> do
        Just df <- createDocumentFragment doc
        (childResult, childPostBuild, childVoidAction) <- lift $ buildChild df k v
        let s = (childResult, childVoidAction)
        modify (>>childPostBuild)
        let placeholder = case Map.lookupGT k curState of
              Nothing -> endPlaceholder
              Just (_, ((_, (start, _)), _)) -> start
        Just p <- getParentNode placeholder
        _ <- insertBefore p (Just df) (Just placeholder)
        return $ Just s
      These state _ -> do
        return $ Just state
    runFrameWithTriggerRef newChildrenTriggerRef newState
    postBuild
  holdDyn Map.empty $ fmap (fmap (fst . fst)) newChildren

listWithKey' :: forall t m k v a. (Ord k, MonadWidget t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))
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
        result <- mkChild k v $ Reflex.select childValChangedSelector $ Const2 k
        childEnd <- text' ""
        return (result, (childStart, childEnd))
  Just dfOrig <- createDocumentFragment doc
  initialState <- iforM initialVals $ \k v -> subWidgetWithVoidActions (toNode dfOrig) $ wrapChild k v --Note: we have to use subWidgetWithVoidActions rather than runWidget here, because running post-build actions during build can cause not-yet-constructed values to be read
  children <- holdDyn initialState newChildren
  addVoidAction $ switch $ fmap (mergeWith (>>) . map snd . Map.elems) $ current children
  Just pOrig <- getParentNode endPlaceholder
  _ <- insertBefore pOrig (Just dfOrig) (Just endPlaceholder)
  addVoidAction $ flip fmap valsChanged $ \newVals -> do
    curState <- sample $ current children
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
        Just p <- getParentNode end
        _ <- insertBefore p (Just df) (Just end)
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
        Just p <- getParentNode placeholder
        _ <- insertBefore p (Just df) (Just placeholder)
        return $ Just s
      This state -> do -- No change
        return $ Just state
    runFrameWithTriggerRef newChildrenTriggerRef newState
    postBuild
  mapDyn (fmap (fst . fst)) children

--TODO: Something better than Dynamic t (Map k v) - we want something where the Events carry diffs, not the whole value
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
    Just df <- createDocumentFragment doc
    curVals <- sample $ current vals
    initialState <- iforM curVals $ \k v -> do
      (result, postBuild, voidAction) <- buildChild df k v
      return ((result, voidAction), postBuild)
    runFrameWithTriggerRef newChildrenTriggerRef $ fmap fst initialState --TODO: Do all these in a single runFrame
    sequence_ $ fmap snd initialState
    Just p <- getParentNode endPlaceholder
    _ <- insertBefore p (Just df) (Just endPlaceholder)
    return ()
  addVoidAction $ flip fmap (updated vals) $ \newVals -> do
    curState <- sample children
    --TODO: Should we remove the parent from the DOM first to avoid reflows?
    (newState, postBuild) <- flip runStateT (return ()) $ liftM (Map.mapMaybe id) $ iforM (align curState newVals) $ \k -> \case
      This ((_, (start, end)), _) -> do
        liftIO $ deleteBetweenInclusive start end
        return Nothing
      That v -> do
        Just df <- createDocumentFragment doc
        (childResult, childPostBuild, childVoidAction) <- lift $ buildChild df k v
        let s = (childResult, childVoidAction)
        modify (>>childPostBuild)
        let placeholder = case Map.lookupGT k curState of
              Nothing -> endPlaceholder
              Just (_, ((_, (start, _)), _)) -> start
        Just p <- getParentNode placeholder
        _ <- insertBefore p (Just df) (Just placeholder)
        return $ Just s
      These state _ -> do
        return $ Just state
    runFrameWithTriggerRef newChildrenTriggerRef newState
    postBuild
  return $ fmap (fmap (fst . fst)) children

--TODO: Deduplicate the various list*WithKey* implementations

selectViewListWithKey_ :: forall t m k v a. (MonadWidget t m, Ord k) => Dynamic t k -> Dynamic t (Map k v) -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -> m (Event t k)
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
  EventResultType 'ScrollTag = Int
  EventResultType 'MousemoveTag = (Int, Int)
  EventResultType 'MousedownTag = (Int, Int)
  EventResultType 'MouseupTag = (Int, Int)
  EventResultType 'MouseenterTag = ()
  EventResultType 'MouseleaveTag = ()
  EventResultType 'FocusTag = ()
  EventResultType 'BlurTag = ()

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
  Mousemove -> getMouseEventCoords
  Mouseup -> getMouseEventCoords
  Mousedown -> getMouseEventCoords
  Mouseenter -> return ()
  Mouseleave -> return ()
  Focus -> return ()
  Blur -> return ()

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

list :: (MonadWidget t m, Ord k) => Dynamic t (Map k v) -> (Dynamic t v -> m a) -> m (Dynamic t (Map k a))
list dm mkChild = listWithKey dm (\_ dv -> mkChild dv)

simpleList :: MonadWidget t m => Dynamic t [v] -> (Dynamic t v -> m a) -> m (Dynamic t [a])
simpleList xs mkChild = mapDyn (map snd . Map.toList) =<< flip list mkChild =<< mapDyn (Map.fromList . zip [(1::Int)..]) xs

elDynHtml' :: MonadWidget t m => String -> Dynamic t String -> m (El t)
elDynHtml' elementTag html = do
  e <- buildEmptyElement elementTag (Map.empty :: Map String String)
  schedulePostBuild $ setInnerHTML e . Just =<< sample (current html)
  addVoidAction $ fmap (setInnerHTML e . Just) $ updated html
  wrapElement e

elDynHtmlAttr' :: MonadWidget t m => String -> Map String String -> Dynamic t String -> m (El t)
elDynHtmlAttr' elementTag attrs html = do
  e <- buildEmptyElement elementTag attrs
  schedulePostBuild $ setInnerHTML e . Just =<< sample (current html)
  addVoidAction $ fmap (setInnerHTML e . Just) $ updated html
  wrapElement e

{-

--TODO: Update dynamically
{-# INLINABLE dynHtml #-}
dynHtml :: MonadWidget t m => Dynamic t String -> m ()
dynHtml ds = do
  let mkSelf h = do
        doc <- askDocument
        Just e <- liftM (fmap castToHTMLElement) $ createElement doc "div"
        setInnerHtml e h
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
  domEvent en el = fmap unEventResult $ Reflex.select (_el_events el) (WrapArg en)

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

tableDynAttr :: forall t m r k v. (MonadWidget t m, Show k, Ord k) => String -> [(String, k -> Dynamic t r -> m v)] -> Dynamic t (Map k r) -> (k -> m (Dynamic t (Map String String))) -> m (Dynamic t (Map k (El t, [v])))
tableDynAttr klass cols dRows rowAttrs = elAttr "div" (Map.singleton "style" "zoom: 1; overflow: auto; background: white;") $ do
    elAttr "table" (Map.singleton "class" klass) $ do
      el "thead" $ el "tr" $ do
        mapM_ (\(h, _) -> el "th" $ text h) cols
      el "tbody" $ do
        listWithKey dRows (\k r -> do
          dAttrs <- rowAttrs k
          elDynAttr' "tr" dAttrs $ mapM (\x -> el "td" $ snd x k r) cols)

--TODO preselect a tab on open
tabDisplay :: forall t m k. (MonadFix m, MonadWidget t m, Show k, Ord k) => String -> String -> Map k (String, m ()) -> m ()
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
  _ <- appendChild p $ Just e
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
