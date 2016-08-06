{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Builder.Immediate where

import Foreign.JavaScript.TH
import Reflex
import Reflex.Dom.Builder.Class
import Reflex.Dom.PerformEvent.Class
import Reflex.Dom.PostBuild.Class
import Reflex.Host.Class

import Control.Concurrent.Chan
import Control.Lens hiding (element)
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Control
import Data.Bitraversable
import Data.Default
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Misc
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import GHCJS.DOM.Document (Document, createDocumentFragment, createElement, createElementNS, createTextNode)
import GHCJS.DOM.Element (getScrollTop, removeAttribute, removeAttributeNS, setAttribute, setAttributeNS)
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.Event as Event
import GHCJS.DOM.EventM (EventM, event, on)
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
import GHCJS.DOM.MouseEvent
import GHCJS.DOM.Node (appendChild, getOwnerDocument, getParentNode, getPreviousSibling, insertBefore,
                       removeChild, toNode, setNodeValue)
import GHCJS.DOM.Types (FocusEvent, IsElement, IsEvent, IsNode, KeyboardEvent, Node, ToDOMString, TouchEvent,
                        WheelEvent, castToHTMLInputElement, castToHTMLTextAreaElement)
import qualified GHCJS.DOM.Types as DOM
import GHCJS.DOM.UIEvent
import qualified GHCJS.DOM.Window as Window

import Debug.Trace hiding (traceEvent)

data TriggerRef t a = TriggerRef { unTriggerRef :: IORef (Maybe (EventTrigger t a)) }

data TriggerInvocation a = TriggerInvocation a (IO ())

data ImmediateDomBuilderEnv t
   = ImmediateDomBuilderEnv { _immediateDomBuilderEnv_document :: Document
                            , _immediateDomBuilderEnv_parent :: Node
                            , _immediateDomBuilderEnv_events :: Chan [DSum (TriggerRef t) TriggerInvocation]
                            }

newtype ImmediateDomBuilderT t m a = ImmediateDomBuilderT { unImmediateDomBuilderT :: ReaderT (ImmediateDomBuilderEnv t) m a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance MonadTransControl (ImmediateDomBuilderT t) where
  type StT (ImmediateDomBuilderT t) a = StT (ReaderT (ImmediateDomBuilderEnv t)) a
  liftWith = defaultLiftWith ImmediateDomBuilderT unImmediateDomBuilderT
  restoreT = defaultRestoreT ImmediateDomBuilderT

instance MonadTrans (ImmediateDomBuilderT t) where
  lift = ImmediateDomBuilderT . lift

{-# INLINABLE runImmediateDomBuilderT #-}
runImmediateDomBuilderT :: ImmediateDomBuilderT t m a -> ImmediateDomBuilderEnv t -> m a
runImmediateDomBuilderT (ImmediateDomBuilderT a) = runReaderT a

{-# INLINABLE askDocument #-}
askDocument :: Monad m => ImmediateDomBuilderT t m Document
askDocument = ImmediateDomBuilderT $ asks _immediateDomBuilderEnv_document

{-# INLINABLE askParent #-}
askParent :: Monad m => ImmediateDomBuilderT t m Node
askParent = ImmediateDomBuilderT $ asks _immediateDomBuilderEnv_parent

{-# INLINABLE askEvents #-}
askEvents :: Monad m => ImmediateDomBuilderT t m (Chan [DSum (TriggerRef t) TriggerInvocation])
askEvents = ImmediateDomBuilderT $ asks _immediateDomBuilderEnv_events

{-# INLINABLE append #-}
append :: (IsNode n, MonadIO m) => n -> ImmediateDomBuilderT t m ()
append n = do
  p <- askParent
  _ <- liftIO $ appendChild p $ Just n
  return ()

{-# INLINABLE textNodeInternal #-}
textNodeInternal :: (MonadIO m, ToDOMString contents) => contents -> ImmediateDomBuilderT t m DOM.Text
textNodeInternal t = do
  doc <- askDocument
  Just n <- liftIO $ createTextNode doc t
  append n
  return n

-- | s and e must both be children of the same node and s must precede e
{-# INLINABLE deleteBetweenInclusive #-}
deleteBetweenInclusive :: (MonadIO m, IsNode start, IsNode end) => start -> end -> m ()
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

type SupportsImmediateDomBuilder t m = (Reflex t, MonadIO m, MonadHold t m, MonadFix m, PerformEvent t m, Performable m ~ m, MonadReflexCreateTrigger t m, Deletable t m, MonadRef m, Ref m ~ Ref IO)

newtype EventFilterTriggerRef t er (en :: EventTag) = EventFilterTriggerRef (IORef (Maybe (EventTrigger t (er en))))

{-# INLINABLE makeElement #-}
makeElement :: forall er t m a. SupportsImmediateDomBuilder t m => Text -> ElementConfig er t (ImmediateDomBuilderT t m) -> ImmediateDomBuilderT t m a -> ImmediateDomBuilderT t m ((Element er GhcjsDomSpace t, a), DOM.Element)
makeElement elementTag cfg child = do
  doc <- askDocument
  Just e <- ImmediateDomBuilderT $ case cfg ^. namespace of
    Nothing -> createElement doc (Just elementTag)
    Just ens -> createElementNS doc (Just ens) (Just elementTag)
  ImmediateDomBuilderT $ iforM_ (cfg ^. initialAttributes) $ \(mAttrNamespace, n) v -> case mAttrNamespace of
    Nothing -> setAttribute e n v
    Just ans -> setAttributeNS e (Just ans) n v
  events <- askEvents
  result <- lift $ runImmediateDomBuilderT child $ ImmediateDomBuilderEnv
    { _immediateDomBuilderEnv_parent = toNode e
    , _immediateDomBuilderEnv_document = doc
    , _immediateDomBuilderEnv_events = events
    }
  append e
  lift $ performEvent_ $ ffor (cfg ^. modifyAttributes) $ imapM_ $ \(mAttrNamespace, n) mv -> case mAttrNamespace of
    Nothing -> maybe (removeAttribute e n) (setAttribute e n) mv
    Just ns -> maybe (removeAttributeNS e (Just ns) n) (setAttributeNS e (Just ns) n) mv
  eventTriggerRefs :: DMap EventName (EventFilterTriggerRef t er) <- liftIO $ fmap DMap.fromList $ forM (DMap.toList $ _elementConfig_eventFilters cfg) $ \(en :=> EventFilter (GhcjsDomHandler f)) -> do
    triggerRef <- newIORef Nothing
    _ <- elementOnEventName en e $ do
      evt <- DOM.event
      (flags, GhcjsDomHandler k) <- liftIO $ f $ GhcjsDomEvent evt
      when (_eventFlags_preventDefault flags) $ withIsEvent en DOM.preventDefault
      case _eventFlags_propagation flags of
        Propagation_Continue -> return ()
        Propagation_Stop -> withIsEvent en DOM.stopPropagation
        Propagation_StopImmediate -> withIsEvent en DOM.stopImmediatePropagation
      mv <- liftIO $ k ()
      liftIO $ forM_ mv $ \v -> writeChan events [TriggerRef triggerRef :=> TriggerInvocation v (return ())]
    return $ en :=> EventFilterTriggerRef triggerRef
  es <- newFanEventWithTrigger $ \(WrapArg en) t -> do
    case DMap.lookup en eventTriggerRefs of
      Just (EventFilterTriggerRef r) -> do
        writeIORef r $ Just t
        return $ do
          writeIORef r Nothing
      Nothing -> elementOnEventName en e $ do
        evt <- DOM.event
        let GhcjsDomHandler1 h = _elementConfig_eventHandler cfg
        mv <- liftIO $ h $ Pair1 en (GhcjsDomEvent evt)
        case mv of
          Nothing1 -> return ()
          Just1 v -> liftIO $ do
            --TODO: I don't think this is quite right: if a new trigger is created between when this is enqueued and when it fires, this may not work quite right
            ref <- newIORef $ Just t
            writeChan events [TriggerRef ref :=> TriggerInvocation v (return ())]
  return ((Element es e, result), e)

newtype GhcjsDomHandler a b = GhcjsDomHandler { unGhcjsDomHandler :: a -> IO b }

newtype GhcjsDomHandler1 a b = GhcjsDomHandler1 { unGhcjsDomHandler1 :: forall (x :: EventTag). a x -> IO (b x) }

newtype GhcjsDomEvent en = GhcjsDomEvent { unGhcjsDomEvent :: EventType en }

data GhcjsDomSpace

instance DomSpace GhcjsDomSpace where
  type DomHandler GhcjsDomSpace = GhcjsDomHandler
  type DomHandler1 GhcjsDomSpace = GhcjsDomHandler1
  type RawEvent GhcjsDomSpace = GhcjsDomEvent
  type RawElement GhcjsDomSpace = DOM.Element
  {-# INLINABLE defaultEventHandler #-}
  defaultEventHandler _ = GhcjsDomHandler1 $ \(Pair1 en (GhcjsDomEvent evt)) -> do
    Just t <- withIsEvent en $ Event.getTarget evt
    mr <- runReaderT (defaultDomEventHandler (Element.castToElement t) en) evt
    return $ case mr of
      Nothing -> Nothing1
      Just r -> Just1 r

instance SupportsImmediateDomBuilder t m => DomBuilder t (ImmediateDomBuilderT t m) where
  type DomBuilderSpace (ImmediateDomBuilderT t m) = GhcjsDomSpace
  {-# INLINABLE textNode #-}
  textNode (TextNodeConfig initialContents eSetContents) = do
    n <- textNodeInternal initialContents
    lift $ performEvent_ $ ffor eSetContents $ \t -> setNodeValue n (Just t)
    return TextNode
  {-# INLINABLE element #-}
  element elementTag cfg child = fst <$> makeElement elementTag cfg child
  {-# INLINABLE placeholder #-}
  placeholder (PlaceholderConfig toInsertAbove delete) = liftThrough (deletable delete) $ do
    n <- textNodeInternal ("" :: Text)
    insertedAbove <- insertImmediateAbove n toInsertAbove
    --Note: "deleteSelf" must come after "insertAbove", because we need to be able to insert above in the same frame that we delete
    deleted <- lift $ performEvent $ ffor delete $ \_ -> do
      mp <- getParentNode n
      forM_ mp $ \p -> removeChild p $ Just n
    return $ Placeholder insertedAbove deleted
  {-# INLINABLE inputElement #-}
  inputElement cfg = do
    ((e, _), domElement) <- makeElement "input" (cfg ^. inputElementConfig_elementConfig) $ return ()
    let domInputElement = castToHTMLInputElement domElement
    Input.setValue domInputElement $ Just (cfg ^. inputElementConfig_initialValue)
    Just v0 <- Input.getValue domInputElement
    let getMyValue = fromMaybe "" <$> Input.getValue domInputElement
    valueChangedByUI <- performEvent $ getMyValue <$ Reflex.select (_element_events e) (WrapArg Input)
    valueChangedBySetValue <- performEvent $ ffor (cfg ^. inputElementConfig_setValue) $ \v' -> do
      Input.setValue domInputElement $ Just v'
      getMyValue -- We get the value after setting it in case the browser has mucked with it somehow
    v <- holdDyn v0 $ leftmost
      [ valueChangedBySetValue
      , valueChangedByUI
      ]
    Input.setChecked domInputElement $ _inputElementConfig_initialChecked cfg
    checkedChangedByUI <- wrapDomEvent domInputElement (`on` Element.click) $ do
      Input.getChecked domInputElement
    checkedChangedBySetChecked <- performEvent $ ffor (_inputElementConfig_setChecked cfg) $ \newChecked -> do
      oldChecked <- Input.getChecked domInputElement
      if newChecked /= oldChecked
        then do Input.setChecked domInputElement newChecked
                return $ Just newChecked
        else return Nothing
    c <- holdDyn (_inputElementConfig_initialChecked cfg) $ leftmost
      [ fmapMaybe id checkedChangedBySetChecked
      , checkedChangedByUI
      ]
    let initialFocus = False --TODO: Is this correct?
    hasFocus <- holdDyn initialFocus $ leftmost
      [ False <$ Reflex.select (_element_events e) (WrapArg Blur)
      , True <$ Reflex.select (_element_events e) (WrapArg Focus)
      ]
    return $ InputElement v (uniqDyn c) valueChangedByUI hasFocus e
  {-# INLINABLE textAreaElement #-}
  textAreaElement cfg = do --TODO
    ((e, _), domElement) <- makeElement "textarea" (cfg ^. textAreaElementConfig_elementConfig) $ return ()
    let domTextAreaElement = castToHTMLTextAreaElement domElement
    Just v0 <- TextArea.getValue domTextAreaElement
    let getMyValue = fromMaybe "" <$> TextArea.getValue domTextAreaElement
    valueChangedByUI <- performEvent $ getMyValue <$ Reflex.select (_element_events e) (WrapArg Input)
    valueChangedBySetValue <- performEvent $ ffor (cfg ^. textAreaElementConfig_setValue) $ \v' -> do
      TextArea.setValue domTextAreaElement $ Just v'
      getMyValue -- We get the value after setting it in case the browser has mucked with it somehow
    v <- holdDyn v0 $ leftmost
      [ valueChangedBySetValue
      , valueChangedByUI
      ]
    let initialFocus = False --TODO: Is this correct?
    hasFocus <- holdDyn initialFocus $ leftmost
      [ False <$ Reflex.select (_element_events e) (WrapArg Blur)
      , True <$ Reflex.select (_element_events e) (WrapArg Focus)
      ]
    return $ TextAreaElement v valueChangedByUI hasFocus e

{-# INLINABLE insertImmediateAbove #-}
insertImmediateAbove :: (Reflex t, IsNode placeholder, PerformEvent t m, MonadIO (Performable m)) => placeholder -> Event t (ImmediateDomBuilderT t (Performable m) a) -> ImmediateDomBuilderT t m (Event t a)
insertImmediateAbove n toInsertAbove = do
  events <- askEvents
  lift $ performEvent $ ffor toInsertAbove $ \new -> do
    Just doc <- getOwnerDocument n
    Just df <- createDocumentFragment doc
    result <- runImmediateDomBuilderT new $ ImmediateDomBuilderEnv
      { _immediateDomBuilderEnv_parent = toNode df
      , _immediateDomBuilderEnv_document = doc
      , _immediateDomBuilderEnv_events = events
      }
    mp <- getParentNode n
    case mp of
      Nothing -> trace "ImmediateDomBuilderT: placeholder: Warning: (getParentNode n) returned Nothing" $ return ()
      Just p -> void $ insertBefore p (Just df) (Just n) -- If there's no parent, that means we've been removed from the DOM; this should not happen if the we're removing ourselves from the performEvent properly
    return result

instance SupportsImmediateDomBuilder t m => Deletable t (ImmediateDomBuilderT t m) where
  {-# INLINABLE deletable #-}
  deletable delete child = liftThrough (deletable delete) $ do
    top <- textNodeInternal ("" :: Text)
    result <- child
    bottom <- textNodeInternal ("" :: Text)
    lift $ performEvent_ $ ffor delete $ \_ -> do
      deleteBetweenInclusive top bottom
    return result

instance PerformEvent t m => PerformEvent t (ImmediateDomBuilderT t m) where
  type Performable (ImmediateDomBuilderT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance PostBuild t m => PostBuild t (ImmediateDomBuilderT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ImmediateDomBuilderT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) => TriggerEvent t (ImmediateDomBuilderT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = do
    (e, t) <- newTriggerEventWithOnComplete
    return (e, \a -> t a $ return ())
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = do
    events <- askEvents
    (eResult, reResultTrigger) <- lift newEventWithTriggerRef
    return $ (,) eResult $ \a cb -> writeChan events [TriggerRef reResultTrigger :=> TriggerInvocation a cb]
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = do
    events <- askEvents
    lift $ newEventWithTrigger $ \t -> f $ \a cb -> do
      reResultTrigger <- newIORef $ Just t
      writeChan events [TriggerRef reResultTrigger :=> TriggerInvocation a cb]

instance HasWebView m => HasWebView (ImmediateDomBuilderT t m) where
  type WebViewPhantom (ImmediateDomBuilderT t m) = WebViewPhantom m
  askWebView = lift askWebView

instance MonadRef m => MonadRef (ImmediateDomBuilderT t m) where
  type Ref (ImmediateDomBuilderT t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (ImmediateDomBuilderT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance (HasJS x m, ReflexHost t) => HasJS x (ImmediateDomBuilderT t m) where
  type JSM (ImmediateDomBuilderT t m) = JSM m
  liftJS = lift . liftJS

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
  {-
  EventType 'BeforecutTag = DOM.Event
  EventType 'CutTag = DOM.Event
  EventType 'BeforecopyTag = DOM.Event
  EventType 'CopyTag = DOM.Event
  EventType 'BeforepasteTag = DOM.Event
  EventType 'PasteTag = DOM.Event
-}
  EventType 'ResetTag = DOM.Event
  EventType 'SearchTag = DOM.Event
--  EventType 'SelectstartTag = DOM.Event
  EventType 'TouchstartTag = TouchEvent
  EventType 'TouchmoveTag = TouchEvent
  EventType 'TouchendTag = TouchEvent
  EventType 'TouchcancelTag = TouchEvent

{-# INLINABLE defaultDomEventHandler #-}
defaultDomEventHandler :: IsElement e => e -> EventName en -> EventM e (EventType en) (Maybe (EventResult en))
defaultDomEventHandler e evt = fmap (Just . EventResult) $ case evt of
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
  {-
  Beforecut -> return ()
  Cut -> return ()
  Beforecopy -> return ()
  Copy -> return ()
  Beforepaste -> return ()
  Paste -> return ()
-}
  Reset -> return ()
  Search -> return ()
--  Selectstart -> return ()
  Touchstart -> return ()
  Touchmove -> return ()
  Touchend -> return ()
  Touchcancel -> return ()
  Mousewheel -> return ()
  Wheel -> return ()

{-# INLINABLE defaultDomWindowEventHandler #-}
defaultDomWindowEventHandler :: DOM.Window -> EventName en -> EventM DOM.Window (EventType en) (Maybe (EventResult en))
defaultDomWindowEventHandler w evt = fmap (Just . EventResult) $ case evt of
  Click -> return ()
  Dblclick -> return ()
  Keypress -> getKeyEvent
  Scroll -> Window.getScrollY w
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
  {-
  Beforecut -> return ()
  Cut -> return ()
  Beforecopy -> return ()
  Copy -> return ()
  Beforepaste -> return ()
  Paste -> return ()
-}
  Reset -> return ()
  Search -> return ()
--  Selectstart -> return ()
  Touchstart -> return ()
  Touchmove -> return ()
  Touchend -> return ()
  Touchcancel -> return ()
  Mousewheel -> return ()
  Wheel -> return ()

{-# INLINABLE withIsEvent #-}
withIsEvent :: EventName en -> (IsEvent (EventType en) => r) -> r
withIsEvent en r = case en of
  Click -> r
  Dblclick -> r
  Keypress -> r
  Scroll -> r
  Keydown -> r
  Keyup -> r
  Mousemove -> r
  Mouseup -> r
  Mousedown -> r
  Mouseenter -> r
  Mouseleave -> r
  Focus -> r
  Blur -> r
  Change -> r
  Drag -> r
  Dragend -> r
  Dragenter -> r
  Dragleave -> r
  Dragover -> r
  Dragstart -> r
  Drop -> r
  Abort -> r
  Contextmenu -> r
  Error -> r
  Input -> r
  Invalid -> r
  Load -> r
  Mouseout -> r
  Mouseover -> r
  Select -> r
  Submit -> r
  {-
  Beforecut -> r
  Cut -> r
  Beforecopy -> r
  Copy -> r
  Beforepaste -> r
  Paste -> r
-}
  Reset -> r
  Search -> r
--  Selectstart -> r
  Touchstart -> r
  Touchmove -> r
  Touchend -> r
  Touchcancel -> r
  Mousewheel -> r
  Wheel -> r

{-# INLINABLE elementOnEventName #-}
elementOnEventName :: IsElement e => EventName en -> e -> EventM e (EventType en) () -> IO (IO ())
elementOnEventName en e = case en of
  Abort -> on e Element.abort
  Blur -> on e Element.blurEvent
  Change -> on e Element.change
  Click -> on e Element.click
  Contextmenu -> on e Element.contextMenu
  Dblclick -> on e Element.dblClick
  Drag -> on e Element.drag
  Dragend -> on e Element.dragEnd
  Dragenter -> on e Element.dragEnter
  Dragleave -> on e Element.dragLeave
  Dragover -> on e Element.dragOver
  Dragstart -> on e Element.dragStart
  Drop -> on e Element.drop
  Error -> on e Element.error
  Focus -> on e Element.focusEvent
  Input -> on e Element.input
  Invalid -> on e Element.invalid
  Keydown -> on e Element.keyDown
  Keypress -> on e Element.keyPress
  Keyup -> on e Element.keyUp
  Load -> on e Element.load
  Mousedown -> on e Element.mouseDown
  Mouseenter -> on e Element.mouseEnter
  Mouseleave -> on e Element.mouseLeave
  Mousemove -> on e Element.mouseMove
  Mouseout -> on e Element.mouseOut
  Mouseover -> on e Element.mouseOver
  Mouseup -> on e Element.mouseUp
  Mousewheel -> on e Element.mouseWheel
  Scroll -> on e Element.scroll
  Select -> on e Element.select
  Submit -> on e Element.submit
  Wheel -> on e Element.wheel
  {-
  Beforecut -> on e Element.beforeCut
  Cut -> on e Element.cut
  Beforecopy -> on e Element.beforeCopy
  Copy -> on e Element.copy
  Beforepaste -> on e Element.beforePaste
  Paste -> on e Element.paste
-}
  Reset -> on e Element.reset
  Search -> on e Element.search
--  Selectstart -> on e Element.selectStart
  Touchstart -> on e Element.touchStart
  Touchmove -> on e Element.touchMove
  Touchend -> on e Element.touchEnd
  Touchcancel -> on e Element.touchCancel

{-# INLINABLE windowOnEventName #-}
windowOnEventName :: EventName en -> DOM.Window -> EventM DOM.Window (EventType en) () -> IO (IO ())
windowOnEventName en e = case en of
  Abort -> on e Window.abort
  Blur -> on e Window.blurEvent
  Change -> on e Window.change
  Click -> on e Window.click
  Contextmenu -> on e Window.contextMenu
  Dblclick -> on e Window.dblClick
  Drag -> on e Window.drag
  Dragend -> on e Window.dragEnd
  Dragenter -> on e Window.dragEnter
  Dragleave -> on e Window.dragLeave
  Dragover -> on e Window.dragOver
  Dragstart -> on e Window.dragStart
  Drop -> on e Window.drop
  Error -> on e Window.error
  Focus -> on e Window.focusEvent
  Input -> on e Window.input
  Invalid -> on e Window.invalid
  Keydown -> on e Window.keyDown
  Keypress -> on e Window.keyPress
  Keyup -> on e Window.keyUp
  Load -> on e Window.load
  Mousedown -> on e Window.mouseDown
  Mouseenter -> on e Window.mouseEnter
  Mouseleave -> on e Window.mouseLeave
  Mousemove -> on e Window.mouseMove
  Mouseout -> on e Window.mouseOut
  Mouseover -> on e Window.mouseOver
  Mouseup -> on e Window.mouseUp
  Mousewheel -> on e Window.mouseWheel
  Scroll -> on e Window.scrollEvent
  Select -> on e Window.select
  Submit -> on e Window.submit
  Wheel -> on e Window.wheel
  {-
  Beforecut -> on e Window.beforeCut
  Cut -> on e Window.cut
  Beforecopy -> on e Window.beforeCopy
  Copy -> on e Window.copy
  Beforepaste -> on e Window.beforePaste
  Paste -> on e Window.paste
-}
  Reset -> on e Window.reset
  Search -> on e Window.search
--  Selectstart -> on e Window.selectStart
  Touchstart -> on e Window.touchStart
  Touchmove -> on e Window.touchMove
  Touchend -> on e Window.touchEnd
  Touchcancel -> on e Window.touchCancel

{-# INLINABLE wrapDomEvent #-}
wrapDomEvent :: TriggerEvent t m => e -> (e -> EventM e event () -> IO (IO ())) -> EventM e event a -> m (Event t a)
wrapDomEvent el elementOnevent getValue = wrapDomEventMaybe el elementOnevent $ fmap Just getValue

{-# INLINABLE subscribeDomEvent #-}
subscribeDomEvent :: (EventM e event () -> IO (IO ()))
                  -> EventM e event (Maybe a)
                  -> Chan [DSum (TriggerRef t) TriggerInvocation]
                  -> EventTrigger t a
                  -> IO (IO ())
subscribeDomEvent elementOnevent getValue eventChan et = elementOnevent $ do
  mv <- getValue
  forM_ mv $ \v -> liftIO $ do
    --TODO: I don't think this is quite right: if a new trigger is created between when this is enqueued and when it fires, this may not work quite right
    etr <- newIORef $ Just et
    writeChan eventChan [TriggerRef etr :=> TriggerInvocation v (return ())]

{-# INLINABLE wrapDomEventMaybe #-}
wrapDomEventMaybe :: TriggerEvent t m
                  => e
                  -> (e -> EventM e event () -> IO (IO ()))
                  -> EventM e event (Maybe a)
                  -> m (Event t a)
wrapDomEventMaybe el elementOnevent getValue = do
  newEventWithLazyTriggerWithOnComplete $ \trigger -> elementOnevent el $ do
    mv <- getValue
    forM_ mv $ \v -> liftIO $ trigger v $ return ()

{-# INLINABLE wrapDomEventsMaybe #-}
wrapDomEventsMaybe :: (MonadIO m, MonadReflexCreateTrigger t m)
                   => e
                   -> (forall en. IsEvent (EventType en) => EventName en -> EventM e (EventType en) (Maybe (f en)))
                   -> (forall en. EventName en -> e -> EventM e (EventType en) () -> IO (IO ()))
                   -> ImmediateDomBuilderT t m (EventSelector t (WrapArg f EventName))
wrapDomEventsMaybe target handlers onEventName = do
  eventChan <- askEvents
  e <- lift $ newFanEventWithTrigger $ \(WrapArg en) -> withIsEvent en $ subscribeDomEvent (onEventName en target) (handlers en) eventChan
  return $! e

{-# INLINABLE getKeyEvent #-}
getKeyEvent :: EventM e KeyboardEvent Int
getKeyEvent = do
  e <- event
  which <- getWhich e
  if which /= 0 then return which else do
    charCode <- getCharCode e
    if charCode /= 0 then return charCode else
      getKeyCode e

{-# INLINABLE getMouseEventCoords #-}
getMouseEventCoords :: EventM e MouseEvent (Int, Int)
getMouseEventCoords = do
  e <- event
  bisequence (getX e, getY e)

instance MonadSample t m => MonadSample t (ImmediateDomBuilderT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (ImmediateDomBuilderT t m) where
  {-# INLINABLE hold #-}
  hold v0 v' = lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = lift $ holdIncremental v0 v'

data WindowConfig t = WindowConfig -- No config options yet

instance Default (WindowConfig t) where
  def = WindowConfig

data Window t = Window
  { _window_events :: EventSelector t (WrapArg EventResult EventName)
  , _window_raw :: DOM.Window
  }

wrapWindow :: (MonadIO m, MonadReflexCreateTrigger t m) => DOM.Window -> WindowConfig t -> ImmediateDomBuilderT t m (Window t)
wrapWindow wv _ = do
  events <- wrapDomEventsMaybe wv (defaultDomWindowEventHandler wv) windowOnEventName
  return $ Window
    { _window_events = events
    , _window_raw = wv
    }
