{-# LANGUAGE OverloadedStrings, ConstraintKinds, TypeFamilies, FlexibleContexts, DataKinds, GADTs, ScopedTypeVariables, FlexibleInstances, RecursiveDo, TemplateHaskell, UndecidableInstances, PolyKinds #-}
module Reflex.Dom.Widget.Input (module Reflex.Dom.Widget.Input, def, (&), (.~)) where

import Prelude

import Reflex.Dom.Class
import Reflex.Dom.Widget.Basic
import Reflex.Dom.PostBuild.Class
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate

-- For dropdown
import Reflex.Dom.PerformEvent.Class
import GHCJS.DOM.HTMLInputElement (HTMLInputElement, castToHTMLInputElement)
import GHCJS.DOM.HTMLTextAreaElement (HTMLTextAreaElement, castToHTMLTextAreaElement)
import GHCJS.DOM.HTMLSelectElement (castToHTMLSelectElement)
import qualified GHCJS.DOM.HTMLSelectElement as HTMLSelectElement
import Data.Maybe
import Data.Semigroup
import Control.Monad.Fix
import Control.Monad.IO.Class

import Reflex
import qualified Data.Map as Map
import Control.Lens hiding (ix, element)
import Control.Monad hiding (forM_)
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import qualified Data.Bimap as Bimap
import Data.Proxy
import Data.Functor.Misc

import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.Event as Event

data TextInput t
   = TextInput { _textInput_value :: Dynamic t Text
               , _textInput_input :: Event t Text
               , _textInput_keypress :: Event t Int
               , _textInput_keydown :: Event t Int
               , _textInput_keyup :: Event t Int
               , _textInput_hasFocus :: Dynamic t Bool
               , _textInput_element :: HTMLInputElement
               }

data TextInputConfig t
   = TextInputConfig { _textInputConfig_inputType :: Text
                     , _textInputConfig_initialValue :: Text
                     , _textInputConfig_setValue :: Event t Text
                     , _textInputConfig_attributes :: Dynamic t (Map Text Text)
                     }

instance Reflex t => Default (TextInputConfig t) where
  {-# INLINABLE def #-}
  def = TextInputConfig { _textInputConfig_inputType = "text"
                        , _textInputConfig_initialValue = ""
                        , _textInputConfig_setValue = never
                        , _textInputConfig_attributes = constDyn mempty
                        }

-- | Create an input whose value is a string.  By default, the "type" attribute is set to "text", but it can be changed using the _textInputConfig_inputType field.  Note that only types for which the value is always a string will work - types whose value may be null will not work properly with this widget.
{-# INLINABLE textInput #-}
textInput :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace) => TextInputConfig t -> m (TextInput t)
textInput (TextInputConfig inputType initial eSetValue dAttrs) = do
  modifyAttrs <- dynamicAttributesToModifyAttributes $ fmap (Map.insert "type" inputType) dAttrs
  i <- inputElement $ def
    & inputElementConfig_initialValue .~ initial
    & inputElementConfig_setValue .~ eSetValue
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttrs
  return $ TextInput
    { _textInput_value = _inputElement_value i
    , _textInput_input = _inputElement_input i
    , _textInput_keypress = domEvent Keypress i
    , _textInput_keydown = domEvent Keydown i
    , _textInput_keyup = domEvent Keyup i
    , _textInput_hasFocus = _inputElement_hasFocus i
    , _textInput_element = castToHTMLInputElement $ _element_raw $ _inputElement_element i
    }
{-
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" =<< mapDyn (Map.insert "type" inputType) dAttrs
  Input.setValue e $ Just initial
  performEvent_ $ fmap (Input.setValue e . Just) eSetValue
  eChange <- wrapDomEvent e (`on` input) $ fromMaybe "" <$> Input.getValue e
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
    unsubscribeOnblur <- on e blurEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity False]
    unsubscribeOnfocus <- on e focusEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity True]
    return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
  dFocus <- holdDyn False eChangeFocus
  eKeypress <- wrapDomEvent e (`on` keyPress) getKeyEvent
  eKeydown <- wrapDomEvent e (`on` keyDown) getKeyEvent
  eKeyup <- wrapDomEvent e (`on` keyUp) getKeyEvent
  dValue <- holdDyn initial $ leftmost [eSetValue, eChange]
  return $ TextInput dValue eChange eKeypress eKeydown eKeyup dFocus e
-}

{-# INLINABLE textInputGetEnter #-}
textInputGetEnter :: Reflex t => TextInput t -> Event t ()
textInputGetEnter i = fmapMaybe (\n -> if n == keycodeEnter then Just () else Nothing) $ _textInput_keypress i

data TextAreaConfig t
   = TextAreaConfig { _textAreaConfig_initialValue :: Text
                    , _textAreaConfig_setValue :: Event t Text
                    , _textAreaConfig_attributes :: Dynamic t (Map Text Text)
                    }

instance Reflex t => Default (TextAreaConfig t) where
  {-# INLINABLE def #-}
  def = TextAreaConfig { _textAreaConfig_initialValue = ""
                       , _textAreaConfig_setValue = never
                       , _textAreaConfig_attributes = constDyn mempty
                       }

data TextArea t
   = TextArea { _textArea_value :: Dynamic t Text
              , _textArea_input :: Event t Text
              , _textArea_hasFocus :: Dynamic t Bool
              , _textArea_keypress :: Event t Int
              , _textArea_element :: HTMLTextAreaElement
              }

{-# INLINABLE textArea #-}
textArea :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace) => TextAreaConfig t -> m (TextArea t)
textArea (TextAreaConfig initial eSet attrs) = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  i <- textAreaElement $ def
    & textAreaElementConfig_initialValue .~ initial
    & textAreaElementConfig_setValue .~ eSet
    & textAreaElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttrs
  return $ TextArea
    { _textArea_value = _textAreaElement_value i
    , _textArea_input = _textAreaElement_input i
    , _textArea_keypress = domEvent Keypress i
    , _textArea_hasFocus = _textAreaElement_hasFocus i
    , _textArea_element = castToHTMLTextAreaElement $ _element_raw $ _textAreaElement_element i
    }
{-
  e <- liftM castToHTMLTextAreaElement $ buildEmptyElement "textarea" attrs
  TextArea.setValue e $ Just initial
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
    unsubscribeOnblur <- on e blurEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity False]
    unsubscribeOnfocus <- on e focusEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity True]
    return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
  performEvent_ $ fmap (TextArea.setValue e . Just) eSet
  f <- holdDyn False eChangeFocus
  ev <- wrapDomEvent e (`on` input) $ fromMaybe "" <$> TextArea.getValue e
  v <- holdDyn initial $ leftmost [eSet, ev]
  eKeypress <- wrapDomEvent e (`on` keyPress) getKeyEvent
  return $ TextArea v ev e f eKeypress
-}

data CheckboxConfig t
    = CheckboxConfig { _checkboxConfig_setValue :: Event t Bool
                     , _checkboxConfig_attributes :: Dynamic t (Map Text Text)
                     }

instance Reflex t => Default (CheckboxConfig t) where
  {-# INLINABLE def #-}
  def = CheckboxConfig { _checkboxConfig_setValue = never
                       , _checkboxConfig_attributes = constDyn mempty
                       }

data Checkbox t
   = Checkbox { _checkbox_value :: Dynamic t Bool
--              , _checkbox_change :: Event t Bool
              }

-- | Create an editable checkbox
--   Note: if the "type" or "checked" attributes are provided as attributes, they will be ignored
{-# INLINABLE checkbox #-}
checkbox :: (DomBuilder t m, PostBuild t m) => Bool -> CheckboxConfig t -> m (Checkbox t)
checkbox checked config = do
  let insertType = Map.insert "type" "checkbox"
      insertChecked = if checked
                      then Map.insert "checked" "checked"
                      else id
      dAttrs = fmap (insertChecked . insertType) $ _checkboxConfig_attributes config
  modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
  i <- inputElement $ def
    & inputElementConfig_setChecked .~ _checkboxConfig_setValue config
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttrs
  return $ Checkbox
    { _checkbox_value = _inputElement_checked i
    }
{-
  attrs <- mapDyn (\c -> Map.insert "type" "checkbox" $ (if checked then Map.insert "checked" "checked" else Map.delete "checked") c) (_checkboxConfig_attributes config)
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" attrs
  eClick <- wrapDomEvent e (`on` click) $ Input.getChecked e
  performEvent_ $ fmap (\v -> Input.setChecked e $! v) $ _checkboxConfig_setValue config
  dValue <- holdDyn checked $ leftmost [_checkboxConfig_setValue config, eClick]
  return $ Checkbox dValue eClick
-}

type family CheckboxViewEventResultType (en :: EventTag) :: * where
  CheckboxViewEventResultType 'ClickTag = Bool
  CheckboxViewEventResultType t = EventResultType t

regularToCheckboxViewEventType :: EventName t -> EventResultType t -> CheckboxViewEventResultType t
regularToCheckboxViewEventType en r = case en of
  Click -> error "regularToCheckboxViewEventType: EventName Click should never be encountered"
  Abort -> r
  Blur -> r
  Change -> r
  Contextmenu -> r
  Dblclick -> r
  Drag -> r
  Dragend -> r
  Dragenter -> r
  Dragleave -> r
  Dragover -> r
  Dragstart -> r
  Drop -> r
  Error -> r
  Focus -> r
  Input -> r
  Invalid -> r
  Keydown -> r
  Keypress -> r
  Keyup -> r
  Load -> r
  Mousedown -> r
  Mouseenter -> r
  Mouseleave -> r
  Mousemove -> r
  Mouseout -> r
  Mouseover -> r
  Mouseup -> r
  Mousewheel -> r
  Scroll -> r
  Select -> r
  Submit -> r
  Wheel -> r
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

newtype CheckboxViewEventResult en = CheckboxViewEventResult { unCheckboxViewEventResult :: CheckboxViewEventResultType en }

--TODO
{-# INLINABLE checkboxView #-}
checkboxView :: forall t m. (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m, MonadHold t m) => Dynamic t (Map Text Text) -> Dynamic t Bool -> m (Event t Bool)
checkboxView dAttrs dValue = do
  let insertType = Map.insert "type" "checkbox"
  modifyAttrs <- dynamicAttributesToModifyAttributes $ fmap insertType dAttrs
  postBuild <- getPostBuild
  let filters :: DMap EventName (EventFilter (DomBuilderSpace m) CheckboxViewEventResult)
      filters = DMap.singleton Click $ EventFilter $ GhcjsDomHandler $ \(GhcjsDomEvent evt) -> do
        Just t <- Event.getTarget evt
        b <- Input.getChecked $ Input.castToHTMLInputElement t
        return $ (,) preventDefault $ GhcjsDomHandler $ \_ -> return $ Just $ CheckboxViewEventResult b
      elementConfig :: ElementConfig CheckboxViewEventResult t m
      elementConfig = (def :: ElementConfig EventResult t m)
        { _elementConfig_modifyAttributes = modifyAttrs
        , _elementConfig_eventFilters = filters
        , _elementConfig_eventHandler = GhcjsDomHandler1 $ \p@(Pair1 en _) -> case en of
            Click -> error "impossible"
            _ -> do
              mr <- unGhcjsDomHandler1 (defaultEventHandler (Proxy :: Proxy (DomBuilderSpace m))) p
              case mr of
                Nothing1 -> return Nothing1
                Just1 (EventResult r) -> return $ Just1 $ CheckboxViewEventResult $ regularToCheckboxViewEventType en r
        }
      inputElementConfig :: InputElementConfig CheckboxViewEventResult t m
      inputElementConfig = (def :: InputElementConfig EventResult t m)
        & inputElementConfig_setChecked .~ leftmost [updated dValue, tag (current dValue) postBuild]
        & inputElementConfig_elementConfig .~ elementConfig
  i <- inputElement inputElementConfig
  return $ fmap unCheckboxViewEventResult $ select (_element_events $ _inputElement_element i) (WrapArg Click)
{-
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" =<< mapDyn (Map.insert "type" "checkbox") dAttrs
  eClicked <- wrapDomEvent e (`on` click) $ do
    preventDefault
    Input.getChecked e
  schedulePostBuild $ do
    v <- sample $ current dValue
    when v $ Input.setChecked e True
  performEvent_ $ fmap (\v -> Input.setChecked e $! v) $ updated dValue
  return eClicked
-}
{-

data FileInput t
   = FileInput { _fileInput_value :: Dynamic t [File]
               , _fileInput_element :: HTMLInputElement
               }

data FileInputConfig t
   = FileInputConfig { _fileInputConfig_attributes :: Dynamic t (Map Text Text)
                     }

instance Reflex t => Default (FileInputConfig t) where
  def = FileInputConfig { _fileInputConfig_attributes = constDyn mempty
                        }

fileInput :: DomBuilder t m => FileInputConfig t -> m (FileInput t)
fileInput (FileInputConfig dAttrs) = do
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" =<< mapDyn (Map.insert "type" "file") dAttrs
  eChange <- wrapDomEvent e (flip on change) $ do
    Just files <- getFiles e
    len <- FileList.getLength files
    mapM (liftM (fromMaybe (error "fileInput: fileListItem returned null")) . FileList.item files) $ init [0..len]
  dValue <- holdDyn [] eChange
  return $ FileInput dValue e
-}

data Dropdown t k
    = Dropdown { _dropdown_value :: Dynamic t k
               , _dropdown_change :: Event t k
               }

data DropdownConfig t k
   = DropdownConfig { _dropdownConfig_setValue :: Event t k
                    , _dropdownConfig_attributes :: Dynamic t (Map Text Text)
                    }

instance Reflex t => Default (DropdownConfig t k) where
  def = DropdownConfig { _dropdownConfig_setValue = never
                       , _dropdownConfig_attributes = constDyn mempty
                       }

type family DropdownViewEventResultType (en :: EventTag) :: * where
  DropdownViewEventResultType 'ChangeTag = Text
  DropdownViewEventResultType t = EventResultType t

newtype DropdownViewEventResult en = DropdownViewEventResult { unDropdownViewEventResult :: DropdownViewEventResultType en }

regularToDropdownViewEventType :: EventName t -> EventResultType t -> DropdownViewEventResultType t
regularToDropdownViewEventType en r = case en of
  Change -> error "regularToDropdownViewEventType: EventName Change should never be encountered"
  Abort -> r
  Blur -> r
  Click -> r
  Contextmenu -> r
  Dblclick -> r
  Drag -> r
  Dragend -> r
  Dragenter -> r
  Dragleave -> r
  Dragover -> r
  Dragstart -> r
  Drop -> r
  Error -> r
  Focus -> r
  Input -> r
  Invalid -> r
  Keydown -> r
  Keypress -> r
  Keyup -> r
  Load -> r
  Mousedown -> r
  Mouseenter -> r
  Mouseleave -> r
  Mousemove -> r
  Mouseout -> r
  Mouseover -> r
  Mouseup -> r
  Mousewheel -> r
  Scroll -> r
  Select -> r
  Submit -> r
  Wheel -> r
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

--TODO: We should allow the user to specify an ordering instead of relying on the ordering of the Map
-- | Create a dropdown box
--   The first argument gives the initial value of the dropdown; if it is not present in the map of options provided, it will be added with an empty string as its text
dropdown :: forall k t m. (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, MonadIO (Performable m), PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace, Ord k) => k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
dropdown k0 options (DropdownConfig setK attrs) = do
  optionsWithAddedKeys <- combineDyn Map.union options <=< foldDyn Map.union (k0 =: "") $ fmap (=: "") setK
  defaultKey <- holdDyn k0 setK
  (indexedOptions, ixKeys) <- splitDyn <=< forDyn optionsWithAddedKeys $ \os ->
    let xs =  map (\(ix, (k, v)) -> ((ix, k), ((ix, k), v))) $ zip [0::Int ..] $ Map.toList os
        ixVals = Map.fromList $ map snd xs
        ixKeys = Bimap.fromList $ map fst xs
    in (ixVals, ixKeys)
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg = (def :: ElementConfig EventResult t m)
        { _elementConfig_modifyAttributes = modifyAttrs
        , _elementConfig_eventFilters = DMap.singleton Change $ EventFilter $ GhcjsDomHandler $ \(GhcjsDomEvent evt) -> do
            Just t <- Event.getTarget evt
            Just v <- HTMLSelectElement.getValue $ HTMLSelectElement.castToHTMLSelectElement t
            return $ (,) mempty $ GhcjsDomHandler $ \_ -> return $ Just $ DropdownViewEventResult v
        , _elementConfig_eventHandler = GhcjsDomHandler1 $ \p@(Pair1 en _) -> case en of
            Click -> error "impossible"
            _ -> do
              mr <- unGhcjsDomHandler1 (defaultEventHandler (Proxy :: Proxy (DomBuilderSpace m))) p
              case mr of
                Nothing1 -> return Nothing1
                Just1 (EventResult r) -> return $ Just1 $ DropdownViewEventResult $ regularToDropdownViewEventType en r
        }
  (eRaw, _) <- element "select" cfg $ listWithKey indexedOptions $ \(ix, k) v -> do
    optionAttrs <- mapDyn (\dk -> "value" =: T.pack (show ix) <> if dk == k then "selected" =: "selected" else mempty) defaultKey
    elDynAttr "option" optionAttrs $ dynText v
  let e = castToHTMLSelectElement $ _element_raw eRaw
  performEvent_ $ fmap (HTMLSelectElement.setValue e . Just . show) $ attachDynWithMaybe (flip Bimap.lookupR) ixKeys setK
  eChange <- return never -- attachDynWith (\ks s -> join $ Bimap.lookup <$> join (readMaybe <$> s) <*> pure ks) ixKeys <$> (wrapDomEvent e (`on` Element.change) $ HTMLSelectElement.getValue e)
  let readKey keys mk = fromMaybe k0 $ do
        k <- mk
        guard $ Bimap.memberR k keys
        return k
  dValue <- combineDyn readKey ixKeys <=< holdDyn (Just k0) $ leftmost [eChange, fmap Just setK]
  return $ Dropdown dValue (attachDynWith readKey ixKeys eChange)

liftM concat $ mapM makeLenses
  [ ''TextAreaConfig
  , ''TextArea
  , ''TextInputConfig
  , ''TextInput
--  , ''FileInputConfig
--  , ''FileInput
  , ''DropdownConfig
  , ''Dropdown
  , ''CheckboxConfig
  , ''Checkbox
  ]

instance HasAttributes (TextAreaConfig t) where
  type Attrs (TextAreaConfig t) = Dynamic t (Map Text Text)
  attributes = textAreaConfig_attributes

instance HasAttributes (TextInputConfig t) where
  type Attrs (TextInputConfig t) = Dynamic t (Map Text Text)
  attributes = textInputConfig_attributes

instance HasAttributes (DropdownConfig t k) where
  type Attrs (DropdownConfig t k) = Dynamic t (Map Text Text)
  attributes = dropdownConfig_attributes

instance HasAttributes (CheckboxConfig t) where
  type Attrs (CheckboxConfig t) = Dynamic t (Map Text Text)
  attributes = checkboxConfig_attributes

{-
instance HasAttributes (FileInputConfig t) where
  type Attrs (FileInputConfig t) = Dynamic t (Map Text Text)
  attributes = fileInputConfig_attributes
-}

class HasSetValue a where
  type SetValue a :: *
  setValue :: Lens' a (SetValue a)

instance HasSetValue (TextAreaConfig t) where
  type SetValue (TextAreaConfig t) = Event t Text
  setValue = textAreaConfig_setValue

instance HasSetValue (TextInputConfig t) where
  type SetValue (TextInputConfig t) = Event t Text
  setValue = textInputConfig_setValue

instance HasSetValue (DropdownConfig t k) where
  type SetValue (DropdownConfig t k) = Event t k
  setValue = dropdownConfig_setValue

instance HasSetValue (CheckboxConfig t) where
  type SetValue (CheckboxConfig t) = Event t Bool
  setValue = checkboxConfig_setValue

class HasValue a where
  type Value a :: *
  value :: a -> Value a

instance HasValue (TextArea t) where
  type Value (TextArea t) = Dynamic t Text
  value = _textArea_value

instance HasValue (TextInput t) where
  type Value (TextInput t) = Dynamic t Text
  value = _textInput_value

{-
instance HasValue (FileInput t) where
  type Value (FileInput t) = Dynamic t [File]
  value = _fileInput_value
-}

instance HasValue (Dropdown t k) where
  type Value (Dropdown t k) = Dynamic t k
  value = _dropdown_value

instance HasValue (Checkbox t) where
  type Value (Checkbox t) = Dynamic t Bool
  value = _checkbox_value

{-
type family Controller sm t a where
  Controller Edit t a = (a, Event t a) -- Initial value and setter
  Controller View t a = Dynamic t a -- Value (always)

type family Output sm t a where
  Output Edit t a = Dynamic t a -- Value (always)
  Output View t a = Event t a -- Requested changes

data CheckboxConfig sm t
   = CheckboxConfig { _checkbox_input :: Controller sm t Bool
                    , _checkbox_attributes :: Attributes
                    }

instance Reflex t => Default (CheckboxConfig Edit t) where
  def = CheckboxConfig (False, never) mempty

data Checkbox sm t
  = Checkbox { _checkbox_output :: Output sm t Bool
             }

data StateMode = Edit | View

--TODO: There must be a more generic way to get this witness and allow us to case on the type-level StateMode
data StateModeWitness (sm :: StateMode) where
  EditWitness :: StateModeWitness Edit
  ViewWitness :: StateModeWitness View

class HasStateModeWitness (sm :: StateMode) where
  stateModeWitness :: StateModeWitness sm

instance HasStateModeWitness Edit where
  stateModeWitness = EditWitness

instance HasStateModeWitness View where
  stateModeWitness = ViewWitness
-}
