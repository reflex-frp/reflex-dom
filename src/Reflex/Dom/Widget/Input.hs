{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Widget.Input (module Reflex.Dom.Widget.Input, def, (&), (.~)) where

import Prelude

import Control.Lens hiding (element, ix)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Bimap as Bimap
import Data.Default
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM.Element (castToElement)
import qualified GHCJS.DOM.Element as Element
import GHCJS.DOM.EventM (on)
import qualified GHCJS.DOM.FileList as FileList
import GHCJS.DOM.HTMLInputElement (HTMLInputElement)
import GHCJS.DOM.HTMLTextAreaElement (HTMLTextAreaElement)
import GHCJS.DOM.Types (File)
import Reflex.Class
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Class
import Reflex.Dom.Widget.Basic
import Reflex.Dynamic
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class
import qualified Text.Read as T

import qualified GHCJS.DOM.Event as Event
import qualified GHCJS.DOM.HTMLInputElement as Input

data TextInput t
   = TextInput { _textInput_value :: Dynamic t Text
               , _textInput_input :: Event t Text
               , _textInput_keypress :: Event t Int
               , _textInput_keydown :: Event t Int
               , _textInput_keyup :: Event t Int
               , _textInput_hasFocus :: Dynamic t Bool
               , _textInput_builderElement :: InputElement EventResult GhcjsDomSpace t
               }

_textInput_element :: TextInput t -> HTMLInputElement
_textInput_element = _inputElement_raw . _textInput_builderElement

instance Reflex t => HasDomEvent t (TextInput t) en where
  type DomEventType (TextInput t) en = DomEventType (InputElement EventResult GhcjsDomSpace t) en
  domEvent en = domEvent en . _textInput_builderElement

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
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
  return $ TextInput
    { _textInput_value = _inputElement_value i
    , _textInput_input = _inputElement_input i
    , _textInput_keypress = domEvent Keypress i
    , _textInput_keydown = domEvent Keydown i
    , _textInput_keyup = domEvent Keyup i
    , _textInput_hasFocus = _inputElement_hasFocus i
    , _textInput_builderElement = i
    }

{-# INLINE textInputGetEnter #-}
{-# DEPRECATED textInputGetEnter "Use 'keypress Enter' instead" #-}
textInputGetEnter :: Reflex t => TextInput t -> Event t ()
textInputGetEnter = keypress Enter

{-# INLINABLE keypress #-}
keypress :: (Reflex t, HasDomEvent t e 'KeypressTag, DomEventType e 'KeypressTag ~ Int) => Key -> e -> Event t ()
keypress key i = fmapMaybe (\n -> if keyCodeLookup n == key then Just () else Nothing) $ domEvent Keypress i

data RangeInputConfig t
   = RangeInputConfig { _rangeInputConfig_initialValue :: Float
                      , _rangeInputConfig_setValue :: Event t Float
                      , _rangeInputConfig_attributes :: Dynamic t (Map Text Text)
                      }

instance Reflex t => Default (RangeInputConfig t) where
  {-# INLINABLE def #-}
  def = RangeInputConfig { _rangeInputConfig_initialValue = 0
                        , _rangeInputConfig_setValue = never
                        , _rangeInputConfig_attributes = constDyn mempty
                        }

data RangeInput t
   = RangeInput { _rangeInput_value :: Dynamic t Float
                , _rangeInput_input :: Event t Float
                , _rangeInput_mouseup :: Event t (Int, Int)
                , _rangeInput_hasFocus :: Dynamic t Bool
                , _rangeInput_element :: HTMLInputElement
                }

-- | Create an input whose value is a float.
--   https://www.w3.org/wiki/HTML/Elements/input/range
{-# INLINABLE rangeInput #-}
rangeInput :: (DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace) => RangeInputConfig t -> m (RangeInput t)
rangeInput (RangeInputConfig initial eSetValue dAttrs) = do
  modifyAttrs <- dynamicAttributesToModifyAttributes $ fmap (Map.insert "type" "range") dAttrs
  i <- inputElement $ def
    & inputElementConfig_initialValue .~ (T.pack . show $ initial)
    & inputElementConfig_setValue .~ (T.pack . show <$> eSetValue)
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
  return $ RangeInput
    { _rangeInput_value = read . T.unpack <$> _inputElement_value i
    , _rangeInput_input = read . T.unpack <$> _inputElement_input i
    , _rangeInput_mouseup = domEvent Mouseup i
    , _rangeInput_hasFocus = _inputElement_hasFocus i
    , _rangeInput_element = _inputElement_raw i
    }

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
    & textAreaElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
  return $ TextArea
    { _textArea_value = _textAreaElement_value i
    , _textArea_input = _textAreaElement_input i
    , _textArea_keypress = domEvent Keypress i
    , _textArea_hasFocus = _textAreaElement_hasFocus i
    , _textArea_element = _textAreaElement_raw i
    }

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
              , _checkbox_change :: Event t Bool
              }

-- | Create an editable checkbox
--   Note: if the "type" or "checked" attributes are provided as attributes, they will be ignored
{-# INLINABLE checkbox #-}
checkbox :: (DomBuilder t m, PostBuild t m) => Bool -> CheckboxConfig t -> m (Checkbox t)
checkbox checked config = do
  let permanentAttrs = "type" =: "checkbox"
      dAttrs = Map.delete "checked" . Map.union permanentAttrs <$> _checkboxConfig_attributes config
  modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
  i <- inputElement $ def
    & inputElementConfig_initialChecked .~ checked
    & inputElementConfig_setChecked .~ _checkboxConfig_setValue config
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ Map.mapKeys (AttributeName Nothing) permanentAttrs
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
  return $ Checkbox
    { _checkbox_value = _inputElement_checked i
    , _checkbox_change = _inputElement_checkedChange i
    }

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
  Beforecut -> r
  Cut -> r
  Beforecopy -> r
  Copy -> r
  Beforepaste -> r
  Paste -> r
  Reset -> r
  Search -> r
  Selectstart -> r
  Touchstart -> r
  Touchmove -> r
  Touchend -> r
  Touchcancel -> r

newtype CheckboxViewEventResult en = CheckboxViewEventResult { unCheckboxViewEventResult :: CheckboxViewEventResultType en }

--TODO
{-# INLINABLE checkboxView #-}
checkboxView :: forall t m. (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m, MonadHold t m) => Dynamic t (Map Text Text) -> Dynamic t Bool -> m (Event t Bool)
checkboxView dAttrs dValue = do
  let permanentAttrs = "type" =: "checkbox"
  modifyAttrs <- dynamicAttributesToModifyAttributes $ fmap (Map.union permanentAttrs) dAttrs
  postBuild <- getPostBuild
  let filters :: DMap EventName (GhcjsEventFilter CheckboxViewEventResult)
      filters = DMap.singleton Click $ GhcjsEventFilter $ \(GhcjsDomEvent evt) -> do
        Just t <- Event.getTarget evt
        b <- Input.getChecked $ Input.castToHTMLInputElement t
        return $ (,) preventDefault $ return $ Just $ CheckboxViewEventResult b
      elementConfig :: ElementConfig CheckboxViewEventResult t m
      elementConfig = (def :: ElementConfig EventResult t m)
        { _elementConfig_modifyAttributes = fmap mapKeysToAttributeName modifyAttrs
        , _elementConfig_initialAttributes = Map.mapKeys (AttributeName Nothing) permanentAttrs
        , _elementConfig_eventSpec = GhcjsEventSpec
            { _ghcjsEventSpec_filters = filters
            , _ghcjsEventSpec_handler = \(en, GhcjsDomEvent evt) -> case en of
                Click -> error "impossible"
                _ -> do
                  Just e <- withIsEvent en $ Event.getTarget evt
                  mr <- runReaderT (defaultDomEventHandler (castToElement e) en) evt
                  return $ ffor mr $ \(EventResult r) -> CheckboxViewEventResult $ regularToCheckboxViewEventType en r
            }
        }
      inputElementConfig :: InputElementConfig CheckboxViewEventResult t m
      inputElementConfig = (def :: InputElementConfig EventResult t m)
        & inputElementConfig_setChecked .~ leftmost [updated dValue, tag (current dValue) postBuild]
        & inputElementConfig_elementConfig .~ elementConfig
  i <- inputElement inputElementConfig
  return $ unCheckboxViewEventResult <$> select (_element_events $ _inputElement_element i) (WrapArg Click)

data FileInput d t
   = FileInput { _fileInput_value :: Dynamic t [File]
               , _fileInput_element :: RawInputElement d
               }

data FileInputConfig t
   = FileInputConfig { _fileInputConfig_attributes :: Dynamic t (Map Text Text)
                     }

instance Reflex t => Default (FileInputConfig t) where
  def = FileInputConfig { _fileInputConfig_attributes = constDyn mempty
                        }

fileInput :: forall t m. (MonadIO m, MonadFix m, MonadHold t m, TriggerEvent t m, DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace)
          => FileInputConfig t -> m (FileInput (DomBuilderSpace m) t)
fileInput config = do
  let insertType = Map.insert "type" "file"
      dAttrs = insertType <$> _fileInputConfig_attributes config
  modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
  let filters = DMap.singleton Change . GhcjsEventFilter $ \_ -> do
        return . (,) mempty $ return . Just $ EventResult ()
      elCfg = (def :: ElementConfig EventResult t m)
        & modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
        & elementConfig_eventSpec . ghcjsEventSpec_filters .~ filters
      cfg = (def :: InputElementConfig EventResult t m) & inputElementConfig_elementConfig .~ elCfg
  eRaw <- inputElement cfg
  let e = _inputElement_raw eRaw
  eChange <- wrapDomEvent e (`on` Element.change) $ do
      Just files <- Input.getFiles e
      len <- FileList.getLength files
      mapM (fmap (fromMaybe (error "fileInput: fileList.item returned null")) . FileList.item files) [0 .. len-1]
  dValue <- holdDyn [] eChange
  return $ FileInput
    { _fileInput_value = dValue
    , _fileInput_element = e
    }

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
  Beforecut -> r
  Cut -> r
  Beforecopy -> r
  Copy -> r
  Beforepaste -> r
  Paste -> r
  Reset -> r
  Search -> r
  Selectstart -> r
  Touchstart -> r
  Touchmove -> r
  Touchend -> r
  Touchcancel -> r

--TODO: We should allow the user to specify an ordering instead of relying on the ordering of the Map
-- | Create a dropdown box
--   The first argument gives the initial value of the dropdown; if it is not present in the map of options provided, it will be added with an empty string as its text
dropdown :: forall k t m. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Ord k) => k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
dropdown k0 options (DropdownConfig setK attrs) = do
  optionsWithAddedKeys <- fmap (zipDynWith Map.union options) $ foldDyn Map.union (k0 =: "") $ fmap (=: "") setK
  defaultKey <- holdDyn k0 setK
  let (indexedOptions, ixKeys) = splitDynPure $ ffor optionsWithAddedKeys $ \os ->
        let xs = fmap (\(ix, (k, v)) -> ((ix, k), ((ix, k), v))) $ zip [0::Int ..] $ Map.toList os
        in (Map.fromList $ map snd xs, Bimap.fromList $ map fst xs)
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg = def
        & selectElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
        & selectElementConfig_setValue .~ fmap (T.pack . show) (attachPromptlyDynWithMaybe (flip Bimap.lookupR) ixKeys setK)
  (eRaw, _) <- selectElement cfg $ listWithKey indexedOptions $ \(ix, k) v -> do
    let optionAttrs = fmap (\dk -> "value" =: T.pack (show ix) <> if dk == k then "selected" =: "selected" else mempty) defaultKey
    elDynAttr "option" optionAttrs $ dynText v
  let lookupSelected ks v = do
        key <- T.readMaybe $ T.unpack v
        Bimap.lookup key ks
  let eChange = attachPromptlyDynWith lookupSelected ixKeys $ _selectElement_change eRaw
  let readKey keys mk = fromMaybe k0 $ do
        k <- mk
        guard $ Bimap.memberR k keys
        return k
  dValue <- fmap (zipDynWith readKey ixKeys) $ holdDyn (Just k0) $ leftmost [eChange, fmap Just setK]
  return $ Dropdown dValue (attachPromptlyDynWith readKey ixKeys eChange)

concat <$> mapM makeLenses
  [ ''TextAreaConfig
  , ''TextArea
  , ''TextInputConfig
  , ''TextInput
  , ''RangeInputConfig
  , ''RangeInput
  , ''FileInputConfig
  , ''FileInput
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

instance HasAttributes (RangeInputConfig t) where
  type Attrs (RangeInputConfig t) = Dynamic t (Map Text Text)
  attributes = rangeInputConfig_attributes

instance HasAttributes (DropdownConfig t k) where
  type Attrs (DropdownConfig t k) = Dynamic t (Map Text Text)
  attributes = dropdownConfig_attributes

instance HasAttributes (CheckboxConfig t) where
  type Attrs (CheckboxConfig t) = Dynamic t (Map Text Text)
  attributes = checkboxConfig_attributes

instance HasAttributes (FileInputConfig t) where
  type Attrs (FileInputConfig t) = Dynamic t (Map Text Text)
  attributes = fileInputConfig_attributes

class HasSetValue a where
  type SetValue a :: *
  setValue :: Lens' a (SetValue a)

instance HasSetValue (TextAreaConfig t) where
  type SetValue (TextAreaConfig t) = Event t Text
  setValue = textAreaConfig_setValue

instance HasSetValue (TextInputConfig t) where
  type SetValue (TextInputConfig t) = Event t Text
  setValue = textInputConfig_setValue

instance HasSetValue (RangeInputConfig t) where
  type SetValue (RangeInputConfig t) = Event t Float
  setValue = rangeInputConfig_setValue

instance HasSetValue (DropdownConfig t k) where
  type SetValue (DropdownConfig t k) = Event t k
  setValue = dropdownConfig_setValue

instance HasSetValue (CheckboxConfig t) where
  type SetValue (CheckboxConfig t) = Event t Bool
  setValue = checkboxConfig_setValue

class HasValue a where
  type Value a :: *
  value :: a -> Value a

instance HasValue (InputElement er d t) where
  type Value (InputElement er d t) = Dynamic t Text
  value = _inputElement_value

instance HasValue (TextAreaElement er d t) where
  type Value (TextAreaElement er d t) = Dynamic t Text
  value = _textAreaElement_value

instance HasValue (TextArea t) where
  type Value (TextArea t) = Dynamic t Text
  value = _textArea_value

instance HasValue (TextInput t) where
  type Value (TextInput t) = Dynamic t Text
  value = _textInput_value

instance HasValue (FileInput d t) where
  type Value (FileInput d t) = Dynamic t [File]
  value = _fileInput_value

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
