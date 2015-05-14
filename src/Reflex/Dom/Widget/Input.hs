{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleContexts, DataKinds, GADTs, ScopedTypeVariables, FlexibleInstances, RecursiveDo, TemplateHaskell #-}
module Reflex.Dom.Widget.Input (module Reflex.Dom.Widget.Input, def, (&), (.~)) where

import Prelude

import Reflex.Dom.Class
import Reflex.Dom.Widget.Basic

import Reflex
import Reflex.Host.Class
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.HTMLTextAreaElement
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLSelectElement
import GHCJS.DOM.EventM
import GHCJS.DOM.UIEvent
import Data.Monoid
import Data.Map as Map
import Control.Lens
import Control.Monad hiding (forM_)
import Control.Monad.IO.Class
import Data.Default
import Data.Maybe
import Safe
import Data.Dependent.Sum (DSum (..))

data TextInput t
   = TextInput { _textInput_value :: Dynamic t String
               , _textInput_input :: Event t String
               , _textInput_keypress :: Event t Int
               , _textInput_keydown :: Event t Int
               , _textInput_keyup :: Event t Int
               , _textInput_hasFocus :: Dynamic t Bool
               , _textInput_element :: HTMLInputElement
               }

data TextInputConfig t
    = TextInputConfig { _textInputConfig_inputType :: String
                      , _textInputConfig_initialValue :: String
                      , _textInputConfig_setValue :: Event t String
                      , _textInputConfig_attributes :: Dynamic t (Map String String)
                      }

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig { _textInputConfig_inputType = "text"
                        , _textInputConfig_initialValue = ""
                        , _textInputConfig_setValue = never
                        , _textInputConfig_attributes = constDyn mempty
                        }

textInput :: MonadWidget t m => TextInputConfig t -> m (TextInput t)
textInput (TextInputConfig inputType initial eSetValue dAttrs) = do
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" =<< mapDyn (Map.insert "type" inputType) dAttrs
  liftIO $ htmlInputElementSetValue e initial
  performEvent_ $ fmap (liftIO . htmlInputElementSetValue e) eSetValue
  eChange <- wrapDomEvent e elementOninput $ liftIO $ htmlInputElementGetValue e
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
    unsubscribeOnblur <- liftIO $ elementOnblur e $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> False]
    unsubscribeOnfocus <- liftIO $ elementOnfocus e $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> True]
    return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
  dFocus <- holdDyn False eChangeFocus
  eKeypress <- wrapDomEvent e elementOnkeypress getKeyEvent
  eKeydown <- wrapDomEvent e elementOnkeydown getKeyEvent
  eKeyup <- wrapDomEvent e elementOnkeyup getKeyEvent
  dValue <- holdDyn initial $ leftmost [eSetValue, eChange]
  return $ TextInput dValue eChange eKeypress eKeydown eKeyup dFocus e

textInputGetEnter :: Reflex t => TextInput t -> Event t ()
textInputGetEnter i = fmapMaybe (\n -> if n == keycodeEnter then Just () else Nothing) $ _textInput_keypress i

data TextAreaConfig t
   = TextAreaConfig { _textAreaConfig_initialValue :: String
                    , _textAreaConfig_setValue :: Event t String
                    , _textAreaConfig_attributes :: Dynamic t (Map String String)
                    }

instance Reflex t => Default (TextAreaConfig t) where
  def = TextAreaConfig { _textAreaConfig_initialValue = ""
                       , _textAreaConfig_setValue = never
                       , _textAreaConfig_attributes = constDyn mempty
                       }

data TextArea t
   = TextArea { _textArea_value :: Dynamic t String
              , _textArea_input :: Event t String
              , _textArea_element :: HTMLTextAreaElement
              , _textArea_hasFocus :: Dynamic t Bool
              , _textArea_keypress :: Event t Int
              }

textArea :: MonadWidget t m => TextAreaConfig t -> m (TextArea t)
textArea (TextAreaConfig initial eSet attrs) = do
  e <- liftM castToHTMLTextAreaElement $ buildEmptyElement "textarea" attrs
  liftIO $ htmlTextAreaElementSetValue e initial
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
    unsubscribeOnblur <- liftIO $ elementOnblur e $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> False]
    unsubscribeOnfocus <- liftIO $ elementOnfocus e $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> True]
    return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
  performEvent_ $ fmap (liftIO . htmlTextAreaElementSetValue e) eSet
  f <- holdDyn False eChangeFocus
  ev <- wrapDomEvent e elementOninput $ liftIO $ htmlTextAreaElementGetValue e
  v <- holdDyn initial $ leftmost [eSet, ev]
  eKeypress <- wrapDomEvent e elementOnkeypress getKeyEvent
  return $ TextArea v ev e f eKeypress

data CheckboxConfig t
    = CheckboxConfig { _checkboxConfig_setValue :: Event t Bool
                     , _checkboxConfig_attributes :: Dynamic t (Map String String)
                     }

instance Reflex t => Default (CheckboxConfig t) where
  def = CheckboxConfig { _checkboxConfig_setValue = never
                       , _checkboxConfig_attributes = constDyn mempty
                       }

data Checkbox t
   = Checkbox { _checkbox_value :: Dynamic t Bool
              , _checkbox_change :: Event t Bool
              }

--TODO: Make attributes possibly dynamic
-- | Create an editable checkbox
--   Note: if the "type" or "checked" attributes are provided as attributes, they will be ignored
checkbox :: MonadWidget t m => Bool -> CheckboxConfig t -> m (Checkbox t)
checkbox checked config = do
  attrs <- mapDyn (\c -> Map.insert "type" "checkbox" $ (if checked then Map.insert "checked" "checked" else Map.delete "checked") c) (_checkboxConfig_attributes config)
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" attrs
  eClick <- wrapDomEvent e elementOnclick $ liftIO $ htmlInputElementGetChecked e
  performEvent_ $ fmap (\v -> liftIO $ htmlInputElementSetChecked e $! v) $ _checkboxConfig_setValue config
  dValue <- holdDyn checked $ leftmost [_checkboxConfig_setValue config, eClick]
  return $ Checkbox dValue eClick

checkboxView :: MonadWidget t m => Dynamic t (Map String String) -> Dynamic t Bool -> m (Event t Bool)
checkboxView dAttrs dValue = do
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" =<< mapDyn (Map.insert "type" "checkbox") dAttrs
  eClicked <- wrapDomEvent e elementOnclick $ do
    preventDefault
    liftIO $ htmlInputElementGetChecked e
  schedulePostBuild $ do
    v <- sample $ current dValue
    when v $ liftIO $ htmlInputElementSetChecked e True
  performEvent_ $ fmap (\v -> liftIO $ htmlInputElementSetChecked e $! v) $ updated dValue
  return eClicked

data Dropdown t k
    = Dropdown { _dropdown_value :: Dynamic t k
               , _dropdown_change :: Event t k
               }

data DropdownConfig t k
   = DropdownConfig { _dropdownConfig_setValue :: Event t k
                    , _dropdownConfig_attributes :: Dynamic t (Map String String)
                    }

instance (Reflex t, Ord k, Show k, Read k) => Default (DropdownConfig t k) where
  def = DropdownConfig { _dropdownConfig_setValue = never
                       , _dropdownConfig_attributes = constDyn mempty
                       }

--TODO: We should allow the user to specify an ordering instead of relying on the ordering of the Map
--TODO: Get rid of Show k and Read k by indexing the possible values ourselves
-- | Create a dropdown box
--   The first argument gives the initial value of the dropdown; if it is not present in the map of options provided, it will be added with an empty string as its text
dropdown :: forall k t m. (MonadWidget t m, Ord k, Show k, Read k) => k -> Dynamic t (Map k String) -> DropdownConfig t k -> m (Dropdown t k)
dropdown k0 options (DropdownConfig setK attrs) = do
  (eRaw, _) <- elDynAttr' "select" attrs $ do
    optionsWithDefault <- mapDyn (`Map.union` (k0 =: "")) options
    listWithKey optionsWithDefault $ \k v -> do
      elAttr "option" ("value" =: show k <> if k == k0 then "selected" =: "selected" else mempty) $ dynText v
  let e = castToHTMLSelectElement $ _el_element eRaw
  performEvent_ $ fmap (liftIO . htmlSelectElementSetValue e . show) setK
  eChange <- wrapDomEvent e elementOnchange $ do
    kStr <- liftIO $ htmlSelectElementGetValue e
    return $ readMay kStr
  let readKey opts mk = fromMaybe k0 $ do
        k <- mk
        guard $ Map.member k opts
        return k
  dValue <- combineDyn readKey options =<< holdDyn (Just k0) (leftmost [eChange, fmap Just setK])
  return $ Dropdown dValue (attachDynWith readKey options eChange)

liftM concat $ mapM makeLenses
  [ ''TextAreaConfig
  , ''TextArea
  , ''TextInputConfig
  , ''TextInput
  , ''DropdownConfig
  , ''Dropdown
  , ''CheckboxConfig
  , ''Checkbox
  ]

class HasAttributes a where
  type Attrs a :: *
  attributes :: Lens' a (Attrs a)

instance HasAttributes (TextAreaConfig t) where
  type Attrs (TextAreaConfig t) = Dynamic t (Map String String)
  attributes = textAreaConfig_attributes

instance HasAttributes (TextInputConfig t) where
  type Attrs (TextInputConfig t) = Dynamic t (Map String String)
  attributes = textInputConfig_attributes

instance HasAttributes (DropdownConfig t k) where
  type Attrs (DropdownConfig t k) = Dynamic t (Map String String)
  attributes = dropdownConfig_attributes

instance HasAttributes (CheckboxConfig t) where
  type Attrs (CheckboxConfig t) = Dynamic t (Map String String)
  attributes = checkboxConfig_attributes

class HasSetValue a where
  type SetValue a :: *
  setValue :: Lens' a (SetValue a)

instance HasSetValue (TextAreaConfig t) where
  type SetValue (TextAreaConfig t) = Event t String
  setValue = textAreaConfig_setValue

instance HasSetValue (TextInputConfig t) where
  type SetValue (TextInputConfig t) = Event t String
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
  type Value (TextArea t) = Dynamic t String
  value = _textArea_value

instance HasValue (TextInput t) where
  type Value (TextInput t) = Dynamic t String
  value = _textInput_value

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

