{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleContexts #-}
module Reflex.Dom.Widget.Input where

import Reflex.Dom.Class
import Reflex.Dom.Widget.Basic

import Reflex
import Data.Map (Map)
import GHCJS.DOM.Document
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.Node
import GHCJS.DOM.Element
import Data.Monoid
import Data.Map as Map
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens

input' :: MonadWidget t m => String -> Event t String -> Dynamic t (Map String String) -> m (TextInput t)
input' inputType eSetValue dAttrs = do
  dSetValue <- holdDyn "" eSetValue
  dynText =<< combineDyn (\v a -> "Input placeholder: " <> show (inputType, v, a)) dSetValue dAttrs
  return $ TextInput (constDyn "") never never never (constDyn False) (constDyn Nothing)
{-
input' inputType eSetValue dAttrs = do
  dEffectiveAttrs <- mapDyn (Map.insert "type" inputType) dAttrs
  let mkSelf (initialValue, initialAttrs) = do
        doc <- askDocument
        Just e <- liftIO $ liftM (fmap castToHTMLInputElement) $ documentCreateElement doc "input"
        liftIO $ htmlInputElementSetValue e initialValue
        iforM_ initialAttrs $ \attr value -> liftIO $ elementSetAttribute e attr value
        eChange <- wrapDomEvent e elementOninput $ liftIO $ htmlInputElementGetValue e
        runFrame <- askRunFrame
        eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
          unsubscribeOnblur <- liftIO $ elementOnblur e $ liftIO $ do
            runFrame $ DMap.singleton eChangeFocusTrigger False
          unsubscribeOnfocus <- liftIO $ elementOnfocus e $ liftIO $ do
            runFrame $ DMap.singleton eChangeFocusTrigger True
          return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
        eKeypress <- wrapDomEvent e elementOnkeypress $ liftIO . uiEventGetKeyCode =<< event
        eKeydown <- wrapDomEvent e elementOnkeydown $ liftIO . uiEventGetKeyCode =<< event
        eKeyup <- wrapDomEvent e elementOnkeyup $ liftIO . uiEventGetKeyCode =<< event
        return (e, eChange, eKeypress, eKeydown, eKeyup, eChangeFocus)
  dInitialValue <- holdDyn "" eSetValue
  dInitial <- combineDyn (,) dInitialValue dEffectiveAttrs
  eCreated <- performEvent . fmap mkSelf . tagDyn dInitial =<< getEInit
  dMyElement <- holdDyn Nothing $ fmap (Just . (^. _1)) eCreated
  performEvent_ . updated =<< combineDyn (maybe (const $ return ()) $ \e attrs -> liftIO $ setElementAttributes e attrs) dMyElement dEffectiveAttrs
  putEChildren $ fmap ((:[]) . toNode . (^. _1)) eCreated
  performEvent_ $ fmapMaybe (fmap $ \(e,v) -> liftIO $ htmlInputElementSetValue e v) $ attachWith (\e v -> case e of
    Nothing -> Nothing
    Just e' -> Just (e',v)) (current dMyElement) eSetValue
  eChange <- liftM switch $ hold never $ fmap (^. _2) eCreated
  dFocus <- holdDyn False . switch =<< hold never (fmap (^. _6) eCreated) --TODO: Check that 'False' is always the correct starting value - i.e.: an element will always receive an 'onfocus' event, even if it gets focus immediately
  dValue <- holdDyn "" $ leftmost [eSetValue, eChange]
  eKeypress <- liftM switch $ hold never $ fmap (^. _3) eCreated
  eKeydown <- liftM switch $ hold never $ fmap (^. _4) eCreated
  eKeyup <- liftM switch $ hold never $ fmap (^. _5) eCreated
  return $ TextInput dValue eKeypress eKeydown eKeyup dFocus dMyElement
-}
checkboxView :: MonadWidget t m => Dynamic t (Map String String) -> Dynamic t Bool -> m (Event t ())
checkboxView dAttrs dValue = do
  dynText =<< mapDyn (\x -> if x then "X" else "_") dValue
  return never
{-
checkboxView dAttrs dValue = do
  dEffectiveAttrs <- mapDyn (Map.insert "type" "checkbox") dAttrs
  let mkSelf (initialValue, initialAttrs) = do
        doc <- askDocument
        Just e <- liftIO $ liftM (fmap castToHTMLInputElement) $ documentCreateElement doc "input"
        iforM_ initialAttrs $ \attr value -> liftIO $ elementSetAttribute e attr value
        liftIO $ htmlInputElementSetChecked e initialValue
        eClicked <- wrapDomEvent e elementOnclick preventDefault
        return (e, eClicked)
  dInitial <- combineDyn (,) dValue dEffectiveAttrs
  eCreated <- performEvent . fmap mkSelf . tagDyn dInitial =<< getEInit
  putEChildren $ fmap ((:[]) . toNode . (^. _1)) eCreated
  dElement <- holdDyn Nothing $ fmap (Just . (^. _1)) eCreated
  performEvent_ . updated =<< combineDyn (\me a -> forM_ me $ \e -> liftIO $ setElementAttributes e a) dElement dEffectiveAttrs
  performEvent_ . updated =<< combineDyn (\v me -> maybe (return ()) (\e -> liftIO $ htmlInputElementSetChecked e v) me) dValue dElement
  liftM switch $ hold never $ fmap (^. _2) eCreated
-}

data TextInput t
  = TextInput { _textInput_value :: Dynamic t String
              , _textInput_keypress :: Event t Int
              , _textInput_keydown :: Event t Int
              , _textInput_keyup :: Event t Int
              , _textInput_hasFocus :: Dynamic t Bool
              , _textInput_element :: Dynamic t (Maybe HTMLInputElement)
              }

textInput :: MonadWidget t m => m (TextInput t)
textInput = input' "text" never (constDyn $ Map.empty)

textInputGetEnter :: Reflex t => TextInput t -> Event t ()
textInputGetEnter i = fmapMaybe (\n -> if n == keycodeEnter then Just () else Nothing) $ _textInput_keypress i

data Dropdown t k
  = Dropdown { _dropdown_value :: Dynamic t k
             }

--TODO
dropdown :: (MonadWidget t m, Show k, Read k) => Dynamic t (Map k String) -> m (Dropdown t (Maybe k))
dropdown dOptions = do
  el "div" $ text "dropdown"
  return $ Dropdown (constDyn Nothing)

--dropdown :: forall t m k. (MonadWidget t m, Show k, Read k) => Dynamic t (Map k String) -> m (Dropdown t (Maybe k))
--dropdown dOptions = do
--  let triggerChange (e, reChangeTrigger) runFrame = do
--        v <- htmlSelectElementGetValue e
--        readRef reChangeTrigger >>= mapM_ (\eChangeTrigger -> runFrame $ DMap.singleton eChangeTrigger $ readMay v)
--  let mkSelf = do
--        doc <- askDocument
--        Just e <- liftIO $ liftM (fmap castToHTMLSelectElement) $ documentCreateElement doc "select"
--        liftIO $ elementSetAttribute e "class" "form-control"
--        runFrame <- askRunFrame
--        reChangeTrigger <- newRef Nothing
--        eChange <- newEventWithTrigger $ \eChangeTrigger -> do
--          writeRef reChangeTrigger $ Just eChangeTrigger
--          unsubscribe <- liftIO $ elementOnchange e $ liftIO $ triggerChange (e, reChangeTrigger) runFrame
--          return $ do
--            writeRef reChangeTrigger Nothing
--            liftIO unsubscribe
--        return ((e, reChangeTrigger), eChange)
--  eCreated <- performEvent . fmap (const mkSelf) =<< getEInit
--  dState <- holdDyn Nothing $ fmap (Just . (^. _1)) eCreated
--  putEChildren $ fmap ((:[]) . toNode . fst . (^. _1)) eCreated
--  let updateOptions :: Map k String -> Maybe (HTMLSelectElement, Ref h (Maybe (EventTrigger t (Maybe k)))) -> h ()
--      updateOptions options = maybe (return ()) $ \myState@(selectElement, _) -> do
--        doc <- askDocument
--        liftIO $ htmlElementSetInnerHTML selectElement ""
--        iforM_ options $ \k optionText -> do
--          Just optionElement <- liftIO $ liftM (fmap castToHTMLOptionElement) $ documentCreateElement doc "option"
--          liftIO $ elementSetAttribute optionElement "value" $ show k
--          _ <- setInnerText optionElement optionText
--          liftIO $ nodeAppendChild selectElement $ Just optionElement
--        runFrame <- askRunFrame
--        liftIO $ triggerChange myState runFrame
--  performEvent_ . updated =<< combineDyn updateOptions dOptions dState
--  eChange <- liftM switch $ hold never $ fmap (^. _2) eCreated
--  dValue <- holdDyn Nothing eChange
--  return $ Dropdown dValue

data Checkbox t
  = Checkbox { _checkbox_checked :: Dynamic t Bool
             }

checkbox :: MonadWidget t m => Bool -> Map String String -> m (Checkbox t)
checkbox checked attrs = do
  let mkSelf = do
        doc <- askDocument
        Just e <- liftIO $ liftM (fmap castToHTMLInputElement) $ documentCreateElement doc "input"
        liftIO $ elementSetAttribute e "type" "checkbox"
        iforM_ attrs $ \attr value -> liftIO $ elementSetAttribute e attr value
        if checked == True then liftIO $ elementSetAttribute e "checked" "true" else return ()
        eChange <- wrapDomEvent e elementOnclick $ liftIO $ htmlInputElementGetChecked e
        return (e, eChange)
  (e,eChange) <- mkSelf
  p <- askParent
  liftIO $ nodeAppendChild p (Just e)
  dValue <- holdDyn checked eChange
  return $ Checkbox dValue
