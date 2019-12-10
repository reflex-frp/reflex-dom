{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Reflex.Dom.Old
       ( MonadWidget
       , El
       , ElConfig (..)
       , elConfig_namespace
       , elConfig_attributes
       , _el_clicked
       , _el_element
       , _el_events
       , addVoidAction
       , AttributeMap
       , Attributes (..)
       , buildElement
       , buildElementNS
       , buildEmptyElement
       , buildEmptyElementNS
       , elDynHtml'
       , elDynHtmlAttr'
       , elStopPropagationNS
       , elWith
       , elWith'
       , emptyElWith
       , emptyElWith'
       , namedNodeMapGetNames
       , nodeClear
       , onEventName
       , schedulePostBuild
       , text'
       , unsafePlaceElement
       , WidgetHost
       , wrapElement
       ) where

import Control.Arrow (first)
#ifdef USE_TEMPLATE_HASKELL
import Control.Lens (makeLenses, (%~), (&), (.~), (^.))
#else
import Control.Lens (Lens, Lens', (%~), (&), (.~), (^.))
#endif
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Default
import Data.Dependent.Map as DMap
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Foreign.JavaScript.TH
import qualified GHCJS.DOM.Element as Element
import GHCJS.DOM.EventM (EventM)
import GHCJS.DOM.NamedNodeMap as NNM
import GHCJS.DOM.Node (getFirstChild, getNodeName, removeChild)
import GHCJS.DOM.Types
       (liftJSM, JSM, IsHTMLElement, IsNode)
import qualified GHCJS.DOM.Types as DOM
import Reflex.Class
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Widget.Basic
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

data ElConfig attrs = ElConfig
  { _elConfig_namespace :: Maybe Text
  , _elConfig_attributes :: attrs
  }

instance attrs ~ Map Text Text => Default (ElConfig attrs) where
  def = ElConfig
    { _elConfig_namespace = Nothing
    , _elConfig_attributes = mempty
    }

#ifdef USE_TEMPLATE_HASKELL
makeLenses ''ElConfig
#else
elConfig_namespace :: Lens' (ElConfig attrs1) (Maybe Text)
elConfig_namespace f (ElConfig a b) = (\a' -> ElConfig a' b) <$> f a
{-# INLINE elConfig_namespace #-}
elConfig_attributes :: Lens (ElConfig attrs1) (ElConfig attrs2) attrs1 attrs2
elConfig_attributes f (ElConfig a b) = (\b' -> ElConfig a b') <$> f b
{-# INLINE elConfig_attributes #-}
#endif

type MonadWidgetConstraints t m =
  ( DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadReflexCreateTrigger t m
  , PostBuild t m
  , PerformEvent t m
  , MonadIO m
  , MonadIO (Performable m)
#ifndef ghcjs_HOST_OS
  , DOM.MonadJSM m
  , DOM.MonadJSM (Performable m)
#endif
  , TriggerEvent t m
  , HasJSContext m
  , HasJSContext (Performable m)
  , HasDocument m
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  )

class MonadWidgetConstraints t m => MonadWidget t m
instance MonadWidgetConstraints t m => MonadWidget t m

type WidgetHost m = Performable m

type El = Element EventResult GhcjsDomSpace

addVoidAction :: MonadWidget t m => Event t (WidgetHost m ()) -> m ()
addVoidAction = performEvent_

type AttributeMap = Map Text Text

buildElement :: (MonadWidget t m, Attributes m attrs t) => Text -> attrs -> m a -> m (RawElement (DomBuilderSpace m), a)
buildElement = buildElementNS Nothing

buildEmptyElement :: (MonadWidget t m, Attributes m attrs t) => Text -> attrs -> m (RawElement (DomBuilderSpace m))
buildEmptyElement elementTag attrs = fst <$> buildElementNS Nothing elementTag attrs blank

buildEmptyElementNS :: (MonadWidget t m, Attributes m attrs t) => Maybe Text -> Text -> attrs -> m (RawElement (DomBuilderSpace m))
buildEmptyElementNS ns elementTag attrs = fst <$> buildElementNS ns elementTag attrs blank

buildElementNS :: (MonadWidget t m, Attributes m attrs t) => Maybe Text -> Text -> attrs -> m a -> m (RawElement (DomBuilderSpace m), a)
buildElementNS ns elementTag attrs child = first _element_raw <$> buildElementInternal ns elementTag attrs child

class Attributes m attrs t where
  buildElementInternal :: MonadWidget t m => Maybe Text -> Text -> attrs -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)

instance Attributes m (Map Text Text) t where
  buildElementInternal ns elementTag attrs child = do
    let cfg = def & elementConfig_namespace .~ ns
    buildElementCommon elementTag child =<< addStaticAttributes attrs cfg

addStaticAttributes :: Applicative m => Map Text Text -> ElementConfig er t (DomBuilderSpace m) -> m (ElementConfig er t (DomBuilderSpace m))
addStaticAttributes attrs cfg = do
  let initialAttrs = Map.fromList $ first (AttributeName Nothing) <$> Map.toList attrs
  pure $ cfg & elementConfig_initialAttributes .~ initialAttrs

instance PostBuild t m => Attributes m (Dynamic t (Map Text Text)) t where
  buildElementInternal ns elementTag attrs child = do
    let cfg = def & elementConfig_namespace .~ ns
    buildElementCommon elementTag child =<< addDynamicAttributes attrs cfg

addDynamicAttributes :: PostBuild t m => Dynamic t (Map Text Text) -> ElementConfig er t (DomBuilderSpace m) -> m (ElementConfig er t (DomBuilderSpace m))
addDynamicAttributes attrs cfg = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  return $ cfg & elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs

buildElementCommon :: MonadWidget t m => Text -> m a -> ElementConfig er t (DomBuilderSpace m) -> m (Element er (DomBuilderSpace m) t, a)
buildElementCommon elementTag child cfg = element elementTag cfg child


onEventName :: IsHTMLElement e => EventName en -> e -> EventM e (EventType en) () -> JSM (JSM ())
onEventName = elementOnEventName

schedulePostBuild :: (PostBuild t m, PerformEvent t m) => WidgetHost m () -> m ()
schedulePostBuild w = do
  postBuild <- getPostBuild
  performEvent_ $ w <$ postBuild

text' :: MonadWidget t m => Text -> m DOM.Text
text' s = _textNode_raw <$> textNode (def & textNodeConfig_initialContents .~ s)

instance HasAttributes (ElConfig attrs) where
  type Attrs (ElConfig attrs) = attrs
  attributes = elConfig_attributes

instance HasNamespace (ElConfig attrs) where
  namespace = elConfig_namespace

elWith :: (MonadWidget t m, Attributes m attrs t) => Text -> ElConfig attrs -> m a -> m a
elWith elementTag cfg child = snd <$> elWith' elementTag cfg child

elWith' :: (MonadWidget t m, Attributes m attrs t) => Text -> ElConfig attrs -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elWith' elementTag cfg = buildElementInternal (cfg ^. namespace) elementTag $ cfg ^. attributes

emptyElWith :: (MonadWidget t m, Attributes m attrs t) => Text -> ElConfig attrs -> m ()
emptyElWith elementTag cfg = void $ emptyElWith' elementTag cfg

emptyElWith' :: (MonadWidget t m, Attributes m attrs t) => Text -> ElConfig attrs -> m (Element EventResult (DomBuilderSpace m) t)
emptyElWith' elementTag cfg = fmap fst $ elWith' elementTag cfg $ return ()

{-# DEPRECATED _el_clicked "Use 'domEvent Click' instead" #-}
_el_clicked :: Reflex t => Element EventResult d t -> Event t ()
_el_clicked = domEvent Click

{-# DEPRECATED _el_element "Use '_element_raw' instead" #-}
_el_element :: El t -> RawElement GhcjsDomSpace
_el_element = _element_raw

{-# DEPRECATED _el_events "Use '_element_events' instead; or, if possible, use 'domEvent' instead to retrieve a particular event" #-}
_el_events :: Element er d t -> EventSelector t (WrapArg er EventName)
_el_events = _element_events

{-# DEPRECATED _el_keypress "Use 'domEvent Keypress' instead" #-}
_el_keypress :: Reflex t => El t -> Event t Word
_el_keypress = domEvent Keypress

{-# DEPRECATED _el_scrolled "Use 'domEvent Scroll' instead" #-}
_el_scrolled :: Reflex t => El t -> Event t Double
_el_scrolled = domEvent Scroll

wrapElement :: forall t m. MonadWidget t m => (forall en. DOM.HTMLElement -> EventName en -> EventM DOM.Element (EventType en) (Maybe (EventResult en))) -> DOM.HTMLElement -> m (El t)
wrapElement eh e = do
  let h :: (EventName en, GhcjsDomEvent en) -> JSM (Maybe (EventResult en))
      h (en, GhcjsDomEvent evt) = runReaderT (eh e en) evt
  wrapRawElement (DOM.toElement e) $ (def :: RawElementConfig EventResult t (DomBuilderSpace m))
    { _rawElementConfig_eventSpec = def
        { _ghcjsEventSpec_handler = GhcjsEventHandler h
        }
    }

unsafePlaceElement :: MonadWidget t m => DOM.HTMLElement -> m (Element EventResult (DomBuilderSpace m) t)
unsafePlaceElement e = do
  placeRawElement $ DOM.toElement e
  wrapRawElement (DOM.toElement e) def

namedNodeMapGetNames :: DOM.NamedNodeMap -> JSM (Set Text)
namedNodeMapGetNames self = do
  l <- NNM.getLength self
  Set.fromList <$> forM (take (fromIntegral l) [0..]) (
    NNM.itemUnchecked self >=> getNodeName)

nodeClear :: IsNode self => self -> JSM ()
nodeClear n = do
  mfc <- getFirstChild n
  case mfc of
    Nothing -> return ()
    Just fc -> do
      _ <- removeChild n fc
      nodeClear n

elStopPropagationNS :: forall t m en a. (MonadWidget t m) => Maybe Text -> Text -> EventName en -> m a -> m a
elStopPropagationNS ns elementTag en child = do
  let f = GhcjsEventFilter $ \_ -> do
        return (stopPropagation, return Nothing)
      cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & namespace .~ ns
        & elementConfig_eventSpec . ghcjsEventSpec_filters %~ DMap.insert en f
  snd <$> element elementTag cfg child

elDynHtmlAttr' :: (DOM.MonadJSM m, MonadWidget t m) => Text -> Map Text Text -> Dynamic t Text -> m (Element EventResult GhcjsDomSpace t)
elDynHtmlAttr' elementTag attrs html = do
  let cfg = def & initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs
  (e, _) <- element elementTag cfg $ return ()
  postBuild <- getPostBuild
  performEvent_ $ liftJSM . Element.setInnerHTML (_element_raw e) <$> leftmost [updated html, tag (current html) postBuild]
  return e

elDynHtml' :: MonadWidget t m => Text -> Dynamic t Text -> m (Element EventResult GhcjsDomSpace t)
elDynHtml' elementTag = elDynHtmlAttr' elementTag mempty
