{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Old
       ( MonadWidget
       , El
       , _el_clicked
       , _el_element
       , _el_events
       , addVoidAction
       , AttributeMap
       , Attributes (..)
       , buildElement
       , buildEmptyElement
       , buildEmptyElementNS
       , deleteBetweenExclusive
       , elWith
       , elWith'
       , emptyElWith
       , emptyElWith'
       , onEventName
       , schedulePostBuild
       , text'
       ) where

import Control.Arrow (first)
import Control.Lens (makeLenses, (&), (.~), (^.))
import Control.Monad
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.Default
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text
import Foreign.JavaScript.TH
import GHCJS.DOM.EventM (EventM)
import GHCJS.DOM.Node (getParentNode, getPreviousSibling, removeChild, toNode)
import GHCJS.DOM.Types (IsElement, IsNode)
import qualified GHCJS.DOM.Types as DOM
import Reflex
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.PerformEvent.Class
import Reflex.Dom.PostBuild.Class
import Reflex.Dom.Widget.Basic
import Reflex.Host.Class

data ElConfig attrs = ElConfig
  { _elConfig_namespace :: Maybe Text
  , _elConfig_attributes :: attrs
  }

makeLenses ''ElConfig

--TODO: HasDocument is still not accounted for
type MonadWidget t m =
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
  , TriggerEvent t m
  , HasWebView m
  , HasWebView (Performable m)
  , MonadAsyncException m
  , MonadAsyncException (Performable m)
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  )

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

addStaticAttributes :: Applicative m => Map Text Text -> ElementConfig er t m -> m (ElementConfig er t m)
addStaticAttributes attrs cfg = do
  let initialAttrs = Map.fromList $ first ((,) Nothing) <$> Map.toList attrs
  pure $ cfg & elementConfig_initialAttributes .~ initialAttrs

instance PostBuild t m => Attributes m (Dynamic t (Map Text Text)) t where
  buildElementInternal ns elementTag attrs child = do
    let cfg = def & elementConfig_namespace .~ ns
    buildElementCommon elementTag child =<< addDynamicAttributes attrs cfg

addDynamicAttributes :: PostBuild t m => Dynamic t (Map Text Text) -> ElementConfig er t m -> m (ElementConfig er t m)
addDynamicAttributes attrs cfg = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  return $ cfg & elementConfig_modifyAttributes .~ modifyAttrs

buildElementCommon :: MonadWidget t m => Text -> m a -> ElementConfig er t m -> m (Element er (DomBuilderSpace m) t, a)
buildElementCommon elementTag child cfg = element elementTag cfg child

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

onEventName :: IsElement e => EventName en -> e -> EventM e (EventType en) () -> IO (IO ())
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
_el_keypress :: Reflex t => El t -> Event t Int
_el_keypress = domEvent Keypress

{-# DEPRECATED _el_scrolled "Use 'domEvent Scroll' instead" #-}
_el_scrolled :: Reflex t => El t -> Event t Int
_el_scrolled = domEvent Scroll
