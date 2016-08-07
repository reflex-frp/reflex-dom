{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Old
       ( MonadWidget
       , El
       , _el_element
       , addVoidAction
       , AttributeMap
       , Attributes (..)
       , buildElement
       , buildEmptyElement
       , buildEmptyElementNS
       , deleteBetweenExclusive
       , onEventName
       , schedulePostBuild
       , text'
       ) where

import Control.Arrow ((***))
import Control.Lens ((&), (.~))
import Control.Monad
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
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

_el_element :: El t -> RawElement GhcjsDomSpace
_el_element = _element_raw

addVoidAction :: MonadWidget t m => Event t (WidgetHost m ()) -> m ()
addVoidAction = performEvent_

type AttributeMap = Map String String

buildElement :: Attributes m attrs => String -> attrs -> m a -> m (RawElement GhcjsDomSpace, a)
buildElement = buildElementNS Nothing

buildEmptyElement :: Monad m => Attributes m attrs => String -> attrs -> m (RawElement GhcjsDomSpace)
buildEmptyElement elementTag attrs = fst <$> buildElementNS Nothing elementTag attrs blank

buildEmptyElementNS :: Monad m => Attributes m attrs => Maybe String -> String -> attrs -> m (RawElement GhcjsDomSpace)
buildEmptyElementNS ns elementTag attrs = fst <$> buildElementNS ns elementTag attrs blank

class Attributes m attrs where
  buildElementNS :: Maybe String -> String -> attrs -> m a -> m (RawElement GhcjsDomSpace, a)

instance MonadWidget t m => Attributes m (Map String String) where
  buildElementNS ns elementTag attrs child = do
    let cfg = def & elementConfig_namespace .~ fmap T.pack ns
    buildElementInternal elementTag child =<< addStaticAttributes attrs cfg

addStaticAttributes :: Applicative m => Map String String -> ElementConfig er t m -> m (ElementConfig er t m)
addStaticAttributes attrs cfg = do
  let initialAttrs = Map.fromList $ fmap (((,) Nothing . T.pack) *** T.pack) $ Map.toList attrs
  pure $ cfg & elementConfig_initialAttributes .~ initialAttrs

instance MonadWidget t m => Attributes m (Dynamic t (Map String String)) where
  buildElementNS ns elementTag attrs child = do
    let cfg = def & elementConfig_namespace .~ fmap T.pack ns
    buildElementInternal elementTag child =<< addDynamicAttributes attrs cfg

addDynamicAttributes :: PostBuild t m => Dynamic t (Map String String) -> ElementConfig er t m -> m (ElementConfig er t m)
addDynamicAttributes attrs cfg = do
  modifyAttrs <- dynamicAttributesToModifyAttributes $ fmap (Map.fromList . fmap (T.pack *** T.pack) . Map.toList) attrs
  return $ cfg & elementConfig_modifyAttributes .~ modifyAttrs

buildElementInternal :: MonadWidget t m => String -> m a -> ElementConfig en t m -> m (DOM.HTMLElement, a)
buildElementInternal elementTag child cfg = do
  (e, result) <- element (T.pack elementTag) cfg child
  return (_element_raw e, result)

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

text' :: MonadWidget t m => String -> m DOM.Text
text' s = _textNode_raw <$> textNode (def & textNodeConfig_initialContents .~ T.pack s)
