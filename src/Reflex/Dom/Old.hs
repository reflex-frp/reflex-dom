{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Dom.Old where

import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Ref
import Foreign.JavaScript.TH
import Reflex
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.PerformEvent.Class
import Reflex.Dom.PostBuild.Class
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
