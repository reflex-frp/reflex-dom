{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Dom.Modals.Base where

import Control.Lens hiding (element)
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.Trans
import Data.Coerce
import Data.Default
import Data.Dependent.Map (DMap, DSum (..), GCompare)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Compose
import Data.Proxy
import Data.Text (Text)
import Foreign.JavaScript.TH
import Language.Javascript.JSaddle (JSM, MonadJSM)

import Reflex
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Class
import Reflex.Dom.Modals.Class
import Reflex.Dom.Widget.Basic
import Reflex.Host.Class

instance (Reflex t, PrimMonad m) => ModalOpener t (ModalsT t m) where
  requestingModal = ModalsT . requesting . fmap Compose

newtype ModalsT t m a = ModalsT { unModalsT :: RequesterT t (Compose (ModalsT t m) (Event t)) Maybe m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance Wrapped (ModalsT t m a) where
  type Unwrapped (ModalsT t m a) = RequesterT t (Compose (ModalsT t m) (Event t)) Maybe m a
  _Wrapped' = iso coerce coerce

instance ModalsT t m a ~ x => Rewrapped (ModalsT t m a) x

instance MonadTrans (ModalsT t) where
  lift = ModalsT . lift

instance (Reflex t, PostBuild t m) => PostBuild t (ModalsT t m) where
  getPostBuild = view _Unwrapped getPostBuild

instance MonadSample t m => MonadSample t (ModalsT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (ModalsT t m) where
  {-# INLINABLE hold #-}
  hold v0 = lift . hold v0
  {-# INLINABLE holdDyn #-}
  holdDyn v0 = lift . holdDyn v0
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 = lift . holdIncremental v0

instance PerformEvent t m => PerformEvent t (ModalsT t m) where
  type Performable (ModalsT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ = lift . performEvent_
  {-# INLINABLE performEvent #-}
  performEvent = lift . performEvent

instance (ReflexHost t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (ModalsT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = ModalsT . lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = ModalsT $ lift $ newFanEventWithTrigger f

instance TriggerEvent t m => TriggerEvent t (ModalsT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance MonadRef m => MonadRef (ModalsT t m) where
  type Ref (ModalsT t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (ModalsT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance (MonadAdjust t m, MonadHold t m) => MonadAdjust t (ModalsT t m) where
  runWithReplace = coerce (runWithReplace :: Unwrapped (ModalsT t m a) -> Event t (Unwrapped (ModalsT t m b)) -> Unwrapped (ModalsT t m (a, Event t b)))
  traverseDMapWithKeyWithAdjust f dm0 dm' = ModalsT $ traverseDMapWithKeyWithAdjust (coerce f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = ModalsT $ traverseDMapWithKeyWithAdjustWithMove (coerce f) dm0 dm'

instance (DomBuilder t m, PerformEvent t m, MonadFix m, MonadHold t m) => DomBuilder t (ModalsT t m) where
  type DomBuilderSpace (ModalsT t m) = DomBuilderSpace m
  element = coerce (element :: Text -> ElementConfig er t (DomBuilderSpace m) -> Unwrapped (ModalsT t m a) -> Unwrapped (ModalsT t m (Element er (DomBuilderSpace m) t, a)))
  inputElement = coerce (inputElement :: InputElementConfig er t (DomBuilderSpace m) -> Unwrapped (ModalsT t m (InputElement er (DomBuilderSpace m) t)))
  textAreaElement = coerce (textAreaElement :: TextAreaElementConfig er t (DomBuilderSpace m) -> Unwrapped (ModalsT t m (TextAreaElement er (DomBuilderSpace m) t)))
  selectElement = coerce (selectElement :: SelectElementConfig er t (DomBuilderSpace m) -> Unwrapped (ModalsT t m a) -> Unwrapped (ModalsT t m (SelectElement er (DomBuilderSpace m) t, a)))
  wrapRawElement e = (view _Unwrapped :: forall a. Unwrapped (ModalsT t m a) -> ModalsT t m a) . wrapRawElement e

instance HasDocument m => HasDocument (ModalsT t m)
instance HasJSContext m => HasJSContext (ModalsT t m) where
  type JSContextPhantom (ModalsT t m) = JSContextPhantom m
  askJSContext = ModalsT askJSContext
#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (ModalsT t m)
#endif

withModalLayerClass :: forall t m a. (Reflex t, MonadFix m, DomBuilder t m, MonadHold t m) => Text -> ModalsT t m a -> m a
withModalLayerClass c (ModalsT a) = elAttr "div" ("class" =: c) $ do
  rec (result, requests) <- do
        runRequesterT a modalDone
      let newModal = ffor requests $ \rs -> case DMap.minViewWithKey rs of
            Nothing -> return never -- This should never happen
            Just (k :=> v, _) -> do
              (overlay, complete) <- elAttr' "div" ("style" =: "position:absolute;left:0;right:0;top:0;bottom:0;background-color:rgba(0,0,0,0.1);display:flex;justify-content:center;align-items:center") $ do --TODO: implement a stack rather than throwing away
                let f = GhcjsEventFilter $ \_ -> return (stopPropagation, return Nothing)
                    cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
                      & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> stopPropagation)
                      & initialAttributes .~ ("style" =: "background-color:white;opacity:1;padding:1em")
                fmap snd $ element "div" cfg $ do
                  fst <$> runRequesterT (unModalsT (getCompose v)) never --TODO: Don't ignore modal requests from the modal
              return $ leftmost
                [ DMap.singleton k . Just <$> complete
                , DMap.singleton k Nothing <$ domEvent Click overlay
                ]
      modal <- widgetHold (return never) $ leftmost [newModal, return never <$ modalDone]
      let modalDone = switch $ current modal
  return result
