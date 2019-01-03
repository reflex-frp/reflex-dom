{-
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-}
module Reflex.Dom.Modals.Base
where
{-
  ( ModalsT (..)
  , ModalLayerConfig (..)
  , withModalLayer
  )where

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
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.JavaScript.TH
import Language.Javascript.JSaddle (MonadJSM)

import Data.Functor.Misc
import Reflex
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Class
import Reflex.Dom.Modals.Class
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

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (ModalsT t m) where
  runWithReplace a0 a' = ModalsT $ runWithReplace (unModalsT a0) (fmapCheap unModalsT a')
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

data ModalLayerConfig t m = ModalLayerConfig
  { _modalLayerConfig_backdropElementConfig :: ElementConfig EventResult t (DomBuilderSpace m)
  , _modalLayerConfig_bodyElementConfig :: ElementConfig EventResult t (DomBuilderSpace m)
  }

instance (DomBuilder t m) => Default (ModalLayerConfig t m) where
  def = ModalLayerConfig
    { _modalLayerConfig_backdropElementConfig = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)
        & initialAttributes .~ ("style" =: T.intercalate ";"
            [ "position:absolute"
            , "left:0", "right:0", "top:0", "bottom:0"
            , "background-color:rgba(0,0,0,0.1)"
            , "display:flex", "justify-content:center", "align-items:center"
            ])
    , _modalLayerConfig_bodyElementConfig = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)
        & initialAttributes .~ ("style" =: "background-color:white;opacity:1;padding:1em")
    }

instance (MonadQuery t q m, Monad m) => MonadQuery t q (ModalsT t m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

withModalLayer :: forall t m a. (Reflex t, MonadFix m, DomBuilder t m, MonadHold t m)
               => ModalLayerConfig t m
               -> ModalsT t m a
               -> m a
withModalLayer cfg (ModalsT a) = do
  rec (result, requests) <- do
        runRequesterT a modalDone
      modalDone <- modalInner requests cfg
  return result

modalInner :: forall t m k. (GCompare k, MonadHold t m, MonadFix m, DomBuilder t m)
           => Event t (DMap k (Compose (ModalsT t m) (Event t)))
           -> ModalLayerConfig t m
           -> m (Event t (DMap k Maybe))
modalInner rqs' cfg = do
  rec let rqs = fmap toInsertionsDMap rqs'
          dones = fmap toDeletionsDMap modalDone
      (m0, mpatch) <- traverseDMapWithKeyWithAdjust (\_ v -> Compose <$> modalBody cfg v) DMap.empty (rqs <> dones)
      modal <- fmap (merge . mapKeyValuePairsMonotonic wrapArgs) . incrementalToDynamic <$> holdIncremental m0 mpatch
      let modalDone = fmap (mapKeyValuePairsMonotonic unwrapArgs) $ switch $ current modal
  return modalDone
 where
  wrapArgs (k :=> v) = WrapArg k :=> getCompose v
  unwrapArgs :: DSum (WrapArg Maybe k) Identity -> DSum k Maybe
  unwrapArgs (WrapArg k :=> Identity v) = k :=> v
  toInsertionsDMap :: DMap k v -> PatchDMap k v
  toInsertionsDMap = coerce . DMap.map (ComposeMaybe . Just)
  toDeletionsDMap :: forall k' v v'. DMap k' v -> PatchDMap k' v'
  toDeletionsDMap = coerce . DMap.map (\(_ :: v a) -> ComposeMaybe (Nothing :: Maybe (v' a)))

modalBody :: forall t m a. (MonadHold t m, MonadFix m, DomBuilder t m)
          => ModalLayerConfig t m
          -> Compose (ModalsT t m) (Event t) a
          -> m (Event t (Maybe a))
modalBody cfg v = do
  (overlay, complete) <- element "div" (_modalLayerConfig_backdropElementConfig cfg) $ do
    fmap snd $ element "div" (_modalLayerConfig_bodyElementConfig cfg) $ do
      rec (rsps, rqs') <- runRequesterT (unModalsT (getCompose v)) innerDone
          innerDone <- modalInner rqs' cfg
      return rsps
  return $ leftmost
    [ Just <$> complete
    , Nothing <$ domEvent Click overlay
    ]
-}
