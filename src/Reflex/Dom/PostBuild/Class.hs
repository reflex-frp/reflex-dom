{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.PostBuild.Class where

import Control.Lens hiding (element)
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Control
import Foreign.JavaScript.TH
import Reflex
import Reflex.Dom.Builder.Class
import Reflex.Dom.PerformEvent.Class
import Reflex.Host.Class

class (Reflex t, Monad m) => PostBuild t m | m -> t where
  getPostBuild :: m (Event t ())

newtype PostBuildT t m a = PostBuildT { unPostBuildT :: ReaderT (Event t ()) m a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadTrans, MonadException, MonadAsyncException)

instance MonadTransControl (PostBuildT t) where
  type StT (PostBuildT t) a = StT (ReaderT (Event t ())) a
  {-# INLINABLE liftWith #-}
  liftWith = defaultLiftWith PostBuildT unPostBuildT
  {-# INLINABLE restoreT #-}
  restoreT = defaultRestoreT PostBuildT

instance (Reflex t, Monad m) => PostBuild t (PostBuildT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = PostBuildT ask

instance Deletable t m => Deletable t (PostBuildT t m) where
  {-# INLINABLE deletable #-}
  deletable = liftThrough . deletable

{-# INLINABLE liftPostBuildTElementConfig #-}
liftPostBuildTElementConfig :: ElementConfig er t (PostBuildT t m) -> ElementConfig er t m
liftPostBuildTElementConfig cfg = cfg
  { _elementConfig_eventSpec = _elementConfig_eventSpec cfg
  }

instance (DomBuilder t m, PerformEvent t m, MonadFix m, MonadHold t m) => DomBuilder t (PostBuildT t m) where
  type DomBuilderSpace (PostBuildT t m) = DomBuilderSpace m
  {-# INLINABLE textNode #-}
  textNode = lift . textNode
  {-# INLINABLE element #-}
  element t cfg child = liftWith $ \run -> element t (liftPostBuildTElementConfig cfg) $ run child
  {-# INLINABLE placeholder #-}
  placeholder cfg = lift $ do
    rec childPostBuild <- deletable (_placeholder_deletedSelf p) $ performEvent $ return () <$ _placeholder_insertedAbove p
        p <- placeholder $ cfg
          { _placeholderConfig_insertAbove = ffor (_placeholderConfig_insertAbove cfg) $ \a -> runPostBuildT a =<< headE childPostBuild
          }
    return p
  {-# INLINABLE inputElement #-}
  inputElement cfg = lift $ inputElement $ cfg & inputElementConfig_elementConfig %~ liftPostBuildTElementConfig
  {-# INLINABLE textAreaElement #-}
  textAreaElement cfg = lift $ textAreaElement $ cfg & textAreaElementConfig_elementConfig %~ liftPostBuildTElementConfig
  placeRawElement = lift . placeRawElement
  wrapRawElement e cfg = liftWith $ \run -> wrapRawElement e $ fmap1 run cfg

instance MonadSample t m => MonadSample t (PostBuildT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (PostBuildT t m) where
  {-# INLINABLE hold #-}
  hold v0 = lift . hold v0
  {-# INLINABLE holdDyn #-}
  holdDyn v0 = lift . holdDyn v0
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 = lift . holdIncremental v0

instance PerformEvent t m => PerformEvent t (PostBuildT t m) where
  type Performable (PostBuildT t m) = PostBuildT t (Performable m)
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = liftWith $ \run -> performEvent_ $ fmap run e
  {-# INLINABLE performEvent #-}
  performEvent e = liftWith $ \run -> performEvent $ fmap run e

instance (ReflexHost t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (PostBuildT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = PostBuildT . lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = PostBuildT $ lift $ newFanEventWithTrigger f

instance TriggerEvent t m => TriggerEvent t (PostBuildT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance MonadRef m => MonadRef (PostBuildT t m) where
  type Ref (PostBuildT t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (PostBuildT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance (HasJS x m, ReflexHost t) => HasJS x (PostBuildT t m) where
  type JSM (PostBuildT t m) = JSM m
  liftJS = lift . liftJS

instance HasWebView m => HasWebView (PostBuildT t m) where
  type WebViewPhantom (PostBuildT t m) = WebViewPhantom m
  askWebView = lift askWebView

{-# INLINABLE runPostBuildT #-}
runPostBuildT :: PostBuildT t m a -> Event t () -> m a
runPostBuildT (PostBuildT a) = runReaderT a

instance PostBuild t m => PostBuild t (ReaderT r m) where
  getPostBuild = lift getPostBuild
