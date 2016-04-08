{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, UndecidableInstances, StandaloneDeriving, FunctionalDependencies #-}
module Reflex.Dom.DynamicWriter where

import Reflex
import Reflex.Host.Class
import Reflex.Dom.Class

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Exception
import qualified Data.Map as Map

instance MonadTrans (DynamicWriterT t w) where
  lift = DynamicWriterT . lift

instance (MonadWidget t m, Monoid w) => MonadWidget t (DynamicWriterT t w m) where
  type WidgetHost (DynamicWriterT t w m) = WidgetHost m
  type GuiAction (DynamicWriterT t w m) = GuiAction m
  type WidgetOutput t (DynamicWriterT t w m) = (Dynamic t w, WidgetOutput t m)
  askParent = lift askParent
  subWidget n (DynamicWriterT child) = DynamicWriterT $ do
    ws <- get
    (result, ws') <- lift $ subWidget n $ runStateT child ws
    put ws'
    return result
  subWidgetWithVoidActions n (DynamicWriterT child) = DynamicWriterT $ do
    ((result, ws), v) <- lift $ subWidgetWithVoidActions n $ runStateT child []
    w <- mconcatDyn ws
    return (result, (w, v))
  liftWidgetHost = lift . liftWidgetHost
  schedulePostBuild = lift . schedulePostBuild
  addVoidAction = lift . addVoidAction
  getRunWidget = do
    runWidget <- lift getRunWidget
    return $ \n (DynamicWriterT child) -> do
      ((result, w), postBuild, o) <- runWidget n $ do
        (result, ws) <- runStateT child []
        w <- mconcatDyn ws
        return (result, w)
      return (result, postBuild, (w, o))
  tellWidgetOutput o = do
    tellDyn =<< mapDyn (mconcat . Map.elems) . joinDynThroughMap =<< mapDyn (fmap fst) o
    lift . tellWidgetOutput =<< mapDyn (fmap snd) o

newtype DynamicWriterT t w m a = DynamicWriterT { unDynamicWriterT :: StateT [Dynamic t w] m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t, MonadSample t, MonadAsyncException, MonadException, HasDocument) -- The list is kept in reverse order

instance HasWebView m => HasWebView (DynamicWriterT t w m) where
  type WebViewPhantom (DynamicWriterT t w m) = WebViewPhantom m
  askWebView = lift askWebView

instance MonadRef m => MonadRef (DynamicWriterT t w m) where
  type Ref (DynamicWriterT t w m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (DynamicWriterT t w m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

deriving instance HasPostGui t h m => HasPostGui t h (DynamicWriterT t w m)

runDynamicWriterT :: (Reflex t, MonadHold t m, Monoid w) => DynamicWriterT t w m a -> m (a, Dynamic t w)
runDynamicWriterT (DynamicWriterT a) = do
  (result, ws) <- runStateT a []
  w <- mconcatDyn $ reverse ws
  return (result, w)

class Monad m => MonadDynamicWriter t w m | m -> t w where
  tellDyn :: Dynamic t w -> m ()

instance Monad m => MonadDynamicWriter t w (DynamicWriterT t w m) where
  tellDyn w = DynamicWriterT $ modify (w:)

instance MonadDynamicWriter t w m => MonadDynamicWriter t w (ReaderT r m) where
  tellDyn = lift . tellDyn

deriving instance MonadReader r m => MonadReader r (DynamicWriterT t w m)

instance MonadState s m => MonadState s (DynamicWriterT t w m) where
  get = lift get
  put = lift . put

instance HasJS x m => HasJS x (DynamicWriterT t w m) where
  type JSM (DynamicWriterT t w m) = JSM m
  liftJS = lift . liftJS
