{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, UndecidableInstances, StandaloneDeriving #-}
module Reflex.Dom.DynamicWriter where

import Reflex
import Reflex.Host.Class
import Reflex.Dom.Class
import Control.Monad.IO.Class
import Control.Monad.State.Strict
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

newtype DynamicWriterT t w m a = DynamicWriterT { unDynamicWriterT :: StateT [Dynamic t w] m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t, MonadSample t, MonadAsyncException, MonadException, HasWebView, HasDocument) -- The list is kept in reverse order

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

tellDyn :: Monad m => Dynamic t w -> DynamicWriterT t w m ()
tellDyn w = DynamicWriterT $ modify (w:)
