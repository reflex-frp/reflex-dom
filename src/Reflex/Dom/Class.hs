{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TemplateHaskell, PolyKinds, TypeOperators, DeriveFunctor, LambdaCase, CPP, ForeignFunctionInterface, DeriveDataTypeable, ConstraintKinds, DefaultSignatures #-}
module Reflex.Dom.Class ( module Reflex.Dom.Class
                        , module Foreign.JavaScript.TH
                        ) where

import Prelude hiding (mapM, mapM_, sequence, concat)

import Reflex
import Reflex.Host.Class
import Foreign.JavaScript.TH

import Control.Monad.Identity hiding (mapM, mapM_, forM, forM_, sequence)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Control.Monad.Ref
import Control.Monad.Reader hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.State hiding (mapM, mapM_, forM, forM_, sequence)
import qualified Control.Monad.State.Strict as Strict hiding (mapM, mapM_, forM, forM_, sequence)
import Data.Dependent.Sum (DSum (..))
import GHCJS.DOM.Types hiding (Event)
--import GHCJS.DOM (WebView)

-- | Alias for Data.Map.singleton
(=:) :: k -> a -> Map k a
(=:) = Map.singleton

keycodeEnter :: Int
keycodeEnter = 13

keycodeEscape :: Int
keycodeEscape = 27

class ( Reflex t, MonadHold t m, MonadIO m, Functor m, MonadReflexCreateTrigger t m
      , HasDocument m, HasWebView m, HasWebView (WidgetHost m), HasWebView (GuiAction m)
      , MonadIO (WidgetHost m), MonadIO (GuiAction m), Functor (WidgetHost m), MonadSample t (WidgetHost m)
      , HasPostGui t (GuiAction m) (WidgetHost m), HasPostGui t (GuiAction m) m, HasPostGui t (GuiAction m) (GuiAction m)
      , MonadRef m, MonadRef (WidgetHost m)
      , Ref m ~ Ref IO, Ref (WidgetHost m) ~ Ref IO --TODO: Eliminate this reliance on IO
      , MonadFix m
      ) => MonadWidget t m | m -> t where
  type WidgetHost m :: * -> *
  type GuiAction m :: * -> *
  type WidgetOutput t m :: *
  type WidgetOutput t m = Event t (WidgetHost m ())
  askParent :: m Node
  subWidget :: Node -> m a -> m a
  subWidgetWithVoidActions :: Node -> m a -> m (a, WidgetOutput t m)
  liftWidgetHost :: WidgetHost m a -> m a --TODO: Is this a good idea?
  schedulePostBuild :: WidgetHost m () -> m ()
  addVoidAction :: Event t (WidgetHost m ()) -> m ()
  getRunWidget :: IsNode n => m (n -> m a -> WidgetHost m (a, WidgetHost m (), WidgetOutput t m))
  tellWidgetOutput :: Ord k => Dynamic t (Map k (WidgetOutput t m)) -> m ()
  default tellWidgetOutput :: (WidgetOutput t m ~ Event t (WidgetHost m ()), Ord k) => Dynamic t (Map k (WidgetOutput t m)) -> m ()
  tellWidgetOutput = addVoidAction . switch . fmap (mergeWith (>>) . toList) . current

class Monad m => HasDocument m where
  askDocument :: m HTMLDocument

instance HasDocument m => HasDocument (ReaderT r m) where
  askDocument = lift askDocument

instance HasDocument m => HasDocument (StateT r m) where
  askDocument = lift askDocument

instance HasDocument m => HasDocument (Strict.StateT r m) where
  askDocument = lift askDocument

newtype Restore m = Restore { restore :: forall a. m a -> IO a }

class Monad m => MonadIORestore m where
  askRestore :: m (Restore m)

instance MonadIORestore m => MonadIORestore (ReaderT r m) where
  askRestore = do
    r <- ask
    parentRestore <- lift askRestore
    return $ Restore $ \a -> restore parentRestore $ runReaderT a r

class (MonadRef h, Ref h ~ Ref m, MonadRef m) => HasPostGui t h m | m -> t h where
  askPostGui :: m (h () -> IO ())
  askRunWithActions :: m ([DSum (EventTrigger t) Identity] -> h ())
  scheduleFollowup :: Ref m (Maybe (EventTrigger t a)) -> a -> m ()

runFrameWithTriggerRef :: (HasPostGui t h m, MonadRef m, MonadIO m) => Ref m (Maybe (EventTrigger t a)) -> a -> m ()
runFrameWithTriggerRef r a = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  liftIO . postGui $ mapM_ (\t -> runWithActions [t :=> Identity a]) =<< readRef r  

runFrameWithTriggerRefNow :: (HasPostGui t m m, MonadRef m, MonadIO m) => Ref m (Maybe (EventTrigger t a)) -> a -> m ()
runFrameWithTriggerRefNow r a = do
  runWithActions <- askRunWithActions
  mapM_ (\t -> runWithActions [t :=> Identity a]) =<< readRef r  

instance HasPostGui t h m => HasPostGui t h (ReaderT r m) where
  askPostGui = lift askPostGui
  askRunWithActions = lift askRunWithActions
  scheduleFollowup r a = lift $ scheduleFollowup r a

instance HasPostGui t h m => HasPostGui t h (StateT s m) where
  askPostGui = lift askPostGui
  askRunWithActions = lift askRunWithActions
  scheduleFollowup r a = lift $ scheduleFollowup r a

instance HasPostGui t h m => HasPostGui t h (Strict.StateT s m) where
  askPostGui = lift askPostGui
  askRunWithActions = lift askRunWithActions
  scheduleFollowup r a = lift $ scheduleFollowup r a

instance MonadWidget t m => MonadWidget t (ReaderT r m) where
  type WidgetHost (ReaderT r m) = WidgetHost m
  type GuiAction (ReaderT r m) = GuiAction m
  type WidgetOutput t (ReaderT r m) = WidgetOutput t m
  askParent = lift askParent
  subWidget n w = do
    r <- ask
    lift $ subWidget n $ runReaderT w r
  subWidgetWithVoidActions n w = do
    r <- ask
    lift $ subWidgetWithVoidActions n $ runReaderT w r
  liftWidgetHost = lift . liftWidgetHost
  schedulePostBuild = lift . schedulePostBuild
  addVoidAction = lift . addVoidAction
  getRunWidget = do
    r <- ask
    runWidget <- lift getRunWidget
    return $ \rootElement w -> do
      (a, postBuild, voidActions) <- runWidget rootElement $ runReaderT w r
      return (a, postBuild, voidActions)
  tellWidgetOutput = lift . tellWidgetOutput

performEvent_ :: MonadWidget t m => Event t (WidgetHost m ()) -> m ()
performEvent_ = addVoidAction

performEvent :: (MonadWidget t m, Ref m ~ Ref IO) => Event t (WidgetHost m a) -> m (Event t a)
performEvent e = do
  (eResult, reResultTrigger) <- newEventWithTriggerRef
  addVoidAction $ ffor e $ \o -> do
    result <- o
    runFrameWithTriggerRef reResultTrigger result
  return eResult

performEventAsync :: forall t m a. MonadWidget t m => Event t ((a -> IO ()) -> WidgetHost m ()) -> m (Event t a)
performEventAsync e = do
  (eResult, reResultTrigger) <- newEventWithTriggerRef
  addVoidAction $ ffor e $ \o -> do
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    o $ \a -> postGui $ mapM_ (\t -> runWithActions [t :=> Identity a]) =<< readRef reResultTrigger
  return eResult

getPostBuild :: MonadWidget t m => m (Event t ())
getPostBuild = do
  (e, trigger) <- newEventWithTriggerRef
  schedulePostBuild $ runFrameWithTriggerRef trigger ()
  return e
