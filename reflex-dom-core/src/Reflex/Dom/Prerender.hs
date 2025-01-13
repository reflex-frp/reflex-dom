{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Render the first widget on the server, and the second on the client.
module Reflex.Dom.Prerender
       ( Prerender (..)
       , prerender_
       , PrerenderClientConstraint
       , PrerenderBaseConstraints
       ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef(..), MonadAtomicRef(..))
import Data.IORef (IORef, newIORef)
import Data.Kind (Type)
import Data.Semigroup.Commutative
import Data.Text (Text)
import Data.Void
import GHCJS.DOM.Types (MonadJSM)
import Reflex hiding (askEvents)
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Hydratable
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Builder.InputDisabled
import Reflex.Dom.Builder.Static
import Reflex.Host.Class
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Types as DOM

type PrerenderClientConstraint t m =
  ( DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , DomRenderHook t m
  , HasDocument m
  , TriggerEvent t m
  , PrerenderBaseConstraints t m
  )

type PrerenderBaseConstraints t m =
  ( MonadFix m
  , MonadHold t m
  , MonadJSM (Performable m)
  , MonadJSM m
  , MonadRef (Performable m)
  , MonadRef m
  , MonadReflexCreateTrigger t m
  , MonadSample t (Performable m)
  , PerformEvent t m
  , PostBuild t m
  , PrimMonad m
  , Ref (Performable m) ~ IORef
  , Ref m ~ IORef
  )

-- | Render the first widget on the server, and the second on the client. The
-- hydration builder will run *both* widgets.
prerender_
  :: (Functor m, Prerender t m)
  => m () ->  Client m () -> m ()
prerender_ server client = void $ prerender server client

class (PrerenderClientConstraint t (Client m), Client (Client m) ~ Client m, Prerender t (Client m)) => Prerender t m | m -> t where
  -- | Monad in which the client widget is built
  type Client m :: Type -> Type
  -- | Render the first widget on the server, and the second on the client. The
  -- hydration builder will run *both* widgets, updating the result dynamic at
  -- switchover time.
  prerender :: m a -> Client m a -> m (Dynamic t a)

instance (ReflexHost t, Adjustable t m, PrerenderBaseConstraints t m) => Prerender t (HydrationDomBuilderT GhcjsDomSpace t m) where
  type Client (HydrationDomBuilderT GhcjsDomSpace t m) = HydrationDomBuilderT GhcjsDomSpace t m
  prerender _ client = pure <$> client

instance (Adjustable t m, PrerenderBaseConstraints t m, ReflexHost t) => Prerender t (HydrationDomBuilderT HydrationDomSpace t m) where
  -- | PostBuildT is needed here because we delay running the client builder
  -- until after switchover, at which point the postBuild of @m@ has already fired
  type Client (HydrationDomBuilderT HydrationDomSpace t m) = PostBuildT t (HydrationDomBuilderT GhcjsDomSpace t m)
  -- | Runs the server widget up until switchover, then replaces it with the
  -- client widget.
  prerender server client = do
    env <- HydrationDomBuilderT ask
    events <- askEvents
    doc <- askDocument
    serverDf <- Document.createDocumentFragment doc -- server dom should not be mounted in the window's doc in hydration
    df <- Document.createDocumentFragment doc
    unreadyChildren <- HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_unreadyChildren
    immediateMode <- liftIO $ newIORef HydrationMode_Immediate
    delayed <- liftIO $ newIORef $ pure ()
    let clientEnv = env
          { _hydrationDomBuilderEnv_parent = Left $ DOM.toNode df
          , _hydrationDomBuilderEnv_hydrationMode = immediateMode
          }
        serverEnv = HydrationDomBuilderEnv
          { _hydrationDomBuilderEnv_document = doc
          , _hydrationDomBuilderEnv_parent = Left $ DOM.toNode serverDf
          , _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
          , _hydrationDomBuilderEnv_commitAction = pure ()
          , _hydrationDomBuilderEnv_delayed = delayed
          , _hydrationDomBuilderEnv_hydrationMode = immediateMode
          , _hydrationDomBuilderEnv_switchover = never
          }
    a0 <- lift $ runHydrationDomBuilderT server serverEnv events
    (a', trigger) <- newTriggerEvent
    getHydrationMode >>= \case
      HydrationMode_Immediate -> do
        liftIO . trigger <=< lift $ runHydrationDomBuilderT (runPostBuildT client $ void a') clientEnv events
        append $ DOM.toNode df
      HydrationMode_Hydrating -> addHydrationStep $ do
        liftIO . trigger <=< lift $ runHydrationDomBuilderT (runPostBuildT client $ void a') clientEnv events
        insertBefore df =<< deleteToPrerenderEnd doc
    holdDyn a0 a'

newtype UnrunnableT t m a = UnrunnableT (ReaderT Void m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

unrunnable :: UnrunnableT t m a
unrunnable = UnrunnableT $ ReaderT $ \case {}

instance (Reflex t, Monad m) => DomBuilder t (UnrunnableT t m) where
  type DomBuilderSpace (UnrunnableT t m) = GhcjsDomSpace
  textNode _ = unrunnable
  commentNode _ = unrunnable
  element _ _ _ = unrunnable
  inputElement _ = unrunnable
  textAreaElement _ = unrunnable
  selectElement _ _ = unrunnable
  placeRawElement _ = unrunnable
  wrapRawElement _ _ = unrunnable
instance (Reflex t, Monad m) => NotReady t (UnrunnableT t m) where
  notReadyUntil _ = unrunnable
  notReady = unrunnable
instance (Reflex t, Monad m) => Adjustable t (UnrunnableT t m) where
  runWithReplace _ _ = unrunnable
  traverseIntMapWithKeyWithAdjust _ _ _ = unrunnable
  traverseDMapWithKeyWithAdjust _ _ _ = unrunnable
  traverseDMapWithKeyWithAdjustWithMove _ _ _ = unrunnable
instance (Reflex t, Monad m) => PerformEvent t (UnrunnableT t m) where
  type Performable (UnrunnableT t m) = UnrunnableT t m
  performEvent _ = unrunnable
  performEvent_ _ = unrunnable
instance Monad m => MonadRef (UnrunnableT t m) where
  type Ref (UnrunnableT t m) = Ref IO
  newRef _ = unrunnable
  readRef _ = unrunnable
  writeRef _ _ = unrunnable
instance Monad m => MonadAtomicRef (UnrunnableT t m) where
  atomicModifyRef _ _ = unrunnable
instance Monad m => HasDocument (UnrunnableT t m) where
  askDocument = unrunnable
instance Monad m => TriggerEvent t (UnrunnableT t m) where
  newTriggerEvent = unrunnable
  newTriggerEventWithOnComplete = unrunnable
  newEventWithLazyTriggerWithOnComplete _ = unrunnable
instance Monad m => MonadReflexCreateTrigger t (UnrunnableT t m) where
  newEventWithTrigger _ = unrunnable
  newFanEventWithTrigger _ = unrunnable
instance Monad m => MonadFix (UnrunnableT t m) where
  mfix _ = unrunnable
instance (Monad m, MonadHold t m) => MonadHold t (UnrunnableT t m) where
  hold _ _ = unrunnable
  holdDyn _ _ = unrunnable
  holdIncremental _ _ = unrunnable
  buildDynamic _ _ = unrunnable
  headE _ = unrunnable
  now = unrunnable
instance Monad m => MonadSample t (UnrunnableT t m) where
  sample _ = unrunnable
instance Monad m => MonadIO (UnrunnableT t m) where
  liftIO _ = unrunnable
#ifndef ghcjs_HOST_OS
instance Monad m => MonadJSM (UnrunnableT t m) where
  liftJSM' _ = unrunnable
#endif
instance (Reflex t, Monad m) => PostBuild t (UnrunnableT t m) where
  getPostBuild = unrunnable
instance Monad m => PrimMonad (UnrunnableT t m) where
  type PrimState (UnrunnableT t m) = PrimState IO
  primitive _ = unrunnable
instance (Reflex t, Monad m) => DomRenderHook t (UnrunnableT t m) where
  withRenderHook _ _ = unrunnable
  requestDomAction _ = unrunnable
  requestDomAction_ _ = unrunnable
instance (Reflex t, Monad m, MonadHold t m) => Prerender t (UnrunnableT t m) where
  type Client (UnrunnableT t m) = UnrunnableT t m
  prerender _ _ = unrunnable

instance (SupportsStaticDomBuilder t m) => Prerender t (StaticDomBuilderT t m) where
  type Client (StaticDomBuilderT t m) = UnrunnableT t m
  prerender server _ = do
    _ <- commentNode $ CommentNodeConfig startMarker Nothing
    a <- server
    _ <- commentNode $ CommentNodeConfig endMarker Nothing
    pure $ pure a

instance (Prerender t m, Monad m) => Prerender t (ReaderT r m) where
  type Client (ReaderT r m) = ReaderT r (Client m)
  prerender server client = do
    r <- ask
    lift $ prerender (runReaderT server r) (runReaderT client r)

instance (Prerender t m, Monad m, MonadFix m, Reflex t, Monoid w) => Prerender t (DynamicWriterT t w m) where
  type Client (DynamicWriterT t w m) = DynamicWriterT t w (Client m)
  prerender server client = do
    x <- lift $ prerender (runDynamicWriterT server) (runDynamicWriterT client)
    let (a, w') = splitDynPure x
        w = join w'
    tellDyn w
    pure a

instance (Prerender t m, Monad m, Reflex t, Semigroup w) => Prerender t (EventWriterT t w m) where
  type Client (EventWriterT t w m) = EventWriterT t w (Client m)
  prerender server client = do
    x <- lift $ prerender (runEventWriterT server) (runEventWriterT client)
    let (a, w') = splitDynPure x
        w = switch $ current w'
    tellEvent w
    pure a

instance (Prerender t m, MonadFix m, Reflex t) => Prerender t (RequesterT t request response m) where
  type Client (RequesterT t request response m) = RequesterT t request response (Client m)
  prerender server client = mdo
    let fannedResponses = fanInt responses
        withFannedResponses :: forall m' a. Monad m' => RequesterT t request response m' a -> Int -> m' (a, Event t (IntMap (RequesterData request)))
        withFannedResponses w selector = do
          (x, e) <- runRequesterT w (selectInt fannedResponses selector)
          pure (x, fmapCheap (IntMap.singleton selector) e)
    (result, requestsDyn) <- fmap splitDynPure $ lift $ prerender (withFannedResponses server 0) (withFannedResponses client 1)
    responses <- fmap (fmapCheap unMultiEntry) $ requesting' $ fmapCheap multiEntry $ switchPromptlyDyn requestsDyn
    return result

instance (Prerender t m, Monad m, Reflex t, MonadFix m, Group q, Commutative q, Query q, Eq q) => Prerender t (QueryT t q m) where
  type Client (QueryT t q m) = QueryT t q (Client m)
  prerender server client = mdo
    result <- queryDyn query
    x <- lift $ prerender (runQueryT server result) (runQueryT client result)
    let (a, inc) = splitDynPure x
        query = incrementalToDynamic =<< inc -- Can we avoid the incrementalToDynamic?
    pure a

instance Prerender t m => Prerender t (InputDisabledT m) where
  type Client (InputDisabledT m) = InputDisabledT (Client m)
  prerender (InputDisabledT server) (InputDisabledT client) = InputDisabledT $ prerender server client

instance Prerender t m => Prerender t (HydratableT m) where
  type Client (HydratableT m) = HydratableT (Client m)
  prerender (HydratableT server) (HydratableT client) = HydratableT $ prerender server client

instance (Prerender t m, Monad m, ReflexHost t) => Prerender t (PostBuildT t m) where
  type Client (PostBuildT t m) = PostBuildT t (Client m)
  prerender server client = PostBuildT $ do
    pb <- ask
    lift $ prerender (runPostBuildT server pb) (runPostBuildT client pb)

startMarker, endMarker :: Text
startMarker = "prerender/start"
endMarker = "prerender/end"

deleteToPrerenderEnd :: (MonadJSM m, Reflex t, MonadFix m) => DOM.Document -> HydrationRunnerT t m DOM.Comment
deleteToPrerenderEnd doc = do
  startNode <- hydrateComment doc startMarker Nothing
  let go (n :: Int) lastNode = Node.getNextSibling lastNode >>= \case
        Nothing -> do
          c <- Document.createComment doc endMarker
          insertAfterPreviousNode c
          pure c
        Just node -> DOM.castTo DOM.Comment node >>= \case
          Nothing -> go n node
          Just c -> Node.getTextContentUnchecked c >>= \case
            t | t == startMarker -> go (succ n) node
              | t == endMarker -> case n of
                0 -> pure c
                _ -> go (pred n) node
              | otherwise -> go n node
  endNode <- go 0 $ DOM.toNode startNode
  deleteBetweenExclusive startNode endNode
  setPreviousNode $ Just $ DOM.toNode endNode
  pure endNode
