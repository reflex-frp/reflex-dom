{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Render the first widget on the server, and the second on the client.
module Reflex.Dom.Prerender
       ( Prerender (..)
       , prerender_
       , PrerenderClientConstraint
       , PrerenderBaseConstraints
       ) where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef(..))
import Data.IORef (IORef, newIORef)
import Data.Semigroup (Semigroup)
import Data.Text (Text)
import Data.Void
import Foreign.JavaScript.TH
import GHCJS.DOM.Types (MonadJSM)
import Reflex hiding (askEvents)
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Builder.InputDisabled
import Reflex.Dom.Builder.Static
import Reflex.Host.Class
import Reflex.Requester.Base
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Types as DOM

type PrerenderClientConstraint js t m =
  ( DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , DomRenderHook t m
  , HasDocument m
  , TriggerEvent t m
  -- , Prerender js t m
  , PrerenderBaseConstraints js t m
  )

type PrerenderBaseConstraints js t m =
  ( HasJSContext (Performable m)
  , HasJSContext m
  , MonadFix m
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
  , HasJS js m
  , HasJS js (Performable m)
  )

-- | Render the first widget on the server, and the second on the client. The
-- hydration builder will run *both* widgets.
prerender_
  :: (Functor m, Reflex t, Prerender js t m)
  => m () ->  Client m () -> m ()
prerender_ server client = void $ prerender server client

class (PrerenderClientConstraint js t (Client m), Client (Client m) ~ Client m, Prerender js t m) => Prerender js t m | m -> t js where
  -- | Monad in which the client widget is built
  type Client m :: * -> *
  -- | Render the first widget on the server, and the second on the client. The
  -- hydration builder will run *both* widgets, updating the result dynamic at
  -- switchover time.
  prerender :: m a -> Client m a -> m (Dynamic t a)

instance (ReflexHost t, Adjustable t m, PrerenderBaseConstraints js t m) => Prerender js t (HydrationDomBuilderT GhcjsDomSpace t m) where
  type Client (HydrationDomBuilderT GhcjsDomSpace t m) = HydrationDomBuilderT GhcjsDomSpace t m
  prerender _ client = pure <$> client

instance (Adjustable t m, PrerenderBaseConstraints js t m, ReflexHost t) => Prerender js t (HydrationDomBuilderT HydrationDomSpace t m) where
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

newtype UnrunnableT js t m a = UnrunnableT { runUnrunnableT :: Void -> m a }

instance Functor (UnrunnableT js t m) where
  fmap _ _ = UnrunnableT $ \case
instance Applicative (UnrunnableT js t m) where
  pure _ = UnrunnableT $ \case
  _ <*> _ = UnrunnableT $ \case
instance Monad (UnrunnableT js t m) where
  _ >>= _ = UnrunnableT $ \case
instance MonadTrans (UnrunnableT js t) where
  lift _ = UnrunnableT $ \case
instance Reflex t => DomBuilder t (UnrunnableT js t m) where
  type DomBuilderSpace (UnrunnableT js t m) = GhcjsDomSpace
  textNode _ = UnrunnableT $ \case
  commentNode _ = UnrunnableT $ \case
  element _ _ _ = UnrunnableT $ \case
  inputElement _ = UnrunnableT $ \case
  textAreaElement _ = UnrunnableT $ \case
  selectElement _ _ = UnrunnableT $ \case
  placeRawElement _ = UnrunnableT $ \case
  wrapRawElement _ _ = UnrunnableT $ \case
instance NotReady t (UnrunnableT js t m) where
  notReadyUntil _ = UnrunnableT $ \case
  notReady = UnrunnableT $ \case
instance Reflex t => Adjustable t (UnrunnableT js t m) where
  runWithReplace _ _ = UnrunnableT $ \case
  traverseIntMapWithKeyWithAdjust _ _ _ = UnrunnableT $ \case
  traverseDMapWithKeyWithAdjust _ _ _ = UnrunnableT $ \case
  traverseDMapWithKeyWithAdjustWithMove _ _ _ = UnrunnableT $ \case
instance Reflex t => PerformEvent t (UnrunnableT js t m) where
  type Performable (UnrunnableT js t m) = UnrunnableT js t m
  performEvent _ = UnrunnableT $ \case
  performEvent_ _ = UnrunnableT $ \case
instance MonadRef (UnrunnableT js t m) where
  type Ref (UnrunnableT js t m) = Ref IO
  newRef _ = UnrunnableT $ \case
  readRef _ = UnrunnableT $ \case
  writeRef _ _ = UnrunnableT $ \case
instance HasDocument (UnrunnableT js t m) where
  askDocument = UnrunnableT $ \case
instance HasJSContext (UnrunnableT js t m) where
  --type JsContextPhantom (UnrunnableT js t m) = ()
instance HasJS JS' (UnrunnableT js t m) where
  type JSX (UnrunnableT js t m) = UnrunnableT js t m
instance MonadJS JS' (UnrunnableT js t m) where
instance TriggerEvent t (UnrunnableT js t m) where
instance MonadReflexCreateTrigger t (UnrunnableT js t m) where
  newEventWithTrigger _ = UnrunnableT $ \case
instance MonadFix (UnrunnableT js t m) where
  mfix _ = UnrunnableT $ \case
instance MonadHold t (UnrunnableT js t m) where
  hold _ _ = UnrunnableT $ \case
  holdDyn _ _ = UnrunnableT $ \case
  holdIncremental _ _ = UnrunnableT $ \case
instance MonadSample t (UnrunnableT js t m)
instance MonadIO (UnrunnableT js t m)
instance MonadJSM (UnrunnableT js t m) where
  liftJSM' _ = UnrunnableT $ \case
instance Reflex t => PostBuild t (UnrunnableT js t m)
instance PrimMonad (UnrunnableT js t m) where
--  type RawDocument (UnrunnableT js t m) = DOM.Document
--instance HasJSContext (UnrunnableT js t m)
--instance MonadJSM (UnrunnableT js t m)
instance Prerender JS' t (UnrunnableT js t m) where
  type Client (UnrunnableT js t m) = UnrunnableT js t m
  prerender _ _ = UnrunnableT $ \case

instance (SupportsStaticDomBuilder t m) => Prerender JS' t (StaticDomBuilderT t m) where
  type Client (StaticDomBuilderT t m) = UnrunnableT JS' t m
  prerender server _ = do
    _ <- commentNode $ CommentNodeConfig startMarker Nothing
    a <- server
    _ <- commentNode $ CommentNodeConfig endMarker Nothing
    pure $ pure a

instance (Prerender js t m, Monad m) => Prerender js t (ReaderT r m) where
  type Client (ReaderT r m) = ReaderT r (Client m)
  prerender server client = do
    r <- ask
    lift $ prerender (runReaderT server r) (runReaderT client r)

instance (Prerender js t m, Monad m, Reflex t, MonadFix m, Monoid w) => Prerender js t (DynamicWriterT t w m) where
  type Client (DynamicWriterT t w m) = DynamicWriterT t w (Client m)
  prerender server client = do
    x <- lift $ prerender (runDynamicWriterT server) (runDynamicWriterT client)
    let (a, w') = splitDynPure x
        w = join w'
    tellDyn w
    pure a

instance (Prerender js t m, Monad m, Reflex t, Semigroup w) => Prerender js t (EventWriterT t w m) where
  type Client (EventWriterT t w m) = EventWriterT t w (Client m)
  prerender server client = do
    x <- lift $ prerender (runEventWriterT server) (runEventWriterT client)
    let (a, w') = splitDynPure x
        w = switch $ current w'
    tellEvent w
    pure a

instance (Prerender js t m, MonadFix m, Reflex t) => Prerender js t (RequesterT t request response m) where
  type Client (RequesterT t request response m) = RequesterT t request response (Client m)
  prerender server client = mdo
    let fannedResponses = fanInt responses
        withFannedResponses :: forall m' a. Monad m' => RequesterT t request response m' a -> Int -> m' (a, Event t (IntMap (RequesterData request)))
        withFannedResponses w selector = do
          (x, e) <- runRequesterT w (selectInt fannedResponses selector)
          pure (x, fmapCheap (IntMap.singleton selector) e)
    (result, requestsDyn) <- fmap splitDynPure $ lift $ prerender (withFannedResponses server 0) (withFannedResponses client 1)
    responses <- fmap (fmapCheap unMultiEntry) $ requesting' $ fmapCheap multiEntry $ switch $ current requestsDyn
    pure result

instance (Prerender js t m, Monad m, Reflex t, MonadFix m, Group q, Additive q, Query q, Eq q) => Prerender js t (QueryT t q m) where
  type Client (QueryT t q m) = QueryT t q (Client m)
  prerender server client = mdo
    result <- queryDyn query
    x <- lift $ prerender (runQueryT server result) (runQueryT client result)
    let (a, inc) = splitDynPure x
        query = incrementalToDynamic =<< inc -- Can we avoid the incrementalToDynamic?
    pure a

instance (Prerender js t m, Monad m) => Prerender js t (InputDisabledT m) where
  type Client (InputDisabledT m) = Client m
  prerender (InputDisabledT server) client = InputDisabledT $ prerender server client

instance (Prerender js t m, Monad m, ReflexHost t) => Prerender js t (PostBuildT t m) where
  type Client (PostBuildT t m) = PostBuildT t (Client m)
  prerender server client = PostBuildT $ do
    pb <- ask
    lift $ prerender (runPostBuildT server pb) (runPostBuildT client pb)

startMarker, endMarker :: Text
startMarker = "prerender/start"
endMarker = "prerender/end"

deleteToPrerenderEnd :: (MonadIO m, MonadJSM m, Reflex t, MonadFix m) => DOM.Document -> HydrationRunnerT t m DOM.Comment
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
