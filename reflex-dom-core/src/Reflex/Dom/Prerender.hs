{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Render the first widget on the server, and the second on the client.
module Reflex.Dom.Prerender
       ( Prerender (..)
       , prerender
       , prerender_
       , PrerenderClientConstraint
       , PrerenderBaseConstraints
       ) where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef)
import Data.Text (Text)
import Foreign.JavaScript.TH
import GHCJS.DOM.Types (MonadJSM)
import Reflex hiding (askEvents)
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Builder.InputDisabled
import Reflex.Dom.Builder.Static
import Reflex.Host.Class

import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Types as DOM

type PrerenderClientConstraint t m =
  ( DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , HasDocument m
  , TriggerEvent t m
  , Prerender t m
  , PrerenderBaseConstraints t m
  )

type PrerenderBaseConstraints t m =
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
  )

-- | Render the first widget on the server, and the second on the client. The
-- hydration builder will run *both* widgets, updating the result dynamic at
-- switchover time.
prerender
  :: (Functor m, Prerender t m)
  => m a -> ((PrerenderClientConstraint t (Client m)) => Client m a) -> m (Dynamic t a)
prerender server client = snd <$> prerenderImpl ((,) () <$> server) client

-- | Render the first widget on the server, and the second on the client. The
-- hydration builder will run *both* widgets.
prerender_
  :: (Functor m, Reflex t, Prerender t m)
  => m () -> ((PrerenderClientConstraint t (Client m)) => Client m ()) -> m ()
prerender_ server client = void $ prerenderImpl ((,) () <$> server) client

class Prerender t m | m -> t where
  -- | Monad in which the client widget is built
  type Client m :: * -> *
  -- | This function should be considered internal: it should only be used where
  -- the resultant 'Maybe' isn't used to change what is built in the DOM, or
  -- hydration will fail.
  prerenderImpl :: m (a, b) -> ((PrerenderClientConstraint t (Client m)) => Client m b) -> m (Maybe a, Dynamic t b)

instance (Adjustable t m, PrerenderBaseConstraints t m) => Prerender t (HydrationDomBuilderT GhcjsDomSpace t m) where
  type Client (HydrationDomBuilderT GhcjsDomSpace t m) = HydrationDomBuilderT GhcjsDomSpace t m
  prerenderImpl _ client = (,) Nothing . pure <$> client

instance (Adjustable t m, PrerenderBaseConstraints t m, ReflexHost t) => Prerender t (HydrationDomBuilderT HydrationDomSpace t m) where
  -- | PostBuildT is needed here because we delay running the client builder
  -- until after switchover, at which point the postBuild of @m@ has already fired
  type Client (HydrationDomBuilderT HydrationDomSpace t m) = PostBuildT t (HydrationDomBuilderT GhcjsDomSpace t m) -- PostBuildT t (ImmediateDomBuilderT t m)
  -- | Runs the server widget up until switchover, then replaces it with the
  -- client widget.
  prerenderImpl server client = do
    env <- HydrationDomBuilderT ask
    events <- askEvents
    doc <- askDocument
    df <- Document.createDocumentFragment doc
    unreadyChildren <- HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_unreadyChildren
    hydrationMode <- liftIO $ newIORef HydrationMode_Immediate
    delayed <- liftIO $ newIORef $ pure ()
    let env' = HydrationDomBuilderEnv
          { _hydrationDomBuilderEnv_document = doc
          , _hydrationDomBuilderEnv_parent = Left $ DOM.toNode df
          , _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
          , _hydrationDomBuilderEnv_commitAction = pure ()
          , _hydrationDomBuilderEnv_delayed = delayed
          , _hydrationDomBuilderEnv_hydrationMode = hydrationMode
          , _hydrationDomBuilderEnv_switchover = never
          }
    (a, b0) <- lift $ runHydrationDomBuilderT server (env { _hydrationDomBuilderEnv_delayed = delayed }) events
    (b', trigger) <- newTriggerEvent
    getHydrationMode >>= \case
      HydrationMode_Immediate -> do
        liftIO . trigger <=< lift $ runHydrationDomBuilderT (runPostBuildT client $ void b') env' events
        append $ DOM.toNode df
      HydrationMode_Hydrating -> addHydrationStep $ do
        liftIO . trigger <=< lift $ runHydrationDomBuilderT (runPostBuildT client $ void b') env' events
        insertBefore df =<< deleteToPrerenderEnd doc
    (,) (Just a) <$> holdDyn b0 b'

instance SupportsStaticDomBuilder t m => Prerender t (StaticDomBuilderT t m) where
  type Client (StaticDomBuilderT t m) = HydrationDomBuilderT GhcjsDomSpace t m
  prerenderImpl server _ = do
    _ <- commentNode $ CommentNodeConfig startMarker Nothing
    (a, b) <- server
    _ <- commentNode $ CommentNodeConfig endMarker Nothing
    pure (Just a, pure b)

instance (Prerender t m, Monad m) => Prerender t (ReaderT r m) where
  type Client (ReaderT r m) = Client m
  prerenderImpl server client = do
    r <- ask
    lift $ prerenderImpl (runReaderT server r) client

instance (Prerender t m, Monad m, Functor (Client m)) => Prerender t (StateT s m) where
  type Client (StateT s m) = Client m
  prerenderImpl server client = do
    s <- get
    (ma, evt) <- lift $ prerenderImpl
      (do ((a, b), s') <- runStateT server s; pure ((a, s'), b))
      client
    traverse_ (put . snd) ma
    pure (fst <$> ma, evt)

instance (Prerender t m, Monad m, Functor (Client m)) => Prerender t (DynamicWriterT t w m) where
  type Client (DynamicWriterT t w m) = Client m
  prerenderImpl (DynamicWriterT server) client = DynamicWriterT $ prerenderImpl server client

instance (Prerender t m, Monad m, Functor (Client m)) => Prerender t (EventWriterT t w m) where
  type Client (EventWriterT t w m) = Client m
  prerenderImpl (EventWriterT server) client = EventWriterT $ prerenderImpl server client

instance (Prerender t m, Monad m, Functor (Client m)) => Prerender t (RequesterT t request response m) where
  type Client (RequesterT t request response m) = Client m
  prerenderImpl (RequesterT server) client = RequesterT $ prerenderImpl server client

instance (Prerender t m, Monad m, Functor (Client m)) => Prerender t (QueryT t q m) where
  type Client (QueryT t q m) = Client m
  prerenderImpl (QueryT server) client = QueryT $ prerenderImpl server client

instance (Prerender t m, Monad m) => Prerender t (InputDisabledT m) where
  type Client (InputDisabledT m) = Client m
  prerenderImpl (InputDisabledT server) client = InputDisabledT $ prerenderImpl server client

instance (Prerender t m, Monad m) => Prerender t (PostBuildT t m) where
  type Client (PostBuildT t m) = Client m
  prerenderImpl (PostBuildT server) client = PostBuildT $ prerenderImpl server client

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
