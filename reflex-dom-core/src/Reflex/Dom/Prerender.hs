{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
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
module Reflex.Dom.Prerender
       ( Prerender (..)
       , prerender
       ) where

import Control.Lens ((&), (.~))
import Control.Monad.Reader
import Control.Monad.Ref (Ref, MonadRef)
import Data.Bifunctor (bimap)
import Data.Constraint
import Data.Default
import Data.IORef (modifyIORef', readIORef, newIORef)
import Data.String (IsString)
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Document as Document
import qualified Data.Map as Map
import Foreign.JavaScript.TH
import GHC.IORef (IORef)
import GHCJS.DOM.Types (MonadJSM)
import Reflex hiding (askEvents)
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.InputDisabled
import Reflex.Dom.Builder.Immediate (GhcjsDomSpace, ImmediateDomBuilderT, runImmediateDomBuilderT, ImmediateDomBuilderEnv(..), SupportsImmediateDomBuilder, insertBefore, deleteBetweenExclusive)
import Reflex.Dom.Builder.Hydration
import Reflex.Dom.Builder.Static
import Reflex.Host.Class

type PrerenderClientConstraint' js m =
  ( HasJS js m
  , HasJS js (Performable m)
  , MonadJSM m
  , MonadJSM (Performable m)
  , HasJSContext m
  , HasJSContext (Performable m)
  , MonadFix m
  , MonadFix (Performable m)
  )

type PrerenderClientConstraint js m =
  ( PrerenderClientConstraint' js m
  , DomBuilderSpace m ~ GhcjsDomSpace
  )

class Prerender js m | m -> js where
  type PrerenderServer m :: * -> *
  type PrerenderClient m :: * -> *
  prerenderClientDict :: Either (PrerenderServer m a -> m a) (Dict (PrerenderClientConstraint js (PrerenderClient m)), PrerenderClient m a -> m a)
  default prerenderClientDict :: forall n t a. (Prerender js n, MonadTrans t, m ~ t n, Monad n, Prerender js n, PrerenderServer m ~ PrerenderServer n, PrerenderClient m ~ PrerenderClient n)
                              => Either (PrerenderServer m a -> m a) (Dict (PrerenderClientConstraint js (PrerenderClient m)), PrerenderClient m a -> m a)
  prerenderClientDict = bimap (lift .) (\(Dict, f) -> (Dict, lift . f)) (prerenderClientDict :: Either (PrerenderServer n a -> n a) (Dict (PrerenderClientConstraint js (PrerenderClient n)), PrerenderClient n a -> n a))

-- | Draw one widget when prerendering (e.g. server-side) and another when the
-- widget is fully instantiated.  In a given execution of this function, there
-- will be exactly one invocation of exactly one of the arguments.
prerender :: forall js m a. Prerender js m => PrerenderServer m a -> (PrerenderClientConstraint js (PrerenderClient m) => PrerenderClient m a) -> m a
prerender server client = case prerenderClientDict of
  Left f -> f server
  Right (Dict, f) -> f client

instance (PrerenderClientConstraint' js m, ReflexHost t) => Prerender js (ImmediateDomBuilderT t m) where
  type PrerenderServer (ImmediateDomBuilderT t m) = ImmediateDomBuilderT t m
  type PrerenderClient (ImmediateDomBuilderT t m) = ImmediateDomBuilderT t m
  prerenderClientDict = Right (Dict, id)

instance (PrerenderClientConstraint' js m, SupportsImmediateDomBuilder t m, ReflexHost t, PerformEvent t m) => Prerender js (HydrationDomBuilderT t m) where
  type PrerenderServer (HydrationDomBuilderT t m) = HydrationDomBuilderT t m
  type PrerenderClient (HydrationDomBuilderT t m) = ImmediateDomBuilderT t m
  prerenderClientDict = Right $ (,) Dict $ \client -> do
    doc <- askDocument
    unreadyChildren <- liftIO $ newIORef 0
    df <- Document.createDocumentFragment doc
    let env = ImmediateDomBuilderEnv
          { _immediateDomBuilderEnv_document = doc
          , _immediateDomBuilderEnv_parent = DOM.toNode df
          , _immediateDomBuilderEnv_unreadyChildren = unreadyChildren
          , _immediateDomBuilderEnv_commitAction = pure ()
          }
    events <- askEvents
    a <- lift $ runImmediateDomBuilderT client env events
    addHydrationStep $ do
      -- Delete up to the end marker
      after <- deleteToPrerenderEnd
      insertBefore df after
    pure a

startMarker, endMarker :: IsString s => s
startMarker = "prerender/start"
endMarker = "prerender/end"

deleteToPrerenderEnd :: (MonadIO m, MonadJSM m) => HydrationRunnerT t m DOM.Element
deleteToPrerenderEnd = do
  depth <- liftIO $ newIORef 0
  ref <- getPreviousNode
  startNode <- hydrateNode (\e -> do
    n :: DOM.JSString <- Element.getTagName e
    mt :: Maybe DOM.JSString <- Element.getAttribute e ("type" :: DOM.JSString)
    pure $ n == "SCRIPT" && mt == Just startMarker) DOM.Element
  endNode <- hydrateNode (\e -> do
    n :: DOM.JSString <- Element.getTagName e
    attrCheck <- Element.getAttribute e ("type" :: DOM.JSString) >>= \case
      Just (t :: DOM.JSString)
        | t == startMarker -> do
          liftIO $ modifyIORef' depth succ
          pure False
        | t == endMarker -> do
          d <- liftIO $ readIORef depth
          if d == 0
            then pure True
            else do
              liftIO $ modifyIORef' depth pred
              pure False
      _ -> pure False
    pure $ n == "SCRIPT" && attrCheck) DOM.Element
  deleteBetweenExclusive startNode endNode
  setPreviousNode $ Just $ DOM.toNode endNode -- TODO probably not necessary
  pure endNode

data NoJavascript -- This type should never have a HasJS instance

instance (js ~ NoJavascript, SupportsStaticDomBuilder t m) => Prerender js (StaticDomBuilderT t m) where
  type PrerenderServer (StaticDomBuilderT t m) = StaticDomBuilderT t m
  type PrerenderClient (StaticDomBuilderT t m) = StaticDomBuilderT t m
  prerenderClientDict = Left $ \server -> do
    -- We must put markers around the prerendered parts because we don't
    -- run the 'server' widget on the frontend: thus we have no idea how many
    -- nodes should be replaced when we are hydrating
    _ <- element "script" (def & initialAttributes .~ Map.singleton "type" startMarker) $ pure ()
    a <- server
    _ <- element "script" (def & initialAttributes .~ Map.singleton "type" endMarker) $ pure ()
    pure a


instance (Monad m, Prerender js m, ReflexHost t) => Prerender js (PostBuildT t m) where
  type PrerenderServer (PostBuildT t m) = PrerenderServer m
  type PrerenderClient (PostBuildT t m) = PrerenderClient m

instance (Monad m, Prerender js m) => Prerender js (DynamicWriterT t w m) where
  type PrerenderServer (DynamicWriterT t w m) = PrerenderServer m
  type PrerenderClient (DynamicWriterT t w m) = PrerenderClient m

instance (Monad m, Prerender js m) => Prerender js (EventWriterT t w m) where
  type PrerenderServer (EventWriterT t w m) = PrerenderServer m
  type PrerenderClient (EventWriterT t w m) = PrerenderClient m

instance (Monad m, Prerender js m) => Prerender js (ReaderT w m) where
  type PrerenderServer (ReaderT w m) = PrerenderServer m
  type PrerenderClient (ReaderT w m) = PrerenderClient m

instance (Monad m, Prerender js m) => Prerender js (RequesterT t request response m) where
  type PrerenderServer (RequesterT t request response m) = PrerenderServer m
  type PrerenderClient (RequesterT t request response m) = PrerenderClient m

instance (Monad m, Prerender js m) => Prerender js (QueryT t q m) where
  type PrerenderServer (QueryT t q m) = PrerenderServer m
  type PrerenderClient (QueryT t q m) = PrerenderClient m

instance (Monad m, Prerender js m) => Prerender js (InputDisabledT m) where
  type PrerenderServer (InputDisabledT m) = PrerenderServer m
  type PrerenderClient (InputDisabledT m) = PrerenderClient m

