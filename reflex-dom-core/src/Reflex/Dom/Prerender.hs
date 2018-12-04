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
       , PrerenderClientConstraint
       ) where

import Control.Lens ((&), (.~))
import Control.Monad.Reader
import Data.Constraint
import Data.Default
import Data.IORef (modifyIORef', readIORef, newIORef, writeIORef)
import Data.String (IsString)
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Document as Document
import qualified Data.Map as Map
import Foreign.JavaScript.TH
import GHCJS.DOM.Types (MonadJSM)
import Reflex hiding (askEvents)
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.InputDisabled
import Reflex.Dom.Builder.Immediate (GhcjsDomSpace, ImmediateDomBuilderT, SupportsImmediateDomBuilder, insertBefore, deleteBetweenExclusive)
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
  prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m))
  prerenderWrapper :: m (m ())
  default prerenderWrapper :: (MonadTrans t, Monad n, m ~ t n, Prerender js n) => m (m ())
  prerenderWrapper = lift $ lift <$> prerenderWrapper

-- | Draw one widget when prerendering (e.g. server-side) and another when the
-- widget is fully instantiated.  In a given execution of this function, there
-- will be exactly one invocation of exactly one of the arguments.
prerender :: forall js m a. (Prerender js m, Monad m) => m a -> (PrerenderClientConstraint js m => m a) -> m a
prerender server client = do
  finalize <- prerenderWrapper
  a <- case prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)) of
    Nothing -> server
    Just Dict -> client
  finalize
  pure a

instance (PrerenderClientConstraint' js m, ReflexHost t) => Prerender js (ImmediateDomBuilderT t m) where
  prerenderClientDict = Just Dict
  prerenderWrapper = pure (pure ())

instance (PrerenderClientConstraint' js m, SupportsImmediateDomBuilder t m, ReflexHost t, PerformEvent t m) => Prerender js (HydrationDomBuilderT t m) where
  prerenderClientDict = Just Dict
  prerenderWrapper = getHydrationMode >>= \case
    HydrationMode_Immediate -> pure (pure ())
    HydrationMode_Hydrating -> do
      parent <- HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_parent
      hydrationMode <- HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_hydrationMode
      df <- Document.createDocumentFragment =<< askDocument
      initialParent <- liftIO $ readIORef parent
      liftIO $ writeIORef parent $ DOM.toNode df
      liftIO $ writeIORef hydrationMode HydrationMode_Immediate
      pure $ do
        liftIO $ writeIORef parent initialParent
        liftIO $ writeIORef hydrationMode HydrationMode_Hydrating
        addHydrationStep $ do
          -- Delete up to the end marker
          after <- deleteToPrerenderEnd
          insertBefore df after

startMarker, endMarker :: IsString s => s
startMarker = "prerender/start"
endMarker = "prerender/end"

deleteToPrerenderEnd :: (MonadIO m, MonadJSM m) => HydrationRunnerT t m DOM.Element
deleteToPrerenderEnd = do
  depth <- liftIO $ newIORef (0 :: Int)
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
  prerenderClientDict = Nothing
  prerenderWrapper = do
    _ <- element "script" (def & initialAttributes .~ Map.singleton "type" startMarker) $ pure ()
    pure $ void $ element "script" (def & initialAttributes .~ Map.singleton "type" endMarker) $ pure ()

instance (Prerender js m, Monad m, ReflexHost t) => Prerender js (PostBuildT t m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance (Prerender js m, Monad m) => Prerender js (DynamicWriterT t w m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance (Prerender js m, Monad m) => Prerender js (EventWriterT t w m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance (Prerender js m, Monad m) => Prerender js (ReaderT w m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance (Prerender js m, Monad m) => Prerender js (RequesterT t request response m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance (Prerender js m, Monad m) => Prerender js (QueryT t q m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance (Prerender js m, Monad m) => Prerender js (InputDisabledT m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

