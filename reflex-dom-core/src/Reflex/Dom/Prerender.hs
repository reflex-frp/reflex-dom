{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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

import Control.Monad.Reader
import Data.Constraint
import Foreign.JavaScript.TH
import GHCJS.DOM.Types (MonadJSM)
import Reflex
import Reflex.Dom.Builder.InputDisabled
import Reflex.Dom.Builder.Immediate
import Reflex.Dom.Builder.Static
import Reflex.Host.Class

type PrerenderClientConstraint js m =
  ( HasJS js m
  , HasJS js (Performable m)
  , MonadJSM m
  , MonadJSM (Performable m)
  , HasJSContext m
  , HasJSContext (Performable m)
  , MonadFix m
  , MonadFix (Performable m)
  )

class Prerender js m | m -> js where
  prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m))

-- | Draw one widget when prerendering (e.g. server-side) and another when the
-- widget is fully instantiated.  In a given execution of this function, there
-- will be exactly one invocation of exactly one of the arguments.
prerender :: forall js m a. Prerender js m => m a -> (PrerenderClientConstraint js m => m a) -> m a
prerender server client = case prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)) of
  Nothing -> server
  Just Dict -> client

instance ( HasJS js m
         , HasJS js (Performable m)
         , HasJSContext m
         , HasJSContext (Performable m)
         , MonadJSM m
         , MonadJSM (Performable m)
         , MonadFix m
         , MonadFix (Performable m)
         , ReflexHost t
         ) => Prerender js (ImmediateDomBuilderT t m) where
  prerenderClientDict = Just Dict

data NoJavaScript -- This type should never have a HasJS instance

instance js ~ NoJavaScript => Prerender js (StaticDomBuilderT t m) where
  prerenderClientDict = Nothing

instance (Prerender js m, ReflexHost t) => Prerender js (PostBuildT t m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance Prerender js m => Prerender js (DynamicWriterT t w m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance Prerender js m => Prerender js (ReaderT w m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance Prerender js m => Prerender js (RequesterT t request response m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance Prerender js m => Prerender js (QueryT t q m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance Prerender js m => Prerender js (InputDisabledT m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))
