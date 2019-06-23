{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Builder.Template where

import Control.Applicative
import Control.Lens (imap, iforM)
import Control.Monad.Reader
import Control.Monad.Fix
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Reflex.Class
import Data.Align
import Data.These

data IntMapTimeline t

{-
data DefIntMap a = DefIntMap a !(IntMap a)
  deriving (Functor)

instance Applicative DefIntMap where
  pure x = DefIntMap x IntMap.empty
  DefIntMap df mf <*> DefIntMap dx mx = DefIntMap (df dx) $ alignWith g mf mx
    where g = \case
            This f -> f dx
            That x -> df x
            These f x -> f x

instance Monad DefIntMap where
  DefIntMap dx mx >>= f =
    let DefIntMap dfdx mfdx = f dx
        mfxs = flip imap mx $ \k x ->
          let DefIntMap dfmx mfmx = f x
          in IntMap.findWithDefault dfmx k mfmx
    in DefIntMap dfdx $ IntMap.intersection mfxs mfdx

newtype IntMapPushM t a = IntMapPushM { unIntMapPushM :: PushM t (DefIntMap a) }

instance Functor (PushM t) => Functor (IntMapPushM t) where
  fmap f = IntMapPushM . fmap (fmap f) . unIntMapPushM

instance Applicative (PushM t) => Applicative (IntMapPushM t) where
  pure = IntMapPushM . pure . pure
  IntMapPushM f <*> IntMapPushM x = IntMapPushM $ liftA2 (<*>) f x

instance Monad (PushM t) => Monad (IntMapPushM t) where
  IntMapPushM xx >>= f = IntMapPushM $ do
    DefIntMap dx mx <- xx
    DefIntMap dfdx mfdx <- unIntMapPushM $ f dx
    mfxs <- iforM mx $ \k x -> do
      DefIntMap dfmx mfmx <- unIntMapPushM $ f x
      pure $ IntMap.findWithDefault dfmx k mfmx
    pure $ DefIntMap dfdx $ IntMap.intersection mfxs mfdx

--Problem: need IntMap to be lazy
instance MonadFix (PushM t) => MonadFix (IntMapPushM t) where
  mfix f = IntMapPushM $ mfix $ \a -> do
    unIntMapPushM $ f a
-}

newtype IntMapPushM t a = IntMapPushM { unIntMapPushM :: ReaderT Int (PushM t) a }

deriving instance Functor (PushM t) => Functor (IntMapPushM t)
deriving instance Applicative (PushM t) => Applicative (IntMapPushM t)
deriving instance Monad (PushM t) => Monad (IntMapPushM t)
deriving instance MonadFix (PushM t) => MonadFix (IntMapPushM t)

newtype IntMapPullM t a = IntMapPullM { unIntMapPullM :: ReaderT Int (PullM t) a }

deriving instance Functor (PullM t) => Functor (IntMapPullM t)
deriving instance Applicative (PullM t) => Applicative (IntMapPullM t)
deriving instance Monad (PullM t) => Monad (IntMapPullM t)
deriving instance MonadFix (PullM t) => MonadFix (IntMapPullM t)

instance Reflex t => Reflex (IntMapTimeline t) where
  newtype Event (IntMapTimeline t) a = IntMapEvent { unIntMapEvent :: Event t (IntMap a) }
  newtype Behavior (IntMapTimeline t) a = IntMapBehavior { unIntMapBehavior :: Behavior t (ReaderT Int (PullM t) a) } --TODO: Fix leak
  newtype Dynamic (IntMapTimeline t) a = IntMapDynamic { unIntMapDynamic :: Dynamic t (Int -> a) }
  newtype Incremental (IntMapTimeline t) a = IntMapIncremental { unIntMapIncremental :: Incremental t (Int -> a) }
  type PushM (IntMapTimeline t) = IntMapPushM t
  type PullM (IntMapTimeline t) = IntMapPullM t
  never = IntMapEvent never
  constant = pure
  push f (IntMapEvent e) = IntMapEvent $ push g e
    where g = \occs -> do
            mouts <- iforM occs $ \k v -> runReaderT (unIntMapPushM $ f v) k
            let outs = IntMap.mapMaybe id mouts
            pure $ if IntMap.null outs
              then Nothing
              else Just outs
  pushCheap = push --TODO
  pull (IntMapPullM a) = IntMapBehavior $ pull $ pure a

instance Functor (Dynamic (IntMapTimeline t))
instance Applicative (Dynamic (IntMapTimeline t))
instance Monad (Dynamic (IntMapTimeline t))

instance (Reflex t, MonadSample t (PushM t)) => MonadHold (IntMapTimeline t) (IntMapPushM t) where
  hold v0 (IntMapEvent v') = IntMapPushM $ do
    IntMapBehavior <$> accum (\old new -> ReaderT $ \k -> maybe (runReaderT old k) id $ pure <$> IntMap.lookup k new) (pure v0) v'
  holdDyn v0 (IntMapEvent v') = IntMapPushM $ do
    IntMapDynamic <$> accum (\old new k -> maybe (old k) id $ IntMap.lookup k new) (\_ -> v0) v'
  holdIncremental v0 = undefined
  buildDynamic = undefined
  headE = undefined

--TODO: Make this a primitive
pullToPush :: Reflex t => PullM t a -> PushM t a
pullToPush = sample . pull

instance (Reflex t, MonadSample t (PushM t)) => MonadSample (IntMapTimeline t) (IntMapPushM t) where
  sample (IntMapBehavior b) = IntMapPushM $ do
    k <- ask
    v <- lift $ sample b
    lift $ pullToPush $ runReaderT v k

instance MonadSample t (PullM t) => MonadSample (IntMapTimeline t) (IntMapPullM t) where
  sample (IntMapBehavior b) = IntMapPullM $ do
    k <- ask
    v <- lift $ sample b
    lift $ runReaderT v k
