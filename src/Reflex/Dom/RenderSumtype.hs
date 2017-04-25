{-# LANGUAGE
  DataKinds
, DeriveGeneric
, GADTs
, KindSignatures
, LambdaCase
, RankNTypes
, ScopedTypeVariables
, TypeOperators
#-}

module Reflex.Dom.RenderSumtype
  ( GTag, renderSumType )

where

import Data.Dependent.Sum
import Data.GADT.Compare
import Data.Maybe (isJust)
import Generics.SOP

import Reflex hiding (tag)
import Reflex.Dom.Widget.Basic
import Reflex.Dom.Old

type GTag t = GTag_ (Code t)
newtype GTag_ t (xs :: [*]) = GTag (NS ((:~:) xs) t)

instance GEq (GTag_ t) where
  geq (GTag (Z Refl)) (GTag (Z Refl)) = Just Refl
  geq (GTag (S x))    (GTag (S y))    = GTag x `geq` GTag y
  geq _               _               = Nothing

toDSum :: Generic t => t -> DSum (GTag t) (NP I)
toDSum = foo (\f b -> GTag f :=> b) . unSOP . from
  where
    foo :: (forall a . (NS ((:~:) a) xs) -> NP I a -> r)
        -> NS (NP I) xs
        -> r
    foo k (Z x) =     (k . Z) Refl x
    foo k (S w) = foo (k . S)      w

data WrapDyn t m a = WrapDyn (m (Dynamic t (NP I a)))

-- TODO consider using 'fan', which may bring a perf benefit
-- | Efficiently render a Dynamic containing a sum type.
--   As inputs, this takes the Dynamic to render, as well as a
--   renderAnything function which is capable of rendering
--   from a Dynamic containing any of the constructors of the
--   sum type. In contrast to dyn/widgetHold, rendering of the
--   constructors is done from a Dynamic, which avoids a full
--   re-render on every update.
--
--   In this extended example, the user provides an arbitrary datatype,
--   UsersSumType, as well as the function renderUsersSumType.
-- > data SubState2
-- > data SubState3a
-- > data SubState3b
-- > data SubState3c
-- >
-- > data UsersSumType
-- >   = First
-- >   | Second SubState2
-- >   | Third SubState3a SubState3b SubState3c
-- >   deriving GHC.Generic
-- >
-- > instance Generic UsersSumType
-- >
-- > data UsersEventType
-- >
-- > renderFirst
-- >   :: forall t m. MonadWidget t m
-- >   => Dynamic t (NP I '[])
-- >   -> m (Event t UsersEventType)
-- > renderFirst d = undefined
-- >
-- > renderSecond
-- >   :: forall t m. MonadWidget t m
-- >   => Dynamic t (NP I '[SubState2])
-- >   -> m (Event t UsersEventType)
-- > renderSecond d =
-- >   let dynState = ((\(I x :* Nil) -> x) <$> d) :: Dynamic t SubState2
-- >   in undefined
-- >
-- > renderThird
-- >   :: forall t m. MonadWidget t m
-- >   => Dynamic t (NP I '[SubState3a, SubState3b, SubState3c])
-- >   -> m (Event t UsersEventType)
-- > renderThird d =
-- >   let dynA = ((\(I a :* I b :* I c :* Nil) -> a) <$> d) :: Dynamic t SubState3a
-- >       dynB = ((\(I a :* I b :* I c :* Nil) -> b) <$> d) :: Dynamic t SubState3b
-- >       dynC = ((\(I a :* I b :* I c :* Nil) -> c) <$> d) :: Dynamic t SubState3c
-- >   in undefined
-- >
-- > renderUsersSumType
-- >   :: MonadWidget t m
-- >   => GTag UsersSumType a
-- >   -> Dynamic t (NP I a)
-- >   -> m (Event t UsersEventType)
-- > renderUsersSumType = \case
-- >   GTag       (Z Refl)   -> renderFirst
-- >   GTag    (S (Z Refl))  -> renderSecond
-- >   GTag (S (S (Z Refl))) -> renderThird
-- >   _                     -> error "this is impossible"
-- >
-- > render
-- >   :: MonadWidget t m
-- >   => Dynamic t UsersSumType
-- >   -> m (Event t UsersEventType)
-- > render = renderSumType renderUsersSumType
renderSumType
  :: forall t m u r. (MonadWidget t m, Generic u)
  => (forall b. GTag u b -> Dynamic t (NP I b) -> m (Event t r))
  -> Dynamic t u
  -> m (Event t r)
renderSumType renderAnything dynState =
  switchPromptly never =<<
    ( dyn
      . fmap toAction
      . uniqDynBy sameConstructor
      . toNestedDyn
      . fmap toDSum
    ) dynState
  where
    toAction :: DSum (GTag u) (WrapDyn t m) -> m (Event t r)
    toAction (t :=> WrapDyn x) = x >>= renderAnything t

    sameConstructor :: DSum (GTag u) a -> DSum (GTag u) a -> Bool
    sameConstructor (t1 :=> _) (t2 :=> _) = isJust (t1 `geq` t2)

    toNestedDyn
      :: Dynamic t (DSum (GTag u) (NP I))
      -> Dynamic t (DSum (GTag u) (WrapDyn t m))
    toNestedDyn d = makeNestedDyn <$> d
      where
        makeNestedDyn :: DSum (GTag u) (NP I) -> DSum (GTag u) (WrapDyn t m)
        makeNestedDyn (t :=> x) =
          t :=> WrapDyn (holdDyn x (eventsForTag t d))

    eventsForTag
      :: GTag u a
      -> Dynamic t (DSum (GTag u) (NP I))
      -> Event t (NP I a)
    eventsForTag tag = fmapMaybe tagToJust . updated
      where tagToJust (t :=> x) = (\Refl -> x) <$> t `geq` tag
