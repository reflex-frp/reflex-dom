{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, RecursiveDo, ScopedTypeVariables, LambdaCase, GADTs #-}
module Reflex.Dom.DynamicWriter where

import Reflex
import Reflex.Host.Class
import Reflex.Dom.Class
import Reflex.Dom.Builder.Class
import Reflex.Dom.PerformEvent.Class
import Reflex.Dom.PostBuild.Class

import Control.Lens hiding (element)
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Exception
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Dependent.Map (DMap)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Functor.Compose
import Data.Functor.Misc
import Data.Semigroup
import Data.Foldable
import Data.Traversable

instance MonadTrans (DynamicWriterT t w) where
  lift = DynamicWriterT . lift

data PatchMap a where
  PatchMap :: Ord k => Map k (Maybe v) -> PatchMap (Map k v)

instance Patch PatchMap where
  apply (PatchMap p) old = Just $! insertions `Map.union` (old `Map.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = Map.mapMaybeWithKey (const id) p
          deletions = Map.mapMaybeWithKey (const nothingToJust) p
          nothingToJust = \case
            Nothing -> Just ()
            Just _ -> Nothing

instance Ord k => Semigroup (PatchMap (Map k v)) where
  PatchMap a <> PatchMap b = PatchMap $ a `mappend` b --TODO: Add a semigroup instance for Map
  -- PatchMap is idempotent, so stimes n is id for every n
#if MIN_VERSION_semigroups(0,17,0)
  stimes = stimesIdempotentMonoid
#else
  times1p n x = case compare n 0 of
    LT -> error "stimesIdempotentMonoid: negative multiplier"
    EQ -> mempty
    GT -> x
#endif

instance Ord k => Monoid (PatchMap (Map k v)) where
  mempty = PatchMap mempty
  mappend = (<>)

mconcatIncremental :: (Reflex t, MonadHold t m, MonadFix m, Monoid v) => Map k v -> Event t (PatchMap (Map k v)) -> m (Dynamic t v)
mconcatIncremental m0 e = do
  d <- foldDynMaybe apply m0 e
  return $ fmap (mconcat . Map.elems) d

mapIncrementalMapValuesWithKey :: Reflex t => (k -> v -> v') -> Incremental t PatchMap (Map k v) -> Incremental t PatchMap (Map k v')
mapIncrementalMapValuesWithKey f = unsafeMapIncremental (Map.mapWithKey f) $ \(PatchMap m) -> PatchMap $ Map.mapWithKey (\k mv -> fmap (f k) mv) m

mapIncrementalMapValues :: Reflex t => (v -> v') -> Incremental t PatchMap (Map k v) -> Incremental t PatchMap (Map k v')
mapIncrementalMapValues f = mapIncrementalMapValuesWithKey $ const f

unsafeMapIncremental :: (Reflex t, Patch p, Patch p') => (a -> a') -> (p a -> p' a') -> Incremental t p a -> Incremental t p' a'
unsafeMapIncremental f g a = unsafeBuildIncremental (fmap f $ sample $ currentIncremental a) $ fmap g $ updatedIncremental a

incrementalExtractFunctorDMap :: Reflex t => Incremental t PatchMap (Map k (f v)) -> Incremental t PatchDMap (DMap (Const2 k v) f)
incrementalExtractFunctorDMap = unsafeMapIncremental mapWithFunctorToDMap $ \(PatchMap m) -> PatchDMap $ mapWithFunctorToDMap $ fmap Compose m

mergeIncrementalMap :: (Reflex t, Ord k) => Incremental t PatchMap (Map k (Event t v)) -> Event t (Map k v)
mergeIncrementalMap = fmap dmapToMap . mergeIncremental . incrementalExtractFunctorDMap

incrementalReplaceableMap :: (Reflex t, MonadHold t m, MonadFix m, Ord k) => Map k (Replaceable t v) -> Event t (PatchMap (Map k (Replaceable t v))) -> m (Incremental t PatchMap (Map k v))
incrementalReplaceableMap v0 e = do
  rec vals <- holdIncremental v0 $ e <> valChanges
      let valChanges = fmap PatchMap $ mergeIncrementalMap $ mapIncrementalMapValues _replaceable_modify vals
  return $ mapIncrementalMapValues _replaceable_value vals

mergeDynIncremental :: (Reflex t, Ord k) => Incremental t PatchMap (Map k (Dynamic t v)) -> Incremental t PatchMap (Map k v)
mergeDynIncremental a = unsafeBuildIncremental (mapM (sample . current) =<< sample (currentIncremental a)) $ addedAndRemovedValues <> changedValues
  where changedValues = fmap (PatchMap . fmap Just) $ mergeIncrementalMap $ mapIncrementalMapValues updated a
        addedAndRemovedValues = flip pushAlways (updatedIncremental a) $ \(PatchMap m) -> PatchMap <$> mapM (mapM (sample . current)) m

data Replaceable t w = Replaceable
  { _replaceable_value :: w
  , _replaceable_modify :: Event t (Maybe (Replaceable t w)) -- "Please delete" is represented with an event of Nothing; all subsequent events will be ignored
  }

type DynamicWriterAccumulator t w = [Replaceable t (Dynamic t w)]

newtype DynamicWriterT t w m a = DynamicWriterT { unDynamicWriterT :: StateT (DynamicWriterAccumulator t w) m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t, MonadSample t, MonadAsyncException, MonadException) -- The list is kept in reverse order

instance HasWebView m => HasWebView (DynamicWriterT t w m) where
  type WebViewPhantom (DynamicWriterT t w m) = WebViewPhantom m
  askWebView = lift askWebView

instance MonadRef m => MonadRef (DynamicWriterT t w m) where
  type Ref (DynamicWriterT t w m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (DynamicWriterT t w m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (DynamicWriterT t w m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

runDynamicWriterTInternal :: DynamicWriterT t w m a -> m (a, DynamicWriterAccumulator t w)
runDynamicWriterTInternal (DynamicWriterT a) = runStateT a []

mconcatIncrementalReplaceableDynMap :: (Reflex t, Monoid v, Ord k) => Incremental t PatchMap (Map k (Dynamic t v)) -> Dynamic t v
mconcatIncrementalReplaceableDynMap = fmap (mconcat . Map.elems) . incrementalToDynamic . mergeDynIncremental

runDynamicWriterT :: (Reflex t, MonadHold t m, Monoid w, MonadFix m) => DynamicWriterT t w m a -> m (a, Dynamic t w)
runDynamicWriterT (DynamicWriterT a) = do
  (result, ws) <- runStateT a []
  i <- incrementalReplaceableMap (Map.fromList $ zip [1 :: Int ..] $ reverse ws) never
  let w = mconcatIncrementalReplaceableDynMap i
  return (result, w)

class Monad m => MonadDynamicWriter t w m | m -> t w where
  tellDyn :: Dynamic t w -> m ()

instance (Monad m, Reflex t) => MonadDynamicWriter t w (DynamicWriterT t w m) where
  tellDyn w = DynamicWriterT $ modify (Replaceable w never :)

instance MonadReader r m => MonadReader r (DynamicWriterT t w m) where
  ask = lift ask
  local f (DynamicWriterT a) = DynamicWriterT $ mapStateT (local f) a
  reader = lift . reader

liftDynamicWriterTThroughSync :: Monad m' => (m (a, DynamicWriterAccumulator t w) -> m' (b, (a, DynamicWriterAccumulator t w))) -> DynamicWriterT t w m a -> DynamicWriterT t w m' (b, a)
liftDynamicWriterTThroughSync f (DynamicWriterT child) = DynamicWriterT $ do
    s <- get
    (b, (a, newS)) <- lift $ f $ runStateT child s
    put newS
    return (b, a)

{-# INLINABLE liftDynamicWriterTElementConfig #-}
liftDynamicWriterTElementConfig :: ElementConfig er t (DynamicWriterT t w m) -> ElementConfig er t m
liftDynamicWriterTElementConfig cfg = cfg
  { _elementConfig_eventFilters = _elementConfig_eventFilters cfg
  , _elementConfig_eventHandler = _elementConfig_eventHandler cfg
  }

instance (DomBuilder t m, Monoid w, MonadHold t m, MonadFix m) => DomBuilder t (DynamicWriterT t w m) where
  type DomBuilderSpace (DynamicWriterT t w m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (DynamicWriterT child) = DynamicWriterT $ do
    s <- get
    let cfg' = cfg
          { _elementConfig_eventFilters = _elementConfig_eventFilters cfg
          , _elementConfig_eventHandler = _elementConfig_eventHandler cfg
          }
    (el, (a, newS)) <- lift $ element elementTag cfg' $ runStateT child s
    put newS
    return (el, a)
  placeholder cfg = do
    let cfg' = cfg
          { _placeholderConfig_insertAbove = fmap runDynamicWriterTInternal $ _placeholderConfig_insertAbove cfg
          }
    let manageChildren :: Event t (NonEmpty (Replaceable t (Dynamic t w))) -- ^ Add nodes on the right; these are in reverse order
                       -> Event t () -- ^ No more nodes will be added after this event fires
                       -> m (Replaceable t (Dynamic t w))
        manageChildren newChildren additionsCeased = do
          rec nextId <- hold (0 :: Int) newNextId -- We assume this will never wrap around
              let numberedNewChildren :: Event t (Int, PatchMap (Map Int (Replaceable t (Dynamic t w))))
                  numberedNewChildren = flip pushAlways newChildren $ \rcs -> do
                    let cs = reverse $ toList rcs
                    myFirstId <- sample nextId
                    let (myNextId, numbered) = mapAccumL (\n v -> (succ n, (n, Just v))) myFirstId cs
                    return (myNextId, PatchMap $ Map.fromList numbered)
                  newNextId = fst <$> numberedNewChildren
          replaceableMap <- incrementalReplaceableMap Map.empty $ snd <$> numberedNewChildren
          noMoreAdditions <- holdDyn False $ True <$ additionsCeased
          let replaceSelf nma i = do
                guard nma
                case Map.toList i of
                  [] -> return Nothing -- Delete self
                  _ -> mzero
          return $ Replaceable (mconcatIncrementalReplaceableDynMap replaceableMap) $ fmapMaybe id $ updated $ zipDynWith replaceSelf noMoreAdditions $ incrementalToDynamic replaceableMap
    rec children <- lift $ manageChildren childOutputs $ cfg ^. deleteSelf
        p <- DynamicWriterT $ do
          modify (children:)
          lift $ placeholder cfg'
        let result = fmap fst $ _placeholder_insertedAbove p
            childOutputs = fmapMaybe (nonEmpty . snd) $ _placeholder_insertedAbove p
    return $ p
      { _placeholder_insertedAbove = result
      }
  inputElement cfg = lift $ inputElement $ cfg & inputElementConfig_elementConfig %~ liftDynamicWriterTElementConfig
  textAreaElement cfg = lift $ textAreaElement $ cfg & textAreaElementConfig_elementConfig %~ liftDynamicWriterTElementConfig

instance (Deletable t m, Reflex t, Monoid w, MonadHold t m, MonadFix m) => Deletable t (DynamicWriterT t w m) where
  deletable delete child = do
    (result, output) <- lift $ deletable delete $ runDynamicWriterT child
    DynamicWriterT $ modify (Replaceable output (Nothing <$ delete) :)
    return result

instance PerformEvent t m => PerformEvent t (DynamicWriterT t w m) where
  type Performable (DynamicWriterT t w m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance TriggerEvent t m => TriggerEvent t (DynamicWriterT t w m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance PostBuild t m => PostBuild t (DynamicWriterT t w m) where
  getPostBuild = lift getPostBuild

instance MonadDynamicWriter t w m => MonadDynamicWriter t w (ReaderT r m) where
  tellDyn = lift . tellDyn

instance MonadState s m => MonadState s (DynamicWriterT t w m) where
  get = lift get
  put = lift . put

instance HasJS x m => HasJS x (DynamicWriterT t w m) where
  type JSM (DynamicWriterT t w m) = JSM m
  liftJS = lift . liftJS
