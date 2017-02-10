{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Lens
import Control.Monad.Identity
import Data.Dependent.Map (DMap)
import Data.Functor.Constant
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import qualified Data.Text as T
import Reflex.Dom
import System.Random

main :: IO ()
main = mainWidget $ do
  let f k v = el "li" $ do
        text $ T.pack $ show (k, v)
        inputElement def
        return ()
  let numKeys = 1000 :: Int
      keys = take numKeys [0 :: Int ..]
      values :: [(Int, Int, Int)]
      values = zip3 (randoms (mkStdGen 0)) (randoms (mkStdGen 1)) (randoms (mkStdGen 2))
      testMap = Map.fromList $ zip keys values
  resort <- leftmost <$> sequence
    [ (comparing (view _1) <$) <$> button "1 ASC"
    , (flip (comparing (view _1)) <$) <$> button "1 DESC"
    , (comparing (view _2) <$) <$> button "2 ASC"
    , (flip (comparing (view _2)) <$) <$> button "2 DESC"
    , (comparing (view _3) <$) <$> button "3 ASC"
    , (flip (comparing (view _3)) <$) <$> button "3 DESC"
    ]
  simpleSortableList f testMap resort
  return ()

mapMapWithAdjust :: forall t m k v v'. (MonadAdjust t m, Ord k) => (k -> v -> m v') -> Map k v -> Event t (PatchMap k v) -> m (Map k v', Event t (PatchMap k v'))
mapMapWithAdjust f m0 m' = do
  (out0 :: DMap (Const2 k v) (Constant v'), out') <- traverseDMapWithKeyWithAdjust (\(Const2 k) (Identity v) -> Constant <$> f k v) (mapToDMap m0) (const2PatchDMapWith Identity <$> m')
  return (dmapToMapWith (\(Constant v') -> v') out0, patchDMapToPatchMapWith (\(Constant v') -> v') <$> out')

mapMapWithAdjustWithMove :: forall t m k v v'. (MonadAdjust t m, Ord k) => (k -> v -> m v') -> Map k v -> Event t (PatchMapWithMove k v) -> m (Map k v', Event t (PatchMapWithMove k v'))
mapMapWithAdjustWithMove f m0 m' = do
  (out0 :: DMap (Const2 k v) (Constant v'), out') <- traverseDMapWithKeyWithAdjustWithMove (\(Const2 k) (Identity v) -> Constant <$> f k v) (mapToDMap m0) (const2PatchDMapWithMoveWith Identity <$> m')
  return (dmapToMapWith (\(Constant v') -> v') out0, patchDMapWithMoveToPatchMapWithMoveWith (\(Constant v') -> v') <$> out')

simpleSortableList :: (MonadHold t m, MonadFix m, MonadAdjust t m, Ord k) => (k -> v -> m ()) -> Map k v -> Event t (v -> v -> Ordering) -> m ()
simpleSortableList f m0 resortFunc = do
  rec let resortPatch = attachWith (flip patchThatSortsMapWith) (currentIncremental m) resortFunc
      m <- holdIncremental m0 resortPatch
  _ <- mapMapWithAdjustWithMove f m0 resortPatch
  return ()
