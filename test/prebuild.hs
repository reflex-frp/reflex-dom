{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Reflex.Dom
import qualified Data.Text as T
import Control.Concurrent

import Control.Monad.State.Strict
import Data.Functor.Misc
import Data.Monoid
import Data.Word
import qualified Data.Map as Map
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import qualified Reflex.Patch.DMapWithMove as PatchDMapWithMove

main :: IO ()
main = mainWidget w

w :: forall t m. (MonadWidget t m, DomRenderHook t m, MountableDomBuilder t m) => m ()
w = do
  let slow :: forall m'. (MonadWidget t m', DomRenderHook t m') => m' ()
      {-
      performEventChain = do
        postBuild <- delay 0 =<< getPostBuild
        rec let maxN = 10000
                n = leftmost [0 <$ postBuild, ffilter (<=maxN) $ succ <$> n']
            n' <- performEvent $ return <$> n
        pausedUntil $ ffilter (==maxN) n'
        _ <- widgetHold (text "Starting") $ text . T.pack . show <$> n
        return ()
      -}
--      {- Many dyns
      slow = elAttr "div" ("style" =: "position:relative;width:256px;height:256px") $ go maxDepth
        where maxDepth = 6 :: Int
              go 0 = blank
              go n = void $ dyn $ pure $ do
                let bgcolor = "rgba(0,0,0," <> T.pack (show (1 - (fromIntegral n / fromIntegral maxDepth) :: Double)) <> ")"
                    s pos = pos <> ";position:absolute;border:1px solid white;background-color:" <> bgcolor
                elAttr "div" ("style" =: s "left:0;right:50%;top:0;bottom:50%") $ go $ pred n
                elAttr "div" ("style" =: s "left:50%;right:0;top:0;bottom:50%") $ go $ pred n
                elAttr "div" ("style" =: s "left:50%;right:0;top:50%;bottom:0") $ go $ pred n
                elAttr "div" ("style" =: s "left:0;right:50%;top:50%;bottom:0") $ go $ pred n
--      -}
      {- Many elDynAttrs
      slow = do
        let size = 64
        replicateM_ size $ elAttr "div" ("style" =: ("height:4px;width:" <> T.pack (show (size*4)) <> "px;line-height:0;background-color:gray")) $ do
          replicateM_ size $ elDynAttr "div" (pure $ "style" =: "display:inline-block;width:4px;height:4px;background-color:black") blank
      -}
      {- Many dynTexts
      slow = el "table" $ do
        let size = 64
        replicateM_ size $ el "tr" $ do
          replicateM_ size $ el "td" $ do
            dynText $ pure "."
      -}
      {- Many simultaneous performEvents
      slow = do
        postBuild <- getPostBuild
        replicateM_ ((2 :: Int) ^ (14 :: Int)) $ performEvent_ $ return () <$ postBuild
        done <- performEvent $ return () <$ postBuild
        _ <- widgetHold (text "Doing performEvent") $ text "Done" <$ done
        return ()
      -}
      {-
      slow = do
        postBuild <- getPostBuild
        postBuild' <- performEvent . (return () <$) =<< performEvent . (return () <$) =<< getPostBuild
        let f x = Map.fromList [(n, x) | n <- [1 :: Int .. 4096]]
        listHoldWithKey (f False) (f (Just True) <$ postBuild') $ \k -> \case
          False -> do
            text "X "
            notReadyUntil =<< getPostBuild
          True -> do
            text ". "
        return ()
      -}
      {-
      slow = do
        let f :: forall a. EitherTag () () a -> Const2 () () a -> m' (Const2 () () a)
            f _ (Const2 ()) = do
              notReadyUntil =<< delay 0.5 =<< getPostBuild
              text "Done"
              return $ Const2 ()
        postBuild <- getPostBuild
        traverseDMapWithKeyWithAdjustWithMove f (DMap.singleton LeftTag $ Const2 ()) $ (PatchDMapWithMove.moveDMapKey LeftTag RightTag) <$ postBuild
        return ()
      -}
      {-
      slow = do
        let h x = do
              liftIO $ putStrLn "render hook"
              result <- x
              liftIO $ putStrLn "render hook done"
              return result
            f :: forall a. Const2 () () a -> Identity a -> m' (Identity a)
            f (Const2 ()) (Identity ()) = do
              liftIO $ putStrLn "f"
              widgetHold notReady . (blank <$) =<< delay 0.1 =<< getPostBuild
              liftIO $ putStrLn "f done"
              return $ Identity ()
        withRenderHook h $ do
          postBuild <- getPostBuild
          _ <- traverseDMapWithKeyWithAdjust f mempty $ PatchDMap (DMap.singleton (Const2 () :: Const2 () () ()) (ComposeMaybe $ Just $ Identity ())) <$ postBuild
          return ()
      -}
  el "div" $ do
    draw <- button "Draw"
    widgetHold blank $ ffor draw $ \_ -> do
      _ <- untilReady (text "Loading...") slow
      return ()
    return ()

#ifdef EXPERIMENTAL_DEPENDENT_SUM_INSTANCES
instance {-# INCOHERENT #-} (Show (f a), Show (f b)) => ShowTag (EitherTag a b) f where
  showTagToShow e _ = case e of
    LeftTag -> id
    RightTag -> id
#endif
