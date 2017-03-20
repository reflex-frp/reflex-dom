{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad
import Control.Monad.Trans
import Reflex.Dom
import qualified Data.Text as T
import Control.Concurrent

import Control.Monad.State.Strict
import Data.Monoid
import Data.Word

main :: IO ()
main = mainWidget w

w :: forall t m. (MonadWidget t m, MountableDomBuilder t m) => m ()
w = do
  let slow :: forall m'. MonadWidget t m' => m' ()
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
  {-
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
  -}
      {-
      slow = do
        let size = 64
        replicateM_ size $ elAttr "div" ("style" =: ("height:4px;width:" <> T.pack (show (size*4)) <> "px;line-height:0;background-color:gray")) $ do
          replicateM_ size $ elDynAttr "div" (pure $ "style" =: "display:inline-block;width:4px;height:4px;background-color:black") blank
      -}
      slow = el "table" $ do
        let size = 64
        replicateM_ size $ el "tr" $ do
          replicateM_ size $ el "td" $ do
            dynText $ pure "."
      {-
      slow = do
        postBuild <- getPostBuild
        replicateM_ ((2 :: Int) ^ (14 :: Int)) $ performEvent_ $ return () <$ postBuild
        done <- performEvent $ return () <$ postBuild
        _ <- widgetHold (text "Doing performEvent") $ text "Done" <$ done
        return ()
      -}
  el "h1" $ text "Bad"
  el "div" $ do
    draw <- button "Draw"
    widgetHold blank $ ffor draw $ \_ -> do
      postBuild <- getPostBuild
      widgetHold (text "Loading...") $ slow <$ postBuild
      return ()
  el "h1" $ text "Bad - with EventWriterT"
  el "div" $ do
    draw <- button "Draw"
    widgetHold blank $ ffor draw $ \_ -> do
      (_, w :: Dynamic t ()) <- runDynamicWriterT slow
      return ()
  el "h1" $ text "Good"
  el "div" $ do
    draw <- button "Draw"
    widgetHold blank $ ffor draw $ \_ -> do
      (df0, _) <- buildDomFragment $ text "Loading..."
      (df', (doneBuilding, _)) <- buildDomFragment $ do
        slow
        postBuild <- getPostBuild
        return (postBuild, ())
      mountDomFragment df0 $ df' <$ doneBuilding
      postBuild <- getPostBuild
      performEvent_ $ liftIO (threadDelay 0) <$ postBuild -- This is necessary so that ghcjs will release the thread back to the DOM so that we see the loading indicator immediately; we could instead adjust the parameters to GHCJS so that the thread quantum is smaller.
    return ()
