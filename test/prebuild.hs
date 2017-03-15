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

type Paused t = DynamicWriterT t All

runPaused :: (Reflex t, MonadFix m) => Paused t m a -> m (Event t (), a)
runPaused a = do
  (result, runnable) <- runDynamicWriterT a
  let done = void $ ffilter getAll $ updated $ uniqDyn runnable
  return (done, result)

--TODO: Perhaps we could use guaranteed-single-shot events here
pausedUntil :: (Reflex t, MonadFix m, MonadHold t m) => Event t a -> Paused t m ()
pausedUntil e = do
  tellDyn =<< holdDyn (All False) (All True <$ e) --TODO: Disconnect after one firing

dyn' :: (DomBuilder t m, PostBuild t m, m ~ Paused t m', MonadFix m', MonadHold t m') => Dynamic t (m a) -> m (Event t a)
dyn' child = do
  postBuild <- getPostBuild
  let newChild = leftmost [updated child, tagCheap (current child) postBuild]
  newChildDone <- snd <$> runWithReplace (return ()) newChild
  pausedUntil newChildDone
  return newChildDone

main :: IO ()
main = mainWidget $ do
  let slow = dynTree
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
      dynTree = elAttr "div" ("style" =: "position:relative;width:256px;height:256px") $ go maxDepth
        where maxDepth = 5 :: Int
              go 0 = dynText $ pure "X"
              go n = void $ dyn $ pure $ do
                let bgcolor = "rgba(0,0,0," <> T.pack (show (1 - (fromIntegral n / fromIntegral maxDepth) :: Double)) <> ")"
                    s pos = pos <> ";position:absolute;border:1px solid white;background-color:" <> bgcolor
                elAttr "div" ("style" =: s "left:0;right:50%;top:0;bottom:50%") $ go $ pred n
                elAttr "div" ("style" =: s "left:50%;right:0;top:0;bottom:50%") $ go $ pred n
                elAttr "div" ("style" =: s "left:50%;right:0;top:50%;bottom:0") $ go $ pred n
                elAttr "div" ("style" =: s "left:0;right:50%;top:50%;bottom:0") $ go $ pred n
  el "h1" $ text "Bad"
  el "div" $ do
    draw <- button "Draw"
    _ <- widgetHold blank $ ffor draw $ \_ -> slow
    return ()
  el "h1" $ text "Good"
  el "div" $ do
    draw <- button "Draw"
    _ <- widgetHold blank $ ffor draw $ \_ -> do
      postBuild <- getPostBuild
      _ <- widgetHold (text "Loading...") $ slow <$ postBuild
      liftIO $ threadDelay 0 -- This is necessary so that ghcjs will release the thread back to the DOM so that we see the loading indicator immediately; we could instead adjust the parameters to GHCJS so that the thread quantum is smaller.
    return ()
