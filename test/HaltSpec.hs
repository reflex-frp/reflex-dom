{-# LANGUAGE FlexibleContexts #-}

import           Control.Monad.IO.Class
import           Data.Foldable
import           GHCJS.DOM
import qualified Graphics.UI.Gtk as Gtk
import           Reflex.Dom
import           Reflex.Spider.Internal (SpiderHostFrame)
import           Test.Hspec

main = hspec spec

spec :: Spec
spec = do
  describe "haltGui" $ do
    it "works" $ do
      getFirstEventAndHalt return `shouldReturn` ()

    it "works multiple times" $ do
      forM_ [1 .. 1000] $ \ i -> do
        getFirstEventAndHalt return `shouldReturn` ()

type SpiderM = Widget Spider (Gui Spider (WithWebView SpiderHost) SpiderHostFrame)

getFirstEventAndHalt :: (Event Spider () -> SpiderM (Event Spider ())) -> IO ()
getFirstEventAndHalt action = do
  _ <- mainWidget $ do
    tick <- getPostBuild
    event <- action tick
    webView <- askWebView
    quit <- getQuitWidget
    performEvent_ (quit <$ event)
    return ()
  return ()
