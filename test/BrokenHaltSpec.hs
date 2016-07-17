{-# LANGUAGE FlexibleContexts #-}

module HaltSpec where

import Control.Monad.IO.Class
import Data.Foldable
import GHCJS.DOM
import qualified Graphics.UI.Gtk as Gtk
import Reflex.Dom
import Reflex.Spider.Internal (SpiderHostFrame)
import Test.Hspec

spec :: Spec
spec = do
  describe "haltGui" $ do
    it "works" $ do
      getFirstEventAndHalt return `shouldReturn` ()

    it "works multiple times" $ do
      forM_ [1 .. 100] $ \ i -> do
        getFirstEventAndHalt return `shouldReturn` ()

type SpiderM =
  Widget Spider (Gui Spider (WithWebView SpiderHost) SpiderHostFrame)

getFirstEventAndHalt :: (Event Spider () -> SpiderM (Event Spider ())) -> IO ()
getFirstEventAndHalt action = do
  _ <- mainWidget $ do
    tick <- getPostBuild
    event <- action tick
    webView <- askWebView
    _ <- foldDynM (\ a () -> liftIO $ haltGui webView) () event
    return ()
  return ()

haltGui :: WebView -> IO ()
haltGui wv = Gtk.postGUIAsync $ do
  w <- Gtk.widgetGetToplevel wv
  Gtk.widgetDestroy w
