{-# LANGUAGE ImportQualifiedPost #-}

module HaltSpec where

import Control.Monad.IO.Class
import Data.Foldable
import GHCJS.DOM
import Graphics.UI.Gtk qualified as Gtk
import Reflex.Dom
import Reflex.Spider.Internal (SpiderHostFrame)
import Test.Hspec

spec :: Spec
spec = do
  describe "haltGui" $ do
    it "works" $ do
      getFirstEventAndHalt pure `shouldReturn` ()

    it "works multiple times" $ do
      for_ [1 .. 100] $ \i -> do
        getFirstEventAndHalt pure `shouldReturn` ()

type SpiderM =
  Widget Spider (Gui Spider (WithWebView SpiderHost) SpiderHostFrame)

getFirstEventAndHalt :: (Event Spider () -> SpiderM (Event Spider ())) -> IO ()
getFirstEventAndHalt action = do
  _ <- mainWidget $ do
    tick <- getPostBuild
    event <- action tick
    webView <- askWebView
    _ <- foldDynM (\a () -> liftIO $ haltGui webView) () event
    pure ()
  pure ()

haltGui :: WebView -> IO ()
haltGui wv = Gtk.postGUIAsync $ do
  w <- Gtk.widgetGetToplevel wv
  Gtk.widgetDestroy w
