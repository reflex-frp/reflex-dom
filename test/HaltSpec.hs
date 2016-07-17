{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.IO.Class
import Data.Foldable
import GHCJS.DOM
import qualified Graphics.UI.Gtk as Gtk
import Reflex.Dom
import Reflex.Spider.Internal (SpiderHostFrame)

main :: IO ()
main = do
  getFirstEventAndHalt return
  putStrLn "stopped once"

  forM_ [1 .. 1000] $ \i -> getFirstEventAndHalt return
  putStrLn "stopped many times"

type SpiderM = Widget Spider (Gui Spider (WithWebView SpiderHost) SpiderHostFrame)

getFirstEventAndHalt :: (Event Spider () -> SpiderM (Event Spider ())) -> IO ()
getFirstEventAndHalt action = mainWidget $ do
  tick <- getPostBuild
  event <- action tick
  webView <- askWebView
  quit <- getQuitWidget
  performEvent_ (quit <$ event)
  return ()
