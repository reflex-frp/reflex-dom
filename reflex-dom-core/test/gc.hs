{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core
import Reflex.Dom.Main

main :: IO ()
main = run 3911 $ do
  mainWidget $ do
    let w = do
          rec let modifyAttrs = flip pushAlways (updated d) $ \_ -> sample $ current d
              (e, _) <- element "button" (def & modifyAttributes .~ modifyAttrs) blank
              d <- holdDyn mempty $ mempty <$ domEvent Click e
          return ()
    redraw <- button "Redraw"
    _ <- widgetHold w $ w <$ redraw
    return ()
  liftIO $ forever $ threadDelay 1000000000
