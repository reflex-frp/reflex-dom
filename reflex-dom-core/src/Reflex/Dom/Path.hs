{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Path (pathWidget, browserHistory, IsPath(..)) where

import Reflex
import Reflex.Dom.Widget
import Reflex.Dom.Builder.Class
import Reflex.Dom.Builder.Immediate

import Data.Text (Text)
import Language.Javascript.JSaddle
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Location as Location
import qualified GHCJS.DOM.History as History
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import GHCJS.DOM.Types (Location)
import qualified GHCJS.DOM.Types as DOM
import Control.Monad
import Control.Lens
import Control.Monad.Fix

-- | This class provides a mapping between the path portion of a URL encoded as Text, and an arbitrary data type.
-- the expectation being that you'll define an algebraic data type for the possible paths, and map invalid paths to
-- some particular value of that (perhaps representing "home" or a "404").
class IsPath path where 
  pathToText :: path -> Text
  textToPath :: Text -> path

instance IsPath Text where
  pathToText = id
  textToPath = id

-- | Return the appended path, search and hash excluding hostname or port.
getPath :: MonadJSM m => Location -> m Text
getPath location = do
  pathname <- Location.getPathname location
  search <- Location.getSearch location
  hash <- Location.getHash location
  return $ mconcat [pathname, search, hash]

-- FIXME decodeURI may throw
decodeURI :: (MonadJSM m, ToJSString a, FromJSString b) => a -> m b
decodeURI input = do
  window <-  DOM.currentWindowUnchecked
  window' <- DOM.liftJSM $ toJSVal window
  DOM.liftJSM $ window' ^. js1 ("decodeURI"::Text) input >>= fromJSValUnchecked

-- | encodeURI is used for full urls and components only for segments like search, hash, or path
getPathDecoded :: MonadJSM m => Location -> m Text
getPathDecoded = decodeURI <=< getPath

browserHistoryText :: (MonadJSM m, TriggerEvent t m) => m (Text, Event t Text)
browserHistoryText = do
  window <- DOM.currentWindowUnchecked
  location <- Window.getLocation window
  loc0 <- getPathDecoded location
  loc <- wrapDomEvent window (`DOM.on` DOM.popState) $ getPathDecoded location
  return (loc0, loc) 

browserHistory :: (IsPath p, MonadJSM m, TriggerEvent t m, Reflex t) => m (p, Event t p) 
browserHistory = do
  (location, locationEvent) <- browserHistoryText
  return (textToPath location, fmap textToPath locationEvent)

-- Constructs a widget which switches according to the current browser location.
-- It takes a function which, given the current path, produces the widget to be displayed,
-- and this widget produces, along with its ordinary result, an Event that, when it occurs
-- will cause the browser history to be pushed (and the URL updated), as well as the widget
-- to be switched accordingly.
-- The overall widget produces a Dynamic result whose value is the result of the currently
-- displayed widget.
pathWidget :: ( IsPath p
              , DomBuilder t m
              , TriggerEvent t m
              , PerformEvent t m
              , MonadHold t m
              , MonadFix m
              , MonadJSM m
              , MonadJSM (Performable m))
           => (p -> m (Event t p, a))
            -- ^ Function which, given a path, will construct the widget to be displayed. This widget
            -- additionally returns an Event which, when it fires, sets the path (updating the browser history)
            -- and causes the widget to be rebuilt with the new path.
           -> m (Dynamic t a)
pathWidget f = do
  (path0, pathUpdate) <- browserHistory
  let pageFor path = Workflow $ do
        (switchE, x) <- f path -- run the child widget, getting an Event that tells us when it would like to switch paths, and a result
        performEvent_ . ffor switchE $ \newPath -> pushState' (pathToText newPath)
        return (x, fmap pageFor (leftmost [switchE, pathUpdate]))
  workflow (pageFor path0)

-- | Wrapper for pushState that doesn't require state or title.
pushState' :: MonadJSM m => Text -> m ()
pushState' r = do
  history <- Window.getHistory =<< DOM.currentWindowUnchecked
  History.pushState history () (""::Text) $ Just r
