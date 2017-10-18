{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Reflex.Dom.Location
  ( browserHistoryWith
  , getLocationAfterHost
  , getLocationFragment
  , getLocationHost
  , getLocationPath
  , getLocationProtocol
  , getLocationUrl
  ) where

import Reflex
import Reflex.Dom.Builder.Immediate (wrapDomEvent)

import Control.Lens ((^.))
import Control.Monad ((>=>))
import Data.Text (Text)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Location as Location
import GHCJS.DOM.Types (Location)
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import Language.Javascript.JSaddle (FromJSString, MonadJSM, ToJSString, fromJSValUnchecked, js1, toJSVal)


withLocation :: (MonadJSM m) => (Location -> m a) -> m a
withLocation f = DOM.currentWindowUnchecked >>= Window.getLocation >>= f

-- | Returns the full URI-decoded URL of the current window location.
getLocationUrl :: (MonadJSM m) => m Text
getLocationUrl = withLocation (Location.getHref >=> decodeURIText)

-- | Returns the host of the current window location
getLocationHost :: (MonadJSM m) => m Text
getLocationHost = withLocation Location.getHost

-- | Returns the protocol/scheme (e.g. @http:@ or @https:@) of the current window location
getLocationProtocol :: (MonadJSM m) => m Text
getLocationProtocol = withLocation Location.getProtocol

-- | Returns the URI-decoded location after the host and port; i.e. returns the path, query, and fragment of the location.
getLocationAfterHost :: (MonadJSM m) => m Text
getLocationAfterHost = withLocation $ \loc -> do
  pathname <- Location.getPathname loc
  search <- Location.getSearch loc
  hash <- Location.getHash loc
  decodeURI (mconcat [pathname, search, hash] :: Text)

-- | Returns the URI-decoded path of the current window location.
getLocationPath :: (MonadJSM m) => m Text
getLocationPath = withLocation (Location.getPathname >=> decodeURIText)

-- | Returns the URI-decoded fragment/hash of the current window location.
getLocationFragment :: (MonadJSM m) => m Text
getLocationFragment = withLocation (Location.getHash >=> decodeURIText)


-- | Decodes a URI with JavaScript's @decodeURI@ function.
--
-- FIXME: @decodeURI@ will throw when URI is malformed
decodeURI :: (MonadJSM m, ToJSString a, FromJSString b) => a -> m b
decodeURI input = do
  window <-  DOM.currentWindowUnchecked
  window' <- DOM.liftJSM $ toJSVal window
  DOM.liftJSM $ window' ^. js1 ("decodeURI"::Text) input >>= fromJSValUnchecked

decodeURIText :: (MonadJSM m) => Text -> m Text
decodeURIText = decodeURI

-- | Builds a Dynamic carrying the current window location.
browserHistoryWith :: (MonadJSM m, TriggerEvent t m, MonadHold t m)
                   => (forall jsm. MonadJSM jsm => Location -> jsm a)
                   -- ^ A function to encode the window location in a more useful form (e.g. @getLocationAfterHost@).
                   -> m (Dynamic t a)
browserHistoryWith f = do
  window <- DOM.currentWindowUnchecked
  location <- Window.getLocation window
  loc0 <- f location
  locEv <- wrapDomEvent window (`DOM.on` DOM.popState) $ f location
  holdDyn loc0 locEv
