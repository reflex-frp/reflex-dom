{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
  , manageHistory
  , manageHistory'
  , HistoryCommand (..)
  , HistoryStateUpdate (..)
  , HistoryItem (..)
  , getLocationUri
  ) where

import Reflex
import Reflex.Dom.Builder.Immediate (wrapDomEvent)

import Control.Lens ((^.))
import Control.Monad ((>=>))
import Control.Monad.Fix (MonadFix)
import Data.Align (align)
import Data.Monoid
import Data.Text (Text)
import Data.These (These(..))
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Location as Location
import qualified GHCJS.DOM.History as History
import qualified GHCJS.DOM.PopStateEvent as PopStateEvent
import GHCJS.DOM.Types (Location, History, SerializedScriptValue (..), liftJSM)
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import Language.Javascript.JSaddle (FromJSString, MonadJSM, ToJSString, fromJSValUnchecked, js1, ToJSVal (..), FromJSVal (..))
import Network.URI

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

--TODO: Pending https://github.com/haskell/network-uri/issues/39, ensure that
--we're handling escaping of URIs correctly
data HistoryItem = HistoryItem
  { _historyItem_state :: SerializedScriptValue
  , _historyItem_uri :: URI
  -- ^ NOTE: All URIs in this module are assumed to be already percent-escaped
  }

data HistoryStateUpdate = HistoryStateUpdate
  { _historyStateUpdate_state :: SerializedScriptValue
  , _historyStateUpdate_title :: Text
  , _historyStateUpdate_uri :: Maybe URI
  -- ^ If Just, update the URI; otherwise leave it unchanged
  -- NOTE: All URIs in this module are assumed to be already percent-escaped
  }

data HistoryCommand
   = HistoryCommand_PushState HistoryStateUpdate
   | HistoryCommand_ReplaceState HistoryStateUpdate

runHistoryCommand :: MonadJSM m => History -> HistoryCommand -> m ()
runHistoryCommand history = \case
  HistoryCommand_PushState su -> History.pushState history
    (_historyStateUpdate_state su)
    (_historyStateUpdate_title su)
    (show <$> _historyStateUpdate_uri su)
  HistoryCommand_ReplaceState su -> History.replaceState history
    (_historyStateUpdate_state su)
    (_historyStateUpdate_title su)
    (show <$> _historyStateUpdate_uri su)

getLocationUriAuth :: MonadJSM m => Location -> m URIAuth
getLocationUriAuth location = URIAuth "" -- Username and password don't seem to be available in most browsers
  <$> Location.getHostname location
  <*> (appendColonIfNotEmpty <$> Location.getPort location)
  where appendColonIfNotEmpty = \case
          "" -> ""
          x -> ":" <> x

getLocationUri :: MonadJSM m => Location -> m URI
getLocationUri location = URI
  <$> Location.getProtocol location
  <*> (Just <$> getLocationUriAuth location)
  <*> Location.getPathname location
  <*> Location.getSearch location
  <*> Location.getHash location

manageHistory :: (MonadJSM m, TriggerEvent t m, MonadHold t m, PerformEvent t m, MonadJSM (Performable m)) => Event t HistoryCommand -> m (Dynamic t HistoryItem)
manageHistory runCmd = do
  window <- DOM.currentWindowUnchecked
  location <- Window.getLocation window
  history <- Window.getHistory window
  let getCurrentHistoryItem = HistoryItem
        <$> History.getState history
        <*> getLocationUri location
  item0 <- liftJSM getCurrentHistoryItem
  itemSetInternal <- performEvent $ ffor runCmd $ \cmd -> liftJSM $ do
    runHistoryCommand history cmd
    getCurrentHistoryItem
  itemSetExternal <- wrapDomEvent window (`DOM.on` DOM.popState) $ do
    e <- DOM.event
    HistoryItem
      <$> (SerializedScriptValue <$> PopStateEvent.getState e)
      <*> getLocationUri location
  holdDyn item0 $ leftmost [itemSetInternal, itemSetExternal]
--TODO: Handle title setting better

manageHistory'
  :: (MonadFix m, MonadJSM m, TriggerEvent t m, MonadHold t m, PerformEvent t m, MonadJSM (Performable m))
  => Event t ()
  -- ^ Don't do anything until this event has fired
  -> Event t HistoryCommand
  -> m (Dynamic t HistoryItem)
manageHistory' switchover runCmd = do
  window <- DOM.currentWindowUnchecked
  location <- Window.getLocation window
  history <- Window.getHistory window
  let getCurrentHistoryItem = HistoryItem
        <$> History.getState history
        <*> getLocationUri location
  item0 <- liftJSM getCurrentHistoryItem
  itemSetExternal' <- wrapDomEvent window (`DOM.on` DOM.popState) $ do
    e <- DOM.event
    HistoryItem
      <$> (SerializedScriptValue <$> PopStateEvent.getState e)
      <*> getLocationUri location
  let f :: (Bool, Maybe a) -> These a () -> (Maybe (Bool, Maybe a), Maybe a)
      f (switched, acc) = \case
        This change
          | switched -> (Nothing, Just change)
          | otherwise -> (Just (switched, Just change), Nothing)
        That () -> (Just (True, Nothing), acc)
        These change () -> (Just (True, Nothing), Just change)
  -- Accumulate the events before switchover
  (_, cmd') <- mapAccumMaybeB f (False, Nothing) $ align (leftmost [Left <$> runCmd, Right <$> itemSetExternal']) switchover
  let (itemSetInternal', itemSetExternal) = fanEither cmd'
  itemSetInternal <- performEvent $ ffor itemSetInternal' $ \cmd -> liftJSM $ do
    runHistoryCommand history cmd
    getCurrentHistoryItem
  holdDyn item0 $ leftmost [itemSetInternal, itemSetExternal]
--TODO: Handle title setting better
