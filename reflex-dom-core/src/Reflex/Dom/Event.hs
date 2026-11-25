{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Reflex.Dom.Event where

import Data.Aeson (toJSON)
import Data.Default (Default, def)
import Data.Text (Text)
import GHCJS.DOM.EventM (EventM, EventName, SaferEventListener(..), newListenerAsync, newListenerSync, releaseListener, removeListener)
import qualified Data.Map as Map
import GHCJS.DOM.Types
  ( AddEventListenerOptions (..), DOM, EventListener (..), IsEvent, IsEventTarget, JSM, ToJSVal
  , toAddEventListenerOptionsOrBool, toJSVal
  )
import GHCJS.DOM.EventTarget (addEventListener)
import GHCJS.DOM.EventTargetClosures (EventName (..), eventNameString)

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLenses)
#endif

-- | Options for 'addEventListener'.
--
-- C.f. @options@ in https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener#Parameters
data EventListenerOptions = EventListenerOptions
  { _eventListenerOptions_capture :: !Bool
  , _eventListenerOptions_passive :: !Bool
  } deriving (Eq, Ord, Show)

instance Default EventListenerOptions where
  def = defaultEventListenerOptions
  {-# INLINABLE def #-}

instance ToJSVal EventListenerOptions where
  toJSVal opts = toJSVal $ toJSON $ Map.fromList
    ( [ ("capture", _eventListenerOptions_capture opts)
      , ("passive", _eventListenerOptions_passive opts)
      ] :: [(Text, Bool)]
    )
  {-# INLINABLE toJSVal #-}

-- | Default 'EventListenerOptions' where both @capture@ and @passive@ are disabled.
defaultEventListenerOptions :: EventListenerOptions
defaultEventListenerOptions = EventListenerOptions False False
{-# INLINABLE defaultEventListenerOptions #-}

-- | Like 'GHCJS.DOM.EventM.on' but normalizes defaults to be the same across all browsers.
--
-- See 'normalizedDefaultEventListenerOptions' for how normalization is done.
on :: (IsEventTarget t, IsEvent e) => t -> EventName t e -> EventM t e () -> DOM (DOM ())
on target eventName@(EventNameSyncDefault _) = onSyncWithOptions (normalizedDefaultEventListenerOptions eventName) target eventName
on target eventName@(EventNameAsyncDefault _) = onAsyncWithOptions (normalizedDefaultEventListenerOptions eventName) target eventName
{-# INLINABLE on #-}

-- | Per-event default for 'EventListenerOptions' that is the same for all browsers.
--
--     * @capture@ is always 'False'.
--
--     * @passive@ is always 'False' unless the event is @touchstart@ or @touchmove@.
--       See https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener#Browser_compatibility
--       for differences.
normalizedDefaultEventListenerOptions :: EventName t e -> EventListenerOptions
normalizedDefaultEventListenerOptions e = case eventNameString e of
  name | name == "touchstart" || name == "touchmove" -> defaultEventListenerOptions { _eventListenerOptions_passive = True }
  _ -> defaultEventListenerOptions
{-# INLINABLE normalizedDefaultEventListenerOptions #-}

-- | Like 'GHCJS.DOM.EventM.on' but with @addEventListener@ options.
onWithOptions :: (IsEventTarget t, IsEvent e) => EventListenerOptions -> t -> EventName t e -> EventM t e () -> DOM (DOM ())
onWithOptions opts target eventName@(EventNameSyncDefault _) = onSyncWithOptions opts target eventName
onWithOptions opts target eventName@(EventNameAsyncDefault _) = onAsyncWithOptions opts target eventName
{-# INLINABLE onWithOptions #-}

-- | Like 'GHCJS.DOM.EventM.onSync' but with @addEventListener@ options.
onSyncWithOptions :: (IsEventTarget t, IsEvent e) => EventListenerOptions -> t -> EventName t e -> EventM t e () -> DOM (DOM ())
onSyncWithOptions opts target eventName callback = do
  l <- newListenerSync callback
  addListenerWithOptions target eventName l opts
  return $ do
    removeListener target eventName l False
    releaseListener l
{-# INLINABLE onSyncWithOptions #-}

-- | Like 'GHCJS.DOM.EventM.onAsync' but with @addEventListener@ options.
onAsyncWithOptions :: (IsEventTarget t, IsEvent e) => EventListenerOptions -> t -> EventName t e -> EventM t e () -> JSM (JSM ())
onAsyncWithOptions opts target eventName callback = do
  l <- newListenerAsync callback
  addListenerWithOptions target eventName l opts
  return $ do
    removeListener target eventName l False
    releaseListener l
{-# INLINABLE onAsyncWithOptions #-}

-- | Like 'GHCJS.DOM.EventM.addListener' but with @addEventListener@ options.
addListenerWithOptions :: (IsEventTarget t, IsEvent e) => t -> EventName t e -> SaferEventListener t e -> EventListenerOptions -> DOM ()
addListenerWithOptions target eventName l opts = do
  raw <- EventListener <$> toJSVal l
  optsVal <- toAddEventListenerOptionsOrBool . AddEventListenerOptions <$> toJSVal opts
  addEventListener target (eventNameString eventName) (Just raw) optsVal
{-# INLINABLE addListenerWithOptions #-}


#ifdef USE_TEMPLATE_HASKELL
makeLenses ''EventListenerOptions
#endif
