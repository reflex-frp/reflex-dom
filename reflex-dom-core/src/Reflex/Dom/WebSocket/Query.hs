{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Dom.WebSocket.Query (runQuery, runWebSocketQuery) where

import Data.Default
import Control.Monad.Fix
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding
import Data.Aeson
import Reflex
import Reflex.Dom.WebSocket
import Reflex.Dom.WebSocket.Internal
import Foreign.JavaScript.TH
import Data.Maybe
import Data.JSString.Text
import Language.Javascript.JSaddle.Types (MonadJSM)

runWebSocketQuery :: (MonadJSM m, MonadJSM (Performable m), HasJSContext m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, Reflex t, ToJSON q, MonadFix m, Query q, FromJSON (QueryResult q), Additive q, Group q, Eq q)
         => Text
         -> QueryT t q m a
         -> m a
runWebSocketQuery url app = do
  rec ws <- textWebSocket url $ def { _webSocketConfig_send = pure . decodeUtf8 . LBS.toStrict . encode <$> reqE }
      let notifications = fromMaybe mempty . jsonDecode . textToJSString <$> _webSocket_recv ws
      (a, reqE) <- runQuery notifications app
  return a

runQuery :: (MonadHold t m, PostBuild t m, MonadFix m, Query q, Additive q, Group q, Eq q)
         => Event t (QueryResult q)
         -> QueryT t q m a
         -> m (a, Event t q)
runQuery notifications app = do
  postBuild <- getPostBuild
  rec (a, requestPatch) <- runQueryT app e
      requestUniq <- holdUniqDyn $ incrementalToDynamic requestPatch
      e <- fromNotifications requestUniq notifications
  let request = leftmost
        [ updated requestUniq
        , tag (current requestUniq) postBuild
        ]
  return (a, request)

fromNotifications :: (Query q, MonadHold t m, Reflex t, MonadFix m)
                  => Dynamic t q
                  -> Event t (QueryResult q)
                  -> m (Dynamic t (QueryResult q))
fromNotifications q = foldDyn (\(q', qr) v -> crop q' (qr `mappend` v)) mempty . attach (current q)
