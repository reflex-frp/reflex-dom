{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Dom.WebSocket.Query (cropQueryT, runWebSocketQuery) where

import Data.Default
import Control.Monad.Fix
import Data.Text (Text)
import Data.Aeson
import Reflex
import Reflex.Dom.WebSocket
import Foreign.JavaScript.TH
import Data.Maybe
import Language.Javascript.JSaddle.Types (MonadJSM)

runWebSocketQuery :: (MonadJSM m, MonadJSM (Performable m), HasJSContext m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, Reflex t, ToJSON q, MonadFix m, Query q, FromJSON (QueryResult q), Additive q, Group q, Eq q)
                  => QueryT t q m a
                  -> Text -- ^ WebSocket url
                  -> m a
runWebSocketQuery app url = do
  postBuild <- getPostBuild
  rec ws <- jsonWebSocket url $ def { _webSocketConfig_send = pure <$> updatedRequest }
      (a, request) <- cropQueryT app $ fromMaybe mempty <$> _webSocket_recv ws
      let updatedRequest = leftmost [updated request, tag (current request) postBuild]
  return a

cropQueryT :: (Reflex t, MonadHold t m, MonadFix m, Query q, Additive q, Group q, Eq q)
           => QueryT t q m a
           -> Event t (QueryResult q)
           -> m (a, Dynamic t q)
cropQueryT app result = do
  rec (a, requestPatch) <- runQueryT app croppedResult
      requestUniq <- holdUniqDyn $ incrementalToDynamic requestPatch
      croppedResult <- cropDyn requestUniq result
  return (a, requestUniq)

cropDyn :: (Query q, MonadHold t m, Reflex t, MonadFix m) => Dynamic t q -> Event t (QueryResult q) -> m (Dynamic t (QueryResult q))
cropDyn q = foldDyn (\(q', qr) v -> crop q' (qr `mappend` v)) mempty . attach (current q)
