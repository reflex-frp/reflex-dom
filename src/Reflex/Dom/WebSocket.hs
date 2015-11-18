{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances, PolyKinds, AllowAmbiguousTypes #-}

module Reflex.Dom.WebSocket where

import Prelude hiding (div, span, mapM, mapM_, concat, concatMap, all, sequence)

import Reflex
import Reflex.Host.Class
import Reflex.Dom.Class
import Reflex.Dom.WebSocket.Foreign

import Control.Concurrent
import Control.Lens
import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Default
import Data.Dependent.Map (DSum (..))
import Data.IORef

data WebSocketConfig t
   = WebSocketConfig { _webSocketConfig_send :: Event t [ByteString]
                     }

instance Reflex t => Default (WebSocketConfig t) where
  def = WebSocketConfig never

data WebSocket t
   = WebSocket { _webSocket_recv :: Event t ByteString
               }

webSocket :: forall t m. (HasWebView m, MonadWidget t m) => String -> WebSocketConfig t -> m (WebSocket t)
webSocket url config = do
  wv <- askWebView
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  (eRecv, eRecvTriggerRef) <- newEventWithTriggerRef
  currentSocketRef <- liftIO $ newIORef Nothing
  --TODO: Disconnect if value no longer needed
  let onMessage :: ByteString -> IO ()
      onMessage m = postGui $ do
        mt <- readRef eRecvTriggerRef
        forM_ mt $ \t -> runWithActions [t :=> m]
      start = do
        ws <- liftIO $ newWebSocket wv url onMessage $ do
          void $ forkIO $ do --TODO: Is the fork necessary, or do event handlers run in their own threads automatically?
            liftIO $ writeIORef currentSocketRef Nothing
            liftIO $ threadDelay 1000000
            start
        liftIO $ writeIORef currentSocketRef $ Just ws
        return ()
  schedulePostBuild $ liftIO start
  performEvent_ $ ffor (_webSocketConfig_send config) $ \payloads -> forM_ payloads $ \payload -> do
    mws <- liftIO $ readIORef currentSocketRef
    case mws of
      Nothing -> return () -- Discard --TODO: should we do something better here? probably buffer it, since we handle reconnection logic; how do we verify that the server has received things?
      Just ws -> do
        liftIO $ webSocketSend ws payload
  return $ WebSocket eRecv

makeLensesWith (lensRules & simpleLenses .~ True) ''WebSocketConfig
