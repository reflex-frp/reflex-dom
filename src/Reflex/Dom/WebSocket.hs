{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.WebSocket where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Reflex.Class
import Reflex.Dom.Class
import Reflex.Dom.WebSocket.Foreign
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Lens
import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Default
import Data.IORef
import Data.Maybe (isJust)
import Data.Text

data WebSocketConfig t a
   = WebSocketConfig { _webSocketConfig_send :: Event t [a]
                     , _webSocketConfig_close :: Event t (Word, Text)
                     , _webSocketConfig_reconnect :: Bool
                     }

instance Reflex t => Default (WebSocketConfig t a) where
  def = WebSocketConfig never never True

type WebSocket t = RawWebSocket t ByteString

data RawWebSocket t a
   = RawWebSocket { _webSocket_recv :: Event t a
                  , _webSocket_open :: Event t ()
                  , _webSocket_error :: Event t () -- eror event does not carry any data and is always
                                                   -- followed by termination of the connection
                                                   -- for details see the close event
                  , _webSocket_close :: Event t ( Bool -- wasClean
                                                , Word -- code
                                                , Text -- reason
                                                )
                  }

-- This can be used to send either binary or text messages for the same websocket connection
instance (IsWebSocketMessage a, IsWebSocketMessage b) => IsWebSocketMessage (Either a b) where
  webSocketSend jws (Left a) = webSocketSend jws a
  webSocketSend jws (Right a) = webSocketSend jws a


webSocket :: (MonadIO m, MonadIO (Performable m), HasWebView m, PerformEvent t m, TriggerEvent t m, PostBuild t m, IsWebSocketMessage a) => Text -> WebSocketConfig t a -> m (WebSocket t)
webSocket url config = webSocket' url config onBSMessage

webSocket' :: (MonadIO m, MonadIO (Performable m), HasWebView m, PerformEvent t m, TriggerEvent t m, PostBuild t m, IsWebSocketMessage a) => Text -> WebSocketConfig t a -> (Either ByteString JSVal -> b) -> m (RawWebSocket t b)
webSocket' url config onRawMessage = do
  wv <- fmap unWebViewSingleton askWebView
  (eRecv, onMessage) <- newTriggerEvent
  currentSocketRef <- liftIO $ newIORef Nothing
  (eOpen, triggerEOpen) <- newTriggerEventWithOnComplete
  (eError, triggerEError) <- newTriggerEvent
  (eClose, triggerEClose) <- newTriggerEvent
  payloadQueue <- liftIO newTQueueIO
  isOpen       <- liftIO newEmptyTMVarIO
  let onOpen = triggerEOpen () $ liftIO $ void $ atomically $ tryPutTMVar isOpen ()
      onError = triggerEError ()
      onClose args = do
        triggerEClose args
        --TODO: Is the fork necessary, or do event handlers run in their own threads automatically?
        void $ forkIO $ do
          _ <- liftIO $ atomically $ tryTakeTMVar isOpen
          liftIO $ writeIORef currentSocketRef Nothing
          when (_webSocketConfig_reconnect config) $ do
            liftIO $ threadDelay 1000000
            start
      start = do
        ws <- liftIO $ newWebSocket wv url (onMessage . onRawMessage) onOpen onError onClose
        liftIO $ writeIORef currentSocketRef $ Just ws
        return ()
  performEvent_ . (liftIO start <$) =<< getPostBuild
  performEvent_ $ ffor (_webSocketConfig_send config) $ \payloads -> forM_ payloads $ \payload ->
    liftIO $ atomically $ writeTQueue payloadQueue payload
  performEvent_ $ ffor (_webSocketConfig_close config) $ \(code,reason) -> liftIO $ do
    mws <- readIORef currentSocketRef
    case mws of
      Nothing -> return ()
      Just ws -> closeWebSocket ws (fromIntegral code) reason

  _ <- liftIO $ forkIO $ forever $ do
    payload <- atomically $ do
      pl     <- readTQueue payloadQueue
      open   <- tryReadTMVar isOpen
      if isJust open then return pl else retry

    mws <- liftIO $ readIORef currentSocketRef
    success <- case mws of
      Nothing -> return False
      Just ws -> liftIO ((webSocketSend ws payload >> return True)
                         `catch`
                         (\(_ :: SomeException) -> return False))
    unless success $ atomically $ unGetTQueue payloadQueue payload
  return $ RawWebSocket eRecv eOpen eError eClose

makeLensesWith (lensRules & simpleLenses .~ True) ''WebSocketConfig
