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

import Reflex
import Reflex.Dom.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Dom.WebSocket.Foreign

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

data WebSocketConfig t
   = WebSocketConfig { _webSocketConfig_send :: Event t [ByteString]
                     }

instance Reflex t => Default (WebSocketConfig t) where
  def = WebSocketConfig never

data WebSocket t
   = WebSocket { _webSocket_recv :: Event t ByteString
               , _webSocket_open :: Event t ()
               }

webSocket :: (MonadIO m, MonadIO (Performable m), HasWebView m, PerformEvent t m, TriggerEvent t m, PostBuild t m) => Text -> WebSocketConfig t -> m (WebSocket t)
webSocket url config = do
  wv <- fmap unWebViewSingleton askWebView
  (eRecv, onMessage) <- newTriggerEvent
  currentSocketRef <- liftIO $ newIORef Nothing
  --TODO: Disconnect if value no longer needed
  (eOpen, triggerEOpen) <- newTriggerEventWithOnComplete
  payloadQueue <- liftIO newTQueueIO
  isOpen       <- liftIO newEmptyTMVarIO
  let onOpen = triggerEOpen () $ do
        liftIO $ void $ atomically $ tryPutTMVar isOpen ()
      --TODO: Is the fork necessary, or do event handlers run in their own threads automatically?
      onClose = void $ forkIO $ do
        _ <- liftIO $ atomically $ tryTakeTMVar isOpen
        liftIO $ writeIORef currentSocketRef Nothing
        liftIO $ threadDelay 1000000
        start
      start = do
        ws <- liftIO $ newWebSocket wv url onMessage onOpen onClose
        liftIO $ writeIORef currentSocketRef $ Just ws
        return ()
  performEvent_ . (liftIO start <$) =<< getPostBuild
  performEvent_ $ ffor (_webSocketConfig_send config) $ \payloads -> forM_ payloads $ \payload ->
    liftIO $ atomically $ writeTQueue payloadQueue payload

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
  return $ WebSocket eRecv eOpen

makeLensesWith (lensRules & simpleLenses .~ True) ''WebSocketConfig
