{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances, PolyKinds, AllowAmbiguousTypes #-}

module Reflex.Dom.WebSocket where

import Prelude hiding (div, span, mapM, mapM_, concat, concatMap, all, sequence)

import Reflex
import Reflex.Host.Class
import Reflex.Dom.Class
import Reflex.Dom.WebSocket.Foreign

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (catch, SomeException)
import Control.Lens
import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Monad.State
import Control.Arrow (second)
import Data.Maybe (isJust)
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
               , _webSocket_open :: Event t ()
               }

webSocket :: forall t m. (HasWebView m, MonadWidget t m) => String -> WebSocketConfig t -> m (WebSocket t)
webSocket url config = do
  wv <- askWebView
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  (eRecv, eRecvTriggerRef) <- newEventWithTriggerRef
  currentSocketRef <- liftIO $ newIORef Nothing
  --TODO: Disconnect if value no longer needed
  (eOpen, eOpenTriggerRef) <- newEventWithTriggerRef
  payloadQueue <- liftIO newTQueueIO
  isOpen       <- liftIO newEmptyTMVarIO
  let onMessage :: ByteString -> IO ()
      onMessage m = postGui $ do
        mt <- readRef eRecvTriggerRef
        forM_ mt $ \t -> runWithActions [t :=> Identity m]
      onOpen = postGui $ do
        mt <- readRef eOpenTriggerRef
        forM_ mt $ \t -> runWithActions [t :=> Identity ()]
        liftIO $ void $ atomically $ tryPutTMVar isOpen ()
      --TODO: Is the fork necessary, or do event handlers run in their own threads automatically?
      onClose = void $ forkIO $ do
        liftIO $ atomically $ tryTakeTMVar isOpen
        liftIO $ writeIORef currentSocketRef Nothing
        liftIO $ threadDelay 1000000
        start
      start = do
        ws <- liftIO $ newWebSocket wv url onMessage onOpen onClose
        liftIO $ writeIORef currentSocketRef $ Just ws
        return ()
  schedulePostBuild $ liftIO start
  performEvent_ $ ffor (_webSocketConfig_send config) $ \payloads -> forM_ payloads $ \payload ->
    liftIO $ atomically $ writeTQueue payloadQueue payload

  liftIO $ forkIO $ forever $ do
    payload <- atomically $ do
      pl     <- readTQueue payloadQueue
      open   <- tryReadTMVar isOpen
      if isJust open then return pl else retry

    mws <- liftIO $ readIORef currentSocketRef
    succ <- case mws of
      Nothing -> return False
      Just ws -> liftIO ((webSocketSend ws payload >> return True)
                         `catch`
                         (\(_ :: SomeException) -> return False))
    unless succ $ atomically $ unGetTQueue payloadQueue payload
  return $ WebSocket eRecv eOpen

makeLensesWith (lensRules & simpleLenses .~ True) ''WebSocketConfig
