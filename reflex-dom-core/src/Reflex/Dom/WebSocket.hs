{-# LANGUAGE CPP #-}
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.WebSocket
  ( module Reflex.Dom.WebSocket
  , jsonDecode
  ) where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Reflex.Class
import Reflex.Dom.Class
import Reflex.Dom.WebSocket.Foreign
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Default
import Data.IORef
import Data.JSString.Text
import Data.Maybe (isJust)
import Data.Text
import Data.Text.Encoding
import Foreign.JavaScript.Utils (jsonDecode)
import GHCJS.DOM.Types (runJSM, askJSM, MonadJSM, liftJSM, JSM)
import GHCJS.DOM.WebSocket (getReadyState)
import GHCJS.Marshal
import qualified Language.Javascript.JSaddle.Monad as JS (catch)

data WebSocketConfig t a
   = WebSocketConfig { _webSocketConfig_send :: Event t [a]
                     , _webSocketConfig_close :: Event t (Word, Text)
                     , _webSocketConfig_reconnect :: Bool
                     , _webSocketConfig_protocols :: [Text]
                     }

instance Reflex t => Default (WebSocketConfig t a) where
  def = WebSocketConfig never never True []

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

webSocket :: (MonadJSM m, MonadJSM (Performable m), HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m, IsWebSocketMessage a) => Text -> WebSocketConfig t a -> m (WebSocket t)
webSocket url config = webSocket' url config onBSMessage

webSocket' :: (MonadJSM m, MonadJSM (Performable m), HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m, IsWebSocketMessage a) => Text -> WebSocketConfig t a -> (Either ByteString JSVal -> JSM b) -> m (RawWebSocket t b)
webSocket' url config onRawMessage = do
  wv <- fmap unJSContextSingleton askJSContext
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
        liftIO $ triggerEClose args
        _ <- liftIO $ atomically $ tryTakeTMVar isOpen
        liftIO $ writeIORef currentSocketRef Nothing
        when (_webSocketConfig_reconnect config) $ forkJSM $ do
          liftIO $ threadDelay 1000000
          start
      start = do
        ws <- newWebSocket wv url (_webSocketConfig_protocols config) (onRawMessage >=> liftIO . onMessage) (liftIO onOpen) (liftIO onError) onClose
        liftIO $ writeIORef currentSocketRef $ Just ws
        return ()
  performEvent_ . (liftJSM start <$) =<< getPostBuild
  performEvent_ $ ffor (_webSocketConfig_send config) $ \payloads -> forM_ payloads $ \payload ->
    liftIO $ atomically $ writeTQueue payloadQueue payload
  performEvent_ $ ffor (_webSocketConfig_close config) $ \(code,reason) -> liftJSM $ do
    mws <- liftIO $ readIORef currentSocketRef
    case mws of
      Nothing -> return ()
      Just ws -> closeWebSocket ws (fromIntegral code) reason

  ctx <- askJSM
  _ <- liftIO $ forkIO $ forever $ do
    payload <- atomically $ do
      pl     <- readTQueue payloadQueue
      open   <- tryReadTMVar isOpen
      if isJust open then return pl else retry

    mws <- liftIO $ readIORef currentSocketRef
    success <- case mws of
      Nothing -> return False
      Just ws -> flip runJSM ctx $ do
        rs <- getReadyState $ unWebSocket ws
        if rs == 1
          then (webSocketSend ws payload >> return True) `JS.catch` (\(_ :: SomeException) -> return False)
          else return False
    unless success $ atomically $ unGetTQueue payloadQueue payload
  return $ RawWebSocket eRecv eOpen eError eClose

textWebSocket :: (IsWebSocketMessage a, MonadJSM m, MonadJSM (Performable m), HasJSContext m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, Reflex t) => Text -> WebSocketConfig t a -> m (RawWebSocket t Text)
textWebSocket url cfg = webSocket' url cfg (either (return . decodeUtf8) fromJSValUnchecked)

jsonWebSocket :: (ToJSON a, FromJSON b, MonadJSM m, MonadJSM (Performable m), HasJSContext m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, Reflex t) => Text -> WebSocketConfig t a -> m (RawWebSocket t (Maybe b))
jsonWebSocket url cfg = do
  ws <- textWebSocket url $ cfg { _webSocketConfig_send = fmap (decodeUtf8 . toStrict . encode) <$> _webSocketConfig_send cfg }
  return ws { _webSocket_recv = jsonDecode . textToJSString <$> _webSocket_recv ws }

forkJSM :: JSM () -> JSM ()
forkJSM a = do
  jsm <- askJSM
  void $ liftIO $ forkIO $ runJSM a jsm

#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''WebSocketConfig
makeLensesWith (lensRules & simpleLenses .~ True) ''RawWebSocket
#else

webSocketConfig_send :: Lens' (WebSocketConfig t a) (Event t [a])
webSocketConfig_send f (WebSocketConfig x1 x2 x3 x4) = (\y -> WebSocketConfig y x2 x3 x4) <$> f x1
{-# INLINE webSocketConfig_send #-}

webSocketConfig_close :: Lens' (WebSocketConfig t a) (Event t (Word, Text))
webSocketConfig_close f (WebSocketConfig x1 x2 x3 x4) = (\y -> WebSocketConfig x1 y x3 x4) <$> f x2
{-# INLINE webSocketConfig_close #-}

webSocketConfig_reconnect :: Lens' (WebSocketConfig t a) Bool
webSocketConfig_reconnect f (WebSocketConfig x1 x2 x3 x4) = (\y -> WebSocketConfig x1 x2 y x4) <$> f x3
{-# INLINE webSocketConfig_reconnect #-}

webSocketConfig_protocols :: Lens' (WebSocketConfig t a) [Text]
webSocketConfig_protocols f (WebSocketConfig x1 x2 x3 x4) = (\y -> WebSocketConfig x1 x2 x3 y) <$> f x4
{-# INLINE webSocketConfig_protocols #-}

webSocket_recv :: Lens' (RawWebSocket t a) (Event t a)
webSocket_recv f (RawWebSocket x1 x2 x3 x4) = (\y -> RawWebSocket y x2 x3 x4) <$> f x1
{-# INLINE webSocket_recv #-}

webSocket_open :: Lens' (RawWebSocket t a) (Event t ())
webSocket_open f (RawWebSocket x1 x2 x3 x4) = (\y -> RawWebSocket x1 y x3 x4) <$> f x2
{-# INLINE webSocket_open #-}

webSocket_error :: Lens' (RawWebSocket t a) (Event t ())
webSocket_error f (RawWebSocket x1 x2 x3 x4) = (\y -> RawWebSocket x1 x2 y x4) <$> f x3
{-# INLINE webSocket_error #-}

webSocket_close :: Lens' (RawWebSocket t a) (Event t (Bool, Word, Text))
webSocket_close f (RawWebSocket x1 x2 x3 x4) = (\y -> RawWebSocket x1 x2 x3 y) <$> f x4
{-# INLINE webSocket_close #-}

#endif
