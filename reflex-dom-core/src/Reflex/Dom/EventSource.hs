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

module Reflex.Dom.EventSource
  ( module Reflex.Dom.EventSource
  , jsonDecode
  ) where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Reflex.Class
import Reflex.Dom.EventSource.Foreign
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default
import Data.IORef
import Data.JSString.Text
import Data.Text
import Data.Text.Encoding
import Foreign.JavaScript.Utils (jsonDecode)
import GHCJS.DOM.Types (runJSM, askJSM, MonadJSM, liftJSM, JSM)
import GHCJS.Marshal

data EventSourceConfig t
   = EventSourceConfig { _eventSourceConfig_close :: Event t ()
                       , _eventSourceConfig_reconnect :: Bool
                       }

instance Reflex t => Default (EventSourceConfig t) where
  def = EventSourceConfig never True

type EventSource t = RawEventSource t ByteString

data RawEventSource t a
   = RawEventSource { _eventSource_recv :: Event t a
                    , _eventSource_open :: Event t ()
                    , _eventSource_error :: Event t ()
                      -- eror event does not carry any data and is always
                      -- followed by termination of the connection for details
                      -- see the close event
                    }

--eventSource :: (MonadJSM m, MonadJSM (Performable m), HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m) => Text -> EventSourceConfig t -> m (EventSource t)
--eventSource url config = eventSource' url config onBSMessage

eventSource' :: (MonadJSM m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m, PostBuild t m) => Text -> Text -> EventSourceConfig t -> (Either ByteString JSVal -> JSM b) -> m (RawEventSource t b)
eventSource' url eventType config onRawMessage = do
  (eRecv, onMessage) <- newTriggerEvent
  currentSocketRef <- liftIO $ newIORef Nothing
  (eOpen, triggerEOpen) <- newTriggerEventWithOnComplete
  (eError, triggerEError) <- newTriggerEvent
  --(eClose, triggerEClose) <- newTriggerEvent
  isOpen       <- liftIO newEmptyTMVarIO
  let onOpen = triggerEOpen () $ liftIO $ void $ atomically $ tryPutTMVar isOpen ()
      onError = triggerEError ()
      -- TODO Not sure how to handle close yet. The spec seems like the server
      -- never closes EventSource connections. But the connection still could
      -- die some other way and we probably want a way to recover.
      --onClose args = do
      --  liftIO $ triggerEClose args
      --  _ <- liftIO $ atomically $ tryTakeTMVar isOpen
      --  liftIO $ writeIORef currentSocketRef Nothing
      --  when (_eventSourceConfig_reconnect config) $ forkJSM $ do
      --    liftIO $ threadDelay 1000000
      --    start
      start = do
        ws <- newEventSource url eventType (onRawMessage >=> liftIO . onMessage) (liftIO onOpen) (liftIO onError)
        liftIO $ writeIORef currentSocketRef $ Just ws
        return ()
  performEvent_ . (liftJSM start <$) =<< getPostBuild
  performEvent_ $ ffor (_eventSourceConfig_close config) $ \_ -> liftJSM $ do
    mws <- liftIO $ readIORef currentSocketRef
    case mws of
      Nothing -> return ()
      Just ws -> closeEventSource ws

  return $ RawEventSource eRecv eOpen eError

textEventSource
  :: (MonadJSM m, MonadJSM (Performable m), PostBuild t m, TriggerEvent t m, PerformEvent t m)
  => Text
  -> Text
  -> EventSourceConfig t
  -> m (RawEventSource t Text)
textEventSource url eventType cfg = eventSource' url eventType cfg (either (return . decodeUtf8) fromJSValUnchecked)

jsonEventSource
  :: (FromJSON a, MonadJSM m, MonadJSM (Performable m), PostBuild t m, TriggerEvent t m, PerformEvent t m)
  => Text
  -> Text
  -> EventSourceConfig t
  -> m (RawEventSource t (Maybe a))
jsonEventSource url eventType cfg = do
  ws <- textEventSource url eventType cfg
  return ws { _eventSource_recv = jsonDecode . textToJSString <$> _eventSource_recv ws }

forkJSM :: JSM () -> JSM ()
forkJSM a = do
  jsm <- askJSM
  void $ liftIO $ forkIO $ runJSM a jsm

#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''EventSourceConfig
makeLensesWith (lensRules & simpleLenses .~ True) ''RawEventSource
#else

eventSourceConfig_close :: Lens' (EventSourceConfig t) (Event t ())
eventSourceConfig_close f (EventSourceConfig x1 x2) = (\y -> EventSourceConfig y x2) <$> f x1
{-# INLINE eventSourceConfig_close #-}

eventSourceConfig_reconnect :: Lens' (EventSourceConfig t) Bool
eventSourceConfig_reconnect f (EventSourceConfig x1 x2) = (\y -> EventSourceConfig x1 y) <$> f x2
{-# INLINE eventSourceConfig_reconnect #-}

eventSource_recv :: Lens' (RawEventSource t a) (Event t a)
eventSource_recv f (RawEventSource x1 x2 x3) = (\y -> RawEventSource y x2 x3) <$> f x1
{-# INLINE eventSource_recv #-}

eventSource_open :: Lens' (RawEventSource t a) (Event t ())
eventSource_open f (RawEventSource x1 x2 x3) = (\y -> RawEventSource x1 y x3) <$> f x2
{-# INLINE eventSource_open #-}

eventSource_error :: Lens' (RawEventSource t a) (Event t ())
eventSource_error f (RawEventSource x1 x2 x3) = (\y -> RawEventSource x1 x2 y) <$> f x3
{-# INLINE eventSource_error #-}

#endif
