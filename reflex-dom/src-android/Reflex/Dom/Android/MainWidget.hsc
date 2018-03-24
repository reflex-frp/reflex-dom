{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Reflex.Dom.Android.MainWidget
  ( startMainWidget
  ) where

import Android.HaskellActivity
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef
import Data.Monoid
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Language.Javascript.JSaddle (JSM, runJSM)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (jsaddleCoreJs, ghcjsHelpers)
import Network.Wai as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Exception
import Network.WebSockets (sendPing, sendTextData, acceptRequest, receiveDataMessage)
import qualified Network.WebSockets as WS
import Network.HTTP.Types (status404)
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)

#include "MainWidget.h"


waitTillClosed :: WS.Connection -> IO ()
waitTillClosed conn = ignore `handle` go 1
  where
    go :: Int -> IO ()
    go i = do
      threadDelay (1 * 1000 * 1000)
      WS.sendPing conn (T.pack $ show i)
      go (i + 1)
    ignore e = case fromException e of
      Just async -> throwIO (async :: AsyncException)
      Nothing    -> return ()


notFound :: Wai.Response
notFound = responseLBS
  status404
  [("Content-Type", "text/plain")]
  "404 - Not Found"

startMainWidget :: HaskellActivity -> ByteString -> JSM () -> IO ()
startMainWidget a url entryPoint = do
  putStrLn "a"
  connRef <- newIORef $ error "jsaddle websocket connection not established"
  putStrLn "b"
  (processResult, processSyncResult, env) <- runJavaScript $ \req -> do
    conn <- readIORef connRef
    sendTextData conn $ encode req
  putStrLn "c"
  let handleResult s = do
        case decode $ LBS.fromStrict s of
          Nothing -> error $ "jsaddle message decode failed: " <> show s
          Just r -> processResult r
  --TODO: Auto-select port
  --TODO: Do we need to worry about other apps connecting?  If so, only accept one connection, and secure it with a nonce or something like that.
  let port = 23578
  let wsApp pendingConn = do
        conn <- acceptRequest pendingConn
        writeIORef connRef conn
        _ <- forkIO $ forever $ receiveDataMessage conn >>= \case
          WS.Text t -> case decode t of
            Nothing -> putStrLn $ "jsaddle response decode failed: " <> show t
            Just r  -> do
              result <- try $ processResult r
              case result of
                Left e@(SomeException _) -> putStrLn $ "jsaddle processResult failed: " <> show e
                Right _ -> return ()
          _ -> error "jsaddle WebSocket unexpected binary data"
        try (runJSM entryPoint env) >>= \case
          Left e@(SomeException _) -> putStrLn $ "jsaddle done: left: " <> show e
          Right _ -> putStrLn $ "jsaddle done: right"
        waitTillClosed conn
  putStrLn "d"
  _ <- forkIO $ runSettings (setPort port (setTimeout 3600 defaultSettings)) $
    websocketsOr defaultConnectionOptions wsApp $ \_ sendResponse -> sendResponse notFound
  putStrLn "e"
  callbacks <- new <=< jsaddleCallbacksToPtrs $ JSaddleCallbacks
    { _jsaddleCallbacks_jsaddleStart = return ()
    , _jsaddleCallbacks_jsaddleResult = \_ -> return ()
    , _jsaddleCallbacks_jsaddleSyncResult = \s -> do
        case decode $ LBS.fromStrict s of
          Nothing -> error $ "jsaddle message decode failed: " <> show s
          Just r -> LBS.toStrict . encode <$> processSyncResult r
    , _jsaddleCallbacks_jsaddleJsData = LBS.toStrict $ jsaddleCoreJs <> ghcjsHelpers <> "\
        \(function() {\n\
        \  var ws = new WebSocket('ws://localhost:" <> LBS.fromStrict (encodeUtf8 $ T.pack $ show port) <> "');\n\
        \  ws.onmessage = function(msg) {\n\
        \    core.processReq(JSON.parse(msg.data));\n\
        \  };\n\
        \  var core = jsaddle(window, function(req) {\n\
        \    ws.send(JSON.stringify(req));\n\
        \  }, function(v) {\n\
        \    return JSON.parse(jsaddleCallbacks.syncMessage(JSON.stringify(v)));\n\
        \  });\n\
        \  return ;\n\
        \})();\n\
        \"
    }
  putStrLn "f"
  BS.useAsCString url $ \curl -> do
    startMainWidget_ a curl callbacks
  putStrLn "g"

newtype JSExecutor = JSExecutor { unJSExecutor :: Ptr JSExecutor }

foreign import ccall safe "Reflex_Dom_Android_MainWidget_start" startMainWidget_ :: HaskellActivity -> CString -> Ptr JSaddleCallbacksPtrs -> IO ()

foreign import ccall safe "Reflex_Dom_Android_MainWidget_runJS" runJS :: JSExecutor -> CString -> CSize -> IO ()

data JSaddleCallbacks = JSaddleCallbacks
  { _jsaddleCallbacks_jsaddleStart :: IO ()
  , _jsaddleCallbacks_jsaddleResult :: ByteString -> IO ()
  , _jsaddleCallbacks_jsaddleSyncResult :: ByteString -> IO ByteString
  , _jsaddleCallbacks_jsaddleJsData :: ByteString
  }

data JSaddleCallbacksPtrs = JSaddleCallbacksPtrs
  { _jsaddleCallbacksPtrs_jsaddleStart :: !(FunPtr (IO ()))
  , _jsaddleCallbacksPtrs_jsaddleResult :: !(FunPtr (CString -> CSize -> IO ()))
  , _jsaddleCallbacksPtrs_jsaddleSyncResult :: !(FunPtr (CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()))
  , _jsaddleCallbacksPtrs_jsaddleJsData :: !CString
  }

foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "wrapper" wrapCStringCSizeIO :: (CString -> CSize -> IO ()) -> IO (FunPtr (CString -> CSize -> IO ()))
foreign import ccall "wrapper" wrapCStringCSizeCStringCSizeIO :: (CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()) -> IO (FunPtr (CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()))

newCStringFromByteString :: ByteString -> IO (CString, CSize)
newCStringFromByteString bs = BSU.unsafeUseAsCStringLen bs $ \(src, len) -> do
  dest <- mallocArray0 len
  copyArray dest src len
  poke (advancePtr dest len) 0
  return (dest, fromIntegral len)

jsaddleCallbacksToPtrs :: JSaddleCallbacks -> IO JSaddleCallbacksPtrs
jsaddleCallbacksToPtrs jc = JSaddleCallbacksPtrs
  <$> wrapIO (_jsaddleCallbacks_jsaddleStart jc)
  <*> wrapCStringCSizeIO (\cstr len -> _jsaddleCallbacks_jsaddleResult jc =<< BS.packCStringLen (cstr, fromIntegral len))
  <*> wrapCStringCSizeCStringCSizeIO (\cstr len result resultLen -> do
                                         inStr <- BS.packCStringLen (cstr, fromIntegral len)
                                         outStr <- _jsaddleCallbacks_jsaddleSyncResult jc inStr
                                         (outStrBytes, outStrLen) <- newCStringFromByteString outStr
                                         poke result outStrBytes
                                         poke resultLen outStrLen)
  <*> (fst <$> newCStringFromByteString (_jsaddleCallbacks_jsaddleJsData jc))

instance Storable JSaddleCallbacksPtrs where
  sizeOf _ = #{size JSaddleCallbacks}
  alignment _ = #{alignment JSaddleCallbacks}
  poke p jc = do
    #{poke JSaddleCallbacks, jsaddleStart} p $ _jsaddleCallbacksPtrs_jsaddleStart jc
    #{poke JSaddleCallbacks, jsaddleResult} p $ _jsaddleCallbacksPtrs_jsaddleResult jc
    #{poke JSaddleCallbacks, jsaddleSyncResult} p $ _jsaddleCallbacksPtrs_jsaddleSyncResult jc
    #{poke JSaddleCallbacks, jsaddleJsData} p $ _jsaddleCallbacksPtrs_jsaddleJsData jc
  peek p = JSaddleCallbacksPtrs
    <$> #{peek JSaddleCallbacks, jsaddleStart} p
    <*> #{peek JSaddleCallbacks, jsaddleResult} p
    <*> #{peek JSaddleCallbacks, jsaddleSyncResult} p
    <*> #{peek JSaddleCallbacks, jsaddleJsData} p
