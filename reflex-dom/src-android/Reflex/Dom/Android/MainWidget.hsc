{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
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

#include "MainWidget.h"


startMainWidget :: HaskellActivity -> ByteString -> JSM () -> IO ()
startMainWidget a url jsm = do
  --TODO: Find a way to eventually release this
  executorRef <- newIORef $ error "startMainWidget: executor not created yet"
  let go batch = do
        executor <- readIORef executorRef
        BSU.unsafeUseAsCStringLen (LBS.toStrict $ "runJSaddleBatch(" <> encode batch <> ");") $ \(cstr, len) -> do
          runJS executor cstr (fromIntegral len)
  (processResult, processSyncResult, env) <- runJavaScript go
  callbacks <- new <=< jsaddleCallbacksToPtrs $ JSaddleCallbacks
    { _jsaddleCallbacks_jsaddleStart = void $ forkIO $ runJSM jsm env
    , _jsaddleCallbacks_jsaddleResult = \s -> do
        case decode $ LBS.fromStrict s of
          Nothing -> error $ "jsaddle message decode failed: " <> show s
          Just r -> processResult r
    , _jsaddleCallbacks_jsaddleSyncResult = \s -> do
        case decode $ LBS.fromStrict s of
          Nothing -> error $ "jsaddle message decode failed: " <> show s
          Just r -> LBS.toStrict . encode <$> processSyncResult r
    , _jsaddleCallbacks_jsaddleJsData = LBS.toStrict $ jsaddleCoreJs <> ghcjsHelpers <> "\
        \runJSaddleBatch = (function() {\n\
        \ var core = jsaddleCoreJs(window, function(a) {\n\
        \     jsaddle.postMessage(JSON.stringify(a));\n\
        \   }, function(a) {\n\
        \     return JSON.parse(jsaddle.syncMessage(JSON.stringify(a)));\n\
        \ }, 20);\n\
        \ return function(batch) {\n\
        \  core.processReqs(batch);\n\
        \ };\n\
        \})();\n\
        \jsaddle.postReady();\n"
    }
  BS.useAsCString url $ \curl -> do
    writeIORef executorRef =<< startMainWidget_ a curl callbacks

newtype JSExecutor = JSExecutor { unJSExecutor :: Ptr JSExecutor }

foreign import ccall safe "Reflex_Dom_Android_MainWidget_start" startMainWidget_ :: HaskellActivity -> CString -> Ptr JSaddleCallbacksPtrs -> IO JSExecutor

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
