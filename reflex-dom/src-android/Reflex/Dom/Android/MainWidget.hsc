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
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

#include "MainWidget.h"


startMainWidget :: HaskellActivity -> ByteString -> JSM () -> IO ()
startMainWidget a url jsm = do
  --TODO: Find a way to eventually release this
  executorRef <- newIORef $ error "startMainWidget: executor not created yet"
  let go batch = do
        executor <- readIORef executorRef
        BS.useAsCString (LBS.toStrict $ "runJSaddleBatch(" <> encode batch <> ");") $ \cstr -> do
          runJS executor cstr
  (processResult, processSyncResult, start) <- runJavaScript go jsm
  callbacks <- new <=< jsaddleCallbacksToPtrs $ JSaddleCallbacks
    { _jsaddleCallbacks_jsaddleStart = void $ forkIO start
    , _jsaddleCallbacks_jsaddleResult = \s -> do
        case decode $ LBS.fromStrict s of
          Nothing -> error $ "jsaddle message decode failed: " <> show s
          Just r -> processResult r
    , _jsaddleCallbacks_jsaddleSyncResult = \s -> do
        case decode $ LBS.fromStrict s of
          Nothing -> error $ "jsaddle message decode failed: " <> show s
          Just r -> LBS.toStrict . encode <$> processSyncResult r
    , _jsaddleCallbacks_jsaddleJsData = LBS.toStrict $ ghcjsHelpers <> "\
        \runJSaddleBatch = (function() {\n\
        \ " <> initState <> "\n\
        \ return function(batch) {\n\
        \ " <> runBatch (\a -> "jsaddle.postMessage(JSON.stringify(" <> a <> "));")
                  (Just (\a -> "JSON.parse(jsaddle.syncMessage(JSON.stringify(" <> a <> ")))")) <> "\
        \ };\n\
        \})();\n\
        \jsaddle.postReady();\n"
    }
  BS.useAsCString url $ \curl -> do
    writeIORef executorRef =<< startMainWidget_ a curl callbacks

newtype JSExecutor = JSExecutor { unJSExecutor :: Ptr JSExecutor }

foreign import ccall safe "Reflex_Dom_Android_MainWidget_start" startMainWidget_ :: HaskellActivity -> CString -> Ptr JSaddleCallbacksPtrs -> IO JSExecutor

foreign import ccall safe "Reflex_Dom_Android_MainWidget_runJS" runJS :: JSExecutor -> CString -> IO ()

data JSaddleCallbacks = JSaddleCallbacks
  { _jsaddleCallbacks_jsaddleStart :: IO ()
  , _jsaddleCallbacks_jsaddleResult :: ByteString -> IO ()
  , _jsaddleCallbacks_jsaddleSyncResult :: ByteString -> IO ByteString
  , _jsaddleCallbacks_jsaddleJsData :: ByteString
  }

data JSaddleCallbacksPtrs = JSaddleCallbacksPtrs
  { _jsaddleCallbacksPtrs_jsaddleStart :: !(FunPtr (IO ()))
  , _jsaddleCallbacksPtrs_jsaddleResult :: !(FunPtr (CString -> IO ()))
  , _jsaddleCallbacksPtrs_jsaddleSyncResult :: !(FunPtr (CString -> IO CString))
  , _jsaddleCallbacksPtrs_jsaddleJsData :: !CString
  }

foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "wrapper" wrapCStringIO :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))
foreign import ccall "wrapper" wrapCStringIOCString :: (CString -> IO CString) -> IO (FunPtr (CString -> IO CString))

newCStringFromByteString :: ByteString -> IO CString
newCStringFromByteString bs = BSU.unsafeUseAsCStringLen bs $ \(src, len) -> do
  dest <- mallocArray0 len
  copyArray dest src len
  poke (advancePtr dest len) 0
  return dest

jsaddleCallbacksToPtrs :: JSaddleCallbacks -> IO JSaddleCallbacksPtrs
jsaddleCallbacksToPtrs jc = JSaddleCallbacksPtrs
  <$> wrapIO (_jsaddleCallbacks_jsaddleStart jc)
  <*> wrapCStringIO (\cstr -> _jsaddleCallbacks_jsaddleResult jc =<< BS.packCString cstr)
  <*> wrapCStringIOCString (\cstr -> newCStringFromByteString =<< _jsaddleCallbacks_jsaddleSyncResult jc =<< BS.packCString cstr)
  <*> newCStringFromByteString (_jsaddleCallbacks_jsaddleJsData jc)

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
