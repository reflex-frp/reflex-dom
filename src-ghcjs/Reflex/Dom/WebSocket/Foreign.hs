{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, TemplateHaskell, NoMonomorphismRestriction, EmptyDataDecls, RankNTypes, GADTs, RecursiveDo, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveDataTypeable, GeneralizedNewtypeDeriving, StandaloneDeriving, ConstraintKinds, UndecidableInstances, PolyKinds, AllowAmbiguousTypes #-}

module Reflex.Dom.WebSocket.Foreign where

import Prelude hiding (div, span, mapM, mapM_, concat, concatMap, all, sequence)

import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Data.ByteString (ByteString)
import Data.Text.Encoding
import qualified Data.ByteString as BS
import Foreign.Ptr
import GHCJS.Foreign
import GHCJS.Types

#define JS(name, js, type) foreign import javascript unsafe js name :: type

newtype JSWebSocket = JSWebSocket { unWebSocket :: JSRef JSWebSocket }

data JSByteArray
JS(extractByteArray, "new Uint8Array($1_1.buf, $1_2, $2)", Ptr a -> Int -> IO (JSRef JSByteArray))

JS(newWebSocket_, "(function() { var ws = new WebSocket($1); ws['binaryType'] = 'arraybuffer'; ws['onmessage'] = function(e){ $2(e['data']); }; ws['onclose'] = function(e){ $3(); }; return ws; })()", JSString -> JSFun (JSString -> IO ()) -> JSFun (IO ()) -> IO (JSRef JSWebSocket))

JS(webSocketSend_, "$1['send'](String.fromCharCode.apply(null, $2))", JSRef JSWebSocket -> JSRef JSByteArray -> IO ())

webSocketSend :: JSWebSocket -> ByteString -> IO ()
webSocketSend ws bs = BS.useAsCString bs $ \cStr -> do
  ba <- extractByteArray cStr $ BS.length bs
  webSocketSend_ (unWebSocket ws) ba

JS(getLocationHost_, "location.host", IO JSString)

getLocationHost :: FromJSString r => a -> IO r
getLocationHost _ = liftM fromJSString getLocationHost_

JS(getLocationProtocol_, "location.protocol", IO JSString)

getLocationProtocol :: FromJSString r => a -> IO r
getLocationProtocol _ = liftM fromJSString getLocationProtocol_

newWebSocket :: a -> String -> (ByteString -> IO ()) -> IO () -> IO JSWebSocket
newWebSocket _ url onMessage onClose = do
  onMessageFun <- syncCallback1 AlwaysRetain True $ onMessage <=< return . encodeUtf8 . fromJSString
  rec onCloseFun <- syncCallback AlwaysRetain True $ do
        release onMessageFun
        release onCloseFun
        onClose
  liftM JSWebSocket $ newWebSocket_ (toJSString url) onMessageFun onCloseFun

