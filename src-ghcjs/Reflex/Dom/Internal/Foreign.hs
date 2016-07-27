{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module Reflex.Dom.Internal.Foreign ( module Reflex.Dom.Internal.Foreign
                                   , runWebGUI
                                   ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign
import Foreign.C
import qualified GHCJS.Buffer as JS
import GHCJS.DOM
import GHCJS.DOM.Types
import qualified GHCJS.Marshal.Pure as JS
import GHCJS.Types
import JavaScript.TypedArray.ArrayBuffer as JS

quitWebView :: WebView -> IO ()
quitWebView = error "quitWebView: unimplemented in GHCJS"

foreign import javascript unsafe "location['host']" getLocationHost_ :: IO JSString

getLocationHost :: FromJSString r => a -> IO r
getLocationHost _ = fmap fromJSString getLocationHost_

foreign import javascript unsafe "location['protocol']" getLocationProtocol_ :: IO JSString

getLocationProtocol :: FromJSString r => a -> IO r
getLocationProtocol _ = fmap fromJSString getLocationProtocol_

withWebViewContext :: a -> (a -> IO b) -> IO b
withWebViewContext a f = f a

foreign import javascript safe "new DataView($3,$1,$2)" js_dataView :: Int -> Int -> JSVal -> JSVal

foreign import javascript unsafe "new Uint8Array($1_1['buf'], $1_2, $2)['buffer']" extractByteArray :: Ptr CChar -> Int -> IO JSVal

bsToArrayBuffer :: a -> ByteString -> IO JSVal
bsToArrayBuffer _ bs = BS.useAsCString bs $ \cStr -> do
  extractByteArray cStr $ BS.length bs

bsFromArrayBuffer :: a -> JSVal -> IO ByteString
bsFromArrayBuffer _ ab = fmap (JS.toByteString 0 Nothing . JS.createFromArrayBuffer) $ JS.unsafeFreeze $ JS.pFromJSVal ab
