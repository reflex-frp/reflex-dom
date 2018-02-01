module Reflex.Dom.WebSocket.Internal where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.JSString (JSString)
import Data.JSString.Text
import Data.Text.Encoding

jsonDecode :: FromJSON a => JSString -> Maybe a
jsonDecode = decode . LBS.fromStrict . encodeUtf8 . textFromJSString
