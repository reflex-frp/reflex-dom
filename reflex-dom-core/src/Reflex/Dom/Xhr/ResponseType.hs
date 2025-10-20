module Reflex.Dom.Xhr.ResponseType where

import Data.ByteString (ByteString)
import Data.Text (Text)
import GHCJS.DOM.Blob (Blob)

data XhrResponseType
  = XhrResponseType_Default
  | XhrResponseType_ArrayBuffer
  | XhrResponseType_Blob
  | XhrResponseType_Text
  deriving (Show, Read, Eq, Ord)

data XhrResponseBody
  = XhrResponseBody_Default Text
  | XhrResponseBody_Text Text
  | XhrResponseBody_Blob Blob
  | XhrResponseBody_ArrayBuffer ByteString
