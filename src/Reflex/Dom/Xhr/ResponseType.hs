{-# LANGUAGE DeriveDataTypeable #-}

module Reflex.Dom.Xhr.ResponseType where

import Data.Typeable
import Data.Text (Text)
import GHCJS.DOM.Blob (Blob)
import Data.ByteString (ByteString)

data XhrResponseType = XhrResponseType_Default
                     | XhrResponseType_ArrayBuffer
                     | XhrResponseType_Blob
                     | XhrResponseType_Text
   deriving (Show, Read, Eq, Ord, Typeable)

data XhrResponseBody = XhrResponseBody_Default Text
                     | XhrResponseBody_Text Text
                     | XhrResponseBody_Blob Blob
                     | XhrResponseBody_ArrayBuffer ByteString
    deriving (Eq)

