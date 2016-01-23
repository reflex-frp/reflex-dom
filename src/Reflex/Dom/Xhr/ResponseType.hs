{-# LANGUAGE DeriveDataTypeable #-}

module Reflex.Dom.Xhr.ResponseType where

import Data.Typeable

data XhrResponseType = XhrResponseType_Default
                     | XhrResponseType_ArrayBuffer
                     | XhrResponseType_Blob
                     | XhrResponseType_Document
                     | XhrResponseType_JSON
                     | XhrResponseType_Text
   deriving (Show, Read, Eq, Ord, Typeable)

