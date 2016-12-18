{-# LANGUAGE DeriveDataTypeable #-}

module Reflex.Dom.Xhr.Exception where

import Control.Exception (Exception (..))
import Data.Typeable

data XhrException = XhrException_Error
                  | XhrException_Aborted
     deriving (Show, Read, Eq, Ord, Typeable)

instance Exception XhrException
