module Reflex.Dom.Xhr.Exception where

import Control.Exception (Exception (..))

data XhrException
  = XhrException_Error
  | XhrException_Aborted
  deriving stock (Eq, Ord, Read, Show)

instance Exception XhrException
