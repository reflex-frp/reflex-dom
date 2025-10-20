module Reflex.Dom.Xhr.Exception where

import Control.Exception (Exception (..))

data XhrException = XhrException_Error
                  | XhrException_Aborted
     deriving (Show, Read, Eq, Ord)

instance Exception XhrException
