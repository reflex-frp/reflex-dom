{-# LANGUAGE ForeignFunctionInterface #-}
module Reflex.Dom.Android.MainWidget
  ( startMainWidget
  ) where

import Android.HaskellActivity
import Data.ByteString
import Foreign.C.String

startMainWidget :: HaskellActivity -> ByteString -> IO ()
startMainWidget a url = useAsCString url $ startMainWidget_ a

foreign import ccall safe "Reflex_Dom_Android_MainWidget_start" startMainWidget_ :: HaskellActivity -> CString -> IO ()
