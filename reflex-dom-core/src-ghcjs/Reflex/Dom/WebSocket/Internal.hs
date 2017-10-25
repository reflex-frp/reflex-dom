{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Dom.WebSocket.Internal where

import Control.Exception
import Data.Aeson
import Data.JSString (JSString)
import Foreign.JavaScript.Utils
import GHCJS.Marshal ()
import Language.Javascript.JSaddle
import System.IO.Unsafe

jsonDecode :: FromJSON a => JSString -> Maybe a
jsonDecode t = do
  result <- unsafePerformIO $ handle (\(_ :: SomeException) -> pure Nothing) $ do
    safeJsonParse t >>= \case
      Nothing -> pure Nothing
      Just a -> fromJSVal a
  case fromJSON result of
    Success a -> pure a
    Error _ -> Nothing
