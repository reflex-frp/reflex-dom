{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Dom.WebSocket.Internal where

import Control.Exception
import Data.Aeson
import Data.JSString (JSString)
import Foreign.JavaScript.Utils
import Language.Javascript.JSaddle
import System.IO.Unsafe
-- When on ghcjs and using packages with jsstring patches it removes the FromJSVal
-- instance for Value from ghcjs-base. This makes it so that if the patches
-- are used then the instance comes from ghcjs-json
#ifdef USE_TEXT_JSSTRING
import JavaScript.JSON.Types.FromJSVal ()
#else
import GHCJS.Marshal ()
#endif

jsonDecode :: FromJSON a => JSString -> Maybe a
jsonDecode t = do
  result <- unsafePerformIO $ handle (\(_ :: SomeException) -> pure Nothing) $ do
    safeJsonParse t >>= \case
      Nothing -> pure Nothing
      Just a -> fromJSVal a
  case fromJSON result of
    Success a -> pure a
    Error _ -> Nothing
