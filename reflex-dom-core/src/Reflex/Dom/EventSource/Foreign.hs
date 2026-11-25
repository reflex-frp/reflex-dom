{-# LANGUAGE CPP #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
{-# LANGUAGE LambdaCase #-}

module Reflex.Dom.EventSource.Foreign
  ( module Reflex.Dom.EventSource.Foreign
  , JSVal
  ) where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.JavaScript.Utils (bsFromMutableArrayBuffer)
import GHCJS.DOM.MessageEvent
import GHCJS.DOM.Types (JSM, JSVal, liftJSM, EventSource(..))
import qualified GHCJS.DOM.EventSource as DOM
import GHCJS.Foreign (JSType(..), jsTypeOf)
import Language.Javascript.JSaddle (fun, eval, toJSVal, call)
import Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)
import Language.Javascript.JSaddle.Types (ghcjsPure)

newtype JSEventSource = JSEventSource { unEventSource :: EventSource }

closeEventSource :: JSEventSource -> JSM ()
closeEventSource (JSEventSource es) = DOM.close es

newEventSource
  :: Text -- url
  -> Text
  -> (Either ByteString JSVal -> JSM ()) -- onmessage
  -> JSM () -- onopen
  -> JSM () -- onerror
  -> JSM JSEventSource
newEventSource url eventType onMessage onOpen onError = do
  let onOpenWrapped = fun $ \_ _ _ -> onOpen
      onErrorWrapped = fun $ \_ _ _ -> onError
      onMessageWrapped = fun $ \_ _ (e:_) -> do
        let e' = MessageEvent e
        d <- getData e'
        liftJSM $ ghcjsPure (jsTypeOf d) >>= \case
          String -> onMessage $ Right d
          _ -> do
            ab <- mutableArrayBufferFromJSVal d
            bsFromMutableArrayBuffer ab >>= onMessage . Left
  newWS <- eval $ unlines
    [ "(function(url, open, error, message) {"
    , "  var es = new window['EventSource'](url);"
    , "  es['addEventListener']('open', open);"
    , "  es['addEventListener']('error', error);"
    , "  es['addEventListener']('" <> T.unpack eventType <> "', message);"
    , "  return es;"
    , "})"
    ]
  url' <- toJSVal url
  onOpen' <- toJSVal onOpenWrapped
  onError' <- toJSVal onErrorWrapped
  onMessage' <- toJSVal onMessageWrapped
  es <- call newWS newWS [url', onOpen', onError', onMessage']
  return $ JSEventSource $ EventSource es
