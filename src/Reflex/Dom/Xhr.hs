{-# LANGUAGE CPP #-}
module Reflex.Dom.Xhr where

#ifdef __GHCJS__
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.DOM.XMLHttpRequest
import Reflex
import Reflex.Dom.Class

data XhrRequest
   = XhrRequest { _xhrRequest_method :: String
                , _xhrRequest_url :: String
                , _xhrRequest_config :: XhrRequestConfig
                }

data XhrRequestConfig
   = XhrRequestConfig { _xhrRequestConfig_headers :: Map String String
                      , _xhrRequestConfig_user :: Maybe String
                      , _xhrRequestConfig_password :: Maybe String
                      , _xhrRequestConfig_responseType :: Maybe String
                      , _xhrRequestConfig_sendData :: Maybe String
                      }

data XhrResponse
   = XhrResponse { _xhrResponse_body :: Maybe JSString
                 }

instance Default XhrRequestConfig where
  def = XhrRequestConfig { _xhrRequestConfig_headers = Map.empty
                         , _xhrRequestConfig_user = Nothing
                         , _xhrRequestConfig_password  = Nothing
                         , _xhrRequestConfig_responseType  = Nothing
                         , _xhrRequestConfig_sendData  = Nothing
                         }

xhrRequest :: String -> String -> XhrRequestConfig -> XhrRequest
xhrRequest = XhrRequest

newXMLHttpRequest :: XhrRequest -> (XhrResponse -> IO a) -> IO XMLHttpRequest
newXMLHttpRequest req cb = do
  xhr <- xmlHttpRequestNew
  let c = _xhrRequest_config req
  xmlHttpRequestOpen
    xhr 
    (_xhrRequest_method req)
    (_xhrRequest_url req)
    True
    (fromMaybe "" $ _xhrRequestConfig_user c)
    (fromMaybe "" $ _xhrRequestConfig_password c)
  iforM (_xhrRequestConfig_headers c) $ xmlHttpRequestSetRequestHeader xhr
  maybe (return ()) (xmlHttpRequestSetResponseType xhr . toJSString) (_xhrRequestConfig_responseType c)
  xmlHttpRequestOnreadystatechange xhr $ do
    readyState <- liftIO $ xmlHttpRequestGetReadyState xhr
    if readyState == 4
        then do
          r <- liftIO $ xmlHttpRequestGetResponseText xhr
          _ <- liftIO $ cb $ XhrResponse r
          return ()
        else return ()
  _ <- xmlHttpRequestSend xhr (_xhrRequestConfig_sendData c)
  return xhr

performRequestAsync :: MonadWidget t m => Event t XhrRequest -> m (Event t XhrResponse)
performRequestAsync req = performEventAsync $ ffor req $ \r cb -> do
  liftIO $ newXMLHttpRequest r cb
  return ()
#endif
