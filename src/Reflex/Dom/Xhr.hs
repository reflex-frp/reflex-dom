module Reflex.Dom.Xhr
  ( module Reflex.Dom.Xhr
  , XMLHttpRequest
  , responseTextToText
  , xmlHttpRequestGetReadyState
  , xmlHttpRequestGetResponseText
  , xmlHttpRequestGetStatus
  , xmlHttpRequestGetStatusText
  , xmlHttpRequestNew
  , xmlHttpRequestOnreadystatechange
  , xmlHttpRequestOpen
  , xmlHttpRequestSend
  , xmlHttpRequestSetRequestHeader
  , xmlHttpRequestSetResponseType
  )
where

import Control.Concurrent
import Control.Lens
import Control.Monad hiding (forM)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import Data.Traversable
import Reflex
import Reflex.Dom.Class
import Reflex.Dom.Xhr.Foreign
import Data.Typeable

data XhrRequest
   = XhrRequest { _xhrRequest_method :: String
                , _xhrRequest_url :: String
                , _xhrRequest_config :: XhrRequestConfig
                }
   deriving (Show, Read, Eq, Ord, Typeable)

data XhrRequestConfig
   = XhrRequestConfig { _xhrRequestConfig_headers :: Map String String
                      , _xhrRequestConfig_user :: Maybe String
                      , _xhrRequestConfig_password :: Maybe String
                      , _xhrRequestConfig_responseType :: Maybe String
                      , _xhrRequestConfig_sendData :: Maybe String
                      }
   deriving (Show, Read, Eq, Ord, Typeable)

data XhrResponse
   = XhrResponse { _xhrResponse_body :: Maybe Text
                 }
   deriving (Show, Read, Eq, Ord, Typeable)

instance Default XhrRequestConfig where
  def = XhrRequestConfig { _xhrRequestConfig_headers = Map.empty
                         , _xhrRequestConfig_user = Nothing
                         , _xhrRequestConfig_password  = Nothing
                         , _xhrRequestConfig_responseType  = Nothing
                         , _xhrRequestConfig_sendData  = Nothing
                         }

xhrRequest :: String -> String -> XhrRequestConfig -> XhrRequest
xhrRequest = XhrRequest

newXMLHttpRequest :: (HasWebView m, MonadIO m, HasPostGui t h m) => XhrRequest -> (XhrResponse -> h ()) -> m XMLHttpRequest
newXMLHttpRequest req cb = do
  wv <- askWebView
  postGui <- askPostGui
  liftIO $ do
    xhr <- xmlHttpRequestNew wv
    let c = _xhrRequest_config req
    xmlHttpRequestOpen
      xhr
      (_xhrRequest_method req)
      (_xhrRequest_url req)
      True
      (fromMaybe "" $ _xhrRequestConfig_user c)
      (fromMaybe "" $ _xhrRequestConfig_password c)
    iforM_ (_xhrRequestConfig_headers c) $ xmlHttpRequestSetRequestHeader xhr
    maybe (return ()) (xmlHttpRequestSetResponseType xhr . toResponseType) (_xhrRequestConfig_responseType c)
    _ <- xmlHttpRequestOnreadystatechange xhr $ do
      readyState <- liftIO $ xmlHttpRequestGetReadyState xhr
      if readyState == 4
          then do
            r <- liftIO $ xmlHttpRequestGetResponseText xhr
            _ <- liftIO $ postGui $ cb $ XhrResponse $ responseTextToText r
            return ()
          else return ()
    _ <- xmlHttpRequestSend xhr (_xhrRequestConfig_sendData c)
    return xhr

performRequestAsync :: (MonadWidget t m) => Event t XhrRequest -> m (Event t XhrResponse)
performRequestAsync req = performEventAsync $ ffor req $ \r cb -> do
  _ <- newXMLHttpRequest r $ liftIO . cb
  return ()

performRequestsAsync :: (Traversable f, MonadWidget t m) => Event t (f XhrRequest) -> m (Event t (f XhrResponse))
performRequestsAsync req = performEventAsync $ ffor req $ \rs cb -> do
  resps <- forM rs $ \r -> do
    resp <- liftIO newEmptyMVar
    _ <- newXMLHttpRequest r $ liftIO . putMVar resp
    return resp
  _ <- liftIO $ forkIO $ cb =<< forM resps takeMVar
  return ()

getAndDecode :: (FromJSON a, MonadWidget t m) => Event t String -> m (Event t (Maybe a))
getAndDecode url = do
  r <- performRequestAsync $ fmap (\x -> XhrRequest "GET" x def) url
  return $ fmap decodeXhrResponse r

getMay :: MonadWidget t m => (Event t a -> m (Event t b)) -> Event t (Maybe a) -> m (Event t (Maybe b))
getMay f e = do
    e' <- f (fmapMaybe id e)
    return $ leftmost [fmap Just e', fmapMaybe (maybe (Just Nothing) (const Nothing)) e]

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . BL.fromStrict . encodeUtf8

decodeXhrResponse :: FromJSON a => XhrResponse -> Maybe a
decodeXhrResponse = join . fmap decodeText . _xhrResponse_body

