{-# LANGUAGE TemplateHaskell, GADTs, DeriveDataTypeable #-}
module Reflex.Dom.Xhr
  ( XMLHttpRequest
  , XhrRequest(..)
  , XhrRequestConfig(..)
  , XhrResponse(..)
  , XhrResponseBody(..)
  , XhrResponseType(..)
  , XhrException(..)
  , _xhrResponse_body
  , xhrResponse_body
  , xhrRequest
  , newXMLHttpRequest
  , newXMLHttpRequestWithError
  , performRequestAsync
  , performMkRequestAsync
  , performRequestAsyncWithError
  , performRequestsAsync
  , performMkRequestsAsync
  , performRequestsAsyncWithError
  , getAndDecode
  , postJson
  , getMay
  , decodeText
  , decodeXhrResponse
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
  , xhrRequest_config
  , xhrRequest_method
  , xhrRequest_url
  , xhrRequestConfig_headers
  , xhrRequestConfig_password
  , xhrRequestConfig_responseType
  , xhrRequestConfig_sendData
  , xhrRequestConfig_user
  , xhrResponse_response
  , xhrResponse_responseText
  , xhrResponse_status
  , xhrResponse_statusText
  )
where

import Reflex
import Reflex.Dom.Class
import Reflex.Dom.Xhr.Foreign

import Control.Concurrent
import Control.Exception (catch)
import Control.Lens
import Control.Monad hiding (forM)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encode
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import Data.Traversable
import Reflex.Dom.Xhr.Exception
import Reflex.Dom.Xhr.ResponseType
import Data.Typeable

data XhrRequest a
   = XhrRequest { _xhrRequest_method :: String
                , _xhrRequest_url :: String
                , _xhrRequest_config :: XhrRequestConfig a
                }
   deriving (Show, Read, Eq, Ord, Typeable)

data XhrRequestConfig a
   = XhrRequestConfig { _xhrRequestConfig_headers :: Map String String
                      , _xhrRequestConfig_user :: Maybe String
                      , _xhrRequestConfig_password :: Maybe String
                      , _xhrRequestConfig_responseType :: Maybe XhrResponseType
                      , _xhrRequestConfig_sendData :: a
                      }
   deriving (Show, Read, Eq, Ord, Typeable)

data XhrResponse
   = XhrResponse { _xhrResponse_status :: Word
                 , _xhrResponse_statusText :: Text
                 , _xhrResponse_response :: Maybe XhrResponseBody
                 , _xhrResponse_responseText :: Maybe Text
                 }
   deriving (Typeable)

{-# DEPRECATED _xhrResponse_body "Use _xhrResponse_response or _xhrResponse_responseText instead." #-}
_xhrResponse_body :: XhrResponse -> Maybe Text
_xhrResponse_body = _xhrResponse_responseText

{-# DEPRECATED xhrResponse_body "Use xhrResponse_response or xhrResponse_responseText instead." #-}
xhrResponse_body :: Lens XhrResponse XhrResponse (Maybe Text) (Maybe Text)
xhrResponse_body = lens _xhrResponse_responseText (\r t -> r { _xhrResponse_responseText = t })

instance a ~ () => Default (XhrRequestConfig a) where
  def = XhrRequestConfig { _xhrRequestConfig_headers = Map.empty
                         , _xhrRequestConfig_user = Nothing
                         , _xhrRequestConfig_password  = Nothing
                         , _xhrRequestConfig_responseType  = Nothing
                         , _xhrRequestConfig_sendData  = ()
                         }

-- | Construct a request object from method, URL, and config record.
xhrRequest :: String -> String -> XhrRequestConfig a -> XhrRequest a
xhrRequest = XhrRequest

-- | Make a new asyncronous XHR request. This does not block (it forks),
-- and returns an XHR object immediately (which you can use to abort
-- the XHR connection), and will pass an exception ('XhrException') to the
-- continuation if the connection cannot be made (or is aborted).
newXMLHttpRequestWithError
    :: (HasWebView m, MonadIO m, HasPostGui t h m, IsXhrPayload a)
    => XhrRequest a
    -- ^ The request to make.
    -> (Either XhrException XhrResponse -> h ())
    -- ^ A continuation to be called once a response comes back, or in
    -- case of error.
    -> m XMLHttpRequest
    -- ^ The XHR request, which could for example be aborted.
newXMLHttpRequestWithError req cb = do
  wv <- askWebView
  postGui <- askPostGui
  xhr <- liftIO $ xmlHttpRequestNew $ unWebViewSingleton wv
  void $ liftIO $ forkIO $ flip catch (postGui . cb . Left) $ void $ do
    let c = _xhrRequest_config req
        rt = _xhrRequestConfig_responseType c
    xmlHttpRequestOpen
      xhr
      (_xhrRequest_method req)
      (_xhrRequest_url req)
      True
      (fromMaybe "" $ _xhrRequestConfig_user c)
      (fromMaybe "" $ _xhrRequestConfig_password c)
    iforM_ (_xhrRequestConfig_headers c) $ xmlHttpRequestSetRequestHeader xhr
    maybe (return ()) (xmlHttpRequestSetResponseType xhr . toResponseType) rt
    _ <- xmlHttpRequestOnreadystatechange xhr $ do
      readyState <- liftIO $ xmlHttpRequestGetReadyState xhr
      status <- liftIO $ xmlHttpRequestGetStatus xhr
      statusText <- liftIO $ xmlHttpRequestGetStatusText xhr
      if readyState == 4
          then do
            t <- if rt == Just XhrResponseType_Text || rt == Nothing
                   then liftIO $ xmlHttpRequestGetResponseText xhr
                   else  return Nothing
            r <- liftIO $ xmlHttpRequestGetResponse xhr
            _ <- liftIO $ postGui $ cb $ Right $
                   XhrResponse { _xhrResponse_status = status
                               , _xhrResponse_statusText = statusText
                               , _xhrResponse_response = r
                               , _xhrResponse_responseText = t
                               }
            return ()
          else return ()
    _ <- xmlHttpRequestSend xhr (_xhrRequestConfig_sendData c)
    return ()
  return xhr

newXMLHttpRequest :: (HasWebView m, MonadIO m, HasPostGui t h m, IsXhrPayload a) => XhrRequest a -> (XhrResponse -> h ()) -> m XMLHttpRequest
newXMLHttpRequest req cb = newXMLHttpRequestWithError req $ mapM_ cb

-- | Given Event of requests, issue them when the Event fires.
-- Returns Event of corresponding responses.
--
-- The request is processed asynchronously, therefore handling does
-- not block or cause a delay while creating the connection.
performRequestAsyncWithError
    :: (MonadWidget t m, IsXhrPayload a)
    => Event t (XhrRequest a)
    -> m (Event t (Either XhrException XhrResponse))
performRequestAsyncWithError = performRequestAsync' newXMLHttpRequestWithError . fmap return

-- | Given Event of request, issue them when the Event fires.  Returns Event of corresponding response.
performRequestAsync :: (MonadWidget t m, IsXhrPayload a) => Event t (XhrRequest a) -> m (Event t XhrResponse)
performRequestAsync = performRequestAsync' newXMLHttpRequest . fmap return

-- | Given Event with an action that creates a request, build and issue the request when the Event fires.  Returns Event of corresponding response.
performMkRequestAsync :: (MonadWidget t m, IsXhrPayload a) => Event t (WidgetHost m (XhrRequest a)) -> m (Event t XhrResponse)
performMkRequestAsync = performRequestAsync' newXMLHttpRequest

performRequestAsync' :: (MonadWidget t m, MonadIO h, IsXhrPayload p) => (XhrRequest p -> (a -> h ()) -> WidgetHost m XMLHttpRequest) -> Event t (WidgetHost m (XhrRequest p)) -> m (Event t a)
performRequestAsync' newXhr req = performEventAsync $ ffor req $ \hr cb -> do
  r <- hr
  newXhr r $ liftIO . cb
  return ()

-- | Issues a collection of requests when the supplied Event fires.
-- When ALL requests from a given firing complete, the results are
-- collected and returned via the return Event.
--
-- The requests are processed asynchronously, therefore handling does
-- not block or cause a delay while creating the connection.
--
-- Order of request execution and completion is not guaranteed, but
-- order of creation and the collection result is preserved.
performRequestsAsyncWithError
    :: (Traversable f, MonadWidget t m, IsXhrPayload a)
    => Event t (f (XhrRequest a)) -> m (Event t (f (Either XhrException XhrResponse)))
performRequestsAsyncWithError = performRequestsAsync' newXMLHttpRequestWithError . fmap return

-- | Issues a collection of requests when the supplied Event fires.  When ALL requests from a given firing complete, the results are collected and returned via the return Event.
performRequestsAsync :: (Traversable f, MonadWidget t m, IsXhrPayload a) => Event t (f (XhrRequest a)) -> m (Event t (f XhrResponse))
performRequestsAsync = performRequestsAsync' newXMLHttpRequest . fmap return

-- | Builds and issues a collection of requests when the supplied Event fires.  When ALL requests from a given firing complete, the results are collected and returned via the return Event.
performMkRequestsAsync :: (Traversable f, MonadWidget t m, IsXhrPayload a) => Event t (WidgetHost m (f (XhrRequest a))) -> m (Event t (f XhrResponse))
performMkRequestsAsync = performRequestsAsync' newXMLHttpRequest

performRequestsAsync' :: (MonadWidget t m, MonadIO h, Traversable f, IsXhrPayload b) => (XhrRequest b -> (a -> h ()) -> WidgetHost m XMLHttpRequest) -> Event t (WidgetHost m (f (XhrRequest b))) -> m (Event t (f a))
performRequestsAsync' newXhr req = performEventAsync $ ffor req $ \hrs cb -> do
  rs <- hrs
  resps <- forM rs $ \r -> do
    resp <- liftIO newEmptyMVar
    _ <- newXhr r $ liftIO . putMVar resp
    return resp
  _ <- liftIO $ forkIO $ cb =<< forM resps takeMVar
  return ()

-- | Simplified interface to "GET" URLs and return decoded results.
getAndDecode :: (FromJSON a, MonadWidget t m) => Event t String -> m (Event t (Maybe a))
getAndDecode url = do
  r <- performRequestAsync $ fmap (\x -> XhrRequest "GET" x def) url
  return $ fmap decodeXhrResponse r

-- | Create a "POST" request from an URL and thing with a JSON representation
postJson :: (ToJSON a) => String -> a -> XhrRequest String
postJson url a =
  XhrRequest "POST" url $ def { _xhrRequestConfig_headers = headerUrlEnc
                              , _xhrRequestConfig_sendData = body
                              }
  where headerUrlEnc = "Content-type" =: "application/json"
        body = LT.unpack $ B.toLazyText $ encodeToTextBuilder $ toJSON a

getMay :: MonadWidget t m => (Event t a -> m (Event t b)) -> Event t (Maybe a) -> m (Event t (Maybe b))
getMay f e = do
    e' <- f (fmapMaybe id e)
    return $ leftmost [fmap Just e', fmapMaybe (maybe (Just Nothing) (const Nothing)) e]

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . BL.fromStrict . encodeUtf8

-- | Convenience function to decode JSON-encoded responses.
decodeXhrResponse :: FromJSON a => XhrResponse -> Maybe a
decodeXhrResponse = join . fmap decodeText . _xhrResponse_responseText

liftM concat $ mapM makeLenses
  [ ''XhrRequest
  , ''XhrRequestConfig
  , ''XhrResponse
  ]
