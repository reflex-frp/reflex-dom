{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Dom.Xhr
  ( XMLHttpRequest
  , XhrRequest (..)
  , XhrRequestConfig (..)
  , XhrResponse (..)
  , XhrResponseBody (..)
  , XhrResponseHeaders (..)
  , XhrResponseType (..)
  , XhrException (..)
  , IsXhrPayload (..)
  , _xhrResponse_body
  , decodeText
  , decodeXhrResponse
  , getAndDecode
  , getMay
  , newXMLHttpRequest
  , newXMLHttpRequestWithError
  , performMkRequestAsync
  , performMkRequestsAsync
  , performRequestAsync
  , performRequestAsyncWithError
  , performRequestsAsync
  , performRequestsAsyncWithError
  , postJson
  , xhrRequest
  , xhrRequestConfig_headers
  , xhrRequestConfig_password
  , xhrRequestConfig_responseType
  , xhrRequestConfig_sendData
  , xhrRequestConfig_user
  , xhrRequestConfig_withCredentials
  , xhrRequestConfig_responseHeaders
  , xhrRequest_config
  , xhrRequest_method
  , xhrRequest_url
  , xhrResponse_body
  , xhrResponse_response
  , xhrResponse_responseText
  , xhrResponse_status
  , xhrResponse_statusText
  , xhrResponse_headers
  , xmlHttpRequestGetReadyState
  , xmlHttpRequestGetResponseText
  , xmlHttpRequestGetStatus
  , xmlHttpRequestGetStatusText
  , xmlHttpRequestNew
  , xmlHttpRequestOnreadystatechange
  , xmlHttpRequestOpen
  , xmlHttpRequestSetRequestHeader
  , xmlHttpRequestSetResponseType
  )
where

import Reflex.Class
import Reflex.Dom.Class
import Reflex.Dom.Xhr.Exception
import Reflex.Dom.Xhr.Foreign
import Reflex.Dom.Xhr.ResponseType
import Reflex.PerformEvent.Class
import Reflex.TriggerEvent.Class

import Control.Concurrent
import Control.Exception (handle)
import Control.Lens
import Control.Monad hiding (forM)
import Control.Monad.IO.Class
import Data.Aeson
#if MIN_VERSION_aeson(1,0,0)
import Data.Aeson.Text
#else
import Data.Aeson.Encode
#endif
import qualified Data.ByteString.Lazy as BL
import Data.Default
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import Data.Traversable
import Data.Typeable

data XhrRequest a
   = XhrRequest { _xhrRequest_method :: Text
                , _xhrRequest_url :: Text
                , _xhrRequest_config :: XhrRequestConfig a
                }
   deriving (Show, Read, Eq, Ord, Typeable, Functor)

data XhrRequestConfig a
   = XhrRequestConfig { _xhrRequestConfig_headers :: Map Text Text
                      , _xhrRequestConfig_user :: Maybe Text
                      , _xhrRequestConfig_password :: Maybe Text
                      , _xhrRequestConfig_responseType :: Maybe XhrResponseType
                      , _xhrRequestConfig_sendData :: a
                      , _xhrRequestConfig_withCredentials :: Bool
                      , _xhrRequestConfig_responseHeaders :: XhrResponseHeaders
                      }
   deriving (Show, Read, Eq, Ord, Typeable, Functor)

data XhrResponse
   = XhrResponse { _xhrResponse_status :: Word
                 , _xhrResponse_statusText :: Text
                 , _xhrResponse_response :: Maybe XhrResponseBody
                 , _xhrResponse_responseText :: Maybe Text
                 , _xhrResponse_headers :: Map Text Text
                 }
   deriving (Typeable)

data XhrResponseHeaders =
    OnlyHeaders (Set.Set Text) -- ^ Parse a subset of headers from the XHR Response
  | AllHeaders -- ^ Parse all headers from the XHR Response
  deriving (Show, Read, Eq, Ord, Typeable)

instance Default XhrResponseHeaders where
  def = OnlyHeaders mempty

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
                         , _xhrRequestConfig_withCredentials = False
                         , _xhrRequestConfig_responseHeaders = def
                         }

-- | Construct a request object from method, URL, and config record.
xhrRequest :: Text -> Text -> XhrRequestConfig a -> XhrRequest a
xhrRequest = XhrRequest

-- | Make a new asyncronous XHR request. This does not block (it forks),
-- and returns an XHR object immediately (which you can use to abort
-- the XHR connection), and will pass an exception ('XhrException') to the
-- continuation if the connection cannot be made (or is aborted).
newXMLHttpRequestWithError
    :: (HasWebView m, MonadIO m, IsXhrPayload a)
    => XhrRequest a
    -- ^ The request to make.
    -> (Either XhrException XhrResponse -> IO ())
    -- ^ A continuation to be called once a response comes back, or in
    -- case of error.
    -> m XMLHttpRequest
    -- ^ The XHR request, which could for example be aborted.
newXMLHttpRequestWithError req cb = do
  wv <- askWebView
  xhr <- liftIO $ xmlHttpRequestNew $ unWebViewSingleton wv
  void $ liftIO $ forkIO $ handle (cb . Left) $ void $ do
    let c = _xhrRequest_config req
        rt = _xhrRequestConfig_responseType c
        creds = _xhrRequestConfig_withCredentials c
    xmlHttpRequestOpen
      xhr
      (_xhrRequest_method req)
      (_xhrRequest_url req)
      True
      (fromMaybe "" $ _xhrRequestConfig_user c)
      (fromMaybe "" $ _xhrRequestConfig_password c)
    iforM_ (_xhrRequestConfig_headers c) $ xmlHttpRequestSetRequestHeader xhr
    maybe (return ()) (xmlHttpRequestSetResponseType xhr . fromResponseType) rt
    xmlHttpRequestSetWithCredentials xhr creds
    _ <- xmlHttpRequestOnreadystatechange xhr $ do
      readyState <- liftIO $ xmlHttpRequestGetReadyState xhr
      status <- liftIO $ xmlHttpRequestGetStatus xhr
      statusText <- liftIO $ xmlHttpRequestGetStatusText xhr
      when (readyState == 4) $ do
        t <- if rt == Just XhrResponseType_Text || isNothing rt
             then liftIO $ xmlHttpRequestGetResponseText xhr
             else return Nothing
        r <- liftIO $ xmlHttpRequestGetResponse xhr
        h <- case _xhrRequestConfig_responseHeaders c of
          AllHeaders -> liftIO $ parseAllHeadersString <$>
            xmlHttpRequestGetAllResponseHeaders xhr
          OnlyHeaders xs -> liftIO $ traverse (xmlHttpRequestGetResponseHeader xhr)
            (Map.fromSet id xs)
        _ <- liftIO $ cb $ Right
             XhrResponse { _xhrResponse_status = status
                         , _xhrResponse_statusText = statusText
                         , _xhrResponse_response = r
                         , _xhrResponse_responseText = t
                         , _xhrResponse_headers = h
                         }
        return ()
    _ <- xmlHttpRequestSend xhr (_xhrRequestConfig_sendData c)
    return ()
  return xhr

parseAllHeadersString :: Text -> Map Text Text
parseAllHeadersString s = Map.fromList $ fmap (stripBoth . T.span (/=':')) $
  L.dropWhileEnd T.null $ T.splitOn (T.pack "\r\n") s
  where stripBoth (txt1, txt2) = (T.strip txt1, T.strip $ T.drop 1 txt2)

newXMLHttpRequest :: (HasWebView m, MonadIO m, IsXhrPayload a) => XhrRequest a -> (XhrResponse -> IO ()) -> m XMLHttpRequest
newXMLHttpRequest req cb = newXMLHttpRequestWithError req $ mapM_ cb

-- | Given Event of requests, issue them when the Event fires.
-- Returns Event of corresponding responses.
--
-- The request is processed asynchronously, therefore handling does
-- not block or cause a delay while creating the connection.
performRequestAsyncWithError
    :: (MonadIO (Performable m), HasWebView (Performable m), PerformEvent t m, TriggerEvent t m, IsXhrPayload a)
    => Event t (XhrRequest a)
    -> m (Event t (Either XhrException XhrResponse))
performRequestAsyncWithError = performRequestAsync' newXMLHttpRequestWithError . fmap return

-- | Given Event of request, issue them when the Event fires.  Returns Event of corresponding response.
performRequestAsync :: (MonadIO (Performable m), HasWebView (Performable m), PerformEvent t m, TriggerEvent t m, IsXhrPayload a) => Event t (XhrRequest a) -> m (Event t XhrResponse)
performRequestAsync = performRequestAsync' newXMLHttpRequest . fmap return

-- | Given Event with an action that creates a request, build and issue the request when the Event fires.  Returns Event of corresponding response.
performMkRequestAsync :: (MonadIO (Performable m), HasWebView (Performable m), PerformEvent t m, TriggerEvent t m, IsXhrPayload a) => Event t (Performable m (XhrRequest a)) -> m (Event t XhrResponse)
performMkRequestAsync = performRequestAsync' newXMLHttpRequest

performRequestAsync' :: (MonadIO (Performable m), PerformEvent t m, TriggerEvent t m) => (XhrRequest p -> (a -> IO ()) -> Performable m XMLHttpRequest) -> Event t (Performable m (XhrRequest p)) -> m (Event t a)
performRequestAsync' newXhr req = performEventAsync $ ffor req $ \hr cb -> do
  r <- hr
  _ <- newXhr r $ liftIO . cb
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
    :: (MonadIO (Performable m), HasWebView (Performable m), PerformEvent t m, TriggerEvent t m, Traversable f, IsXhrPayload a)
    => Event t (f (XhrRequest a)) -> m (Event t (f (Either XhrException XhrResponse)))
performRequestsAsyncWithError = performRequestsAsync' newXMLHttpRequestWithError . fmap return

-- | Issues a collection of requests when the supplied Event fires.  When ALL requests from a given firing complete, the results are collected and returned via the return Event.
performRequestsAsync :: (MonadIO (Performable m), HasWebView (Performable m), PerformEvent t m, TriggerEvent t m, Traversable f, IsXhrPayload a) => Event t (f (XhrRequest a)) -> m (Event t (f XhrResponse))
performRequestsAsync = performRequestsAsync' newXMLHttpRequest . fmap return

-- | Builds and issues a collection of requests when the supplied Event fires.  When ALL requests from a given firing complete, the results are collected and returned via the return Event.
performMkRequestsAsync :: (MonadIO (Performable m), HasWebView (Performable m), PerformEvent t m, TriggerEvent t m, Traversable f, IsXhrPayload a) => Event t (Performable m (f (XhrRequest a))) -> m (Event t (f XhrResponse))
performMkRequestsAsync = performRequestsAsync' newXMLHttpRequest

performRequestsAsync' :: (MonadIO (Performable m), PerformEvent t m, TriggerEvent t m, Traversable f) => (XhrRequest b -> (a -> IO ()) -> Performable m XMLHttpRequest) -> Event t (Performable m (f (XhrRequest b))) -> m (Event t (f a))
performRequestsAsync' newXhr req = performEventAsync $ ffor req $ \hrs cb -> do
  rs <- hrs
  resps <- forM rs $ \r -> do
    resp <- liftIO newEmptyMVar
    _ <- newXhr r $ liftIO . putMVar resp
    return resp
  _ <- liftIO $ forkIO $ cb =<< forM resps takeMVar
  return ()

-- | Simplified interface to "GET" URLs and return decoded results.
getAndDecode :: (MonadIO m, MonadIO (Performable m), PerformEvent t m, HasWebView (Performable m), TriggerEvent t m, FromJSON a) => Event t Text -> m (Event t (Maybe a))
getAndDecode url = do
  r <- performRequestAsync $ fmap (\x -> XhrRequest "GET" x def) url
  return $ fmap decodeXhrResponse r

-- | Create a "POST" request from an URL and thing with a JSON representation
postJson :: (ToJSON a) => Text -> a -> XhrRequest Text
postJson url a =
  XhrRequest "POST" url $ def { _xhrRequestConfig_headers = headerUrlEnc
                              , _xhrRequestConfig_sendData = body
                              }
  where headerUrlEnc = "Content-type" =: "application/json"
        body = LT.toStrict $ B.toLazyText $ encodeToTextBuilder $ toJSON a

getMay :: (Monad m, Reflex t) => (Event t a -> m (Event t b)) -> Event t (Maybe a) -> m (Event t (Maybe b))
getMay f e = do
    e' <- f (fmapMaybe id e)
    return $ leftmost [fmap Just e', fmapMaybe (maybe (Just Nothing) (const Nothing)) e]

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . BL.fromStrict . encodeUtf8

-- | Convenience function to decode JSON-encoded responses.
decodeXhrResponse :: FromJSON a => XhrResponse -> Maybe a
decodeXhrResponse = join . fmap decodeText . _xhrResponse_responseText

concat <$> mapM makeLenses
  [ ''XhrRequest
  , ''XhrRequestConfig
  , ''XhrResponse
  ]
