{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif

-- | A module for performing asynchronous HTTP calls from JavaScript
-- using the
-- <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest XMLHttpRequest>
-- API (essentially AJAX). Despite the name, there is nothing whatsoever specific to XML.
--
-- The API has two components:
--
--  * convenient functions for common usecases like GET and POST
--    requests to APIs using JSON.
--
--  * a flexible set of functions for creating and executing arbitrary
--    requests and handling responses.
--
module Reflex.Dom.Xhr
  ( -- * Common Patterns

    -- | Functions that conveniently expose common uses like GET and
    -- POST to JSON APIs.
    getAndDecode
  , getMay
  , postJson

  , decodeXhrResponse
  , decodeText

  -- * General Request API

  -- | This is the most general flow for sending XHR requests:
  --
  --   1. Create an 'Event' stream of 'XhrRequest' records (ie
  --   @Event t (XhrRequest a)@). The records configure the request,
  --   and the 'Event' controls when the request or requests are
  --   actually sent.
  --
  --   2. Plug the @Event t (XhrRequest a)@ into one of the functions
  --   for performing requests like 'performRequestAsync'.
  --
  --   3. Consume the resulting stream of 'XhrResponse' events,
  --   parsing the body of the response however appropriate. A really
  --   common pattern is turning the 'Event' into a 'Dynamic' with
  --   'holdDyn' or a related function.
  --
  -- Here is an example of calling a search API whenever the user
  -- types in a text input field and printing the result on the page:
  --
  -- @
  -- url query = "http:\/\/example.com\/search?query=" \<> query
  --
  -- search queries = do
  --   responses \<- performRequestAsync $ toRequest \<$> queries
  --   return $ view xhrResponse_responseText \<$> responses
  --   where toRequest query = XhrRequest \"GET" (url query) def
  --
  -- main = mainWidget $ do
  --   input \<- textInput def
  --   let queries = updated $ input ^. textInput_value
  --   results \<- search queries
  --   asText \<- holdDyn "No results." $ pack . show \<$> results
  --   dynText asText
  -- @

  -- ** XHR Requests
  , XhrRequest (..)
  , XhrRequestConfig (..)

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

  -- ** Performing Requests
  , performMkRequestAsync
  , performMkRequestsAsync
  , performRequestAsync
  , performRequestAsyncWithError
  , performRequestsAsync
  , performRequestsAsyncWithError

  -- ** XHR Responses
  , XhrResponse (..)
  , XhrResponseBody (..)
  , XhrResponseHeaders (..)
  , XhrResponseType (..)

  , xhrResponse_response
  , xhrResponse_responseText
  , xhrResponse_status
  , xhrResponse_statusText
  , xhrResponse_headers

  -- *** Deprecated
  , xhrResponse_body
  , _xhrResponse_body

  -- ** Error Handling
  , XhrException (..)
  , IsXhrPayload (..)

  -- * JavaScript XMLHttpRequest Objects

  -- | 'XMLHttpRequest' is the type of JavaScript's underlying runtime
  -- objects that represent XHR requests.
  --
  -- Chances are you shouldn't need these in day-to-day code.
  , XMLHttpRequest

  -- ** Constructors
  , newXMLHttpRequest
  , newXMLHttpRequestWithError

  -- ** Fields
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
import Reflex.PerformEvent.Class
import Reflex.TriggerEvent.Class
import Reflex.Dom.Xhr.Exception
import Reflex.Dom.Xhr.Foreign
import Reflex.Dom.Xhr.ResponseType

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
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
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

import Language.Javascript.JSaddle.Monad (JSM, askJSM, runJSM, MonadJSM, liftJSM)

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
                 , _xhrResponse_headers :: Map (CI Text) Text
                 }
   deriving (Typeable)

data XhrResponseHeaders =
    OnlyHeaders (Set.Set (CI Text)) -- ^ Parse a subset of headers from the XHR Response
  | AllHeaders -- ^ Parse all headers from the XHR Response
  deriving (Show, Read, Eq, Ord, Typeable)

instance Default XhrResponseHeaders where
  def = OnlyHeaders mempty

{-# DEPRECATED _xhrResponse_body "Use _xhrResponse_response or _xhrResponse_responseText instead." #-}
_xhrResponse_body :: XhrResponse -> Maybe Text
_xhrResponse_body = _xhrResponse_responseText

{-# DEPRECATED xhrResponse_body "Use xhrResponse_response or xhrResponse_responseText instead." #-}
xhrResponse_body :: Lens' XhrResponse (Maybe Text)
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
    :: (HasJSContext m, MonadJSM m, IsXhrPayload a)
    => XhrRequest a
    -- ^ The request to make.
    -> (Either XhrException XhrResponse -> JSM ())
    -- ^ A continuation to be called once a response comes back, or in
    -- case of error.
    -> m XMLHttpRequest
    -- ^ The XHR request, which could for example be aborted.
newXMLHttpRequestWithError req cb = do
  xhr <- xmlHttpRequestNew
  ctx <- askJSM
  void $ liftIO $ forkIO $ handle ((`runJSM` ctx) . cb . Left) $ void . (`runJSM` ctx) $ do
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
      readyState <- xmlHttpRequestGetReadyState xhr
      status <- xmlHttpRequestGetStatus xhr
      statusText <- xmlHttpRequestGetStatusText xhr
      when (readyState == 4) $ do
        t <- if rt == Just XhrResponseType_Text || isNothing rt
             then xmlHttpRequestGetResponseText xhr
             else return Nothing
        r <- xmlHttpRequestGetResponse xhr
        h <- case _xhrRequestConfig_responseHeaders c of
          AllHeaders -> parseAllHeadersString <$>
            xmlHttpRequestGetAllResponseHeaders xhr
          OnlyHeaders xs -> traverse (xmlHttpRequestGetResponseHeader xhr)
            (Map.fromSet CI.original xs)
        _ <- liftJSM $ cb $ Right
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

parseAllHeadersString :: Text -> Map (CI Text) Text
parseAllHeadersString s = Map.fromList $ fmap (stripBoth . T.span (/=':')) $
  L.dropWhileEnd T.null $ T.splitOn (T.pack "\r\n") s
  where stripBoth (txt1, txt2) = (CI.mk $ T.strip txt1, T.strip $ T.drop 1 txt2)

newXMLHttpRequest :: (HasJSContext m, MonadJSM m, IsXhrPayload a) => XhrRequest a -> (XhrResponse -> JSM ()) -> m XMLHttpRequest
newXMLHttpRequest req cb = newXMLHttpRequestWithError req $ mapM_ cb

-- | Given Event of requests, issue them when the Event fires.
-- Returns Event of corresponding responses.
--
-- The request is processed asynchronously, therefore handling does
-- not block or cause a delay while creating the connection.
performRequestAsyncWithError
    :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, IsXhrPayload a)
    => Event t (XhrRequest a)
    -> m (Event t (Either XhrException XhrResponse))
performRequestAsyncWithError = performRequestAsync' newXMLHttpRequestWithError . fmap return

-- | Given Event of request, issue them when the Event fires.  Returns Event of corresponding response.
performRequestAsync :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, IsXhrPayload a) => Event t (XhrRequest a) -> m (Event t XhrResponse)
performRequestAsync = performRequestAsync' newXMLHttpRequest . fmap return

-- | Given Event with an action that creates a request, build and issue the request when the Event fires.  Returns Event of corresponding response.
performMkRequestAsync :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, IsXhrPayload a) => Event t (Performable m (XhrRequest a)) -> m (Event t XhrResponse)
performMkRequestAsync = performRequestAsync' newXMLHttpRequest

performRequestAsync' :: (MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => (XhrRequest p -> (a -> JSM ()) -> Performable m XMLHttpRequest) -> Event t (Performable m (XhrRequest p)) -> m (Event t a)
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
    :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, Traversable f, IsXhrPayload a)
    => Event t (f (XhrRequest a)) -> m (Event t (f (Either XhrException XhrResponse)))
performRequestsAsyncWithError = performRequestsAsync' newXMLHttpRequestWithError . fmap return

-- | Issues a collection of requests when the supplied Event fires.  When ALL requests from a given firing complete, the results are collected and returned via the return Event.
performRequestsAsync :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, Traversable f, IsXhrPayload a) => Event t (f (XhrRequest a)) -> m (Event t (f XhrResponse))
performRequestsAsync = performRequestsAsync' newXMLHttpRequest . fmap return

-- | Builds and issues a collection of requests when the supplied Event fires.  When ALL requests from a given firing complete, the results are collected and returned via the return Event.
performMkRequestsAsync :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, Traversable f, IsXhrPayload a) => Event t (Performable m (f (XhrRequest a))) -> m (Event t (f XhrResponse))
performMkRequestsAsync = performRequestsAsync' newXMLHttpRequest

performRequestsAsync' :: (MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m, Traversable f) => (XhrRequest b -> (a -> JSM ()) -> Performable m XMLHttpRequest) -> Event t (Performable m (f (XhrRequest b))) -> m (Event t (f a))
performRequestsAsync' newXhr req = performEventAsync $ ffor req $ \hrs cb -> do
  rs <- hrs
  resps <- forM rs $ \r -> do
    resp <- liftIO newEmptyMVar
    _ <- newXhr r $ liftIO . putMVar resp
    return resp
  _ <- liftIO $ forkIO $ cb =<< forM resps takeMVar
  return ()

-- | Simplified interface to "GET" URLs and return decoded results.
getAndDecode :: (MonadIO m, MonadJSM (Performable m), PerformEvent t m, HasJSContext (Performable m), TriggerEvent t m, FromJSON a) => Event t Text -> m (Event t (Maybe a))
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
decodeXhrResponse = decodeText <=< _xhrResponse_responseText

#ifdef USE_TEMPLATE_HASKELL
concat <$> mapM makeLenses
  [ ''XhrRequest
  , ''XhrRequestConfig
  , ''XhrResponse
  ]
#else

xhrRequest_method :: Lens' (XhrRequest a) Text
xhrRequest_method f (XhrRequest x1 x2 x3) = (\y -> XhrRequest y x2 x3) <$> f x1
{-# INLINE xhrRequest_method #-}

xhrRequest_url :: Lens' (XhrRequest a) Text
xhrRequest_url f (XhrRequest x1 x2 x3) = (\y -> XhrRequest x1 y x3) <$> f x2
{-# INLINE xhrRequest_url #-}

xhrRequest_config :: Lens' (XhrRequest a) (XhrRequestConfig a)
xhrRequest_config f (XhrRequest x1 x2 x3) = (\y -> XhrRequest x1 x2 y) <$> f x3
{-# INLINE xhrRequest_config #-}

xhrRequestConfig_headers :: Lens' (XhrRequestConfig a) (Map Text Text)
xhrRequestConfig_headers f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) = (\y -> XhrRequestConfig y x2 x3 x4 x5 x6 x7) <$> f x1
{-# INLINE xhrRequestConfig_headers #-}

xhrRequestConfig_user :: Lens' (XhrRequestConfig a) (Maybe Text)
xhrRequestConfig_user f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) = (\y -> XhrRequestConfig x1 y x3 x4 x5 x6 x7) <$> f x2
{-# INLINE xhrRequestConfig_user #-}

xhrRequestConfig_password :: Lens' (XhrRequestConfig a) (Maybe Text)
xhrRequestConfig_password f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) = (\y -> XhrRequestConfig x1 x2 y x4 x5 x6 x7) <$> f x3
{-# INLINE xhrRequestConfig_password #-}

xhrRequestConfig_responseType :: Lens' (XhrRequestConfig a) (Maybe XhrResponseType)
xhrRequestConfig_responseType f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) = (\y -> XhrRequestConfig x1 x2 x3 y x5 x6 x7) <$> f x4
{-# INLINE xhrRequestConfig_responseType #-}

xhrRequestConfig_sendData :: Lens (XhrRequestConfig a) (XhrRequestConfig b) a b
xhrRequestConfig_sendData f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) = (\y -> XhrRequestConfig x1 x2 x3 x4 y x6 x7) <$> f x5
{-# INLINE xhrRequestConfig_sendData #-}

xhrRequestConfig_withCredentials :: Lens' (XhrRequestConfig a) Bool
xhrRequestConfig_withCredentials f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) = (\y -> XhrRequestConfig x1 x2 x3 x4 x5 y x7) <$> f x6
{-# INLINE xhrRequestConfig_withCredentials #-}

xhrRequestConfig_responseHeaders :: Lens' (XhrRequestConfig a) XhrResponseHeaders
xhrRequestConfig_responseHeaders f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) = (\y -> XhrRequestConfig x1 x2 x3 x4 x5 x6 y) <$> f x7
{-# INLINE xhrRequestConfig_responseHeaders #-}

xhrResponse_status :: Lens' XhrResponse Word
xhrResponse_status f (XhrResponse x1 x2 x3 x4 x5) = (\y -> XhrResponse y x2 x3 x4 x5) <$> f x1
{-# INLINE xhrResponse_status #-}

xhrResponse_statusText :: Lens' XhrResponse Text
xhrResponse_statusText f (XhrResponse x1 x2 x3 x4 x5) = (\y -> XhrResponse x1 y x3 x4 x5) <$> f x2
{-# INLINE xhrResponse_statusText #-}

xhrResponse_response :: Lens' XhrResponse (Maybe XhrResponseBody)
xhrResponse_response f (XhrResponse x1 x2 x3 x4 x5) = (\y -> XhrResponse x1 x2 y x4 x5) <$> f x3
{-# INLINE xhrResponse_response #-}

xhrResponse_responseText :: Lens' XhrResponse (Maybe Text)
xhrResponse_responseText f (XhrResponse x1 x2 x3 x4 x5) = (\y -> XhrResponse x1 x2 x3 y x5) <$> f x4
{-# INLINE xhrResponse_responseText #-}

xhrResponse_headers :: Lens' XhrResponse (Map (CI Text) Text)
xhrResponse_headers f (XhrResponse x1 x2 x3 x4 x5) = (\y -> XhrResponse x1 x2 x3 x4 y) <$> f x5
{-# INLINE xhrResponse_headers #-}

#endif
