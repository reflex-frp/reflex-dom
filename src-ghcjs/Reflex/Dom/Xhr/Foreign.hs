{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, OverloadedStrings #-}

module Reflex.Dom.Xhr.Foreign (
    XMLHttpRequest
  , XMLHttpRequestResponseType
  , module Reflex.Dom.Xhr.Foreign
) where

import Prelude hiding (error)
import Data.Text (Text)
import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM
import GHCJS.DOM.Enums
import GHCJS.DOM.XMLHttpRequest
import Data.Maybe (fromMaybe)
import GHCJS.DOM.EventTarget (dispatchEvent)
import GHCJS.DOM.EventM (EventM, on)

prepareWebView :: WebView -> IO ()
prepareWebView _ = return ()

xmlHttpRequestNew :: a -> IO XMLHttpRequest
xmlHttpRequestNew _ = newXMLHttpRequest -- XMLHttpRequest <$> ghcjs_dom_xml_http_request_new

xmlHttpRequestOpen ::
                   (ToJSString method, ToJSString url, ToJSString user, ToJSString password) =>
                     XMLHttpRequest -> method -> url -> Bool -> user -> password -> IO ()
xmlHttpRequestOpen = open

-- This used to be a non blocking call, but now it uses an interruptible ffi
xmlHttpRequestSend :: ToJSString payload => XMLHttpRequest -> Maybe payload -> IO ()
xmlHttpRequestSend self = maybe (send self) (sendString self)

xmlHttpRequestSetRequestHeader :: (ToJSString header, ToJSString value)
                               => XMLHttpRequest -> header -> value -> IO ()
xmlHttpRequestSetRequestHeader = setRequestHeader

xmlHttpRequestAbort :: XMLHttpRequest -> IO ()
xmlHttpRequestAbort = abort

xmlHttpRequestGetAllResponseHeaders :: XMLHttpRequest -> IO Text
xmlHttpRequestGetAllResponseHeaders self = fromMaybe "" <$> getAllResponseHeaders self

xmlHttpRequestGetResponseHeader :: (ToJSString header)
                                => XMLHttpRequest -> header -> IO Text
xmlHttpRequestGetResponseHeader self header = fromMaybe "" <$> getResponseHeader self header

xmlHttpRequestOverrideMimeType :: ToJSString override => XMLHttpRequest -> override -> IO ()
xmlHttpRequestOverrideMimeType = overrideMimeType

xmlHttpRequestDispatchEvent :: IsEvent evt => XMLHttpRequest -> Maybe evt -> IO Bool
xmlHttpRequestDispatchEvent = dispatchEvent

xmlHttpRequestOnabort :: XMLHttpRequest -> EventM XMLHttpRequest XMLHttpRequestProgressEvent () -> IO (IO ())
xmlHttpRequestOnabort = (`on` abortEvent)

xmlHttpRequestOnerror :: XMLHttpRequest -> EventM XMLHttpRequest XMLHttpRequestProgressEvent () -> IO (IO ())
xmlHttpRequestOnerror = (`on` error)

xmlHttpRequestOnload :: XMLHttpRequest -> EventM XMLHttpRequest XMLHttpRequestProgressEvent () -> IO (IO ())
xmlHttpRequestOnload = (`on` load)

xmlHttpRequestOnloadend :: XMLHttpRequest -> EventM XMLHttpRequest ProgressEvent () -> IO (IO ())
xmlHttpRequestOnloadend = (`on` loadEnd)

xmlHttpRequestOnloadstart :: XMLHttpRequest -> EventM XMLHttpRequest ProgressEvent () -> IO (IO ())
xmlHttpRequestOnloadstart = (`on` loadStart)

xmlHttpRequestOnprogress :: XMLHttpRequest -> EventM XMLHttpRequest XMLHttpRequestProgressEvent () -> IO (IO ())
xmlHttpRequestOnprogress = (`on` progress)

xmlHttpRequestOntimeout :: XMLHttpRequest -> EventM XMLHttpRequest ProgressEvent () -> IO (IO ())
xmlHttpRequestOntimeout = (`on` timeout)

xmlHttpRequestOnreadystatechange :: XMLHttpRequest -> EventM XMLHttpRequest Event () -> IO (IO ())
xmlHttpRequestOnreadystatechange = (`on` readyStateChange)

xmlHttpRequestSetTimeout :: XMLHttpRequest -> Word -> IO ()
xmlHttpRequestSetTimeout = setTimeout

xmlHttpRequestGetTimeout :: XMLHttpRequest -> IO Word
xmlHttpRequestGetTimeout = getTimeout

xmlHttpRequestGetReadyState :: XMLHttpRequest -> IO Word
xmlHttpRequestGetReadyState = getReadyState

xmlHttpRequestSetWithCredentials :: XMLHttpRequest -> Bool -> IO ()
xmlHttpRequestSetWithCredentials = setWithCredentials

xmlHttpRequestGetWithCredentials :: XMLHttpRequest -> IO Bool
xmlHttpRequestGetWithCredentials = getWithCredentials

xmlHttpRequestGetUpload :: XMLHttpRequest -> IO (Maybe XMLHttpRequestUpload)
xmlHttpRequestGetUpload = getUpload

xmlHttpRequestGetResponseText :: FromJSString result => XMLHttpRequest -> IO (Maybe result)
xmlHttpRequestGetResponseText = getResponseText

responseTextToText :: Maybe Text -> Maybe Text
responseTextToText = id

xmlHttpRequestGetResponseXML :: XMLHttpRequest -> IO (Maybe Document)
xmlHttpRequestGetResponseXML = getResponseXML

xmlHttpRequestSetResponseType :: XMLHttpRequest -> XMLHttpRequestResponseType -> IO ()
xmlHttpRequestSetResponseType = setResponseType

toResponseType :: a -> a
toResponseType = id

xmlHttpRequestGetResponseType :: XMLHttpRequest -> IO XMLHttpRequestResponseType
xmlHttpRequestGetResponseType = getResponseType

xmlHttpRequestGetStatus :: XMLHttpRequest -> IO Word
xmlHttpRequestGetStatus = getStatus

xmlHttpRequestGetStatusText :: FromJSString result => XMLHttpRequest -> IO result
xmlHttpRequestGetStatusText = getStatusText

xmlHttpRequestGetResponseURL :: FromJSString result => XMLHttpRequest -> IO result
xmlHttpRequestGetResponseURL = getResponseURL

statusTextToText :: Text -> Text
statusTextToText = id
