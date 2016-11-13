{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Reflex.Dom.Xhr.Foreign (
    XMLHttpRequest
  , XMLHttpRequestResponseType(..)
  , module Reflex.Dom.Xhr.Foreign
) where

import Control.Exception (catch, throwIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHCJS.DOM
import GHCJS.DOM.Enums
import GHCJS.DOM.EventM (EventM, on)
import GHCJS.DOM.EventTarget (dispatchEvent)
import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM.XMLHttpRequest
import GHCJS.Types
import Prelude hiding (error)
import Reflex.Dom.Internal.Foreign
import Reflex.Dom.Xhr.Exception
import Reflex.Dom.Xhr.ResponseType

prepareWebView :: WebView -> IO ()
prepareWebView _ = return ()

xmlHttpRequestNew :: a -> IO XMLHttpRequest
xmlHttpRequestNew _ = newXMLHttpRequest

xmlHttpRequestOpen ::
                   (ToJSString method, ToJSString url, ToJSString user, ToJSString password) =>
                     XMLHttpRequest -> method -> url -> Bool -> user -> password -> IO ()
xmlHttpRequestOpen = open

convertException :: XHRError -> XhrException
convertException e = case e of
  XHRError -> XhrException_Error
  XHRAborted -> XhrException_Aborted

class IsXhrPayload a where
  sendXhrPayload :: XMLHttpRequest -> a -> IO ()

instance IsXhrPayload () where
  sendXhrPayload xhr _ = send xhr

instance IsXhrPayload String where
  sendXhrPayload = sendString

instance IsXhrPayload Text where
  sendXhrPayload = sendString

instance IsXhrPayload FormData where
  sendXhrPayload = sendFormData

instance IsXhrPayload Document where
  sendXhrPayload = sendDocument

instance IsXhrPayload Blob where
  sendXhrPayload = sendBlob

newtype XhrPayload = XhrPayload { unXhrPayload :: JSVal }

-- This used to be a non blocking call, but now it uses an interruptible ffi
xmlHttpRequestSend :: IsXhrPayload payload => XMLHttpRequest -> payload -> IO ()
xmlHttpRequestSend self p = sendXhrPayload self p `catch` (throwIO . convertException)


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

xmlHttpRequestGetResponseXML :: XMLHttpRequest -> IO (Maybe Document)
xmlHttpRequestGetResponseXML = getResponseXML

xmlHttpRequestSetResponseType :: XMLHttpRequest -> XMLHttpRequestResponseType -> IO ()
xmlHttpRequestSetResponseType = setResponseType

fromResponseType :: XhrResponseType -> XMLHttpRequestResponseType
fromResponseType XhrResponseType_Default = XMLHttpRequestResponseType
fromResponseType XhrResponseType_ArrayBuffer = XMLHttpRequestResponseTypeArraybuffer
fromResponseType XhrResponseType_Blob = XMLHttpRequestResponseTypeBlob
fromResponseType XhrResponseType_Text = XMLHttpRequestResponseTypeText

toResponseType :: XMLHttpRequestResponseType -> Maybe XhrResponseType
toResponseType XMLHttpRequestResponseType = Just XhrResponseType_Default
toResponseType XMLHttpRequestResponseTypeArraybuffer = Just XhrResponseType_ArrayBuffer
toResponseType XMLHttpRequestResponseTypeBlob = Just XhrResponseType_Blob
toResponseType XMLHttpRequestResponseTypeText = Just XhrResponseType_Text
toResponseType _ = Nothing

xmlHttpRequestGetResponseType :: XMLHttpRequest -> IO (Maybe XhrResponseType)
xmlHttpRequestGetResponseType = fmap toResponseType . getResponseType

xmlHttpRequestGetStatus :: XMLHttpRequest -> IO Word
xmlHttpRequestGetStatus = getStatus

xmlHttpRequestGetStatusText :: FromJSString result => XMLHttpRequest -> IO result
xmlHttpRequestGetStatusText = getStatusText

xmlHttpRequestGetResponseURL :: FromJSString result => XMLHttpRequest -> IO result
xmlHttpRequestGetResponseURL = getResponseURL

xmlHttpRequestGetResponse :: XMLHttpRequest -> IO (Maybe XhrResponseBody)
xmlHttpRequestGetResponse xhr = do
  mr <- getResponse xhr
  rt <- xmlHttpRequestGetResponseType xhr
  case rt of
       Just XhrResponseType_Blob -> return $ XhrResponseBody_Blob . castToBlob <$> mr
       Just XhrResponseType_Text -> Just . XhrResponseBody_Text <$> xmlHttpRequestGetStatusText xhr
       Just XhrResponseType_Default -> Just . XhrResponseBody_Text <$> xmlHttpRequestGetStatusText xhr
       Just XhrResponseType_ArrayBuffer -> case fmap unGObject mr of
         Nothing -> return Nothing
         Just ptr -> Just . XhrResponseBody_ArrayBuffer <$> bsFromArrayBuffer ptr ptr
       _ -> return Nothing

