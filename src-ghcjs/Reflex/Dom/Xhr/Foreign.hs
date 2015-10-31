{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module Reflex.Dom.Xhr.Foreign where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import qualified Data.Text as T
import Data.Text (Text)
import Data.Word
import GHCJS.DOM.Types hiding (Text)
import Control.Applicative ((<$>))
import GHCJS.DOM.EventM
import GHCJS.DOM
import Data.Function

prepareWebView :: WebView -> IO ()
prepareWebView _ = return ()

foreign import javascript unsafe "h$isInstanceOf $1 $2"
    typeInstanceIsA' :: JSRef a -> JSRef GType -> Bool

typeInstanceIsA :: JSRef a -> GType -> Bool
typeInstanceIsA o (GType t) = typeInstanceIsA' o t

castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objRef)
      | typeInstanceIsA objRef gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


newtype XMLHttpRequest = XMLHttpRequest { unXMLHttpRequest :: JSRef XMLHttpRequest }

instance Eq XMLHttpRequest where
  (==) = eqRef `on` unXMLHttpRequest

instance ToJSRef XMLHttpRequest where
  toJSRef = return . unXMLHttpRequest
  {-# INLINE toJSRef #-}

instance FromJSRef XMLHttpRequest where
  fromJSRef = return . fmap XMLHttpRequest . maybeJSNull
  {-# INLINE fromJSRef #-}

class GObjectClass o => IsXMLHttpRequest o
toXMLHttpRequest :: IsXMLHttpRequest o => o -> XMLHttpRequest
toXMLHttpRequest = unsafeCastGObject . toGObject

instance IsXMLHttpRequest XMLHttpRequest
instance GObjectClass XMLHttpRequest where
  toGObject = GObject . castRef . unXMLHttpRequest
  unsafeCastGObject = XMLHttpRequest . castRef . unGObject

castToXMLHttpRequest :: GObjectClass obj => obj -> XMLHttpRequest
castToXMLHttpRequest = castTo gTypeXMLHttpRequest "XMLHttpRequest"

foreign import javascript unsafe "window[\"XMLHttpRequest\"]" gTypeXMLHttpRequest' :: JSRef GType
gTypeXMLHttpRequest :: GType
gTypeXMLHttpRequest = GType gTypeXMLHttpRequest'


newtype XMLHttpRequestUpload = XMLHttpRequestUpload { unXMLHttpRequestUpload :: JSRef XMLHttpRequestUpload }

instance Eq XMLHttpRequestUpload where
  (==) = eqRef `on` unXMLHttpRequestUpload

instance ToJSRef XMLHttpRequestUpload where
  toJSRef = return . unXMLHttpRequestUpload
  {-# INLINE toJSRef #-}

instance FromJSRef XMLHttpRequestUpload where
  fromJSRef = return . fmap XMLHttpRequestUpload . maybeJSNull
  {-# INLINE fromJSRef #-}

class GObjectClass o => IsXMLHttpRequestUpload o
toXMLHttpRequestUpload :: IsXMLHttpRequestUpload o => o -> XMLHttpRequestUpload
toXMLHttpRequestUpload = unsafeCastGObject . toGObject

instance IsXMLHttpRequestUpload XMLHttpRequestUpload
instance GObjectClass XMLHttpRequestUpload where
  toGObject = GObject . castRef . unXMLHttpRequestUpload
  unsafeCastGObject = XMLHttpRequestUpload . castRef . unGObject

castToXMLHttpRequestUpload :: GObjectClass obj => obj -> XMLHttpRequestUpload
castToXMLHttpRequestUpload = castTo gTypeXMLHttpRequestUpload "XMLHttpRequestUpload"

foreign import javascript unsafe "window[\"XMLHttpRequestUpload\"]" gTypeXMLHttpRequestUpload' :: JSRef GType
gTypeXMLHttpRequestUpload :: GType
gTypeXMLHttpRequestUpload = GType gTypeXMLHttpRequestUpload'

foreign import javascript unsafe "new XMLHttpRequest()"
        ghcjs_dom_xml_http_request_new ::
        IO (JSRef XMLHttpRequest)

xmlHttpRequestNew :: a -> IO XMLHttpRequest
xmlHttpRequestNew _ = XMLHttpRequest <$> ghcjs_dom_xml_http_request_new

foreign import javascript unsafe "$1[\"open\"]($2, $3, $4, $5, $6)"
        ghcjs_dom_xml_http_request_open ::
        JSRef XMLHttpRequest -> JSString -> JSString -> JSBool -> JSString -> JSString -> IO ()

xmlHttpRequestOpen ::
                   (IsXMLHttpRequest self, ToJSString method, ToJSString url, ToJSString user, ToJSString password) =>
                     self -> method -> url -> Bool -> user -> password -> IO ()
xmlHttpRequestOpen self method url async user password
  = ghcjs_dom_xml_http_request_open
      (unXMLHttpRequest (toXMLHttpRequest self))
      (toJSString method)
      (toJSString url)
      (toJSBool async)
      (toJSString user)
      (toJSString password)

foreign import javascript unsafe "($2===null)?$1[\"send\"]():$1[\"send\"]($2)"
        ghcjs_dom_xml_http_request_send ::
        JSRef XMLHttpRequest -> JSString -> IO ()

xmlHttpRequestSend ::
                   (IsXMLHttpRequest self, ToJSString payload) =>
                     self -> Maybe payload -> IO ()
xmlHttpRequestSend self payload
  = ghcjs_dom_xml_http_request_send
      (unXMLHttpRequest (toXMLHttpRequest self))
      (case payload of
            Just p -> toJSString p
            Nothing -> jsNull)


foreign import javascript unsafe "$1[\"setRequestHeader\"]($2, $3)"
        ghcjs_dom_xml_http_request_set_request_header ::
        JSRef XMLHttpRequest -> JSString -> JSString -> IO ()

xmlHttpRequestSetRequestHeader ::
                               (IsXMLHttpRequest self, ToJSString header, ToJSString value) =>
                                 self -> header -> value -> IO ()
xmlHttpRequestSetRequestHeader self header value
  = ghcjs_dom_xml_http_request_set_request_header
      (unXMLHttpRequest (toXMLHttpRequest self))
      (toJSString header)
      (toJSString value)

foreign import javascript unsafe "$1[\"abort\"]()"
        ghcjs_dom_xml_http_request_abort :: JSRef XMLHttpRequest -> IO ()

xmlHttpRequestAbort :: (IsXMLHttpRequest self) => self -> IO ()
xmlHttpRequestAbort self
  = ghcjs_dom_xml_http_request_abort
      (unXMLHttpRequest (toXMLHttpRequest self))

foreign import javascript unsafe "$1[\"getAllResponseHeaders\"]()"
        ghcjs_dom_xml_http_request_get_all_response_headers ::
        JSRef XMLHttpRequest -> IO JSString

xmlHttpRequestGetAllResponseHeaders ::
                                    (IsXMLHttpRequest self, FromJSString result) =>
                                      self -> IO result
xmlHttpRequestGetAllResponseHeaders self
  = fromJSString <$>
      (ghcjs_dom_xml_http_request_get_all_response_headers
         (unXMLHttpRequest (toXMLHttpRequest self)))

foreign import javascript unsafe "$1[\"getResponseHeader\"]($2)"
        ghcjs_dom_xml_http_request_get_response_header ::
        JSRef XMLHttpRequest -> JSString -> IO JSString

xmlHttpRequestGetResponseHeader ::
                                (IsXMLHttpRequest self, ToJSString header, FromJSString result) =>
                                  self -> header -> IO result
xmlHttpRequestGetResponseHeader self header
  = fromJSString <$>
      (ghcjs_dom_xml_http_request_get_response_header
         (unXMLHttpRequest (toXMLHttpRequest self))
         (toJSString header))

foreign import javascript unsafe "$1[\"overrideMimeType\"]($2)"
        ghcjs_dom_xml_http_request_override_mime_type ::
        JSRef XMLHttpRequest -> JSString -> IO ()

xmlHttpRequestOverrideMimeType ::
                               (IsXMLHttpRequest self, ToJSString override) =>
                                 self -> override -> IO ()
xmlHttpRequestOverrideMimeType self override
  = ghcjs_dom_xml_http_request_override_mime_type
      (unXMLHttpRequest (toXMLHttpRequest self))
      (toJSString override)

foreign import javascript unsafe
        "($1[\"dispatchEvent\"]($2) ? 1 : 0)"
        ghcjs_dom_xml_http_request_dispatch_event ::
        JSRef XMLHttpRequest -> JSRef Event -> IO Bool

xmlHttpRequestDispatchEvent ::
                            (IsXMLHttpRequest self, IsEvent evt) =>
                              self -> Maybe evt -> IO Bool
xmlHttpRequestDispatchEvent self evt
  = ghcjs_dom_xml_http_request_dispatch_event
      (unXMLHttpRequest (toXMLHttpRequest self))
      (maybe jsNull (unEvent . toEvent) evt)

cUNSENT :: Integer
cUNSENT = 0

cOPENED :: Integer
cOPENED = 1

cHEADERS_RECEIVED :: Integer
cHEADERS_RECEIVED = 2

cLOADING :: Integer
cLOADING = 3

cDONE :: Integer
cDONE = 4

xmlHttpRequestOnabort ::
                      (IsXMLHttpRequest self) => Signal self (EventM UIEvent self ())
xmlHttpRequestOnabort = (connect "abort")

xmlHttpRequestOnerror ::
                      (IsXMLHttpRequest self) => Signal self (EventM UIEvent self ())
xmlHttpRequestOnerror = (connect "error")

xmlHttpRequestOnload ::
                     (IsXMLHttpRequest self) => Signal self (EventM UIEvent self ())
xmlHttpRequestOnload = (connect "load")

xmlHttpRequestOnloadend ::
                        (IsXMLHttpRequest self) => Signal self (EventM UIEvent self ())
xmlHttpRequestOnloadend = (connect "loadend")

xmlHttpRequestOnloadstart ::
                          (IsXMLHttpRequest self) => Signal self (EventM UIEvent self ())
xmlHttpRequestOnloadstart = (connect "loadstart")

xmlHttpRequestOnprogress ::
                         (IsXMLHttpRequest self) => Signal self (EventM UIEvent self ())
xmlHttpRequestOnprogress = (connect "progress")

xmlHttpRequestOntimeout ::
                        (IsXMLHttpRequest self) => Signal self (EventM UIEvent self ())
xmlHttpRequestOntimeout = (connect "timeout")

xmlHttpRequestOnreadystatechange ::
                                 (IsXMLHttpRequest self) => Signal self (EventM UIEvent self ())
xmlHttpRequestOnreadystatechange = (connect "readystatechange")

foreign import javascript unsafe "$1[\"timeout\"] = $2;"
        ghcjs_dom_xml_http_request_set_timeout ::
        JSRef XMLHttpRequest -> Word -> IO ()

xmlHttpRequestSetTimeout ::
                         (IsXMLHttpRequest self) => self -> Word -> IO ()
xmlHttpRequestSetTimeout self val
  = ghcjs_dom_xml_http_request_set_timeout
      (unXMLHttpRequest (toXMLHttpRequest self))
      val

foreign import javascript unsafe "$1[\"timeout\"]"
        ghcjs_dom_xml_http_request_get_timeout ::
        JSRef XMLHttpRequest -> IO Word

xmlHttpRequestGetTimeout ::
                         (IsXMLHttpRequest self) => self -> IO Word
xmlHttpRequestGetTimeout self
  = ghcjs_dom_xml_http_request_get_timeout
      (unXMLHttpRequest (toXMLHttpRequest self))

foreign import javascript unsafe "$1[\"readyState\"]"
        ghcjs_dom_xml_http_request_get_ready_state ::
        JSRef XMLHttpRequest -> IO Word

xmlHttpRequestGetReadyState ::
                            (IsXMLHttpRequest self) => self -> IO Word
xmlHttpRequestGetReadyState self
  = ghcjs_dom_xml_http_request_get_ready_state
      (unXMLHttpRequest (toXMLHttpRequest self))

foreign import javascript unsafe "$1[\"withCredentials\"] = $2;"
        ghcjs_dom_xml_http_request_set_with_credentials ::
        JSRef XMLHttpRequest -> Bool -> IO ()

xmlHttpRequestSetWithCredentials ::
                                 (IsXMLHttpRequest self) => self -> Bool -> IO ()
xmlHttpRequestSetWithCredentials self val
  = ghcjs_dom_xml_http_request_set_with_credentials
      (unXMLHttpRequest (toXMLHttpRequest self))
      val

foreign import javascript unsafe
        "($1[\"withCredentials\"] ? 1 : 0)"
        ghcjs_dom_xml_http_request_get_with_credentials ::
        JSRef XMLHttpRequest -> IO Bool

xmlHttpRequestGetWithCredentials ::
                                 (IsXMLHttpRequest self) => self -> IO Bool
xmlHttpRequestGetWithCredentials self
  = ghcjs_dom_xml_http_request_get_with_credentials
      (unXMLHttpRequest (toXMLHttpRequest self))

foreign import javascript unsafe "$1[\"upload\"]"
        ghcjs_dom_xml_http_request_get_upload ::
        JSRef XMLHttpRequest -> IO (JSRef XMLHttpRequestUpload)

xmlHttpRequestGetUpload ::
                        (IsXMLHttpRequest self) => self -> IO (Maybe XMLHttpRequestUpload)
xmlHttpRequestGetUpload self
  = fmap XMLHttpRequestUpload . maybeJSNull <$>
      (ghcjs_dom_xml_http_request_get_upload
         (unXMLHttpRequest (toXMLHttpRequest self)))

foreign import javascript unsafe "$1[\"responseText\"]"
        ghcjs_dom_xml_http_request_get_response_text ::
        JSRef XMLHttpRequest -> IO JSString

xmlHttpRequestGetResponseText ::
                             (IsXMLHttpRequest self, FromJSString result) => self -> IO (Maybe result)
xmlHttpRequestGetResponseText self
  = fmap fromJSString . maybeJSNull <$>
      (ghcjs_dom_xml_http_request_get_response_text
         (unXMLHttpRequest (toXMLHttpRequest self)))

responseTextToText :: (Maybe JSString) -> Maybe Text
responseTextToText r = fmap (T.pack . fromJSString) r

statusTextToText :: Text -> Text
statusTextToText = id

foreign import javascript unsafe "$1[\"responseXML\"]"
        ghcjs_dom_xml_http_request_get_response_xml ::
        JSRef XMLHttpRequest -> IO (JSRef Document)

xmlHttpRequestGetResponseXML ::
                             (IsXMLHttpRequest self) => self -> IO (Maybe Document)
xmlHttpRequestGetResponseXML self
  = fmap Document . maybeJSNull <$>
      (ghcjs_dom_xml_http_request_get_response_xml
         (unXMLHttpRequest (toXMLHttpRequest self)))

foreign import javascript unsafe "$1[\"responseType\"] = $2;"
        ghcjs_dom_xml_http_request_set_response_type ::
        JSRef XMLHttpRequest -> JSString -> IO ()

xmlHttpRequestSetResponseType ::
                              (IsXMLHttpRequest self, ToJSString val) => self -> val -> IO ()
xmlHttpRequestSetResponseType self val
  = ghcjs_dom_xml_http_request_set_response_type
      (unXMLHttpRequest (toXMLHttpRequest self))
      (toJSString val)

toResponseType :: (ToJSString a) => a -> JSString
toResponseType = toJSString

foreign import javascript unsafe "$1[\"responseType\"]"
        ghcjs_dom_xml_http_request_get_response_type ::
        JSRef XMLHttpRequest -> IO JSString

xmlHttpRequestGetResponseType ::
                              (IsXMLHttpRequest self, FromJSString result) => self -> IO result
xmlHttpRequestGetResponseType self
  = fromJSString <$>
      (ghcjs_dom_xml_http_request_get_response_type
         (unXMLHttpRequest (toXMLHttpRequest self)))

foreign import javascript unsafe "$1[\"status\"]"
        ghcjs_dom_xml_http_request_get_status ::
        JSRef XMLHttpRequest -> IO Word

xmlHttpRequestGetStatus ::
                        (IsXMLHttpRequest self) => self -> IO Word
xmlHttpRequestGetStatus self
  = ghcjs_dom_xml_http_request_get_status
      (unXMLHttpRequest (toXMLHttpRequest self))

foreign import javascript unsafe "$1[\"statusText\"]"
        ghcjs_dom_xml_http_request_get_status_text ::
        JSRef XMLHttpRequest -> IO JSString

xmlHttpRequestGetStatusText ::
                            (IsXMLHttpRequest self, FromJSString result) => self -> IO result
xmlHttpRequestGetStatusText self
  = fromJSString <$>
      (ghcjs_dom_xml_http_request_get_status_text
         (unXMLHttpRequest (toXMLHttpRequest self)))

foreign import javascript unsafe "$1[\"responseURL\"]"
        ghcjs_dom_xml_http_request_get_response_url ::
        JSRef XMLHttpRequest -> IO JSString

xmlHttpRequestGetResponseURL ::
                             (IsXMLHttpRequest self, FromJSString result) => self -> IO result
xmlHttpRequestGetResponseURL self
  = fromJSString <$>
      (ghcjs_dom_xml_http_request_get_response_url
         (unXMLHttpRequest (toXMLHttpRequest self)))

