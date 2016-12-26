{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Foreign.JavaScript.TH ( module Foreign.JavaScript.TH
                             , Safety (..)
                             ) where

import Reflex.Class
import Reflex.DynamicWriter
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Base
import Reflex.Requester.Base

import Language.Haskell.TH

#ifdef __GHCJS__
import qualified GHCJS.Buffer as JS
import GHCJS.DOM
import GHCJS.DOM.Types hiding (Text, fromJSString)
import qualified GHCJS.DOM.Types as JS
import qualified GHCJS.Foreign as JS
import qualified GHCJS.Foreign.Callback as JS
import qualified GHCJS.Foreign.Callback.Internal (Callback (..))
import qualified GHCJS.Marshal as JS
import qualified GHCJS.Marshal.Pure as JS
import qualified GHCJS.Types as JS
import qualified JavaScript.Array as JS
import qualified JavaScript.Array.Internal (SomeJSArray (..))
import qualified JavaScript.Object as JS
import qualified JavaScript.Object.Internal (Object (..))
import qualified JavaScript.TypedArray.ArrayBuffer as JSArrayBuffer

import Data.Hashable
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Text.Encoding.Z
#else
import Foreign.Marshal
import Graphics.UI.Gtk.WebKit.DOM.Node
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame
import Graphics.UI.Gtk.WebKit.WebView
import System.Glib.FFI
#endif

import Control.Concurrent
import Control.Monad
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

class Monad m => HasWebView m where
  type WebViewPhantom m :: *
  askWebView :: m (WebViewSingleton (WebViewPhantom m))

instance HasWebView m => HasWebView (ReaderT r m) where
  type WebViewPhantom (ReaderT r m) = WebViewPhantom m
  askWebView = lift askWebView

instance HasWebView m => HasWebView (StateT r m) where
  type WebViewPhantom (StateT r m) = WebViewPhantom m
  askWebView = lift askWebView

instance HasWebView m => HasWebView (Strict.StateT r m) where
  type WebViewPhantom (Strict.StateT r m) = WebViewPhantom m
  askWebView = lift askWebView

instance HasWebView m => HasWebView (PostBuildT t m) where
  type WebViewPhantom (PostBuildT t m) = WebViewPhantom m
  askWebView = lift askWebView

instance (ReflexHost t, HasWebView (HostFrame t)) => HasWebView (PerformEventT t m) where
  type WebViewPhantom (PerformEventT t m) = WebViewPhantom (HostFrame t)
  askWebView = PerformEventT $ lift askWebView

instance HasWebView m => HasWebView (DynamicWriterT t w m) where
  type WebViewPhantom (DynamicWriterT t w m) = WebViewPhantom m
  askWebView = lift askWebView

instance HasWebView m => HasWebView (RequesterT t request response m) where
  type WebViewPhantom (RequesterT t request response m) = WebViewPhantom m
  askWebView = lift askWebView

newtype WithWebView x m a = WithWebView { unWithWebView :: ReaderT (WebViewSingleton x) m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans, MonadException, MonadAsyncException)

instance PrimMonad m => PrimMonad (WithWebView x m) where
  type PrimState (WithWebView x m) = PrimState m
  primitive = lift . primitive

instance MonadAdjust t m => MonadAdjust t (WithWebView x m) where
  runWithReplace a0 a' = WithWebView $ runWithReplace (coerce a0) (coerceEvent a')
  sequenceDMapWithAdjust dm0 dm' = WithWebView $ sequenceDMapWithAdjust (coerce dm0) (coerceEvent dm')

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (WithWebView x m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (WithWebView x m) where
  {-# INLINABLE subscribeEvent #-}
  subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (WithWebView x m) where
  type ReadPhase (WithWebView x m) = ReadPhase m
  {-# INLINABLE fireEventsAndRead #-}
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  {-# INLINABLE runHostFrame #-}
  runHostFrame = lift . runHostFrame

instance MonadSample t m => MonadSample t (WithWebView x m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (WithWebView x m) where
  {-# INLINABLE hold #-}
  hold v0 = lift . hold v0
  {-# INLINABLE holdDyn #-}
  holdDyn v0 = lift . holdDyn v0
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 = lift . holdIncremental v0

instance MonadTransControl (WithWebView x) where
  type StT (WithWebView x) a = StT (ReaderT (WebViewSingleton x)) a
  {-# INLINABLE liftWith #-}
  liftWith = defaultLiftWith WithWebView unWithWebView
  {-# INLINABLE restoreT #-}
  restoreT = defaultRestoreT WithWebView

instance PerformEvent t m => PerformEvent t (WithWebView x m) where
  type Performable (WithWebView x m) = WithWebView x (Performable m)
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = liftWith $ \run -> performEvent_ $ fmap run e
  {-# INLINABLE performEvent #-}
  performEvent e = liftWith $ \run -> performEvent $ fmap run e

runWithWebView :: WithWebView x m a -> WebViewSingleton x -> m a
runWithWebView = runReaderT . unWithWebView

instance (Monad m) => HasWebView (WithWebView x m) where
  type WebViewPhantom (WithWebView x m) = x
  askWebView = WithWebView ask

instance MonadRef m => MonadRef (WithWebView x m) where
  type Ref (WithWebView x m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (WithWebView x m) where
  atomicModifyRef r = lift . atomicModifyRef r

withWebViewSingleton :: WebView -> (forall x. WebViewSingleton x -> r) -> r
withWebViewSingleton wv f = f $ WebViewSingleton wv

-- | Warning: `withWebViewSingletonMono` does not provide the same guarantees that `withWebViewSingleton` does.
withWebViewSingletonMono :: WebView -> (WebViewSingleton () -> r) -> r
withWebViewSingletonMono wv f = f $ WebViewSingleton wv

-- | A singleton type for a given WebView; we use this to statically guarantee that different WebViews (and thus different javscript contexts) don't get mixed up
newtype WebViewSingleton x = WebViewSingleton { unWebViewSingleton :: WebView }

#ifdef __GHCJS__
type JSFFI_Internal = JS.MutableJSArray -> IO JS.JSVal
newtype JSFFI = JSFFI JSFFI_Internal
#else
newtype JSFFI = JSFFI String
#endif

newtype JSFun x = JSFun { unJSFun :: JSRef x }

instance ToJS x (JSFun x) where
  withJS (JSFun r) f = f r

instance FromJS x (JSFun x) where
  fromJS = return . JSFun

class IsJSContext x where
  data JSRef x

class (Monad m, MonadIO (JSM m), MonadFix (JSM m), MonadJS x (JSM m)) => HasJS x m | m -> x where
  type JSM m :: * -> *
  liftJS :: JSM m a -> m a

instance HasJS x m => HasJS x (ReaderT r m) where
  type JSM (ReaderT r m) = JSM m
  liftJS = lift . liftJS

instance (HasJS x m, ReflexHost t) => HasJS x (PostBuildT t m) where
  type JSM (PostBuildT t m) = JSM m
  liftJS = lift . liftJS

instance (HasJS x (HostFrame t), ReflexHost t) => HasJS x (PerformEventT t m) where
  type JSM (PerformEventT t m) = JSM (HostFrame t)
  liftJS = PerformEventT . lift . liftJS

instance HasJS x m => HasJS x (DynamicWriterT t w m) where
  type JSM (DynamicWriterT t w m) = JSM m
  liftJS = lift . liftJS

instance HasJS x m => HasJS x (RequesterT t request response m) where
  type JSM (RequesterT t request response m) = JSM m
  liftJS = lift . liftJS

-- | A Monad that is capable of executing JavaScript
class Monad m => MonadJS x m | m -> x where
  runJS :: JSFFI -> [JSRef x] -> m (JSRef x)
  forkJS :: m () -> m ThreadId
  mkJSUndefined :: m (JSRef x)
  isJSNull :: JSRef x -> m Bool
  isJSUndefined :: JSRef x -> m Bool
  fromJSBool :: JSRef x -> m Bool
  fromJSString :: JSRef x -> m String
  fromJSArray :: JSRef x -> m [JSRef x]
  fromJSUint8Array :: JSRef x -> m ByteString
  fromJSNumber :: JSRef x -> m Double
  withJSBool :: Bool -> (JSRef x -> m r) -> m r
  withJSString :: String -> (JSRef x -> m r) -> m r
  withJSNumber :: Double -> (JSRef x -> m r) -> m r
  withJSArray :: [JSRef x] -> (JSRef x -> m r) -> m r
  withJSUint8Array :: ByteString -> (JSUint8Array x -> m r) -> m r
  -- | Create a JSFun with zero arguments; should be equilvant to `syncCallback AlwaysRetain True` in GHCJS
  mkJSFun :: ([JSRef x] -> m (JSRef x)) -> m (JSFun x) --TODO: Support 'this', exceptions
  freeJSFun :: JSFun x -> m ()
  setJSProp :: String -> JSRef x -> JSRef x -> m ()
  getJSProp :: String -> JSRef x -> m (JSRef x)
  withJSNode :: Node -> (JSRef x -> m r) -> m r

#ifdef __GHCJS__

data JSCtx_IO

instance MonadIO m => HasJS JSCtx_IO (WithWebView x m) where
  type JSM (WithWebView x m) = IO
  liftJS = liftIO

instance IsJSContext JSCtx_IO where
  newtype JSRef JSCtx_IO = JSRef_IO { unJSRef_IO :: JS.JSVal }

instance MonadJS JSCtx_IO IO where
  runJS (JSFFI f) l = fmap JSRef_IO . f =<< JS.fromListIO (coerce l)
  forkJS = forkIO
  mkJSUndefined = return $ JSRef_IO JS.jsUndefined
  isJSNull (JSRef_IO r) = return $ JS.isNull r
  isJSUndefined (JSRef_IO r) = return $ JS.isUndefined r
  fromJSBool (JSRef_IO r) = return $ JS.fromJSBool r
  fromJSString (JSRef_IO r) = return $ JS.fromJSString $ JS.pFromJSVal r
  fromJSArray (JSRef_IO r) = fmap coerce $ JS.toListIO $ coerce r
  fromJSUint8Array (JSRef_IO r) = fmap (JS.toByteString 0 Nothing . JS.createFromArrayBuffer) $ JSArrayBuffer.unsafeFreeze $ JS.pFromJSVal r --TODO: Assert that this is immutable
  fromJSNumber (JSRef_IO r) = do
    Just n <- JS.fromJSVal r
    return n
  withJSBool b f = f $ JSRef_IO $ JS.toJSBool b
  withJSString s f = f $ JSRef_IO $ JS.pToJSVal $ JS.toJSString s
  withJSNumber n f = do
    r <- JS.toJSVal n
    f $ JSRef_IO r
  withJSArray l f = do
    r <- JS.fromListIO $ coerce l
    f $ JSRef_IO $ coerce r
  withJSUint8Array payload f = BS.useAsCString payload $ \cStr -> do
    ba <- extractByteArray cStr $ BS.length payload
    f $ JSUint8Array $ JSRef_IO ba
  mkJSFun f = do
    cb <- JS.syncCallback1' $ \args -> do
      l <- JS.toListIO $ coerce args
      JSRef_IO result <- f $ coerce l
      return result
    fmap (JSFun . JSRef_IO) $ funWithArguments $ coerce cb
  freeJSFun (JSFun (JSRef_IO r)) = JS.releaseCallback $ coerce r
  setJSProp s (JSRef_IO v) (JSRef_IO o) = JS.setProp (JS.toJSString s) v $ coerce o
  getJSProp s (JSRef_IO o) = do
    r <- JS.getProp (JS.toJSString s) $ coerce o
    return $ JSRef_IO r
  withJSNode n f = f $ JSRef_IO $ unNode n

foreign import javascript unsafe "new Uint8Array($1_1.buf, $1_2, $2)" extractByteArray :: Ptr CChar -> Int -> IO JS.JSVal

foreign import javascript unsafe "function(){ return $1(arguments); }" funWithArguments :: JS.Callback (JS.MutableJSArray -> IO a) -> IO JS.JSVal

#else

-- | Make a JS string available that is not wrapped up as a JSValueRef
withJSStringRaw :: MonadAsyncException m => String -> (JSStringRef -> m r) -> m r
withJSStringRaw str a = do
  bracket (liftIO $ jsstringcreatewithutf8cstring str) (liftIO . jsstringrelease) $ \strRef -> do
    a strRef

fromJSNumberRaw :: (MonadIO m, HasJSContext m) => JSValueRef -> m Double
fromJSNumberRaw val = do
  jsContext <- askJSContext
  liftIO $ jsvaluetonumber jsContext val nullPtr --TODO: Exceptions


data JSCtx_JavaScriptCore x

instance IsJSContext (JSCtx_JavaScriptCore x) where
  newtype JSRef (JSCtx_JavaScriptCore x) = JSRef_JavaScriptCore { unJSRef_JavaScriptCore :: JSValueRef }

instance MonadIO m => HasJS (JSCtx_JavaScriptCore x) (WithWebView x m) where
  type JSM (WithWebView x m) = WithWebView x IO
  liftJS a = do
    wv <- askWebView
    liftIO $ runWithWebView a wv

newtype WithJSContext x m a = WithJSContext { unWithJSContext :: ReaderT JSContextRef m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans, MonadException, MonadAsyncException)

runWithJSContext :: WithJSContext x m a -> JSContextRef -> m a
runWithJSContext = runReaderT . unWithJSContext

class Monad m => HasJSContext m where
  askJSContext :: m JSContextRef

instance MonadIO m => HasJSContext (WithWebView x m) where
  askJSContext = do
    wv <- askWebView
    liftIO $ webFrameGetGlobalContext =<< webViewGetMainFrame (unWebViewSingleton wv)

instance Monad m => HasJSContext (WithJSContext x m) where
  askJSContext = WithJSContext ask

lowerWithJSContext :: (HasJSContext m, MonadIO m) => WithJSContext x IO a -> m a
lowerWithJSContext a = do
  c <- askJSContext
  liftIO $ runWithJSContext a c

liftWithWebViewThroughWithJSContext :: (HasJSContext m, HasWebView m, MonadIO m, MonadTrans t, Monad m1)
                                    => ((t1 -> t m1 a) -> WithJSContext x IO b)
                                    -> (t1 -> WithWebView (WebViewPhantom m) m1 a)
                                    -> m b
liftWithWebViewThroughWithJSContext f a = do
  wv <- askWebView
  lowerWithJSContext $ f $ \b' -> lift $ runWithWebView (a b') wv

instance MonadJS (JSCtx_JavaScriptCore x) (WithWebView x IO) where
  forkJS a = do
    wv <- askWebView
    liftIO $ forkIO $ runWithWebView a wv
  mkJSFun a = do
    wv <- askWebView
    lowerWithJSContext $ mkJSFun $ \args -> lift $ runWithWebView (a args) wv
  runJS expr args = lowerWithJSContext $ runJS expr args
  mkJSUndefined = lowerWithJSContext mkJSUndefined
  isJSNull = lowerWithJSContext . isJSNull
  isJSUndefined = lowerWithJSContext . isJSUndefined
  fromJSBool = lowerWithJSContext . fromJSBool
  fromJSString = lowerWithJSContext . fromJSString
  fromJSArray = lowerWithJSContext . fromJSArray
  fromJSUint8Array = lowerWithJSContext . fromJSUint8Array
  fromJSNumber = lowerWithJSContext . fromJSNumber
  freeJSFun = lowerWithJSContext . freeJSFun
  withJSBool = liftWithWebViewThroughWithJSContext . withJSBool
  withJSString = liftWithWebViewThroughWithJSContext . withJSString
  withJSNumber = liftWithWebViewThroughWithJSContext . withJSNumber
  withJSArray = liftWithWebViewThroughWithJSContext . withJSArray
  withJSUint8Array = liftWithWebViewThroughWithJSContext . withJSUint8Array
  withJSNode = liftWithWebViewThroughWithJSContext . withJSNode
  setJSProp propName valRef objRef = lowerWithJSContext $ setJSProp propName valRef objRef
  getJSProp propName objRef = lowerWithJSContext $ getJSProp propName objRef

instance MonadJS (JSCtx_JavaScriptCore x) (WithJSContext x IO) where
  runJS (JSFFI body) args = do
    jsContext <- askJSContext
    withJSArray args $ \(JSRef_JavaScriptCore this) -> liftIO $ do
      result <- bracket (jsstringcreatewithutf8cstring body) jsstringrelease $ \script -> jsevaluatescript jsContext script this nullPtr 1 nullPtr
      return $ JSRef_JavaScriptCore result --TODO: Protect and unprotect result
  forkJS a = do
    c <- askJSContext
    liftIO $ forkIO $ runWithJSContext a c
  mkJSUndefined = do
    jsContext <- askJSContext
    fmap JSRef_JavaScriptCore $ liftIO $ jsvaluemakeundefined jsContext
  isJSNull (JSRef_JavaScriptCore r) = do
    jsContext <- askJSContext
    liftIO $ jsvalueisnull jsContext r
  isJSUndefined (JSRef_JavaScriptCore r) = do
    jsContext <- askJSContext
    liftIO $ jsvalueisundefined jsContext r
  fromJSBool (JSRef_JavaScriptCore r) = do
    jsContext <- askJSContext
    liftIO $ jsvaluetoboolean jsContext r
  fromJSString (JSRef_JavaScriptCore r) = do
    jsContext <- askJSContext
    liftIO $ do
      s <- jsvaluetostringcopy jsContext r nullPtr --TODO: Deal with exceptions
      l <- jsstringgetmaximumutf8cstringsize s
      allocaBytes (fromIntegral l) $ \ps -> do
        _ <- jsstringgetutf8cstring'_ s ps (fromIntegral l)
        peekCString ps
  withJSBool b a = do
    jsContext <- askJSContext
    valRef <- liftIO $ jsvaluemakeboolean jsContext b
    a $ JSRef_JavaScriptCore valRef
  withJSString str a = do
    jsContext <- askJSContext
    withJSStringRaw str $ \strRef -> do
      valRef <- liftIO $ jsvaluemakestring jsContext strRef
      a $ JSRef_JavaScriptCore valRef --TODO: Protect/unprotect valRef
  withJSNumber n a = do
    jsContext <- askJSContext
    valRef <- liftIO $ jsvaluemakenumber jsContext n
    a $ JSRef_JavaScriptCore valRef --TODO: Protect/unprotect valRef
  withJSArray elems a = do
    jsContext <- askJSContext
    let numElems = length elems
    bracket (liftIO $ mallocArray numElems) (liftIO . free) $ \elemsArr -> do
      liftIO $ pokeArray elemsArr $ coerce elems
      bracket (liftIO $ jsobjectmakearray jsContext (fromIntegral numElems) elemsArr nullPtr) (const $ return ()) $ \arrRef -> do --TODO: Do we need to protect/unprotect the array object?
        a $ JSRef_JavaScriptCore arrRef --TODO: Protect/unprotect valRef
  --TODO: When supported by webkitgtk3-javascriptcore, go directly from C string to Uint8Array without creating each item individually
  withJSUint8Array payload f = withJSArrayFromList (BS.unpack payload) $ \x -> do
    payloadRef <- runJS (JSFFI "new Uint8Array(this[0])") [x]
    f $ JSUint8Array payloadRef
  fromJSArray (JSRef_JavaScriptCore a) = do
    jsContext <- askJSContext
    lenRef <- withJSStringRaw "length" $ \lengthStr -> do
      liftIO $ jsobjectgetproperty jsContext a lengthStr nullPtr --TODO: Exceptions
    lenDouble <- fromJSNumberRaw lenRef
    let len = round lenDouble
    liftIO $ forM [0..len-1] $ \i -> do
      JSRef_JavaScriptCore <$> jsobjectgetpropertyatindex jsContext a i nullPtr --TODO: Exceptions
  fromJSUint8Array a = do
    vals <- fromJSArray a
    doubles <- mapM fromJSNumber vals
    return $ BS.pack $ map round doubles
  fromJSNumber (JSRef_JavaScriptCore val) = do
    jsContext <- askJSContext
    liftIO $ jsvaluetonumber jsContext val nullPtr --TODO: Exceptions
  mkJSFun a = do
    jsContext <- askJSContext
    cb <- liftIO $ mkJSObjectCallAsFunctionCallback $ \_ _ _ argc argv _ -> do --TODO: Use the exception pointer to handle haskell exceptions
      args <- forM [0..fromIntegral (argc-1)] $ \n -> do
        x <- peekElemOff argv n
        jsvalueprotect jsContext x
        return $ JSRef_JavaScriptCore x --TODO: Unprotect eventually
      unJSRef_JavaScriptCore <$> runWithJSContext (a args) jsContext
    cbRef <- liftIO $ jsobjectmakefunctionwithcallback jsContext nullPtr cb
    liftIO $ jsvalueprotect jsContext cbRef
    return $ JSFun $ JSRef_JavaScriptCore cbRef
  freeJSFun (JSFun (JSRef_JavaScriptCore f)) = do
    jsContext <- askJSContext
    liftIO $ jsvalueunprotect jsContext f
  setJSProp propName (JSRef_JavaScriptCore valRef) (JSRef_JavaScriptCore objRef) = do
    withJSStringRaw propName $ \propNameRaw -> do
      jsContext <- askJSContext
      liftIO $ jsobjectsetproperty jsContext objRef propNameRaw valRef 0 nullPtr --TODO: property attribute, exceptions
  getJSProp propName (JSRef_JavaScriptCore objRef) = do
    withJSStringRaw propName $ \propNameRaw -> do
      jsContext <- askJSContext
      liftIO $ JSRef_JavaScriptCore <$> jsobjectgetproperty jsContext objRef propNameRaw nullPtr --TODO: property attribute, exceptions
  withJSNode = error "withJSNode is only supported in ghcjs"

#endif

class FromJS x a where
  fromJS :: MonadJS x m => JSRef x -> m a

instance FromJS x () where
  fromJS _ = return () --TODO: Should this do some kind of checking for the js value?

instance FromJS x Bool where
  fromJS = fromJSBool

instance ToJS x Bool where
  withJS = withJSBool

instance FromJS x String where
  fromJS = fromJSString

instance FromJS x Text where
  fromJS s = T.pack <$> fromJSString s

instance FromJS x a => FromJS x (Maybe a) where
  fromJS x = do
    n <- isJSNull x
    if n then return Nothing else Just <$> fromJS x

class ToJS x a where
  withJS :: MonadJS x m => a -> (JSRef x -> m r) -> m r

instance ToJS x (JSRef x) where
  withJS r = ($ r)

instance FromJS x (JSRef x) where
  fromJS = return

instance ToJS x String where
  withJS = withJSString

instance ToJS x Text where
  withJS = withJSString . T.unpack

newtype JSArray a = JSArray { unJSArray :: [a] }

instance ToJS x a => ToJS x (JSArray a) where
  withJS = withJSArrayFromList . unJSArray

instance FromJS x a => FromJS x (JSArray a) where
  fromJS = fmap JSArray . mapM fromJS <=< fromJSArray

withJSArrayFromList :: (ToJS x a, MonadJS x m) => [a] -> (JSRef x -> m r) -> m r
withJSArrayFromList as f = go as []
  where go [] jsRefs = withJSArray (reverse jsRefs) f
        go (h:t) jsRefs = withJS h $ \hRef -> go t (hRef : jsRefs)

newtype JSUint8Array x = JSUint8Array { unJSUint8Array :: JSRef x }

instance ToJS x (JSUint8Array x) where
  withJS (JSUint8Array r) = ($ r)

instance FromJS x (JSUint8Array x) where
  fromJS = return . JSUint8Array

instance ToJS x Word8 where
  withJS n = withJSNumber $ fromIntegral n --TODO: Check things; throw exceptions

instance ToJS x Int where
  withJS n = withJSNumber $ fromIntegral n --TODO: Check things; throw exceptions

instance FromJS x Int where
  fromJS = fmap round . fromJSNumber --TODO: Check things; throw exceptions

instance ToJS x Double where
  withJS = withJSNumber

instance FromJS x Double where
  fromJS = fromJSNumber

instance ToJS x Node where
  withJS = withJSNode


importJS :: Safety -> String -> String -> Q Type -> Q [Dec]
importJS safety body name qt = do
  t <- qt
  let (argTypes, _) = parseType t
  argNames <- forM argTypes $ \_ -> do
    arg <- newName "arg"
    argRef <- newName "argRef"
    return (arg, argRef)
  (jsffiDecs, jsffiExp) <- mkJSFFI safety body
  let go [] = [| runJS $(return jsffiExp) $(listE $ map (varE . snd) argNames) >>= fromJS
               |]
      go ((arg, argRef) : args) = [| withJS $(varE arg) $ $(lamE [varP argRef] $ go args) |]
  e <- lamE (map (varP. fst) argNames) $ go argNames
  let n = mkName name
  return $ jsffiDecs ++
    [ SigD n t
    , ValD (VarP n) (NormalB e) []
    ]

mkJSFFI :: Safety -> String -> Q ([Dec], Exp)
#ifdef __GHCJS__
mkJSFFI safety body = do
  -- n <- newName "jsffi" --TODO: Should use newName, but that doesn't seem to work with ghcjs
  l <- location
  n <- newName $ "jsffi_" <> zEncodeString (loc_package l <> ":" <> loc_module l) <> "_" <> show (abs (hash (show safety, body)))
  t <- [t| JSFFI_Internal |]
  let wrappedBody = "(function(){ return (" <> body <> "); }).apply($1)"
  let decs = [ForeignD $ ImportF JavaScript safety wrappedBody n t]
  e <- [| JSFFI $(varE n) |]
  return (decs, e)
#else
mkJSFFI _ body = do
  e <- [| JSFFI body |]
  return ([], e)
#endif

parseType :: Type -> ([Type], Type)
parseType (ForallT _ [AppT (AppT (ConT monadJs) (VarT _)) (VarT m)] funType)
  | monadJs == ''MonadJS = go funType
  where go t = case t of
          AppT (AppT ArrowT arg) t' ->
            let (args, result) = go t'
            in (arg : args, result)
          AppT (VarT m') result
            | m' == m -> ([], result)
          _ -> error $ "parseType: can't parse type " <> show t
parseType t = error $ "parseType: can't parse type " <> show t
