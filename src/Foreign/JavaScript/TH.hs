{-# LANGUAGE ForeignFunctionInterface, CPP, TemplateHaskell, TypeFamilies, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, ExistentialQuantification, FunctionalDependencies, EmptyDataDecls, FlexibleContexts, RankNTypes, UndecidableInstances #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Foreign.JavaScript.TH ( module Foreign.JavaScript.TH
                             , Safety (..)
                             ) where

import Language.Haskell.TH

#ifdef __GHCJS__
import qualified GHCJS.Marshal as JS
import qualified GHCJS.Foreign as JS
import qualified GHCJS.Types as JS
import GHCJS.DOM
import GHCJS.DOM.Types
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Data.Hashable
#else
import System.Glib.FFI
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame
import Graphics.UI.Gtk.WebKit.DOM.Node
import Graphics.UI.Gtk.WebKit.Types
#endif

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as Strict
import Foreign.Marshal
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Concurrent
import Data.Coerce
import Text.Encoding.Z
import Data.Monoid

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

newtype WithWebView x m a = WithWebView { unWithWebView :: ReaderT (WebViewSingleton x) m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException, MonadAsyncException)

runWithWebView :: WithWebView x m a -> WebViewSingleton x -> m a
runWithWebView = runReaderT . unWithWebView

instance (Monad m) => HasWebView (WithWebView x m) where
  type WebViewPhantom (WithWebView x m) = x
  askWebView = WithWebView ask

withWebViewSingleton :: WebView -> (forall x. WebViewSingleton x -> r) -> r
withWebViewSingleton wv f = f $ WebViewSingleton wv

-- | A singleton type for a given WebView; we use this to statically guarantee that different WebViews (and thus different javscript contexts) don't get mixed up
newtype WebViewSingleton x = WebViewSingleton { unWebViewSingleton :: WebView }

#ifdef __GHCJS__
type JSFFI_Internal = JS.JSArray (JSRef JSCtx_IO) -> IO (JS.JSRef (JSRef JSCtx_IO))
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
  withJSUint8Array :: MonadJS x m => ByteString -> (JSUint8Array x -> m r) -> m r
  -- | Create a JSFun with zero arguments; should be equilvant to `syncCallback AlwaysRetain True` in GHCJS
  mkJSFun :: ([JSRef x] -> m (JSRef x)) -> m (JSFun x) --TODO: Support 'this', exceptions
  freeJSFun :: JSFun x -> m ()
  setJSProp :: String -> JSRef x -> JSRef x -> m ()
  getJSProp :: String -> JSRef x -> m (JSRef x)
  withJSNode :: Node -> (JSRef x -> m r) -> m r

#ifdef __GHCJS__

data JSCtx_IO

instance IsJSContext JSCtx_IO where
  newtype JSRef JSCtx_IO = JSRef_IO { unJSRef_IO :: JS.JSRef (JSRef JSCtx_IO) }

instance MonadJS JSCtx_IO IO where
  runJS (JSFFI f) l = liftM JSRef_IO . f =<< JS.toArray (coerce l)
  forkJS = forkIO
  mkJSUndefined = return $ JSRef_IO JS.jsUndefined
  isJSNull (JSRef_IO r) = return $ JS.isNull r
  isJSUndefined (JSRef_IO r) = return $ JS.isUndefined r
  fromJSBool (JSRef_IO r) = return $ JS.fromJSBool $ JS.castRef r
  fromJSString (JSRef_IO r) = return $ JS.fromJSString $ JS.castRef r
  fromJSArray (JSRef_IO r) = liftM coerce $ JS.fromArray $ JS.castRef r
  fromJSUint8Array (JSRef_IO r) = JS.bufferByteString 0 0 $ JS.castRef r
  fromJSNumber (JSRef_IO r) = do
    Just n <- JS.fromJSRef $ JS.castRef r
    return n
  withJSBool b f = f $ JSRef_IO $ JS.castRef $ JS.toJSBool b
  withJSString s f = f $ JSRef_IO $ JS.castRef $ JS.toJSString s
  withJSNumber n f = do
    r <- JS.toJSRef n
    f $ JSRef_IO $ JS.castRef r
  withJSArray l f = do
    r <- JS.toArray $ coerce l
    f $ JSRef_IO $ JS.castRef r
  withJSUint8Array payload f = BS.useAsCString payload $ \cStr -> do
    ba <- extractByteArray cStr $ BS.length payload
    f $ JSUint8Array $ JSRef_IO ba
  mkJSFun f = do
    cb <- JS.syncCallback1 JS.AlwaysRetain True $ \args -> do
      l <- JS.fromArray args
      JSRef_IO result <- f $ coerce l
      return $ JS.castRef result
    liftM (JSFun . JSRef_IO) $ funWithArguments cb
  freeJSFun (JSFun (JSRef_IO r)) = JS.release $ JS.castRef r
  setJSProp s (JSRef_IO v) (JSRef_IO o) = JS.setProp s v o
  getJSProp s (JSRef_IO o) = do
    r <- JS.getProp s o
    return $ JSRef_IO r
  withJSNode n f = f $ JSRef_IO $ JS.castRef $ unNode n

foreign import javascript unsafe "new Uint8Array($1_1.buf, $1_2, $2)" extractByteArray :: Ptr CChar -> Int -> IO (JS.JSRef (JSRef JSCtx_IO))

foreign import javascript unsafe "function(){ return $1(arguments); }" funWithArguments :: JS.JSFun (JS.JSArray (JS.JSRef b) -> IO a) -> IO (JS.JSRef (JSRef JSCtx_IO))

#else

askJSContext :: MonadIO m => WithWebView x m JSContextRef
askJSContext = do
  wv <- askWebView
  liftIO $ webFrameGetGlobalContext =<< webViewGetMainFrame (unWebViewSingleton wv)

-- | Make a JS string available that is not wrapped up as a JSValueRef
withJSStringRaw :: MonadAsyncException m => String -> (JSStringRef -> m r) -> m r
withJSStringRaw str a = do
  bracket (liftIO $ jsstringcreatewithutf8cstring str) (liftIO . jsstringrelease) $ \strRef -> do
    a strRef

fromJSNumberRaw :: MonadIO m => JSValueRef -> WithWebView x m Double
fromJSNumberRaw val = do
  jsContext <- askJSContext
  liftIO $ jsvaluetonumber jsContext val nullPtr --TODO: Exceptions

foreign import ccall "wrapper" mkJSObjectCallAsFunctionCallback :: JSObjectCallAsFunctionCallback' -> IO (FunPtr JSObjectCallAsFunctionCallback')

data JSCtx_JavaScriptCore x

instance IsJSContext (JSCtx_JavaScriptCore x) where
  newtype JSRef (JSCtx_JavaScriptCore x) = JSRef_JavaScriptCore { unJSRef_JavaScriptCore :: JSValueRef }

instance MonadIO m => HasJS (JSCtx_JavaScriptCore x) (WithWebView x m) where
  type JSM (WithWebView x m) = WithWebView x IO
  liftJS a = do
    wv <- askWebView
    liftIO $ runWithWebView a wv

instance MonadJS (JSCtx_JavaScriptCore x) (WithWebView x IO) where
  runJS (JSFFI body) args = do
    jsContext <- askJSContext
    withJSArray args $ \(JSRef_JavaScriptCore this) -> liftIO $ do
      result <- bracket (jsstringcreatewithutf8cstring body) jsstringrelease $ \script -> jsevaluatescript jsContext script this nullPtr 1 nullPtr
      return $ JSRef_JavaScriptCore result --TODO: Protect and unprotect result
  forkJS a = do
    wv <- askWebView
    liftIO $ forkIO $ runWithWebView a wv
  mkJSUndefined = do
    jsContext <- askJSContext
    liftM JSRef_JavaScriptCore $ liftIO $ jsvaluemakeundefined jsContext
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
      liftM JSRef_JavaScriptCore $ jsobjectgetpropertyatindex jsContext a i nullPtr --TODO: Exceptions
  fromJSUint8Array a = do
    vals <- fromJSArray a
    doubles <- mapM fromJSNumber vals
    return $ BS.pack $ map round doubles
  fromJSNumber (JSRef_JavaScriptCore val) = do
    jsContext <- askJSContext
    liftIO $ jsvaluetonumber jsContext val nullPtr --TODO: Exceptions
  mkJSFun a = do
    wv <- askWebView
    jsContext <- askJSContext
    cb <- liftIO $ mkJSObjectCallAsFunctionCallback $ \_ _ _ argc argv _ -> do --TODO: Use the exception pointer to handle haskell exceptions
      args <- forM [0..fromIntegral (argc-1)] $ \n -> do
        x <- peekElemOff argv n
        jsvalueprotect jsContext x
        return $ JSRef_JavaScriptCore x --TODO: Unprotect eventually
      liftM unJSRef_JavaScriptCore $ runWithWebView (a args) wv
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
      liftIO $ liftM JSRef_JavaScriptCore $ jsobjectgetproperty jsContext objRef propNameRaw nullPtr --TODO: property attribute, exceptions
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

instance FromJS x a => FromJS x (Maybe a) where
  fromJS x = do
    n <- isJSNull x
    if n then return Nothing else liftM Just $ fromJS x

class ToJS x a where
  withJS :: MonadJS x m => a -> (JSRef x -> m r) -> m r

instance ToJS x (JSRef x) where
  withJS r = ($ r)

instance FromJS x (JSRef x) where
  fromJS = return

instance ToJS x String where
  withJS = withJSString

newtype JSArray a = JSArray { unJSArray :: [a] }

instance ToJS x a => ToJS x (JSArray a) where
  withJS = withJSArrayFromList . unJSArray

instance FromJS x a => FromJS x (JSArray a) where
  fromJS = liftM JSArray . mapM fromJS <=< fromJSArray

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
  fromJS = liftM round . fromJSNumber --TODO: Check things; throw exceptions

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
