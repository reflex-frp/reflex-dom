{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Foreign.JavaScript.TH ( module Foreign.JavaScript.TH
#ifdef USE_TEMPLATE_HASKELL
                             , Safety (..)
#endif
                             ) where

import Foreign.JavaScript.Orphans ()
import Prelude hiding ((!!))
import Reflex.Class
import Reflex.DynamicWriter
import Reflex.EventWriter
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Base
import Reflex.Requester.Base
import Reflex.Query.Base (QueryT (..))

#ifdef USE_TEMPLATE_HASKELL
import Language.Haskell.TH
#endif

import GHCJS.DOM.Types (JSContextRef, Node (..), askJSM)
#ifdef ghcjs_HOST_OS
import qualified GHCJS.Buffer as JS
import GHCJS.DOM.Types (MonadJSM)
import qualified GHCJS.DOM.Types as JS
import qualified GHCJS.Foreign as JS
import qualified GHCJS.Foreign.Callback as JS
import qualified GHCJS.Foreign.Callback.Internal (Callback (..))
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
import Control.Lens.Operators ((^.))
import Data.Word (Word8)
import GHCJS.DOM.Types (JSVal, MonadJSM (..), liftJSM, runJSM, toJSString, toJSVal)
import Language.Javascript.JSaddle (Function (..), array, eval, freeFunction, function, js, js1, jss, valBool,
                                    valIsNull, valIsUndefined, valMakeNumber, valMakeString, valToBool,
                                    valToNumber, valToText, valUndefined, (!!))
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
import Data.Coerce (coerce)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

class Monad m => HasJSContext m where
  type JSContextPhantom m :: *
  askJSContext :: m (JSContextSingleton (JSContextPhantom m))

type HasWebView = HasJSContext
-- Not sure if we should deprecate this {-# DEPRECATED HasWebView "Use HasJSContext" #-}

instance HasJSContext m => HasJSContext (ReaderT r m) where
  type JSContextPhantom (ReaderT r m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance HasJSContext m => HasJSContext (StateT r m) where
  type JSContextPhantom (StateT r m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance HasJSContext m => HasJSContext (Strict.StateT r m) where
  type JSContextPhantom (Strict.StateT r m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance HasJSContext m => HasJSContext (PostBuildT t m) where
  type JSContextPhantom (PostBuildT t m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance (ReflexHost t, HasJSContext (HostFrame t)) => HasJSContext (PerformEventT t m) where
  type JSContextPhantom (PerformEventT t m) = JSContextPhantom (HostFrame t)
  askJSContext = PerformEventT $ lift askJSContext

instance HasJSContext m => HasJSContext (EventWriterT t w m) where
  type JSContextPhantom (EventWriterT t w m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance HasJSContext m => HasJSContext (DynamicWriterT t w m) where
  type JSContextPhantom (DynamicWriterT t w m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance HasJSContext m => HasJSContext (RequesterT t request response m) where
  type JSContextPhantom (RequesterT t request response m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance HasJSContext m => HasJSContext (QueryT t q m) where
  type JSContextPhantom (QueryT t q m) = JSContextPhantom m
  askJSContext = QueryT askJSContext

newtype WithJSContextSingleton x m a = WithJSContextSingleton { unWithJSContextSingleton :: ReaderT (JSContextSingleton x) m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans, MonadException, MonadAsyncException)

instance PrimMonad m => PrimMonad (WithJSContextSingleton x m) where
  type PrimState (WithJSContextSingleton x m) = PrimState m
  primitive = lift . primitive

instance MonadAdjust t m => MonadAdjust t (WithJSContextSingleton x m) where
  runWithReplace a0 a' = WithJSContextSingleton $ runWithReplace (coerce a0) (coerceEvent a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = WithJSContextSingleton $ traverseDMapWithKeyWithAdjust (\k v -> unWithJSContextSingleton $ f k v) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = WithJSContextSingleton $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unWithJSContextSingleton $ f k v) (coerce dm0) (coerceEvent dm')

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (WithJSContextSingleton x m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (WithJSContextSingleton x m) where
  {-# INLINABLE subscribeEvent #-}
  subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (WithJSContextSingleton x m) where
  type ReadPhase (WithJSContextSingleton x m) = ReadPhase m
  {-# INLINABLE fireEventsAndRead #-}
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  {-# INLINABLE runHostFrame #-}
  runHostFrame = lift . runHostFrame

instance MonadSample t m => MonadSample t (WithJSContextSingleton x m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (WithJSContextSingleton x m) where
  {-# INLINABLE hold #-}
  hold v0 = lift . hold v0
  {-# INLINABLE holdDyn #-}
  holdDyn v0 = lift . holdDyn v0
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 = lift . holdIncremental v0

instance MonadTransControl (WithJSContextSingleton x) where
  type StT (WithJSContextSingleton x) a = StT (ReaderT (JSContextSingleton x)) a
  {-# INLINABLE liftWith #-}
  liftWith = defaultLiftWith WithJSContextSingleton unWithJSContextSingleton
  {-# INLINABLE restoreT #-}
  restoreT = defaultRestoreT WithJSContextSingleton

instance PerformEvent t m => PerformEvent t (WithJSContextSingleton x m) where
  type Performable (WithJSContextSingleton x m) = WithJSContextSingleton x (Performable m) --TODO: Can we eliminate this wrapper?
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = liftWith $ \run -> performEvent_ $ fmap run e
  {-# INLINABLE performEvent #-}
  performEvent e = liftWith $ \run -> performEvent $ fmap run e

runWithJSContextSingleton :: WithJSContextSingleton x m a -> JSContextSingleton x -> m a
runWithJSContextSingleton = runReaderT . unWithJSContextSingleton

instance (Monad m) => HasJSContext (WithJSContextSingleton x m) where
  type JSContextPhantom (WithJSContextSingleton x m) = x
  askJSContext = WithJSContextSingleton ask

instance MonadRef m => MonadRef (WithJSContextSingleton x m) where
  type Ref (WithJSContextSingleton x m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (WithJSContextSingleton x m) where
  atomicModifyRef r = lift . atomicModifyRef r

withJSContextSingleton :: MonadJSM m => (forall x. JSContextSingleton x -> m r) -> m r
withJSContextSingleton f = askJSM >>= f . JSContextSingleton

-- | Warning: `withJSContextSingletonMono` does not provide the same guarantees that `withJSContextSingleton` does.
withJSContextSingletonMono :: MonadJSM m => (JSContextSingleton () -> m r) -> m r
withJSContextSingletonMono f = askJSM >>= f . JSContextSingleton

-- | A singleton type for a given JSContext; we use this to statically guarantee that different JSContexts don't get mixed up
newtype JSContextSingleton x = JSContextSingleton { unJSContextSingleton :: JSContextRef }

#ifdef ghcjs_HOST_OS
type JSFFI_Internal = JS.MutableJSArray -> IO JS.JSVal
newtype JSFFI = JSFFI JSFFI_Internal
#else
newtype JSFFI = JSFFI String
#endif

data JSFun x = JSFun { unJSFun :: JSRef x
#ifndef ghcjs_HOST_OS
    , unJSFunction :: Function
#endif
    }

instance ToJS x (JSFun x) where
  withJS r f = f (unJSFun r)

class IsJSContext x where
  data JSRef x

class (Monad m, MonadJSM (JSX m), MonadFix (JSX m), MonadJS x (JSX m)) => HasJS x m | m -> x where
  type JSX m :: * -> *
  liftJS :: JSX m a -> m a

instance HasJS x m => HasJS x (ReaderT r m) where
  type JSX (ReaderT r m) = JSX m
  liftJS = lift . liftJS

instance (HasJS x m, ReflexHost t) => HasJS x (PostBuildT t m) where
  type JSX (PostBuildT t m) = JSX m
  liftJS = lift . liftJS

instance (HasJS x (HostFrame t), ReflexHost t) => HasJS x (PerformEventT t m) where
  type JSX (PerformEventT t m) = JSX (HostFrame t)
  liftJS = PerformEventT . lift . liftJS

instance HasJS x m => HasJS x (DynamicWriterT t w m) where
  type JSX (DynamicWriterT t w m) = JSX m
  liftJS = lift . liftJS

instance HasJS x m => HasJS x (RequesterT t request response m) where
  type JSX (RequesterT t request response m) = JSX m
  liftJS = lift . liftJS

instance HasJS x m => HasJS x (QueryT t q m) where
  type JSX (QueryT t q m) = JSX m
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

#ifdef ghcjs_HOST_OS

data JSCtx_IO

instance MonadIO m => HasJS JSCtx_IO (WithJSContextSingleton x m) where
  type JSX (WithJSContextSingleton x m) = IO
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
  fromJSNumber (JSRef_IO r) = JS.fromJSValUnchecked r
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

data JSCtx_JavaScriptCore x

instance IsJSContext (JSCtx_JavaScriptCore x) where
  newtype JSRef (JSCtx_JavaScriptCore x) = JSRef_JavaScriptCore { unJSRef_JavaScriptCore :: JSVal }

instance MonadIO m => HasJS (JSCtx_JavaScriptCore x) (WithJSContextSingleton x m) where
  type JSX (WithJSContextSingleton x m) = WithJSContextSingleton x IO
  liftJS a = do
    wv <- askJSContext
    liftIO $ runWithJSContextSingleton a wv

newtype WithJSContext x m a = WithJSContext { unWithJSContext :: ReaderT JSContextRef m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans, MonadException, MonadAsyncException)

runWithJSContext :: WithJSContext x m a -> JSContextRef -> m a
runWithJSContext = runReaderT . unWithJSContext

instance MonadIO m => MonadJSM (WithJSContextSingleton x m) where
  liftJSM' f = do
    wv <- askJSContext
    runJSM f $ unJSContextSingleton wv

instance MonadIO m => MonadJSM (WithJSContext x m) where
  liftJSM' f =
    runJSM f =<< WithJSContext ask

lowerWithJSContext :: MonadJSM m => WithJSContext x IO a -> m a
lowerWithJSContext a = do
  c <- askJSM
  liftIO $ runWithJSContext a c

liftWithJSContextSingletonThroughWithJSContext :: (HasJSContext m, MonadJSM m, MonadTrans t, Monad m1)
                                    => ((t1 -> t m1 a) -> WithJSContext x IO b)
                                    -> (t1 -> WithJSContextSingleton (JSContextPhantom m) m1 a)
                                    -> m b
liftWithJSContextSingletonThroughWithJSContext f a = do
  wv <- askJSContext
  lowerWithJSContext $ f $ \b' -> lift $ runWithJSContextSingleton (a b') wv

instance MonadJS (JSCtx_JavaScriptCore x) (WithJSContextSingleton x IO) where
  forkJS a = do
    wv <- askJSContext
    liftIO $ forkIO $ runWithJSContextSingleton a wv
  mkJSFun a = do
    wv <- askJSContext
    lowerWithJSContext $ mkJSFun $ \args -> lift $ runWithJSContextSingleton (a args) wv
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
  withJSBool = liftWithJSContextSingletonThroughWithJSContext . withJSBool
  withJSString = liftWithJSContextSingletonThroughWithJSContext . withJSString
  withJSNumber = liftWithJSContextSingletonThroughWithJSContext . withJSNumber
  withJSArray = liftWithJSContextSingletonThroughWithJSContext . withJSArray
  withJSUint8Array = liftWithJSContextSingletonThroughWithJSContext . withJSUint8Array
  withJSNode = liftWithJSContextSingletonThroughWithJSContext . withJSNode
  setJSProp propName valRef objRef = lowerWithJSContext $ setJSProp propName valRef objRef
  getJSProp propName objRef = lowerWithJSContext $ getJSProp propName objRef

instance MonadJS (JSCtx_JavaScriptCore x) (WithJSContext x IO) where
  runJS (JSFFI body) args =
    withJSArray args $ \(JSRef_JavaScriptCore this) -> do
      result <- liftJSM $ eval ("(function(){ return (" <> body <> "); })") ^. js1 "apply" this
      return $ JSRef_JavaScriptCore result
  forkJS a = do
    c <- askJSM
    liftIO . forkIO $ runWithJSContext a c
  mkJSUndefined = return $ JSRef_JavaScriptCore valUndefined
  isJSNull (JSRef_JavaScriptCore r) = liftJSM $ valIsNull r
  isJSUndefined (JSRef_JavaScriptCore r) = liftJSM $ valIsUndefined r
  fromJSBool (JSRef_JavaScriptCore r) = liftJSM $ valToBool r
  fromJSString (JSRef_JavaScriptCore r) = liftJSM (T.unpack <$> valToText r)
  withJSBool b a = a $ JSRef_JavaScriptCore (valBool b)
  withJSString str a = a . JSRef_JavaScriptCore =<< liftJSM (valMakeString $ toJSString str)
  withJSNumber n a = a . JSRef_JavaScriptCore =<< liftJSM (valMakeNumber n)
  withJSArray elems a = a . JSRef_JavaScriptCore =<< liftJSM
    (toJSVal =<< array (map (\(JSRef_JavaScriptCore r) -> r) elems))
  withJSUint8Array payload f = withJSArrayFromList (BS.unpack payload) $ \x -> do
    payloadRef <- runJS (JSFFI "new Uint8Array(this[0])") [x]
    f $ JSUint8Array payloadRef
  fromJSArray (JSRef_JavaScriptCore a) = liftJSM $ do
    len <- round <$> (valToNumber =<< (a ^. js "length"))
    forM [0..len-1] $ \i -> JSRef_JavaScriptCore <$> a !! i
  fromJSUint8Array a = do
    vals <- fromJSArray a
    doubles <- mapM fromJSNumber vals
    return $ BS.pack $ map round doubles
  fromJSNumber (JSRef_JavaScriptCore val) = liftJSM $ valToNumber val
  mkJSFun a = liftJSM $ do
    ctx <- askJSM
    f <- function $ \_ _ args -> liftIO $ void $ runWithJSContext (a $ map JSRef_JavaScriptCore args) ctx
    fRef <- toJSVal f
    return $ JSFun (JSRef_JavaScriptCore fRef) f
  freeJSFun (JSFun _ f) = liftJSM $ freeFunction f
  setJSProp propName (JSRef_JavaScriptCore valRef) (JSRef_JavaScriptCore objRef) =
    liftJSM $ objRef ^. jss propName valRef
  getJSProp propName (JSRef_JavaScriptCore objRef) =
    JSRef_JavaScriptCore <$> liftJSM (objRef ^. js propName)
  withJSNode n f = f . JSRef_JavaScriptCore =<< liftJSM (toJSVal n)

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


#ifdef USE_TEMPLATE_HASKELL

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
#ifdef ghcjs_HOST_OS
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

#endif
