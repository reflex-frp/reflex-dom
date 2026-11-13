{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
#ifdef USE_TEMPLATE_HASKELL
module Foreign.JavaScript.TH
  ( module Foreign.JavaScript.TH
  , Safety (..)
  ) where
#else
module Foreign.JavaScript.TH
  ( module Foreign.JavaScript.TH
  ) where
#endif

import Reflex.Adjustable.Class
import Reflex.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Prelude hiding ((!!))

import Foreign.JavaScript.Orphans ()

#ifdef USE_TEMPLATE_HASKELL
import Language.Haskell.TH
#endif

#ifdef ghcjs_HOST_OS
import GHCJS.DOM.Types (JSContextRef, MonadJSM, askJSM)
#ifdef __GHCJS__
import GHCJS.DOM.Types qualified as JS
import GHCJS.Foreign qualified as JS
#endif
#if __GLASGOW_HASKELL__ < 900
import GHCJS.Foreign.Callback qualified as JS
import GHCJS.Foreign.Callback.Internal qualified (Callback (..))
import JavaScript.Array qualified as JS
import JavaScript.Array.Internal qualified (SomeJSArray (..))
import JavaScript.Object qualified as JS
import JavaScript.Object.Internal qualified (Object (..))
import JavaScript.TypedArray.ArrayBuffer qualified as JSArrayBuffer

import Data.Hashable
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Text.Encoding.Z
#endif

#else
import GHCJS.DOM.Types (JSContextRef, MonadJSM (..), askJSM, runJSM)
#endif

import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Control
import Data.Coerce (coerce)
import Data.Kind qualified as Kind

newtype WithJSContextSingleton x m a = WithJSContextSingleton {unWithJSContextSingleton :: ReaderT (JSContextSingleton x) m a} deriving newtype (Applicative, Functor, Monad, MonadAsyncException, MonadException, MonadFix, MonadIO, MonadTrans)

instance PrimMonad m => PrimMonad (WithJSContextSingleton x m) where
  type PrimState (WithJSContextSingleton x m) = PrimState m
  primitive = lift . primitive

instance Adjustable t m => Adjustable t (WithJSContextSingleton x m) where
  runWithReplace a0 a' = WithJSContextSingleton $ runWithReplace (coerce a0) (coerceEvent a')
  traverseIntMapWithKeyWithAdjust f dm0 dm' = WithJSContextSingleton $ traverseIntMapWithKeyWithAdjust (\k v -> unWithJSContextSingleton $ f k v) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjust f dm0 dm' = WithJSContextSingleton $ traverseDMapWithKeyWithAdjust (\k v -> unWithJSContextSingleton $ f k v) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = WithJSContextSingleton $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unWithJSContextSingleton $ f k v) (coerce dm0) (coerceEvent dm')

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (WithJSContextSingleton x m) where
  {-# INLINEABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINEABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (WithJSContextSingleton x m) where
  {-# INLINEABLE subscribeEvent #-}
  subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (WithJSContextSingleton x m) where
  type ReadPhase (WithJSContextSingleton x m) = ReadPhase m
  {-# INLINEABLE fireEventsAndRead #-}
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  {-# INLINEABLE runHostFrame #-}
  runHostFrame = lift . runHostFrame

instance MonadSample t m => MonadSample t (WithJSContextSingleton x m) where
  {-# INLINEABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (WithJSContextSingleton x m) where
  {-# INLINEABLE hold #-}
  hold v0 = lift . hold v0
  {-# INLINEABLE holdDyn #-}
  holdDyn v0 = lift . holdDyn v0
  {-# INLINEABLE holdIncremental #-}
  holdIncremental v0 = lift . holdIncremental v0
  {-# INLINEABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINEABLE headE #-}
  headE = lift . headE

instance MonadTransControl (WithJSContextSingleton x) where
  type StT (WithJSContextSingleton x) a = StT (ReaderT (JSContextSingleton x)) a
  {-# INLINEABLE liftWith #-}
  liftWith = defaultLiftWith WithJSContextSingleton unWithJSContextSingleton
  {-# INLINEABLE restoreT #-}
  restoreT = defaultRestoreT WithJSContextSingleton

instance PerformEvent t m => PerformEvent t (WithJSContextSingleton x m) where
  type Performable (WithJSContextSingleton x m) = WithJSContextSingleton x (Performable m) -- TODO: Can we eliminate this wrapper?
  {-# INLINEABLE performEvent_ #-}
  performEvent_ e = liftWith $ \run -> performEvent_ $ fmap run e
  {-# INLINEABLE performEvent #-}
  performEvent e = liftWith $ \run -> performEvent $ fmap run e

runWithJSContextSingleton :: WithJSContextSingleton x m a -> JSContextSingleton x -> m a
runWithJSContextSingleton = runReaderT . unWithJSContextSingleton

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
newtype JSContextSingleton (x :: Kind.Type) = JSContextSingleton {unJSContextSingleton :: JSContextRef}

#ifndef ghcjs_HOST_OS
instance MonadIO m => MonadJSM (WithJSContextSingleton x m) where
  liftJSM' f = do
    wv <- WithJSContextSingleton ask
    runJSM f $ unJSContextSingleton wv
#endif
