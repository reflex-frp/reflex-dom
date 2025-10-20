{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Reflex.Adjustable.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class

#ifdef USE_TEMPLATE_HASKELL
import Language.Haskell.TH
#endif

import GHCJS.DOM.Types (JSContextRef, askJSM)
#ifdef ghcjs_HOST_OS
import qualified GHCJS.Buffer as JS
import GHCJS.DOM.Types (MonadJSM)
import qualified GHCJS.DOM.Types as JS
import qualified GHCJS.Foreign as JS
#if __GLASGOW_HASKELL__ < 900
import qualified GHCJS.Foreign.Callback as JS
import qualified GHCJS.Foreign.Callback.Internal (Callback (..))
#else
import qualified GHC.JS.Foreign.Callback as JS
#endif
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
import GHCJS.DOM.Types (MonadJSM (..), runJSM)
#endif

import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Control
import Data.Coerce (coerce)

newtype WithJSContextSingleton x m a = WithJSContextSingleton { unWithJSContextSingleton :: ReaderT (JSContextSingleton x) m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans, MonadException, MonadAsyncException)

instance PrimMonad m => PrimMonad (WithJSContextSingleton x m) where
  type PrimState (WithJSContextSingleton x m) = PrimState m
  primitive = lift . primitive

instance Adjustable t m => Adjustable t (WithJSContextSingleton x m) where
  runWithReplace a0 a' = WithJSContextSingleton $ runWithReplace (coerce a0) (coerceEvent a')
  traverseIntMapWithKeyWithAdjust f dm0 dm' = WithJSContextSingleton $ traverseIntMapWithKeyWithAdjust (\k v -> unWithJSContextSingleton $ f k v) (coerce dm0) (coerceEvent dm')
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
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE

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

#ifndef ghcjs_HOST_OS
instance MonadIO m => MonadJSM (WithJSContextSingleton x m) where
  liftJSM' f = do
    wv <- WithJSContextSingleton ask
    runJSM f $ unJSContextSingleton wv
#endif
