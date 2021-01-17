-- | Threadsafe, shared, atomic counters
--
-- This is based on "Data.Atomics.Counter".
-- TODO benchmark, and test uniqueness.
module Protocol.Fresh
  ( fresh,
    incrementAndGet,
    newCounterVar,
    HasCounterVar (getCounterVar),
    CounterVar (),
  )
where

import Control.Monad.Reader (MonadReader, asks)
import Data.Atomics.Counter
  ( AtomicCounter,
    incrCounter,
    newCounter,
  )
import Data.Coerce (Coercible, coerce)
import UnliftIO (MonadIO (..))

-- | A threadsafe atomic a

-- | Atomically increment and get the value of the 'Counter'
-- for type @a@ that must be present in the @env@.
{-# INLINE fresh #-}
fresh ::
  forall a env m.
  ( MonadReader env m,
    MonadIO m,
    HasCounterVar a env,
    Coercible a Int
  ) =>
  m a
fresh =
  asks (getCounterVar @a) >>= incrementAndGet

-- | Atomically increment and get the value of the 'Counter'
-- for type @a@ that must be present in the @env@.
{-# INLINE incrementAndGet #-}
incrementAndGet ::
  forall a m.
  ( MonadIO m,
    Coercible a Int
  ) =>
  CounterVar a -> m a
incrementAndGet (MkCounterVar !atomicCounter) =
  coerce <$> liftIO (incrCounter 1 atomicCounter)


-- | Create a new 'CounterVar' starting at @0@.
{-# INLINE newCounterVar #-}
newCounterVar ::
  forall a m.
  MonadIO m =>
  m (CounterVar a)
newCounterVar =
  MkCounterVar <$> liftIO (newCounter 0)

-- | An 'AtomicCounter'.
newtype CounterVar a = MkCounterVar AtomicCounter

-- | A type class for @MonadReader@ based
-- applications.
class HasCounterVar a env | env -> a where
  getCounterVar :: env -> CounterVar a

instance HasCounterVar t (CounterVar t) where
  getCounterVar = id
