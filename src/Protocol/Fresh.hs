-- | Threadsafe, shared, atomic counters
--
-- This is based on "Data.Atomics.Counter".
-- TODO benchmark, and test uniqueness.
module Protocol.Fresh
  ( fresh,
    newFromSystemTime,
    newCounterVar,
    HasCounterVar (getCounterVar, putCounterVar),
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
import Data.Time.Clock.POSIX (getPOSIXTime)
import UnliftIO (MonadIO (..))

-- | A threadsafe atomic a

-- | Atomically increment and get the value of the 'Counter'
-- for type @a@ that must be present in the @env@.
fresh ::
  forall a env m.
  ( MonadReader env m,
    MonadIO m,
    HasCounterVar a env,
    Coercible a Int
  ) =>
  m a
fresh =
  asks (getCounterVar @a) >>= \(MkCounterVar !atomicCounter) ->
    coerce <$> liftIO (incrCounter 1 atomicCounter)

-- | Create a new 'CounterVar' starting at @0@.
newCounterVar ::
  forall a m.
  MonadIO m =>
  m (CounterVar a)
newCounterVar =
  MkCounterVar <$> liftIO (newCounter 0)

-- | Create a new 'CounterVar' starting at the current
--   system time in millis.
newFromSystemTime ::
  forall a m.
  MonadIO m =>
  m (CounterVar a)
newFromSystemTime =
  MkCounterVar <$> liftIO (currentTimeMillis >>= newCounter)
  where
    currentTimeMillis = round . (1000 *) <$> getPOSIXTime

newtype CounterVar a = MkCounterVar AtomicCounter

class HasCounterVar a env where
  getCounterVar :: env -> CounterVar a
  putCounterVar :: CounterVar a -> env -> env

instance HasCounterVar t (CounterVar t) where
  getCounterVar = id
  putCounterVar x _ = x
