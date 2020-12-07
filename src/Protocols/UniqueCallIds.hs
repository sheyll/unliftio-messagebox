{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

-- | This module provides thread-wide unique values
-- to identify calls.
--
-- This is based on CAS. TODO benchmark, and test uniqueness.
module Protocols.UniqueCallIds
  ( nextCallId,
    CallId (..),
    newAtomicCallIdCounter,
    HasAtomicCallIdCounter (getAtomicCallIdCounter),
    AtomicCallIdCounter (),
  )
where

import Control.Monad.Reader (MonadReader, asks)
import Data.Atomics.Counter
  ( AtomicCounter,
    incrCounter,
    newCounter,
  )
import Data.Time.Clock.POSIX (getPOSIXTime)
import UnliftIO (MonadIO (..))

-- | A globally unique value to identify a 'Call' message.
newtype CallId = MkCallId {fromCallId :: Int}
  deriving stock (Eq, Ord)

newtype AtomicCallIdCounter = MkAtomicCallIdCounter AtomicCounter

class HasAtomicCallIdCounter env where
  getAtomicCallIdCounter :: env -> AtomicCallIdCounter

newAtomicCallIdCounter :: MonadIO m => m AtomicCallIdCounter
newAtomicCallIdCounter =
  MkAtomicCallIdCounter <$> liftIO (currentTimeMillis >>= newCounter)
  where
    currentTimeMillis = round . (1000 *) <$> getPOSIXTime

nextCallId ::
  ( MonadReader env m,
    MonadIO m,
    HasAtomicCallIdCounter env
  ) =>
  m CallId
nextCallId =
  asks getAtomicCallIdCounter >>= \(MkAtomicCallIdCounter !atomicCounter) ->
    MkCallId <$> liftIO (incrCounter 1 atomicCounter)
