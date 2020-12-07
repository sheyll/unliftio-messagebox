{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

-- | This module provides thread-wide unique values
-- to identify calls.
--
-- This is based on CAS. TODO benchmark, and test uniqueness.
module Protocols.Fresh
  ( fresh,
    Fresh (..),
    newFreshCounter,
    HasFreshCounter (getFreshCounter),
    FreshCounter (),
  )
where

import Control.Monad.Reader (runReaderT, MonadReader, asks)
import Data.Atomics.Counter
  ( AtomicCounter,
    incrCounter,
    newCounter,
  )
import Data.Time.Clock.POSIX (getPOSIXTime)
import UnliftIO (MonadUnliftIO, MonadIO (..))
import UnliftIO.Concurrent (forkIO)
import Control.Monad.Reader (local)
import Control.Monad (void)


-- | A globally unique value to identify a 'Call' message.
newtype Fresh (tag :: tagKind) = MkFresh {unFresh :: Int}
  deriving stock (Eq, Ord)

newtype FreshCounter (tag :: tagKind) = MkFreshCounter AtomicCounter

class HasFreshCounter tag env where
  getFreshCounter ::  env -> FreshCounter tag

newFreshCounter :: forall tag m 
  . MonadIO m 
  => m (FreshCounter tag)
newFreshCounter =
  MkFreshCounter <$> liftIO (currentTimeMillis >>= newCounter)
  where
    currentTimeMillis = round . (1000 *) <$> getPOSIXTime

fresh ::
  forall tag env m . 
  ( MonadReader env m,
    MonadIO m,
    HasFreshCounter tag env
  ) =>
  m (Fresh tag)
fresh =
  asks (getFreshCounter @tag) >>= \(MkFreshCounter !atomicCounter) ->
    MkFresh <$> liftIO (incrCounter 1 atomicCounter)


data App = MkApp { 
    self :: Fresh "PID",
    pidCounter :: FreshCounter "PID" , 
    callIdCounter :: FreshCounter "CallId"
  }

instance HasFreshCounter "PID" App where 
  getFreshCounter = pidCounter

instance HasFreshCounter t (FreshCounter t) where
  getFreshCounter = id

instance HasFreshCounter "CallId" App where 
  getFreshCounter = callIdCounter

fork :: (MonadReader App m, MonadUnliftIO m) => m () -> m ()
fork f = do 
  self <- fresh 
  zero <- newFreshCounter @"CallId"
  void $ forkIO $ local (\app -> app {callIdCounter = zero, self}) f

main :: IO ()
main = do  
  pidCounter <- newFreshCounter @"PID"
  callIdCounter <- newFreshCounter @"CallId"
  pidZero <- runReaderT fresh pidCounter
  flip runReaderT (MkApp {self = pidZero, pidCounter, callIdCounter }) $ do
   asks self >>= liftIO . putStrLn . show . unFresh
   fork $ do
     asks self >>= liftIO . putStrLn . show . unFresh
     fork $ do
       asks self >>= liftIO . putStrLn . show . unFresh
