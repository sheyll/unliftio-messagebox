module Protocol.Command.CallId
  ( CallId (MkCallId),
    HasCallIdCounter (..),
    takeNext,
    newCallIdCounter,
  )
where

import Control.Monad.Reader (MonadReader, asks)
import Protocol.Fresh
  ( CounterVar,
    incrementAndGet,
    newCounterVar,
  )
import UnliftIO (MonadIO, MonadUnliftIO)

-- | An identifier value every command send by 'call's.
newtype CallId = MkCallId Int
  deriving newtype (Eq, Ord)

instance Show CallId where
  showsPrec _ (MkCallId !i) =
    showChar '<' . shows i . showChar '>'

-- | Class of environment records containing a 'CounterVar' for 'CallId's.
class HasCallIdCounter env where
  getCallIdCounter :: env -> CounterVar CallId

instance HasCallIdCounter (CounterVar CallId) where
  {-# INLINE getCallIdCounter #-}
  getCallIdCounter = id

-- | Create a new 'CallId' 'CounterVar'.
{-# INLINE newCallIdCounter #-}
newCallIdCounter :: MonadIO m => m (CounterVar CallId)
newCallIdCounter = newCounterVar

-- | Increment and get a new 'CallId'.
{-# INLINE takeNext #-}
takeNext :: (MonadReader env m, HasCallIdCounter env, MonadUnliftIO m) => m CallId
takeNext = asks getCallIdCounter >>= incrementAndGet
