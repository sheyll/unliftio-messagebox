-- | This module contains a type class that
-- describes exchangable operations on messages
-- boxes.
module Protocol.MessageBoxClass
  ( IsInBoxConfig (..),
    IsInBox (..),
    IsOutBox (..),
    Future(..),
    takeNow,
    WithTimeout (..),
    handleMessage,
  )
where

import Control.Exception (BlockedIndefinitelyOnMVar, BlockedIndefinitelyOnSTM)
import Data.Kind (Type)
import UnliftIO (liftIO, MonadUnliftIO, timeout, try)

-- | A type class for creating 'InBox' instances.
--
-- The configuration parameter (if any) for the creation of an
-- inbox. For a limited message box this might be the limit of
-- the message queue.
class IsInBox inbox => IsInBoxConfig cfg inbox | cfg -> inbox where
  -- | Create a new @inbox@.
  -- This is required to receive a message.
  -- NOTE: Only one process may receive on an inbox.
  newInBox :: MonadUnliftIO m => cfg -> m (inbox a)

-- | A type class for inbox types.
-- A common interface for receiving messages.
class IsOutBox (OutBox2 inbox) => IsInBox inbox where
  -- | Type of the corresponding outbox
  type OutBox2 inbox :: Type -> Type

  -- | Receive a message. Take whatever time it takes.
  -- Return 'Just' the value or 'Nothing' when an error 
  -- occurred.
  receive :: MonadUnliftIO m => inbox a -> m (Maybe a)

  -- | Return a 'Future' that can be used to wait for the
  -- arrival of the next message.
  -- NOTE: If you call this function more than once without
  -- waiting for all 'Future's, messages will be lost!
  tryReceive :: MonadUnliftIO m => inbox a -> m (Future a)

  -- | Create a new @outbox@ that enqueus messages,
  -- which are received by the @inbox@
  newOutBox2 :: MonadUnliftIO m => inbox a -> m (OutBox2 inbox a)

-- | A wrapper around an IO action that returns value 
-- in the future.
newtype Future a = Future { 
  -- | Return 'Just' the value or 'Nothing', 
  --   when the value is not available yet.
  fromFuture :: IO (Maybe a) 
  }

-- | Return 'Just' the value or 'Nothing', 
--   when the value is not available yet.
{-# INLINE takeNow #-}
takeNow :: MonadUnliftIO m => Future a -> m (Maybe a) 
takeNow = liftIO . fromFuture

-- | A type class for outbox types.
-- A common interface for delivering messages.
--
-- The parameters are the types of the inbox and the outbox
-- values of a message box.
class IsOutBox outbox where
  -- | Send a message. Take whatever time it takes.
  -- Depending on the implementation, this might
  -- be a non-blocking operation.
  -- Return if the operation was successful.
  deliver :: MonadUnliftIO m => outbox a -> a -> m Bool

-- * Receiving Messages

-- | Receive a message and apply a function to it.
--  TODO exception handling: indefinitely blocked in mvar!!
handleMessage ::
  (MonadUnliftIO m, IsInBox inbox) =>
  inbox message ->
  (message -> m b) ->
  m (Maybe b)
handleMessage !inbox !onMessage = do
  !maybeMessage <- receive inbox
  case maybeMessage of
    Nothing -> pure Nothing
    Just !message -> do
      Just <$> onMessage message

-- * Message Box Modifiers

-- ** Timeouts

-- | A generic wrapper for 'IsOutBox' or 'IsInBox' instances
-- that uses 'timeout' around 'deliver' and 'receive'.
-- TODO tests
data WithTimeout box msg = WithTimeout !Int !(box msg)

-- | A generic wrapper for 'IsInBox' instances
-- that uses 'timeout' around 'receive'.
instance IsInBox box => IsInBox (WithTimeout box) where
  type OutBox2 (WithTimeout box) = WithTimeout (OutBox2 box)
  {-# INLINE receive #-}
  receive (WithTimeout !t !o) =
    timeout t (receive o)
      >>= maybe
        (pure Nothing)
        return
  {-# INLINE newOutBox2 #-}
  newOutBox2 (WithTimeout !t !o) =
    WithTimeout t <$> newOutBox2 o

-- | A generic wrapper for 'IsOutBox' instances
-- that uses 'timeout' around 'deliver'.
instance IsOutBox box => IsOutBox (WithTimeout box) where
  {-# INLINE deliver #-}
  deliver (WithTimeout !t !o) !m =
    timeout t (deliver o m)
      >>= maybe
        (pure False)
        return

-- ** Exception safety

-- | A generic wrapper for 'IsOutBox' or 'IsInBox' instances
-- that uses 'try' to catch 'BlockedIndefinitelyOnSTM'
-- or 'BlockedIndefinitelyOnMVar' thrown
-- from the underlying 'deliver' and 'receive'.
newtype CatchExceptions box msg = CatchExceptions (box msg)

-- | A generic wrapper for 'IsInBox' instances
-- that uses 'try' around 'receive' and returns 'Nothing'
-- if an exception is caught.
instance IsInBox box => IsInBox (CatchExceptions box) where
  type OutBox2 (CatchExceptions box) = CatchExceptions (OutBox2 box)
  {-# INLINE receive #-}
  receive (CatchExceptions !o) =
    try @_ @BlockedIndefinitelyOnSTM
      ( try @_ @BlockedIndefinitelyOnMVar (receive o)
          >>= either
            (const (pure Nothing))
            return
      )
      >>= either
        (const (pure Nothing))
        return

  newOutBox2 (CatchExceptions !o) =
    CatchExceptions <$> newOutBox2 o

-- | A generic wrapper for 'IsOutBox' instances
-- that uses 'try' around 'deliver' and returns 'False'
-- if an exception is caught.
instance IsOutBox box => IsOutBox (CatchExceptions box) where
  {-# INLINE deliver #-}
  deliver (CatchExceptions !o) !m =
    try @_ @BlockedIndefinitelyOnSTM
      ( try @_ @BlockedIndefinitelyOnMVar (deliver o m)
          >>= either
            (const (pure False))
            return
      )
      >>= either
        (const (pure False))
        return
