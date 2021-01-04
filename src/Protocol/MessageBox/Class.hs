-- | This module contains a type class that
-- describes exchangable operations on messages
-- boxes.
module Protocol.MessageBox.Class
  ( IsMessageBoxFactory (..),
    IsMessageBox (..),
    IsInput (..),
    Future(..),
    tryNow,
    WithTimeout (..),
    handleMessage,
  )
where

import Control.Exception (BlockedIndefinitelyOnMVar, BlockedIndefinitelyOnSTM)
import Data.Kind (Type)
import UnliftIO (liftIO, MonadUnliftIO, timeout, try)

-- | Create 'IsMessageBox' instances from a parameter.
-- Types that determine 'MessageBox' values.
--
-- For a limited message box this might be the limit of
-- the message queue.
class IsMessageBox msgBox => IsMessageBoxFactory cfg msgBox | cfg -> msgBox where
  -- | Create a new @msgBox@.
  -- This is required to receive a message.
  -- NOTE: Only one process may receive on an msgBox.
  newMessageBox :: MonadUnliftIO m => cfg -> m (msgBox a)

-- | A type class for msgBox types.
-- A common interface for receiving messages.
class IsInput (Input msgBox) => IsMessageBox msgBox where
  -- | Type of the corresponding input
  type Input msgBox :: Type -> Type

  -- | Receive a message. Take whatever time it takes.
  -- Return 'Just' the value or 'Nothing' when an error 
  -- occurred.
  receive :: MonadUnliftIO m => msgBox a -> m (Maybe a)

  -- | Return a 'Future' that can be used to wait for the
  -- arrival of the next message.
  -- NOTE: Each future value represents the next slot in the queue
  -- so one future corresponds to exactly that message (should it arrive)
  -- and if that future value is dropped, that message will be lost!
  tryReceive :: MonadUnliftIO m => msgBox a -> m (Future a)

  -- | Create a new @input@ that enqueus messages,
  -- which are received by the @msgBox@
  newInput :: MonadUnliftIO m => msgBox a -> m (Input msgBox a)

-- | A wrapper around an IO action that returns value 
-- in the future.
newtype Future a = Future { 
  -- | Return 'Just' the value or 'Nothing', 
  --   when the value is not available yet.
  fromFuture :: IO (Maybe a) 
  }

-- | Return 'Just' the value or 'Nothing', 
--   when the value is not available yet.
--
--   Once the value is available, that value
--   will be returned everytime this function is
--   invoked.
{-# INLINE tryNow #-}
tryNow :: MonadUnliftIO m => Future a -> m (Maybe a) 
tryNow = liftIO . fromFuture

-- | A type class for input types.
-- A common interface for delivering messages.
class IsInput input where
  -- | Send a message. Take whatever time it takes.
  -- Depending on the implementation, this might
  -- be a non-blocking operation.
  -- Return if the operation was successful.
  deliver :: MonadUnliftIO m => input a -> a -> m Bool

-- * Receiving Messages

-- | Receive a message and apply a function to it.
--  TODO exception handling: indefinitely blocked in mvar... is it really necessary to catch here?
handleMessage ::
  (MonadUnliftIO m, IsMessageBox msgBox) =>
  msgBox message ->
  (message -> m b) ->
  m (Maybe b)
handleMessage !msgBox !onMessage = do
  !maybeMessage <- receive msgBox
  case maybeMessage of
    Nothing -> pure Nothing
    Just !message -> do
      Just <$> onMessage message

-- * Message Box Modifiers

-- ** Timeouts

-- | A generic wrapper for 'IsInput' or 'IsMessageBox' instances
-- that uses 'timeout' around 'deliver' and 'receive'.
-- TODO tests
data WithTimeout box msg = WithTimeout !Int !(box msg)

-- | A generic wrapper for 'IsMessageBox' instances
-- that uses 'timeout' around 'receive'.
instance IsMessageBox box => IsMessageBox (WithTimeout box) where -- TODO tests
  type Input (WithTimeout box) = WithTimeout (Input box)
  {-# INLINE receive #-}
  receive (WithTimeout !t !o) =
    timeout t (receive o)
      >>= maybe
        (pure Nothing)
        return
  {-# INLINE newInput #-}
  newInput (WithTimeout !t !o) =
    WithTimeout t <$> newInput o

-- | A generic wrapper for 'IsInput' instances
-- that uses 'timeout' around 'deliver'.
instance IsInput box => IsInput (WithTimeout box) where
  {-# INLINE deliver #-}
  deliver (WithTimeout !t !o) !m =
    timeout t (deliver o m)
      >>= maybe
        (pure False)
        return

-- ** Exception safety

-- | A generic wrapper for 'IsInput' or 'IsMessageBox' instances
-- that uses 'try' to catch 'BlockedIndefinitelyOnSTM'
-- or 'BlockedIndefinitelyOnMVar' thrown
-- from the underlying 'deliver' and 'receive'.
newtype CatchExceptions box msg = CatchExceptions (box msg)

-- | A generic wrapper for 'IsMessageBox' instances
-- that uses 'try' around 'receive' and returns 'Nothing'
-- if an exception is caught.
instance IsMessageBox box => IsMessageBox (CatchExceptions box) where -- TODO tests
  type Input (CatchExceptions box) = CatchExceptions (Input box)
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

  newInput (CatchExceptions !o) =
    CatchExceptions <$> newInput o

-- | A generic wrapper for 'IsInput' instances
-- that uses 'try' around 'deliver' and returns 'False'
-- if an exception is caught.
instance IsInput box => IsInput (CatchExceptions box) where
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
