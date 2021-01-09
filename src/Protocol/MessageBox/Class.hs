-- | This module contains a type class that
-- describes exchangable operations on messages
-- boxes.
module Protocol.MessageBox.Class
  ( IsMessageBoxFactory (..),
    IsMessageBox (..),
    IsInput (..),
    handleMessage,
  )
where

import Data.Kind (Type)
import UnliftIO (MonadUnliftIO, timeout)
import Protocol.Future ( Future, awaitFuture )

-- | Create 'IsMessageBox' instances from a parameter.
-- Types that determine 'MessageBox' values.
--
-- For a limited message box this might be the limit of
-- the message queue.
class
  (IsMessageBox (MessageBox cfg), IsInput (Input (MessageBox cfg))) =>
  IsMessageBoxFactory cfg
  where
  type MessageBox cfg :: Type -> Type

  -- | Create a new @msgBox@.
  -- This is required to receive a message.
  -- NOTE: Only one process may receive on an msgBox.
  newMessageBox :: MonadUnliftIO m => cfg -> m (MessageBox cfg a)

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

  -- | Wait for an incoming message or run a timeout action if no message arrives.
  --
  -- The default implementation uses 'tryReceive' to get a
  -- 'Future' on which 'awaitFuture' inside a 'timeout' is called.
  --
  -- Instances might override this with more performant implementations
  -- especially non-blocking Unagi channel based implementation.
  --
  -- TODO exception handling: indefinitely blocked in mvar... is it really necessary to catch here?
  -- TODO benchmark
  -- TODO test
  receiveAfter ::
    MonadUnliftIO m =>
    -- | Message box
    msgBox a ->
    -- | Time in micro seconds to wait until the
    -- action is invoked.
    Int ->
    -- | The action to run after the time ran out.
    m (Maybe a) ->
    m (Maybe a)
  receiveAfter !mbox !t !onAfter =
    tryReceive mbox
      >>= timeout t . awaitFuture
      >>= maybe onAfter (return . Just)

  -- | Create a new @input@ that enqueus messages,
  -- which are received by the @msgBox@
  newInput :: MonadUnliftIO m => msgBox a -> m (Input msgBox a)

-- | A type class for input types.
-- A common interface for delivering messages.
class IsInput input where
  -- | Send a message. Take whatever time it takes.
  -- Depending on the implementation, this might
  -- be a non-blocking operation.
  -- Return if the operation was successful.
  deliver :: MonadUnliftIO m => input a -> a -> m Bool

-- ** Utility Functions for Receiving Messages

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

-- -- * Message BlockingBox Modifiers

-- -- ** Timeouts

-- -- | A generic wrapper for 'IsInput' or 'IsMessageBox' instances
-- -- that uses 'timeout' around 'deliver' and 'receive'.
-- -- TODO tests
-- data WithTimeout box msg = WithTimeout !Int !(box msg)

-- -- | A generic wrapper for the 'IsMessageBoxFactory' instances
-- -- that produce 'WithTimeout' message boxes reflecting
-- -- a giving timeout in milliseconds.
-- data WithTimeoutCfg box cfg = WithTimeoutCfg !Int !cfg
--   deriving stock (Show)

-- instance
--   (MessageBox cfg ~ box, IsMessageBoxFactory cfg) =>
--   IsMessageBoxFactory (WithTimeoutCfg box cfg)
--   where
--   type MessageBox (WithTimeoutCfg box cfg) = WithTimeout box
--   newMessageBox (WithTimeoutCfg !t !cfg) =
--     WithTimeout t <$> newMessageBox cfg

-- instance IsMessageBox box => IsMessageBox (WithTimeout box) where -- TODO tests
--   type Input (WithTimeout box) = WithTimeout (Input box)
--   {-# INLINE receive #-}
--   receive (WithTimeout !t !o) =
--     timeout t (receive o)
--       >>= maybe
--         (pure Nothing)
--         return
--   tryReceive (WithTimeout _ !o) =
--     tryReceive o
--   {-# INLINE newInput #-}
--   newInput (WithTimeout !t !o) =
--     WithTimeout t <$> newInput o

-- -- | A generic wrapper for 'IsInput' instances
-- -- that uses 'timeout' around 'deliver'.
-- instance IsInput box => IsInput (WithTimeout box) where
--   {-# INLINE deliver #-}
--   deliver (WithTimeout !t !o) !m =
--     timeout t (deliver o m)
--       >>= maybe
--         (pure False)
--         return
