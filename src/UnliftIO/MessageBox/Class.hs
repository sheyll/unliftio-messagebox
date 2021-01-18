-- | This module contains a type class that
-- describes exchangable operations on messages
-- boxes.
module UnliftIO.MessageBox.Class
  ( IsMessageBoxFactory (..),
    IsMessageBox (..),
    IsInput (..),
    handleMessage,
  )
where

import Data.Kind (Type)
import Protocol.Future (Future, awaitFuture)
import UnliftIO (MonadUnliftIO, timeout)

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

  -- | Return a message limit.
  --
  -- NOTE: This method was added for unit tests.
  -- Although the method is totally valid, it
  -- might not be super useful in production code.
  -- Also note that the naming follows the rule:
  -- Reserve short names for entities that are
  -- used often.
  getConfiguredMessageLimit :: cfg -> Maybe Int

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
  --
  -- NOTE: Nothing may sporadically be returned, especially
  -- when there is a lot of load, so please make sure to 
  -- build your application in such a way, that it 
  -- anticipates failure.
  receive :: MonadUnliftIO m => msgBox a -> m (Maybe a)

  -- | Return a 'Future' that can be used to wait for the
  -- arrival of the next message.
  -- NOTE: Each future value represents the next slot in the queue
  -- so one future corresponds to exactly that message (should it arrive)
  -- and if that future value is dropped, that message will be lost!
  tryReceive :: MonadUnliftIO m => msgBox a -> m (Future a)

  -- | Wait for an incoming message or return Nothing.
  --
  -- The default implementation uses 'tryReceive' to get a
  -- 'Future' on which 'awaitFuture' inside a 'timeout' is called.
  --
  -- Instances might override this with more performant implementations
  -- especially non-blocking Unagi channel based implementation.
  --
  -- NOTE: Nothing may sporadically be returned, especially
  -- when there is a lot of load, so please make sure to 
  -- build your application in such a way, that it 
  -- anticipates failure.
  receiveAfter ::
    MonadUnliftIO m =>
    -- | Message box
    msgBox a ->
    -- | Time in micro seconds to wait until the
    -- action is invoked.
    Int ->
    m (Maybe a)
  receiveAfter !mbox !t =
    tryReceive mbox >>= timeout t . awaitFuture

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
  --
  -- NOTE: @False@ may sporadically be returned, especially
  -- when there is a lot of load, so please make sure to 
  -- build your application in such a way, that it 
  -- anticipates failure.
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
