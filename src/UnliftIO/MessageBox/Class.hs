-- | This module contains a type class that
-- describes exchangable operations on messages
-- boxes.
module UnliftIO.MessageBox.Class
  ( IsMessageBoxArg (..),
    IsMessageBox (..),
    IsInput (..),
    handleMessage,
  )
where

import Data.Kind (Type)
import UnliftIO.MessageBox.Util.Future (Future, awaitFuture)
import UnliftIO (MonadUnliftIO, timeout)

-- | Types that configure and allow the creation of a 'MessageBox'.
--
-- Create 'IsMessageBox' instances from a parameter.
-- Types that determine 'MessageBox' values.
--
-- For a limited message box this might be the limit of
-- the message queue.
class
  (IsMessageBox (MessageBox argument), IsInput (Input (MessageBox argument))) =>
  IsMessageBoxArg argument
  where
  -- | The message box that can be created from the
  -- message box argument
  type MessageBox argument :: Type -> Type

  -- | Return a message limit.
  --
  -- NOTE: This method was added for unit tests.
  -- Although the method is totally valid, it
  -- might not be super useful in production code.
  -- Also note that the naming follows the rule:
  -- Reserve short names for entities that are
  -- used often.
  getConfiguredMessageLimit :: argument -> Maybe Int

  -- | Create a new @msgBox@ according to the @argument@.
  -- This is required to receive a message.
  -- NOTE: Only one process may receive on an msgBox.
  newMessageBox :: MonadUnliftIO m => argument -> m (MessageBox argument a)

-- | A type class for msgBox types.
-- A common interface for receiving messages.
class IsInput (Input box) => IsMessageBox box where
  -- | Type of the corresponding input
  type Input box :: Type -> Type

  -- | Receive a message. Take whatever time it takes.
  -- Return 'Just' the value or 'Nothing' when an error
  -- occurred.
  --
  -- NOTE: Nothing may sporadically be returned, especially
  -- when there is a lot of load, so please make sure to 
  -- build your application in such a way, that it 
  -- anticipates failure.
  receive :: MonadUnliftIO m => box a -> m (Maybe a)

  -- | Return a 'Future' that can be used to wait for the
  -- arrival of the next message.
  -- NOTE: Each future value represents the next slot in the queue
  -- so one future corresponds to exactly that message (should it arrive)
  -- and if that future value is dropped, that message will be lost!
  tryReceive :: MonadUnliftIO m => box a -> m (Future a)

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
    box a ->
    -- | Time in micro seconds to wait until the
    -- action is invoked.
    Int ->
    m (Maybe a)
  receiveAfter !mbox !t =
    tryReceive mbox >>= timeout t . awaitFuture

  -- | Create a new @input@ that enqueus messages,
  -- which are received by the @box@
  newInput :: MonadUnliftIO m => box a -> m (Input box a)

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
handleMessage ::
  (MonadUnliftIO m, IsMessageBox box) =>
  box message ->
  (message -> m b) ->
  m (Maybe b)
handleMessage !box !onMessage = do
  !maybeMessage <- receive box
  case maybeMessage of
    Nothing -> pure Nothing
    Just !message -> do
      Just <$> onMessage message
