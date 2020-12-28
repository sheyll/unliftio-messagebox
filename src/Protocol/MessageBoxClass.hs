-- | This module contains a type class that
-- describes exchangable operations on messages
-- boxes.
module Protocol.MessageBoxClass
  ( IsMessageBox (..),
    IsInBox (..),
    IsOutBox (..),
    TimeoutWrapper(..),
    handleMessage,
  )
where

import UnliftIO (MonadUnliftIO, timeout)

-- | A type class for common operations on messages boxes.
--
-- The parameters are the types of the inbox and the outbox
-- values of a message box.
class (IsInBox inbox, IsOutBox outbox) => IsMessageBox inbox outbox | inbox -> outbox, outbox -> inbox where
  -- | The configuration parameter (if any) for the creation of an
  -- inbox. For a bounded message box this might be the limit of
  -- the message queue.
  data InBoxConfig inbox

  -- | Create a new @inbox@.
  -- This is required to receive a message.
  -- NOTE: Only one process may receive on an inbox.
  newInBox ::
    MonadUnliftIO m =>
    InBoxConfig inbox ->
    m (inbox a)

  -- | Create a new @outbox@ that enqueus messages,
  -- which are received by the @inbox@
  newOutBox :: MonadUnliftIO m => inbox a -> m (outbox a)

-- | A type class for inbox types.
-- A common interface for receiving messages.
class IsInBox inbox where
  -- | Receive a message. Take whatever time it takes.
  -- Depending on the implementation, this might
  -- be a non-blocking operation.
  -- Return @Just@ the value on success.
  receive :: MonadUnliftIO m => inbox a -> m (Maybe a)

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

-- | A generic wrapper for 'IsOutBox' or 'IsInBox' instances
-- that uses 'timeout' around 'deliver' and 'receive'.
data TimeoutWrapper box msg = MkTimeoutWrapper !Int !(box msg)

-- | A generic wrapper for 'IsOutBox' instances
-- that uses 'timeout' around 'deliver'.
instance IsOutBox box => IsOutBox (TimeoutWrapper box) where
  deliver (MkTimeoutWrapper !t !o) !m =
    timeout t (deliver o m)
      >>= maybe
        (pure False)
        return

-- | A generic wrapper for 'IsInBox' instances
-- that uses 'timeout' around 'receive'.
instance IsInBox box => IsInBox (TimeoutWrapper box) where
  receive (MkTimeoutWrapper !t !o) =
    timeout t (receive o)
      >>= maybe
        (pure Nothing)
        return

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
