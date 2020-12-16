-- | This module contains a type class that
-- describes exchangable operations on messages 
-- boxes.
module Protocol.MessageBoxClass (IsMessageBox(..)) where 

import UnliftIO ( MonadUnliftIO )

-- | A type class for common operations on messages boxes.
--
-- The parameters are the types of the inbox and the outbox
-- values of a message box.
class IsMessageBox inbox outbox | inbox -> outbox, outbox -> inbox where
  -- | The configuration parameter (if any) for the creation of an 
  -- inbox. For a bounded message box this might be the limit of 
  -- the message queue.
  data InBoxConfig inbox
  -- | Create a new @inbox@.
  -- This is required to receive a message.
  -- NOTE: Only one process may receive on an inbox.
  newInBox :: MonadUnliftIO m => InBoxConfig inbox -> m (inbox a)
  -- | Create a new @outbox@ that enqueus messages,
  -- which are received by the @inbox@
  newOutBox :: MonadUnliftIO m => inbox a -> m (outbox a)
  -- | Send a message. Take whatever time it takes.
  deliver :: MonadUnliftIO m => outbox a -> a -> m ()
  -- | Receive a message. Take whatever time it takes.
  receive :: MonadUnliftIO m => inbox a -> m a
