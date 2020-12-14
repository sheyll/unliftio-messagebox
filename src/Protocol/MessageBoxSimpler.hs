-- | Thread safe queues for message passing
-- between many concurrent processes.
--
-- This message box is __BOUNDED__.
--
-- Use this module if the producer(s) outperform the consumer.
--
-- For example, when many processes produce log messages and send
-- then to the 'MessageBox' of a process that formats and forwards
-- them to @syslogd@ over the network.
module Protocol.MessageBoxSimpler
  ( createInBox,
    receive,
    tryReceive,
    createOutBoxForInbox,
    trySend,
    trySendAndWait,
    blockingSend,
    InBox (),
    OutBox (),
  )
where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import Data.Maybe (fromMaybe)
import UnliftIO
  ( MonadIO (liftIO),
    MonadUnliftIO,
    timeout,
  )

-- | Create an 'InBox' with an underlying
-- message queue with a given message limit.
--
-- From an 'InBox' a corresponding 'OutBox' can
-- be made, that can be passed to some potential
-- communication partners.
--
-- The 'OutBox' contains an 'MVar' containing the
--  'Unagi.OutChan'. When a 'closeInBox' is called upon the
-- 'InBox' write to the 'OutBox' will fail.
{-# INLINE createInBox #-}
createInBox :: MonadUnliftIO m => Int -> m (InBox a)
createInBox !limit = do
  (!inChan, !outChan) <- liftIO (Unagi.newChan limit)
  return $! MkInBox inChan outChan limit

-- | Wait for and receive a message from an 'InBox'.
{-# INLINE receive #-}
receive :: MonadUnliftIO m => InBox a -> m a
receive (MkInBox _ !s _) =
  liftIO (Unagi.readChan s)

-- | Try to receive a message from an 'InBox',
-- return @Nothing@ if the queue is empty.
{-# INLINE tryReceive #-}
tryReceive :: MonadUnliftIO m => InBox a -> m (Maybe a)
tryReceive (MkInBox _ !s _) = liftIO $ do
  (!promise, _) <- Unagi.tryReadChan s
  Unagi.tryRead promise

-- | Create an 'OutBox' to write the items
-- that the given 'InBox' receives.
{-# INLINE createOutBoxForInbox #-}
createOutBoxForInbox :: MonadUnliftIO m => InBox a -> m (OutBox a)
createOutBoxForInbox (MkInBox !s _ !l) = return $! MkOutBox s l

-- | Try to put a message into the 'OutBox'
-- of an 'InBox', such that the process
-- reading the 'InBox' receives the message.
--
-- If the 'InBox' is full return @Left@ 'OutBoxBlocked'.
{-# INLINE trySend #-}
trySend ::
  MonadUnliftIO m =>
  OutBox a ->
  a ->
  m Bool
trySend (MkOutBox !s _) !a =
  liftIO $ Unagi.tryWriteChan s a

-- | Send a message by putting it into the 'OutBox'
-- of an 'InBox', such that the process
-- reading the 'InBox' receives the message.
--
-- Return @Left@ 'OutBoxBlocked' if the
-- 'InBox' has been closed or is full.
trySendAndWait ::
  MonadUnliftIO m =>
  Int ->
  OutBox a ->
  a ->
  m Bool
trySendAndWait !t !o !a =
  trySend o a
    >>= \case
      True -> return True
      False ->
        fromMaybe False <$> timeout t (blockingSend o a)

-- | A message queue out of which messages can by 'receive'd.
--
-- This is the counter part of 'OutBox'. Can be used for reading
-- messages.
--
-- Messages can be received by 'receive' or 'tryReceive'.
data InBox a
  = MkInBox
      !(Unagi.InChan a)
      !(Unagi.OutChan a)
      !Int

-- | A message queue into which messages can be enqued by,
--   e.g. 'trySend'.
--   Messages can be received from an 'InBox`.
--
--   The 'OutBox' is the counter part of an 'InBox'.
data OutBox a
  = MkOutBox
      !(Unagi.InChan a)
      !Int

-- internal functions

{-# INLINE blockingSend #-}
blockingSend ::
  MonadUnliftIO m =>
  OutBox a ->
  a ->
  m Bool
blockingSend (MkOutBox !s _) !a =
  do
    liftIO $ Unagi.writeChan s a
    return True
