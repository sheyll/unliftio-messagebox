{-# LANGUAGE StrictData #-}

-- | Thread safe queues for message passing
-- between many concurrent processes.
module Protocols.MessageBox
  ( InBox (),
    createInBox,
    receive,
    tryReceive,
    InBoxFailure (..),
    OutBox (..),
    newOutBox,
    trySend,
    OutBoxFailure (..),
  )
where

import Numeric.Natural (Natural)
import System.Mem.Weak (Weak, deRefWeak)
import UnliftIO
  ( MonadIO (liftIO),
    MonadUnliftIO,
    TBQueue,
    TVar,
    atomically,
    finally,
    isEmptyTBQueue,
    isFullTBQueue,
    mkWeakTVar,
    newTBQueue,
    newTVar,
    newTVarIO,
    readTBQueue,
    readTVar,
    tryReadTBQueue,
    writeTBQueue,
    writeTVar,
  )

-- | A message queue out of which messages can by 'receive'd.
--
-- This is the counter part of 'OutBox'. Can be used for reading
-- messages.
--
-- Messages can be received by 'receive' or 'tryReceive'.
data InBox a = MkInBox
  { _inBoxQueue :: TBQueue a,
    _inBoxClosed :: TVar Bool
  }

-- | Create an 'InBox' with an underlying
-- message queue with a given message limit.
--
-- From an 'InBox' a corresponding 'OutBox' can
-- be made, that can be passed to some potential
-- communication partners.
--
-- The 'OutBox' contains only a 'Weak' reference to
-- the message queue, and hence, an external process
-- can never prevent garbage collection of a message
-- queue, that is obsolete.
--
-- When the callback returns or throws an exception, the
-- message queue will be closed.
--
-- When the limit is reached, 'isFull' will return @True@.
--
-- When the inner code exits, even through an exception,
-- the queue will be flushed and 'isClosed' will return @True@
-- for ever.
createInBox :: MonadUnliftIO m => Natural -> (InBox a -> m b) -> m b
createInBox limit k = do
  mq <-
    atomically
      ( MkInBox <$> newTBQueue limit <*> newTVar False
      )
  k mq `finally` atomically (writeTVar (_inBoxClosed mq) True)

-- | Wait for and receive a message from an 'InBox'.
receive :: MonadUnliftIO m => InBox a -> m (Either InBoxFailure a)
receive MkInBox {_inBoxClosed, _inBoxQueue} = atomically $ do
  qClosed <- readTVar _inBoxClosed
  qEmptyAndClosed <-
    if qClosed
      then isEmptyTBQueue _inBoxQueue
      else pure False
  if qEmptyAndClosed
    then pure (Left InBoxEmptyAndClosed)
    else Right <$> readTBQueue _inBoxQueue

-- | Try to receive a message from an 'InBox',
-- return @Nothing@ if the queue is empty.
tryReceive :: MonadUnliftIO m => InBox a -> m (Either InBoxFailure (Maybe a))
tryReceive MkInBox {_inBoxClosed, _inBoxQueue} = atomically $ do
  qClosed <- readTVar _inBoxClosed
  qEmptyAndClosed <-
    if qClosed
      then isEmptyTBQueue _inBoxQueue
      else pure False
  if qEmptyAndClosed
    then pure (Left InBoxEmptyAndClosed)
    else Right <$> tryReadTBQueue _inBoxQueue

-- | A failure that occurs when receiving from an 'InBox'.
-- See 'receive' and 'tryReceive'.
data InBoxFailure
  = -- | An error that is returned when receiving messages
    -- from a close and empty 'InBox'.
    InBoxEmptyAndClosed

-- | A message queue into which messages can be enqued by,
--   e.g. 'trySend'.
--   Messages can be received from an 'InBox`.
--
--   The 'OutBox' is the counter part of an 'InBox'
--   Both are very similar, except that 'InBox' uses
--   'Weak' references to the actual message queue,
--   this way
data OutBox a = MkOutBox
  { _outBoxQueue :: Weak (TVar (TBQueue a)),
    _outBoxClosed :: TVar Bool
  }

-- | Convert an 'InBox' to an 'OutBox'.
-- This is used by the owner of the 'InBox'
-- to get the 'OutBox' that will allow
-- putting messages into the 'InBox'.
newOutBox :: MonadUnliftIO m => InBox a -> m (OutBox a)
newOutBox MkInBox {_inBoxClosed, _inBoxQueue} = do
  tq <- newTVarIO _inBoxQueue
  wtq <- mkWeakTVar tq (pure ())
  return MkOutBox {_outBoxQueue = wtq, _outBoxClosed = _inBoxClosed}

-- | Send a message by putting it into the 'OutBox'
-- of a message queue, such that the process
-- reading the 'InBox' receives the message.
--
-- Return @False@ if the message queue is closed
-- or full and does not handle any more messages.
--
-- Return @False@ even if the queue has reached
-- the maximum message limit and 'isOutBoxFull'
-- returns @True@.
trySend :: MonadUnliftIO m => OutBox a -> a -> m (Maybe OutBoxFailure)
trySend (MkOutBox !q !c) !a = do
  tbQueueVarM <- liftIO (deRefWeak q)
  case tbQueueVarM of
    Nothing -> return (Just OutBoxQueueDestroyed)
    Just tbQueueVar ->
      atomically $ do
        closed <- readTVar c
        if closed
          then return (Just OutBoxClosed)
          else do
            tbQueue <- readTVar tbQueueVar
            isFull <- isFullTBQueue tbQueue
            if isFull
              then return (Just OutBoxFull)
              else Nothing <$ writeTBQueue tbQueue a

-- | Different ways in that 'trySend' did not succeed
-- in sending a message to the 'OutBox'.
data OutBoxFailure
  = OutBoxClosed
  | OutBoxFull
  | OutBoxQueueDestroyed
