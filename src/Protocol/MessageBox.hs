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
module Protocol.MessageBox
  ( createInBox,   
    receive,
    tryReceive,    
    closeInBox,
    createOutBoxForInbox,
    trySend,
    trySendAndWait,
    blockingSend,
    InBox (_inBoxLimit, _inBoxSource),
    OutBox (..),
    OutBoxFailure (..),
    OutBoxSuccess (..),
  )
where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import Data.Functor (void)
import Data.Maybe (isNothing)
import UnliftIO
  ( MVar,
    MonadIO (liftIO),
    MonadUnliftIO,
    newMVar,
    readMVar,
    swapMVar,
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
createInBox :: MonadUnliftIO m => Int -> m (InBox a)
createInBox !limit = do
  (!inChan, !outChan) <- liftIO (Unagi.newChan limit)
  !inChanVar <- newMVar (Just inChan)
  return
    MkInBox
      { _inBoxSink = inChanVar,
        _inBoxSource = outChan,
        _inBoxLimit = limit
      }

-- | Wait for and receive a message from an 'InBox'.
{-# INLINE receive #-}
receive :: MonadUnliftIO m => InBox a -> m a
receive MkInBox {_inBoxSource} =
  liftIO (Unagi.readChan _inBoxSource)

-- | Try to receive a message from an 'InBox',
-- return @Nothing@ if the queue is empty.
tryReceive :: MonadUnliftIO m => InBox a -> m (Maybe a)
tryReceive MkInBox {_inBoxSource} = liftIO $ do
  (!promise, _) <- Unagi.tryReadChan _inBoxSource
  Unagi.tryRead promise

-- | Read all contents of the 'InBox' at once and atomically
-- set the 'InBox' to the closed state.
{-# INLINE closeInBox #-}
closeInBox :: MonadUnliftIO m => InBox a -> m ()
closeInBox !i = void $! swapMVar (_inBoxSink i) Nothing

-- | Create am'OutBox' to write the items
-- that the given 'InBox' receives.
{-# INLINE createOutBoxForInbox #-}
createOutBoxForInbox :: MonadUnliftIO m => InBox a -> m (OutBox a)
createOutBoxForInbox MkInBox {_inBoxSink, _inBoxLimit} = do
  return MkOutBox {_outBoxSink = _inBoxSink, _outBoxLimit = _inBoxLimit}

-- | Try to put a message into the 'OutBox'
-- of an 'InBox', such that the process
-- reading the 'InBox' receives the message.
--
-- If the  'InBox' is full return @Left@ 'OutBoxFull'.
-- Return @Left@ 'OutBoxClosed' if the 'InBox' has been closed.
trySend ::
  MonadUnliftIO m =>
  OutBox a ->
  a ->
  m (Either OutBoxFailure OutBoxSuccess)
trySend o@MkOutBox {_outBoxSink, _outBoxLimit} !a =
  readMVar _outBoxSink
    >>= maybe
      (return (Left OutBoxClosed))
      ( \ !sink ->
          do
            !didWrite <- liftIO $ Unagi.tryWriteChan sink a
            if didWrite
              then Right <$> checkOutBoxLimit sink _outBoxLimit
              else do
                !closed <- isInBoxClosed o
                return
                  ( Left
                      ( if closed
                          then OutBoxClosed
                          else OutBoxFull
                      )
                  )
      )

-- | Send a message by putting it into the 'OutBox'
-- of an 'InBox', such that the process
-- reading the 'InBox' receives the message.
--
-- If the message cannot be delivered in time,
-- the 'OutBoxFull' error is returned
--
-- Return @Left@ 'OutBoxClosed' if the
-- 'InBox' has been 'closed'.
trySendAndWait ::
  MonadUnliftIO m =>
  Int ->
  OutBox a ->
  a ->
  m (Either OutBoxFailure OutBoxSuccess)
trySendAndWait !t !o !a =
  trySend o a
    >>= \case
      Right !x -> return (Right x)
      Left !_ ->
        timeout t (blockingSend o a)
          >>= maybe
            ( do
                !closed <- isInBoxClosed o
                return
                  ( Left
                      ( if closed
                          then OutBoxClosed
                          else OutBoxTimeout
                      )
                  )
            )
            ( maybe
                (return (Left OutBoxClosed))
                (return . Right)
            )

-- | A message queue out of which messages can by 'receive'd.
--
-- This is the counter part of 'OutBox'. Can be used for reading
-- messages.
--
-- Messages can be received by 'receive' or 'tryReceive'.
data InBox a = MkInBox
  { _inBoxSink :: MVar (Maybe (Unagi.InChan a)),
    _inBoxSource :: Unagi.OutChan a,
    _inBoxLimit :: Int
  }

-- | A message queue into which messages can be enqued by,
--   e.g. 'trySend'.
--   Messages can be received from an 'InBox`.
--
--   The 'OutBox' is the counter part of an 'InBox'.
data OutBox a = MkOutBox
  { _outBoxSink :: MVar (Maybe (Unagi.InChan a)),
    _outBoxLimit :: Int
  }

-- | Different ways in that 'trySend' did not succeed
-- in sending a message to the 'OutBox'.
data OutBoxFailure
  = OutBoxFull
  | OutBoxTimeout
  | OutBoxClosed
  deriving stock (Show, Eq)

-- | Result of a 'trySend' that succeeded.
data OutBoxSuccess
  = -- | The item was enqueued, and the queue is relatively empty
    OutBoxOk
  | -- | Although the item was enqueue, the queue has filled up to more than half of the limit
    --   given to
    OutBoxCriticallyFull
  deriving stock (Show, Eq)

-- internal functions

{-# INLINE blockingSend #-}
blockingSend ::
  MonadUnliftIO m =>
  OutBox a ->
  a ->
  m (Maybe OutBoxSuccess)
blockingSend MkOutBox {_outBoxSink = !sRef, _outBoxLimit = !l} !a =
  readMVar sRef
    >>= maybe
      (return Nothing)
      ( \ !s -> do
          liftIO $ Unagi.writeChan s a
          Just <$> checkOutBoxLimit s l
      )

{-# INLINE checkOutBoxLimit #-}
checkOutBoxLimit :: MonadIO m => Unagi.InChan a -> Int -> m OutBoxSuccess
checkOutBoxLimit !sink !limit = liftIO $ do
  !s <- Unagi.estimatedLength sink
  return $
    if 2 * s < limit
      then OutBoxOk
      else OutBoxCriticallyFull

{-# INLINE isInBoxClosed #-}
isInBoxClosed :: MonadIO m => OutBox a -> m Bool
isInBoxClosed = fmap isNothing . readMVar . _outBoxSink