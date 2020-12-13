-- | Thread safe queues for message passing
-- between many concurrent processes.
--
-- This impl
module Protocol.MessageBox
  ( InBox (_inBoxLimit, _inBoxSource),
    createInBox,
    receive,
    tryReceive,
    OutBox (..),
    createOutBoxForInbox,
    trySend,
    closeInBox,
    trySendAndWait,
    trySendAndWaitForever,
    OutBoxFailure (..),
    OutBoxSuccess (..),
  )
where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import Control.Monad ( void )
import UnliftIO
    ( MonadIO(liftIO),
      isEmptyMVar,
      newMVar,
      readMVar,
      takeMVar,
      tryReadMVar,
      timeout,
      MVar,
      MonadUnliftIO )

-- | A message queue out of which messages can by 'receive'd.
--
-- This is the counter part of 'OutBox'. Can be used for reading
-- messages.
--
-- Messages can be received by 'receive' or 'tryReceive'.
data InBox a = MkInBox
  { _inBoxSink :: MVar (Unagi.InChan a),
    _inBoxSource :: Unagi.OutChan a,
    _inBoxLimit :: Int
  }

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
  inChanVar <- newMVar inChan
  return
    MkInBox
      { _inBoxSink = inChanVar,
        _inBoxSource = outChan,
        _inBoxLimit = limit
      }

-- | Wait for and receive a message from an 'InBox'.
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
closeInBox :: MonadUnliftIO m => InBox a -> m ()
closeInBox MkInBox {_inBoxSink} = do
  void (takeMVar _inBoxSink)  

-- | A message queue into which messages can be enqued by,
--   e.g. 'trySend'.
--   Messages can be received from an 'InBox`.
--
--   The 'OutBox' is the counter part of an 'InBox'.
data OutBox a = MkOutBox
  { _outBoxSink :: MVar (Unagi.InChan a),
    _outBoxLimit :: Int
  }

-- | Create am'OutBox' to write the items
-- that the given 'InBox' receives.
createOutBoxForInbox :: MonadUnliftIO m => InBox a -> m (OutBox a)
createOutBoxForInbox MkInBox {_inBoxSink, _inBoxLimit} = do
  return MkOutBox {_outBoxSink = _inBoxSink, _outBoxLimit = _inBoxLimit}

-- | Send a message by putting it into the 'OutBox'
-- of an 'InBox', such that the process
-- reading the 'InBox' receives the message.
--
-- Return @Left@ 'OutBoxClosed' if the
-- 'InBox' has been 'closed'.
trySend ::
  MonadUnliftIO m =>
  OutBox a ->
  a ->
  m (Either OutBoxFailure OutBoxSuccess)
trySend MkOutBox {_outBoxSink, _outBoxLimit} !a =
  tryReadMVar _outBoxSink
    >>= maybe
      (return (Left OutBoxClosed))
      ( \ !sink ->
          do
            !didWrite <- liftIO $ Unagi.tryWriteChan sink a
            if didWrite
              then do
                !s <- liftIO $ Unagi.estimatedLength sink
                return . Right $
                  if 2 * s < _outBoxLimit
                    then OutBoxOk
                    else OutBoxCriticallyFull
              else do
                !closed <- isEmptyMVar _outBoxSink
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
trySendAndWait !t o@MkOutBox {_outBoxSink} !a =
  timeout t (trySendAndWaitForever o a)
    >>= maybe
      ( do
          !closed <- isEmptyMVar _outBoxSink
          return
            ( Left
                ( if closed
                    then OutBoxClosed
                    else OutBoxTimeout
                )
            )
      )
      (return . Right)

-- | Send a message by putting it into the 'OutBox'
-- of an 'InBox', such that the process
-- reading the 'InBox' receives the message.
--
-- This might block forever.
--
-- Return @Nothing@ if the 'InBox' has been 'closed'.
trySendAndWaitForever ::
  MonadUnliftIO m =>
  OutBox a ->
  a ->
  m OutBoxSuccess
trySendAndWaitForever MkOutBox {_outBoxSink, _outBoxLimit} !a =
  readMVar _outBoxSink >>= \ !sink -> liftIO $ do
    Unagi.writeChan sink a
    s <- Unagi.estimatedLength sink
    return $
      if 2 * s < _outBoxLimit
        then OutBoxOk
        else OutBoxCriticallyFull

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
