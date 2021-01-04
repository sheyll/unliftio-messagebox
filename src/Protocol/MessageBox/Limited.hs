-- | Thread safe queues for uni directional message passing
-- between threads.
--
-- This message box has an upper limit.
--
-- Use this module if the producer(s) outperform the consumer,
-- but you want the extra safety that the queue blocks the
-- 'Input' after a certain message limit is reached.
--
-- If you are sure that the producers fire at a slower rate
-- then the rate at which the consumer consumes messages, use this
-- module.
module Protocol.MessageBox.Limited
  ( create,
    receive,
    tryReceive,
    newInput,
    tryToDeliver,
    tryToDeliverAndWait,
    deliver,
    LimitedMessageBox (..),
    MessageBox (),
    Input (),
    InputNB (..),
  )
where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import qualified Protocol.MessageBox.Class as Class
import UnliftIO
  ( MonadIO (liftIO),
    MonadUnliftIO,
    timeout,
  )

-- | Create a message box to which messages can be
-- enqueued using an 'Input'.
--
-- The underlying message queue has roughly the
-- given message limit.
--
-- Use 'newInput' to create a corresponding 
-- 'Input', that can be passed to some potential
-- communication partners/threads.
{-# INLINE create #-}
create :: MonadUnliftIO m => Int -> m (MessageBox a)
create !limit = do
  (!inChan, !outChan) <- liftIO (Unagi.newChan limit)
  return $! MkOutput inChan outChan

-- | Wait for and receive a message from an 'MessageBox'.
{-# INLINE receive #-}
receive :: MonadUnliftIO m => MessageBox a -> m a
receive (MkOutput _ !s) =
  liftIO (Unagi.readChan s)

-- | Return a 'Future' for the next value that will be received.
{-# INLINE tryReceive #-}
tryReceive :: MonadUnliftIO m => MessageBox a -> m (Class.Future a)
tryReceive (MkOutput _ !s) = liftIO $ do
  (!promise, _) <- Unagi.tryReadChan s
  return (Class.Future (Unagi.tryRead promise))

-- | Create an 'Input' to write the items
-- that the given 'MessageBox' receives.
{-# INLINE newInput #-}
newInput :: MonadUnliftIO m => MessageBox a -> m (Input a)
newInput (MkOutput !s _) = return $! MkInput s

-- | Put a message into an 'Input', such that the 'MessageBox' 
-- receives the message.
--
-- If the 'MessageBox' is full, wait until the end of time
-- or that the message box is not full anymore.
{-# INLINE deliver #-}
deliver :: MonadUnliftIO m => Input a -> a -> m ()
deliver (MkInput !s) !a =
  liftIO $ Unagi.writeChan s a

-- | Try to put a message into the 'Input'
-- of an 'MessageBox', such that the process
-- reading the 'MessageBox' receives the message.
--
-- If the 'MessageBox' is full return False.
{-# INLINE tryToDeliver #-}
tryToDeliver :: MonadUnliftIO m => Input a -> a -> m Bool
tryToDeliver (MkInput !s) !a =
  liftIO $ Unagi.tryWriteChan s a

-- | Send a message by putting it into the 'Input'
-- of an 'MessageBox', such that the process
-- reading the 'MessageBox' receives the message.
--
-- Return False if the
-- 'MessageBox' has been closed or is full.
--
-- This assumes that the queue is likely empty, and
-- tries 'tryToDeliver' first before wasting any
-- precious cpu cycles entering 'timeout'.
tryToDeliverAndWait ::
  MonadUnliftIO m =>
  Int ->
  Input a ->
  a ->
  m Bool
tryToDeliverAndWait !t !o !a =
  -- Benchmarks have shown great improvements
  -- when calling tryToDeliver once before doing
  -- deliver in a System.Timeout.timeout;
  --
  -- We even tried calling 'tryToDeliver' more than once,
  -- but that did not lead to convinving improvements.
  --
  -- Benachmarks have also shown, that sending pessimistically
  -- (i.e. avoiding `tryToDeliver`) does not improve performance,
  -- even when the message queue is congested
  --
  -- See benchmark results:
  -- `benchmark-results/optimistic-vs-pessimistic.html`
  tryToDeliver o a >>= \case
    True -> return True
    False ->
      fromMaybe False <$> timeout t (deliver o a $> True)

-- | A message queue out of which messages can by 'receive'd.
--
-- This is the counter part of 'Input'. Can be used for reading
-- messages.
--
-- Messages can be received by 'receive' or 'tryReceive'.
data MessageBox a
  = MkOutput
      !(Unagi.InChan a)
      !(Unagi.OutChan a)

-- | A message queue into which messages can be enqued by,
--   e.g. 'tryToDeliver'.
--   Messages can be received from an 'MessageBox`.
--
--   The 'Input' is the counter part of an 'MessageBox'.
newtype Input a = MkInput (Unagi.InChan a)

-- * 'Class.IsMessageBoxFactory' instances

-- ** Blocking

-- | Contains the (vague) limit of messages that
-- can be enqueued in an 'Input' to be read from
-- an 'MessageBox'. 
newtype LimitedMessageBox = LimitedMessageBox Int
  deriving stock (Show)

instance Class.IsMessageBoxFactory LimitedMessageBox MessageBox where
  {-# INLINE newMessageBox #-}
  newMessageBox (LimitedMessageBox !limit) = create limit

-- | A blocking instance that invokes 'receive'.
instance Class.IsMessageBox MessageBox where
  type Input MessageBox = Input
  {-# INLINE receive #-}
  receive !i = Just <$> receive i
  {-# INLINE tryReceive #-}
  tryReceive !i = tryReceive i
  {-# INLINE newInput #-}
  newInput !i = newInput i

-- | A blocking instance that invokes 'deliver'.
instance Class.IsInput Input where
  {-# INLINE deliver #-}
  deliver !o !a = deliver o a $> True

--  ** Non-Blocking

-- | A wrapper around 'Input'
-- that invokes 'tryToDeliver' instead of 'deliver'.
--
-- Used in conjunction with 'InputNB'.
newtype InputNB a = InputNB (Input a)

-- | A non-blocking instance
-- that invokes 'tryToDeliver'.
instance Class.IsInput InputNB where
  {-# INLINE deliver #-}
  deliver (InputNB !o) !a = tryToDeliver o a
