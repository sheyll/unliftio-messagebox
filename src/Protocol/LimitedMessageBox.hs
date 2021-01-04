-- | Thread safe queues for message passing
-- between many concurrent processes.
--
-- This message box is __LIMITED__.
--
-- Use this module if the producer(s) outperform the consumer,
-- but you want the extra safety that the queue blocks the
-- 'OutBox' after a certain message limit is reached.
--
-- If you are sure that the producers fire at a slower rate
-- then the rate at which the consumer consumes messages, use this
-- module.
module Protocol.LimitedMessageBox
  ( createInBox,
    receive,
    tryReceive,
    createOutBoxForInbox,
    tryToDeliver,
    tryToDeliverAndWait,
    deliver,
    LimitedMessageBox (..),
    InBox (),
    OutBox (),
    OutBoxNB (..),
  )
where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import qualified Protocol.MessageBoxClass as Class
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
{-# INLINE createInBox #-}
createInBox :: MonadUnliftIO m => Int -> m (InBox a)
createInBox !limit = do
  (!inChan, !outChan) <- liftIO (Unagi.newChan limit)
  return $! MkInBox inChan outChan

-- | Wait for and receive a message from an 'InBox'.
{-# INLINE receive #-}
receive :: MonadUnliftIO m => InBox a -> m a
receive (MkInBox _ !s) =
  liftIO (Unagi.readChan s)

-- | Return a 'Future' for the next value that will be received.
{-# INLINE tryReceive #-}
tryReceive :: MonadUnliftIO m => InBox a -> m (Class.Future a)
tryReceive (MkInBox _ !s) = liftIO $ do
  (!promise, _) <- Unagi.tryReadChan s
  return (Class.Future (Unagi.tryRead promise))

-- | Create an 'OutBox' to write the items
-- that the given 'InBox' receives.
{-# INLINE createOutBoxForInbox #-}
createOutBoxForInbox :: MonadUnliftIO m => InBox a -> m (OutBox a)
createOutBoxForInbox (MkInBox !s _) = return $! MkOutBox s

-- | Put a message into the 'OutBox'
-- of an 'InBox', such that the process
-- reading the 'InBox' receives the message.
--
-- If the 'InBox' is full, wait until the end of time
-- or that the message box is not full anymore.
{-# INLINE deliver #-}
deliver :: MonadUnliftIO m => OutBox a -> a -> m ()
deliver (MkOutBox !s) !a =
  liftIO $ Unagi.writeChan s a

-- | Try to put a message into the 'OutBox'
-- of an 'InBox', such that the process
-- reading the 'InBox' receives the message.
--
-- If the 'InBox' is full return False.
{-# INLINE tryToDeliver #-}
tryToDeliver :: MonadUnliftIO m => OutBox a -> a -> m Bool
tryToDeliver (MkOutBox !s) !a =
  liftIO $ Unagi.tryWriteChan s a

-- | Send a message by putting it into the 'OutBox'
-- of an 'InBox', such that the process
-- reading the 'InBox' receives the message.
--
-- Return False if the
-- 'InBox' has been closed or is full.
--
-- This assumes that the queue is likely empty, and
-- tries 'tryToDeliver' first before wasting any
-- precious cpu cycles entering 'timeout'.
tryToDeliverAndWait ::
  MonadUnliftIO m =>
  Int ->
  OutBox a ->
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
-- This is the counter part of 'OutBox'. Can be used for reading
-- messages.
--
-- Messages can be received by 'receive' or 'tryReceive'.
data InBox a
  = MkInBox
      !(Unagi.InChan a)
      !(Unagi.OutChan a)

-- | A message queue into which messages can be enqued by,
--   e.g. 'tryToDeliver'.
--   Messages can be received from an 'InBox`.
--
--   The 'OutBox' is the counter part of an 'InBox'.
newtype OutBox a = MkOutBox (Unagi.InChan a)

-- * 'Class.IsInBoxConfig' instances

-- ** Blocking

-- | Contains the (vague) limit of messages that
-- can be enqueued in an 'OutBox' to be read from
-- an 'InBox'. 
newtype LimitedMessageBox = LimitedMessageBox Int
  deriving stock (Show)

instance Class.IsInBoxConfig LimitedMessageBox InBox where
  {-# INLINE newInBox #-}
  newInBox (LimitedMessageBox !limit) = createInBox limit

-- | A blocking instance that invokes 'receive'.
instance Class.IsInBox InBox where
  type OutBox2 InBox = OutBox
  {-# INLINE receive #-}
  receive !i = Just <$> receive i
  {-# INLINE tryReceive #-}
  tryReceive !i = tryReceive i
  {-# INLINE newOutBox2 #-}
  newOutBox2 !i = createOutBoxForInbox i

-- | A blocking instance that invokes 'deliver'.
instance Class.IsOutBox OutBox where
  {-# INLINE deliver #-}
  deliver !o !a = deliver o a $> True

--  ** Non-Blocking

-- | A wrapper around 'OutBox' to have a
-- non-blocking instance of 'Class.IsInBoxConfig'
-- that invokes 'tryToDeliver' instead of 'deliver'.
--
-- Used in conjunction with 'OutBoxNB'.
newtype OutBoxNB a = OutBoxNB (OutBox a)

-- | A non-blocking instance
-- that invokes 'tryToDeliver'.
instance Class.IsOutBox OutBoxNB where
  {-# INLINE deliver #-}
  deliver (OutBoxNB !o) !a = tryToDeliver o a
