-- | Thread safe queues for uni directional message passing
-- between threads.
--
-- This message box has an upper limit, that means that
-- sometimes delivery either fails or is blocked until
-- the receiving thread has consumed more messages.
--
-- Use this module if the producer(s) outperform the consumer,
-- but you want the extra safety that the queue blocks the
-- 'Input' after a certain message limit is reached.
--
-- If you are sure that the producers fire at a slower rate
-- then the rate at which the consumer consumes messages, use this
-- module.
module Protocol.MessageBox.Limited
  ( MessageLimit (..),
    messageLimitToInt,
    BlockingBoxLimit (..),
    BlockingBox (),
    BlockingInput (),
    NonBlockingBoxLimit (..),
    NonBlockingBox (),
    NonBlockingInput (..),
    WaitingBoxLimit (..),
    WaitingBox (..),
    WaitingInput (..),
  )
where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import Control.Monad (unless)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Protocol.Future (Future (..))
import qualified Protocol.MessageBox.Class as Class
import UnliftIO
  ( MonadIO (liftIO),
    MonadUnliftIO,
    timeout,
  )
import UnliftIO.Concurrent (threadDelay)

-- | Message Limit
--
-- The message limit must be a reasonable small positive integer
-- that is also a power of two. This stems from the fact that
-- Unagi is used under the hood.
--
-- The limit is a lower bound.
data MessageLimit
  = MessageLimit_1
  | MessageLimit_2
  | MessageLimit_4
  | MessageLimit_8
  | MessageLimit_16
  | MessageLimit_32
  | MessageLimit_64
  | MessageLimit_128
  | MessageLimit_256
  | MessageLimit_512
  | MessageLimit_1024
  | MessageLimit_2048
  | MessageLimit_4096
  deriving stock
    (Eq, Ord, Show, Bounded, Enum)

-- | Convert a 'MessageLimit' to the
-- 'Int' representation.
{-# INLINE messageLimitToInt #-}
messageLimitToInt :: MessageLimit -> Int
messageLimitToInt =
  \case
    MessageLimit_1 -> 1
    MessageLimit_2 -> 2
    MessageLimit_4 -> 4
    MessageLimit_8 -> 8
    MessageLimit_16 -> 16
    MessageLimit_32 -> 32
    MessageLimit_64 -> 64
    MessageLimit_128 -> 128
    MessageLimit_256 -> 256
    MessageLimit_512 -> 512
    MessageLimit_1024 -> 1024
    MessageLimit_2048 -> 2048
    MessageLimit_4096 -> 4096

-- * 'Class.IsMessageBoxFactory' instances

-- ** Blocking

-- | Contains the (vague) limit of messages that a 'BlockingBox'
-- can buffer, i.e. that 'deliver' can put into a 'BlockingInput'
-- of a 'BlockingBox'.
newtype BlockingBoxLimit = BlockingBoxLimit MessageLimit
  deriving stock (Eq)

instance Show BlockingBoxLimit where
  showsPrec _ (BlockingBoxLimit !l) =
    showString "Blocking" . showsPrec 9 (messageLimitToInt l)

-- | A message queue out of which messages can by 'receive'd.
--
-- This is the counter part of 'Input'. Can be used for reading
-- messages.
--
-- Messages can be received by 'receive' or 'tryReceive'.
data BlockingBox a
  = MkBlockingBox
      !(Unagi.InChan a)
      !(Unagi.OutChan a)

-- | A message queue into which messages can be enqued by,
--   e.g. 'tryToDeliver'.
--   Messages can be received from an 'BlockingBox`.
--
--   The 'Input' is the counter part of a 'BlockingBox'.
newtype BlockingInput a = MkBlockingInput (Unagi.InChan a)

instance Class.IsMessageBoxFactory BlockingBoxLimit where
  type MessageBox BlockingBoxLimit = BlockingBox
  {-# INLINE newMessageBox #-}
  newMessageBox (BlockingBoxLimit !limit) = create limit
  getConfiguredMessageLimit (BlockingBoxLimit !limit) =
    Just (messageLimitToInt limit)

-- | A blocking instance that invokes 'receive'.
instance Class.IsMessageBox BlockingBox where
  type Input BlockingBox = BlockingInput

  {-# INLINE receive #-}
  receive !i = Just <$> receive i
  {-# INLINE tryReceive #-}
  tryReceive !i = tryReceive i
  {-# INLINE newInput #-}
  newInput !i = newInput i
  receiveAfter (MkBlockingBox _ !s) !rto =
    do
      (!promise, !blocker) <- liftIO (Unagi.tryReadChan s)
      liftIO (Unagi.tryRead promise)
        >>= maybe
          (timeout rto (liftIO blocker))
          (return . Just)

-- | A blocking instance that invokes 'deliver'.
instance Class.IsInput BlockingInput where
  {-# INLINE deliver #-}
  deliver !o !a = deliver o a $> True

--  ** A wrapper around 'BlockingBox' for Non-Blocking Input (NBI)

-- | A 'BlockingBoxLimit' wrapper for non-blocking 'Class.IsMessageBoxFactory' instances.
newtype NonBlockingBoxLimit = NonBlockingBoxLimit MessageLimit
  deriving stock (Eq)

instance Show NonBlockingBoxLimit where
  showsPrec _ (NonBlockingBoxLimit !l) =
    showString "NonBlocking" . showsPrec 9 (messageLimitToInt l)

instance Class.IsMessageBoxFactory NonBlockingBoxLimit where
  type MessageBox NonBlockingBoxLimit = NonBlockingBox
  {-# INLINE newMessageBox #-}
  newMessageBox (NonBlockingBoxLimit !l) =
    NonBlockingBox <$> Class.newMessageBox (BlockingBoxLimit l)
  getConfiguredMessageLimit (NonBlockingBoxLimit !limit) =
    Just (messageLimitToInt limit)

-- | A 'BlockingBox' wrapper for non-blocking 'Class.IsMessageBox' instances.
--
-- The difference to the 'BlockingBox' instance is that 'Class.deliver'
-- immediately returns if the message box limit is surpassed.
newtype NonBlockingBox a = NonBlockingBox (BlockingBox a)

instance Class.IsMessageBox NonBlockingBox where
  type Input NonBlockingBox = NonBlockingInput
  {-# INLINE receive #-}
  receive (NonBlockingBox !i) = Just <$> receive i
  {-# INLINE tryReceive #-}
  tryReceive (NonBlockingBox !i) = tryReceive i
  {-# INLINE receiveAfter #-}
  receiveAfter (NonBlockingBox !b) !rto =
    Class.receiveAfter b rto
  {-# INLINE newInput #-}
  newInput (NonBlockingBox !i) = NonBlockingInput <$> newInput i

-- | A wrapper around 'BlockingInput' with a non-blocking 'Class.IsInput' instance.
--
-- 'deliver' will enqueue the message or return 'False' immediately,
-- if the message box already contains more messages than
-- it's limit allows.
newtype NonBlockingInput a = NonBlockingInput (BlockingInput a)

instance Class.IsInput NonBlockingInput where
  {-# INLINE deliver #-}
  deliver (NonBlockingInput !o) !a = do
    !res <- tryToDeliver o a
    unless res (threadDelay 10)
    return res

--  ** 'BlockingBox' Wrapper with Timeout

-- | A 'Class.IsMessageBoxFactory' instance wrapping the 'BlockingBox'
--  with independently configurable timeouts for 'receive' and 'deliver'.
data WaitingBoxLimit
  = WaitingBoxLimit
      !(Maybe Int)
      !Int
      !MessageLimit
  deriving stock (Eq)

instance Show WaitingBoxLimit where
  showsPrec _ (WaitingBoxLimit !t0 !t1 !l) =
    showString "Waiting_"
      . ( case t0 of
            Nothing -> id
            Just !t -> showsPrec 9 t . showChar '_'
        )
      . showsPrec 9 t1
      . showChar '_'
      . showsPrec 9 (messageLimitToInt l)

instance Class.IsMessageBoxFactory WaitingBoxLimit where
  type MessageBox WaitingBoxLimit = WaitingBox
  {-# INLINE newMessageBox #-}
  newMessageBox l@(WaitingBoxLimit _ _ !c) =
    WaitingBox l <$> Class.newMessageBox (BlockingBoxLimit c)
  getConfiguredMessageLimit (WaitingBoxLimit _ _ !limit) =
    Just (messageLimitToInt limit)

-- | A 'BlockingBox' an a 'WaitingBoxLimit' for
-- the 'Class.IsMessageBox' instance.
data WaitingBox a
  = WaitingBox WaitingBoxLimit (BlockingBox a)

instance Class.IsMessageBox WaitingBox where
  type Input WaitingBox = WaitingInput
  {-# INLINE receive #-}
  receive (WaitingBox (WaitingBoxLimit (Just !rto) _ _) (MkBlockingBox _ !s)) =
    liftIO $ do
      (!promise, !blocker) <- Unagi.tryReadChan s
      Unagi.tryRead promise
        >>= maybe
          (timeout rto blocker)
          (return . Just)
  receive (WaitingBox !_ !m) =
    Class.receive m
  {-# INLINE receiveAfter #-}
  receiveAfter (WaitingBox _ !b) !rto =
    Class.receiveAfter b rto
  {-# INLINE tryReceive #-}
  tryReceive (WaitingBox _ !m) = tryReceive m
  {-# INLINE newInput #-}
  newInput (WaitingBox (WaitingBoxLimit _ !dto _) !m) =
    WaitingInput dto <$> newInput m

-- | An input for a 'BlockingBox' that will block
-- for not much more than the given timeout when
-- the message box is full.
data WaitingInput a
  = WaitingInput
      !Int
      !(BlockingInput a)

instance Class.IsInput WaitingInput where
  {-# INLINE deliver #-}
  deliver (WaitingInput !t !o) !a = tryToDeliverAndWait t o a

-- Internal Functions

{-# INLINE create #-}
create :: MonadUnliftIO m => MessageLimit -> m (BlockingBox a)
create !limit = do
  (!inChan, !outChan) <- liftIO (Unagi.newChan (messageLimitToInt limit))
  return $! MkBlockingBox inChan outChan

{-# INLINE receive #-}
receive :: MonadUnliftIO m => BlockingBox a -> m a
receive (MkBlockingBox _ !s) =
  liftIO (Unagi.readChan s)

-- | Return a 'Future' for the next value that will be received.
{-# INLINE tryReceive #-}
tryReceive :: MonadUnliftIO m => BlockingBox a -> m (Future a)
tryReceive (MkBlockingBox _ !s) = liftIO $ do
  (!promise, _) <- Unagi.tryReadChan s
  return (Future (Unagi.tryRead promise))

{-# INLINE newInput #-}
newInput :: MonadUnliftIO m => BlockingBox a -> m (BlockingInput a)
newInput (MkBlockingBox !s _) = return $! MkBlockingInput s

{-# INLINE deliver #-}
deliver :: MonadUnliftIO m => BlockingInput a -> a -> m ()
deliver (MkBlockingInput !s) !a =
  liftIO $ Unagi.writeChan s a

-- | Try to put a message into the 'BlockingInput'
-- of a 'MessageBox', such that the process
-- reading the 'MessageBox' receives the message.
--
-- If the 'MessageBox' is full return False.
{-# INLINE tryToDeliver #-}
tryToDeliver :: MonadUnliftIO m => BlockingInput a -> a -> m Bool
tryToDeliver (MkBlockingInput !s) !a =
  liftIO $ Unagi.tryWriteChan s a

-- | Send a message by putting it into the 'BlockingInput'
-- of a 'MessageBox', such that the process
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
  BlockingInput a ->
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
