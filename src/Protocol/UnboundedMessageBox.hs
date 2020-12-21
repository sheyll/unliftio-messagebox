-- | Thread safe queues for message passing
-- between many concurrent processes.
--
-- This message box is __UNBOUNDED__.
--
-- Good single producer/single consumer performance
--
-- If you are sure that the producer(s) send messages
-- at a lower rate than the rate at which the consumer
-- consumes messages, use this module.
--
-- Otherwise use the more conservative
-- "Protocol.BoundedMessageBox" module.
module Protocol.UnboundedMessageBox
  ( createInBox,
    receive,
    tryReceive,
    createOutBoxForInbox,
    deliver,
    InBox (),
    OutBox (),
    InBoxNB (..),
    Class.InBoxConfig (..),
  )
where

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as Unagi
import Data.Functor
import qualified Protocol.MessageBoxClass as Class
import UnliftIO
  ( MonadIO (liftIO),
    MonadUnliftIO,
  )

-- | Create an 'InBox'.
--
-- From an 'InBox' a corresponding 'OutBox' can
-- be made, that can be passed to some potential
-- communication partners.
{-# INLINE createInBox #-}
createInBox :: MonadUnliftIO m => m (InBox a)
createInBox = do
  (!inChan, !outChan) <- liftIO Unagi.newChan
  return $! MkInBox inChan outChan

-- | Wait for and receive a message from an 'InBox'.
{-# INLINE receive #-}
receive :: MonadUnliftIO m => InBox a -> m a
receive (MkInBox _ !s) =
  liftIO (Unagi.readChan IO.yield s)

-- | Try to receive a message from an 'InBox',
-- return @Nothing@ if the queue is empty.
{-# INLINE tryReceive #-}
tryReceive :: MonadUnliftIO m => InBox a -> m (Maybe a)
tryReceive (MkInBox _ !s) = liftIO $ do
  !promise <- Unagi.tryReadChan s
  Unagi.tryRead promise

-- | Create an 'OutBox' to write the items
-- that the given 'InBox' receives.
{-# INLINE createOutBoxForInbox #-}
createOutBoxForInbox :: MonadUnliftIO m => InBox a -> m (OutBox a)
createOutBoxForInbox (MkInBox !s _) = return $! MkOutBox s

-- | Put a message into the 'OutBox'
-- of an 'InBox', such that the process
-- reading the 'InBox' receives the message.
{-# INLINE deliver #-}
deliver :: MonadUnliftIO m => OutBox a -> a -> m ()
deliver (MkOutBox !s) !a =
  liftIO $ Unagi.writeChan s a

-- | A message queue out of which messages can
--   by 'receive'd.
--
-- This is the counter part of 'OutBox'. Can be
-- used for reading messages.
--
-- Messages can be received by 'receive' or 'tryReceive'.
data InBox a
  = MkInBox
      !(Unagi.InChan a)
      !(Unagi.OutChan a)

-- | A message queue into which messages can be enqued by,
--   e.g. 'deliver'.
--   Messages can be received from an 'InBox`.
--
--   The 'OutBox' is the counter part of an 'InBox'.
newtype OutBox a = MkOutBox (Unagi.InChan a)

instance Class.IsMessageBox InBox OutBox where
  data InBoxConfig InBox = UnboundedMessageBox
    deriving stock (Show)
  {-# INLINE newInBox #-}
  newInBox _ = createInBox
  {-# INLINE newOutBox #-}
  newOutBox !i = createOutBoxForInbox i

-- | A blocking instance that invokes 'receive'.
instance Class.IsInBox InBox where
  {-# INLINE receive #-}
  receive !i = Just <$> receive i

-- | A blocking instance that invokes 'deliver'.
instance Class.IsOutBox OutBox where
  {-# INLINE deliver #-}
  deliver !o !m = deliver o m $> True

-- | A wrapper around 'InBox' to have a
-- non-blocking instance of 'Class.IsMessageBox'
-- that invokes 'tryReceive' instead of 'receive'.
--
-- Used in conjunction with 'OutBoxNB'.
newtype InBoxNB a = InBoxNB (InBox a)

-- | A non-blocking instance that invokes 'tryReceive'.
instance Class.IsInBox InBoxNB where
  {-# INLINE receive #-}
  receive (InBoxNB !i) = tryReceive i
