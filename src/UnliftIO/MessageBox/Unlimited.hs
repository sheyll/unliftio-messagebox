-- | Thread safe queues for message passing
-- between many concurrent processes.
--
-- This message box is __UNLIMITED__.
--
-- Good single producer/single consumer performance
--
-- If you are sure that the producer(s) send messages
-- at a lower rate than the rate at which the consumer
-- consumes messages, use this module.
--
-- Otherwise use the more conservative
-- "UnliftIO.MessageBox.Limited" module.
module UnliftIO.MessageBox.Unlimited
  ( BlockingUnlimited (..),
    UnlimitedBox (),
    UnlimitedBoxInput (),
  )
where

-- import qualified Control.Concurrent.Chan.Unagi.NoBlocking as Unagi
import qualified Control.Concurrent.Chan.Unagi as Unagi
import Data.Functor (($>))
import UnliftIO.MessageBox.Util.Future (Future (..))
import qualified UnliftIO.MessageBox.Class as Class
import UnliftIO
  ( MonadIO (liftIO),
    MonadUnliftIO,
  )

-- | A message queue out of which messages can
--   by 'receive'd.
--
-- This is the counter part of 'Input'. Can be
-- used for reading messages.
--
-- Messages can be received by 'receive' or 'tryReceive'.
data UnlimitedBox a
  = MkUnlimitedBox
      !(Unagi.InChan a)
      !(Unagi.OutChan a)

-- | A message queue into which messages can be enqued by,
--   e.g. 'deliver'.
--   Messages can be received from an 'UnlimitedBox`.
--
--   The 'UnlimitedBoxInput' is the counter part of a 'UnlimitedBox'.
newtype UnlimitedBoxInput a = MkUnlimitedBoxInput (Unagi.InChan a)

-- | The (empty) configuration for creating
-- 'UnlimitedBox'es using the 'Class.IsMessageBoxArg' methods.
data BlockingUnlimited = BlockingUnlimited

instance Show BlockingUnlimited where
  showsPrec _ _ = showString "Unlimited"

instance Class.IsMessageBoxArg BlockingUnlimited where
  type MessageBox BlockingUnlimited = UnlimitedBox
  {-# INLINE newMessageBox #-}
  newMessageBox BlockingUnlimited = create
  getConfiguredMessageLimit _ = Nothing    

-- | A blocking instance that invokes 'receive'.
instance Class.IsMessageBox UnlimitedBox where
  type Input UnlimitedBox = UnlimitedBoxInput
  {-# INLINE receive #-}
  receive !i = Just <$> receive i
  {-# INLINE tryReceive #-}
  tryReceive !i = tryReceive i
  {-# INLINE newInput #-}
  newInput !i = newInput i

-- | A blocking instance that invokes 'deliver'.
instance Class.IsInput UnlimitedBoxInput where
  {-# INLINE deliver #-}
  deliver !o !m = deliver o m $> True


-- | Create a 'MessageBox'.
--
-- From a 'MessageBox' a corresponding 'Input' can
-- be made, that can be passed to some potential
-- communication partners.
{-# INLINE create #-}
create :: MonadUnliftIO m => m (UnlimitedBox a)
create = do
  (!inChan, !outChan) <- liftIO Unagi.newChan
  return $! MkUnlimitedBox inChan outChan

-- | Wait for and receive a message from a 'MessageBox'.
{-# INLINE receive #-}
receive :: MonadUnliftIO m => UnlimitedBox a -> m a
receive (MkUnlimitedBox _ !s) =
  --liftIO (Unagi.readChan IO.yield s)
  liftIO (Unagi.readChan s)

-- | Try to receive a message from a 'MessageBox',
-- return @Nothing@ if the queue is empty.
{-# INLINE tryReceive #-}
tryReceive :: MonadUnliftIO m => UnlimitedBox a -> m (Future a)
tryReceive (MkUnlimitedBox _ !s) = liftIO $ do
  (!promise, _) <- Unagi.tryReadChan s
  return (Future (Unagi.tryRead promise))

-- | Create an 'Input' to write the items
-- that the given 'MessageBox' receives.
{-# INLINE newInput #-}
newInput :: MonadUnliftIO m => UnlimitedBox a -> m (UnlimitedBoxInput a)
newInput (MkUnlimitedBox !s _) = return $! MkUnlimitedBoxInput s

-- | Put a message into the 'Input'
-- of a 'MessageBox', such that the process
-- reading the 'MessageBox' receives the message.
{-# INLINE deliver #-}
deliver :: MonadUnliftIO m => UnlimitedBoxInput a -> a -> m ()
deliver (MkUnlimitedBoxInput !s) !a =
  liftIO $ Unagi.writeChan s a
