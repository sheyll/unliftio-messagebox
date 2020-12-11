-- | Functions to address processes/threads, without holding
-- all that memory:
--
-- Taken from GHC.Conc.Sync from base-4.14.0.0:
--
-- > /Note/: in GHC, if you have a 'ThreadId', you essentially have
-- > a pointer to the thread itself.  This means the thread itself can\'t be
-- > garbage collected until you drop the 'ThreadId'.
-- > This misfeature will hopefully be corrected at a later date.
--
-- I hope that the thread can become garbage collected even though
-- we hold the weak reference to it.
module Protocol.WeakThreadId
  ( WeakThreadId (..),
    mkWeakThreadId,
    myWeakThreadId,
    withThreadId,
    deRefWeakThreadId,
  )
where

import Data.Hashable (Hashable (hash))
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import UnliftIO (MonadIO (..), MonadUnliftIO)
import UnliftIO.Concurrent (ThreadId, myThreadId)

-- | Create a 'Weak' reference to a 'ThreadId'.
mkWeakThreadId :: MonadIO m => ThreadId -> m WeakThreadId
mkWeakThreadId !t = do
  !w <- liftIO $ mkWeakPtr t Nothing
  return
    ( MkPid
        ( case show t of
            'T' : 'h' : 'r' : 'e' : 'a' : 'd' : 'I' : 'd' : ' ' : num -> read num
            _ -> hash t,
          w
        )
    )

-- | Create a 'WeakThreadId' from 'myThreadId'.
-- Convencience function e.g. to avoid unnessary modules imports
-- for 'myThreadId'.
myWeakThreadId :: MonadIO m => m WeakThreadId
myWeakThreadId = myThreadId >>= mkWeakThreadId

-- | Unpack the 'Weak' reference to a 'ThreadId' and apply
-- a give function to it.
--
-- If the reference was GCed return 'Nothing'.
deRefWeakThreadId :: MonadIO m => WeakThreadId -> m (Maybe ThreadId)
deRefWeakThreadId (MkPid (_, r)) = liftIO (deRefWeak r)

-- | Unpack the 'Weak' reference to a 'ThreadId' and apply
-- a give function to it.
--
-- If the reference was GCed  the function is applied to
-- 'Nothing'.
withThreadId :: MonadUnliftIO m => WeakThreadId -> (Maybe ThreadId -> m a) -> m a
withThreadId w k = deRefWeakThreadId w >>= k

-- | A unique number to identify processes e.g. in a system log.
-- 'WeakThreadId' values can be converted to 'Weak' 'ThreadId'.
newtype WeakThreadId = MkPid (Int, Weak ThreadId)


instance Show WeakThreadId where
  showsPrec _ (MkPid (!x, _)) = shows x

instance Eq WeakThreadId where
  (==) (MkPid (!a, _)) (MkPid (!b, _)) = a == b

instance Ord WeakThreadId where
  compare (MkPid (!a, _)) (MkPid (!b, _)) = compare a b
