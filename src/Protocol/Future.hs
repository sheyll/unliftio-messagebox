module Protocol.Future
  ( Future (Future),
    tryNow,
    awaitFuture,
  )
where

import UnliftIO (MonadIO (liftIO), MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)

-- | A wrapper around an IO action that returns value
-- in the future.
newtype Future a = Future
  { -- | Return 'Just' the value or 'Nothing',
    --   when the value is not available yet.
    fromFuture :: IO (Maybe a) -- TODO add a blockUntilAvailable for efficient timeout reading
  }

-- | Return 'Just' the value or 'Nothing',
--   when the value is not available yet.
--
--   Once the value is available, that value
--   will be returned everytime this function is
--   invoked.
-- TODO: test for dead lock exceptions
{-# INLINE tryNow #-}
tryNow :: MonadUnliftIO m => Future a -> m (Maybe a)
tryNow = liftIO . fromFuture

-- | Poll a Future until the value is present.
-- TODO: test for dead lock exceptions
awaitFuture :: MonadUnliftIO m => Future b -> m b
awaitFuture !f =
  tryNow f >>= maybe (threadDelay 10 >> awaitFuture f) return