-- | Functions to fork processes that share 
-- a counter for processIds and callIds.
module Protocols.Fork (fork, ProcessCounters(), HasProcessCounter) where


import Control.Monad.Reader (MonadReader, asks)
import UnliftIO (MonadUnliftIO, MonadIO (..))
import UnliftIO.Concurrent (forkIO)
import Control.Monad.Reader (local)
import Control.Monad (void)


fork :: (MonadReader App m, MonadUnliftIO m) => m () -> m ()
fork f = do 
  self <- fresh 
  zero <- newFreshCounter @"CallId"
  void $ forkIO $ local (\app -> app {callIdCounter = zero, self}) 