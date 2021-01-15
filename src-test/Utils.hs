{-# LANGUAGE BangPatterns #-}

module Utils (untilJust, untilM, withCallIds) where

import qualified Protocol.Command.CallId as CallId
import Protocol.Fresh (CounterVar)
import RIO (MonadIO, RIO, runRIO)

untilJust :: (Monad m) => m (Maybe a) -> m a
untilJust loopBody = do
  !res <- loopBody
  case res of
    Nothing ->
      untilJust loopBody
    Just r ->
      return r

untilM :: Monad m => m Bool -> m ()
untilM loopBody = do
  !isOk <- loopBody
  if isOk
    then return ()
    else untilM loopBody

withCallIds :: 
  MonadIO m => RIO (CounterVar CallId.CallId) b -> m b
withCallIds f = 
  CallId.newCallIdCounter >>= flip runRIO f