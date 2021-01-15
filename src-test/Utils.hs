{-# LANGUAGE BangPatterns #-}

module Utils (untilJust, untilM) where

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
