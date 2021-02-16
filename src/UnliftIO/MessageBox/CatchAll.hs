{-# LANGUAGE Strict #-}

-- | Utilities for exception safe message boxes.
--
-- This provides a wrapper around "UnliftIO.MessageBox.Class" instances
-- to catch 'SomeException' in all methods like 'deliver' and 'receive'.
module UnliftIO.MessageBox.CatchAll
  ( CatchAllArg (..),
    CatchAllBox (..),
    CatchAllInput (..),
  )
where

import UnliftIO.MessageBox.Util.Future (Future (Future))
import UnliftIO.MessageBox.Class
  ( IsInput (..),
    IsMessageBox (..),
    IsMessageBoxArg (..),
  )
import UnliftIO (SomeException, liftIO, try)
import UnliftIO.Concurrent (threadDelay)

-- | A wrapper around values that are instances
-- of 'IsMessageBoxArg'. The factory wraps
-- the result of the delegated 'newMessageBox'
-- invocation into a 'CatchAllBox'.
newtype CatchAllArg cfg = CatchAllArg cfg
  deriving stock (Eq, Ord, Show)

-- | A wrapper around values that are instances
-- of 'IsMessageBox'.
--
-- The 'Input' type will be wrapped using
-- 'CatchAllInput'.
newtype CatchAllBox box a = CatchAllBox (box a)

-- | A wrapper around values that are instances
-- of 'IsInput'.
newtype CatchAllInput i a = CatchAllInput (i a)

instance IsMessageBoxArg cfg => IsMessageBoxArg (CatchAllArg cfg) where
  type MessageBox (CatchAllArg cfg) = CatchAllBox (MessageBox cfg)
  {-# INLINE newMessageBox #-}
  newMessageBox (CatchAllArg !cfg) = CatchAllBox <$> newMessageBox cfg
  getConfiguredMessageLimit (CatchAllArg !cfg) =
    getConfiguredMessageLimit cfg

instance IsMessageBox box => IsMessageBox (CatchAllBox box) where
  type Input (CatchAllBox box) = CatchAllInput (Input box)
  {-# INLINE newInput #-}
  newInput (CatchAllBox !b) =
    CatchAllInput <$> newInput b
  {-# INLINE receive #-}
  receive (CatchAllBox !box) =
    try @_ @SomeException
      (receive box)
      >>= \case
        Left _e -> liftIO (print _e) >> return Nothing
        Right r -> return r
  {-# INLINE receiveAfter #-}
  -- | Call the wrapped 'receiveAfter' and catch all sync exceptions.
  -- 
  -- When an exception is caught return 'Nothing'.
  receiveAfter (CatchAllBox !box) !t =
    try @_ @SomeException
      (receiveAfter box t)
      >>= \case
        Left _e -> liftIO (print _e) >> pure Nothing
        Right r -> return r
  {-# INLINE tryReceive #-}
  tryReceive (CatchAllBox !box) =
    try @_ @SomeException
      (tryReceive box)
      >>= \case
        Left _e ->
          liftIO (print _e)
            >> return
              ( Future
                  ( do
                      -- suspense...
                      threadDelay 1000
                      -- ... anyway, the truth is: there is no spoon.
                      return Nothing
                  )
              )
        Right r -> return r

instance (IsInput i) => IsInput (CatchAllInput i) where
  {-# INLINE deliver #-}
  deliver (CatchAllInput !i) !msg =
    try @_ @SomeException
      (deliver i msg)
      >>= \case
        Left _e -> liftIO (print _e) >> return False
        Right r -> return r