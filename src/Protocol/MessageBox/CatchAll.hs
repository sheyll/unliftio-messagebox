{-# LANGUAGE Strict #-}

-- | Utilities for exception safe message boxes.
--
-- This provides a wrapper around "Protocol.MessageBox.Class" instances
-- to catch 'SomeException' in all methods like 'deliver' and 'receive'.
module Protocol.MessageBox.CatchAll
  ( CatchAllFactory (..),
    CatchAllBox (..),
    CatchAllInput (..),
  )
where

import Protocol.Future (Future (Future))
import Protocol.MessageBox.Class
  ( IsInput (..),
    IsMessageBox (..),
    IsMessageBoxFactory (..),
  )
import UnliftIO (SomeException, try)
import UnliftIO.Concurrent (threadDelay)

-- | A wrapper around values that are instances
-- of 'IsMessageBoxFactory'. The factory wraps
-- the result of the delegated 'newMessageBox'
-- invocation into a 'CatchAllBox'.
newtype CatchAllFactory cfg = CatchAllFactory cfg
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

instance IsMessageBoxFactory cfg => IsMessageBoxFactory (CatchAllFactory cfg) where
  type MessageBox (CatchAllFactory cfg) = CatchAllBox (MessageBox cfg)
  {-# INLINE newMessageBox #-}
  -- | Delegate to 'newMessageBox' of the underlying type.
  -- This call in not surrounded by 'try'. 
  newMessageBox (CatchAllFactory !cfg) = CatchAllBox <$> newMessageBox cfg

instance IsMessageBox box => IsMessageBox (CatchAllBox box) where
  type Input (CatchAllBox box) = CatchAllInput (Input box)
  {-# INLINE newInput #-}
  -- | Delegate and wrap the result.
  newInput (CatchAllBox !b) =
    CatchAllInput <$> newInput b
  {-# INLINE receive #-}
  -- | Delegate and wrap the result,
  -- catch all synchronous exceptions.
  receive (CatchAllBox !box) =
    try @_ @SomeException
      (receive box)
      >>= \case
        Left _e -> return Nothing
        Right r -> return r
  {-# INLINE receiveAfter #-}
  -- | Delegate and wrap the result,
  -- catch all synchronous exceptions.
  receiveAfter (CatchAllBox !box) !t !f =
    try @_ @SomeException
      (receiveAfter box t f)
      >>= \case
        Left _e -> return Nothing
        Right r -> return r
  {-# INLINE tryReceive #-}
  -- | Delegate and wrap the result,
  -- catch all synchronous exceptions.
  tryReceive (CatchAllBox !box) =
    try @_ @SomeException
      (tryReceive box)
      >>= \case
        Left _e ->
          return
            ( Future
                ( do
                    -- suspense TODO reduce/remove/yield?
                    threadDelay 10
                    -- the truth is: there is no spoon.
                    return Nothing
                )
            )
        Right r -> return r

instance (IsInput i) => IsInput (CatchAllInput i) where
  {-# INLINE deliver #-}
  -- | Delegate and wrap the result,
  -- catch all synchronous exceptions.
  deliver (CatchAllInput !i) !msg =
    try @_ @SomeException
      (deliver i msg)
      >>= \case
        Left _e -> return False
        Right r -> return r