-- | This module provides thread-wide unique values
-- to identify calls.
--
-- This is based on CAS. TODO benchmark, and test uniqueness.
module Protocols.UniqueCallIds  (nextCallId, CallId(), withCallIdCounter) where 

import Data.Atomics 
import Control.Monad.Reader
import Lens.Micro

-- | A globally unique value to identify a 'Call' message.
newtype CallId = MkCallId { fromCallId :: Word64 }
  deriving (Eq, Ord)


class HasUniqueCallIds env where
  callIdCounterLens :: Lens' env CallIdCounter

withCallIdCounter 