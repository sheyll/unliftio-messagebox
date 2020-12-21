{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module CommandBenchmark (benchmark) where

import qualified BookStoreBenchmark
import Control.Monad (replicateM)
import Criterion.Main (defaultMain)
import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import qualified Data.IntMap as Map
import Data.Map (Map)
import Data.Semigroup (Semigroup (stimes), getAll)
import Data.Set (Set)
import qualified MediaBenchmark
import Protocol.BoundedMessageBox (InBoxConfig (BoundedMessageBox))
import Protocol.Command as Command
import Protocol.Fresh
import Protocol.MessageBoxClass (IsMessageBox (..), deliver, receive)
import Protocol.UnboundedMessageBox (InBoxConfig (UnboundedMessageBox))
import RIO
import UnliftIO (MonadUnliftIO, conc, runConc)

benchmark =
  bgroup
    "Command"
    [ MediaBenchmark.benchmark,
      BookStoreBenchmark.benchmark
    ]
