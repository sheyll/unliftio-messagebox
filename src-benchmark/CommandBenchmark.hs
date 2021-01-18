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
import Criterion.Types
  ( Benchmark,
    bgroup,
  )
import qualified MediaBenchmark
import UnliftIO.MessageBox.CatchAll
  ( CatchAllFactory (CatchAllFactory),
  )
import UnliftIO.MessageBox.Class
  ( IsMessageBoxFactory (..),
  )
import qualified UnliftIO.MessageBox.Limited as L
import qualified UnliftIO.MessageBox.Unlimited as U

benchmark =
  bgroup
    "Command"
    ( foldMap
        go
        [ SomeBench MediaBenchmark.benchmark,
          SomeBench BookStoreBenchmark.benchmark
        ]
    )
  where
    go :: SomeBench -> [Benchmark]
    go (SomeBench b) =
      [ (\x -> bgroup (show x) [b x]) U.BlockingUnlimited,
        (\x -> bgroup (show x) [b x]) (CatchAllFactory U.BlockingUnlimited),
        (\x -> bgroup (show x) [b x]) (L.BlockingBoxLimit L.MessageLimit_256),
        (\x -> bgroup (show x) [b x]) (L.WaitingBoxLimit Nothing 5_000_000 L.MessageLimit_256),
        (\x -> bgroup (show x) [b x]) (L.WaitingBoxLimit (Just 60_000_000) 5_000_000 L.MessageLimit_256)
      ]

newtype SomeBench = SomeBench {_fromSomeBench :: forall cfg. (Show cfg, IsMessageBoxFactory cfg) => (cfg -> Benchmark)}