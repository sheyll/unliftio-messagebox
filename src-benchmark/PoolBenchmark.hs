module PoolBenchmark (benchmark) where

import Criterion.Types
  ( Benchmark,
    bgroup,
  )

benchmark :: Benchmark
benchmark =
  bgroup
    "Pool" []
