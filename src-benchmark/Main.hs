{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import qualified CommandBenchmark
import Control.Monad (replicateM)
import Criterion.Main (defaultMain)
import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import Data.Semigroup (Semigroup (stimes))
import Protocol.LimitedMessageBox (LimitedMessageBox (LimitedMessageBox))
import Protocol.MessageBoxClass (newOutBox2, IsInBoxConfig (..), deliver, receive)
import Protocol.UnlimitedMessageBox (UnlimitedMessageBox (UnlimitedMessageBox))
import RIO (MonadUnliftIO, conc, runConc)

main =
  defaultMain
    [ CommandBenchmark.benchmark,
      bgroup
        "unidirectionalMessagePassing"
        [ bench
            ( mboxImplTitle <> " "
                <> show noMessages
                <> " "
                <> show senderNo
                <> " >>= "
                <> show receiverNo
            )
            ( nfAppIO
                impl
                (senderNo, noMessages, receiverNo)
            )
          | noMessages <- [100_000],
            (mboxImplTitle, impl) <-
              [ let x = LimitedMessageBox 16 in (show x, unidirectionalMessagePassing mkTestMessage x),
                let x = UnlimitedMessageBox in (show x, unidirectionalMessagePassing mkTestMessage x),
                let x = LimitedMessageBox 4096 in (show x, unidirectionalMessagePassing mkTestMessage x)
              ],
            (senderNo, receiverNo) <-
              [ (1, 1000),
                (10, 100),
                (1, 1),
                --
                (1000, 1)
              ]
        ]
    ]

mkTestMessage :: Int -> TestMessage
mkTestMessage !i =
  MkTestMessage
    ( "The not so very very very very very very very very very very very very very very very very very very very very very very very very " ++ show i,
      "large",
      "meeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeessssssssssssssssssssssssssssssssss" ++ show i,
      ( "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
        even i,
        123423421111111111111111111123234 * toInteger i
      )
    )

newtype TestMessage = MkTestMessage (String, String, String, (String, String, Bool, Integer))
  deriving newtype (Show)

unidirectionalMessagePassing ::
  (MonadUnliftIO m, IsInBoxConfig cfg inbox) =>
  (Int -> TestMessage) ->
  cfg ->
  (Int, Int, Int) ->
  m ()
unidirectionalMessagePassing !msgGen !impl (!nP, !nM, !nC) = do
  (ccs, cs) <- consumers
  let ps = producers cs
  runConc (ps <> ccs)
  where
    producers !cs = stimes nP (conc producer)
      where
        producer =
          mapM_
            (uncurry (flip deliver))
            ((,) <$> (msgGen <$> [0 .. (nM `div` (nC * nP)) - 1]) <*> cs)
    consumers = do
      cis <- replicateM nC (newInBox impl)
      let ccs = foldMap (conc . consume (nM `div` nC)) cis
      cs <- traverse newOutBox2 cis
      return (ccs, cs)
      where
        consume 0 _inBox = return ()
        consume workLeft inBox = do
          !_msg <- receive inBox
          consume (workLeft - 1) inBox
