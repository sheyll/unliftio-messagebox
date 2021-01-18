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
import Control.Monad (replicateM, unless)
import Criterion.Main (defaultMain)
import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import Data.Semigroup (Semigroup (stimes))
import UnliftIO.MessageBox.CatchAll
  ( CatchAllFactory (..),
  )
import UnliftIO.MessageBox.Class
  ( IsInput (..),
    IsMessageBox (..),
    IsMessageBoxFactory (..),
    deliver,
    newInput,
    receive,
  )
import qualified UnliftIO.MessageBox.Limited as L
import qualified UnliftIO.MessageBox.Unlimited as U
import UnliftIO (MonadUnliftIO, conc, runConc)

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
                <> " : "
                <> show receiverNo
            )
            ( nfAppIO
                impl
                (senderNo, noMessages, receiverNo)
            )
          | noMessages <- [100_000],
            (isNonBlocking, mboxImplTitle, impl) <-
              [ let x = U.UnlimitedMessageBox
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = L.BlockingBoxLimit L.MessageLimit_1
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = L.BlockingBoxLimit L.MessageLimit_16
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = L.BlockingBoxLimit L.MessageLimit_32
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = L.BlockingBoxLimit L.MessageLimit_64
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = L.BlockingBoxLimit L.MessageLimit_128
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = L.BlockingBoxLimit L.MessageLimit_256
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = L.BlockingBoxLimit L.MessageLimit_512
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = L.BlockingBoxLimit L.MessageLimit_4096
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = L.NonBlockingBoxLimit L.MessageLimit_128
                 in (True, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = L.WaitingBoxLimit Nothing 5_000_000 L.MessageLimit_128
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = L.WaitingBoxLimit (Just 60_000_000) 5_000_000 L.MessageLimit_128
                 in (True, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = CatchAllFactory U.UnlimitedMessageBox
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x),
                let x = CatchAllFactory (L.BlockingBoxLimit L.MessageLimit_128)
                 in (False, show x, unidirectionalMessagePassing mkTestMessage x)
              ],
            (senderNo, receiverNo) <-
              [ (1, 1000),
                (10, 100),
                (1, 1),
                (1000, 1)
              ],
            not isNonBlocking || senderNo == 1 && receiverNo > 1
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
  (MonadUnliftIO m, IsMessageBoxFactory cfg) =>
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
            ( \(!msg, !cons) -> do
                !ok <- deliver cons msg
                unless ok (error ("producer failed to deliver: " <> show msg))
            )
            ((,) <$> (msgGen <$> [0 .. (nM `div` (nC * nP)) - 1]) <*> cs)
    consumers = do
      cis <- replicateM nC (newMessageBox impl)
      let ccs = foldMap (conc . consume (nM `div` nC)) cis
      cs <- traverse newInput cis
      return (ccs, cs)
      where
        consume 0 _inBox = return ()
        consume workLeft inBox = do
          receive inBox
            >>= maybe
              (error "consumer failed to receive")
              (const (consume (workLeft - 1) inBox))
