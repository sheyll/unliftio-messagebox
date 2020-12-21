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

-- | Basic benchmark for the Command api.
-- In this benchmark n producers `Donate` books to the book store,
-- which are processed by m consumers
module BookStoreBenchmark (benchmark) where

import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import Data.Semigroup (Semigroup (stimes))
import Protocol.BoundedMessageBox (InBoxConfig (BoundedMessageBox))
import Protocol.Command as Command
  ( CallId,
    Command,
    Message (NonBlocking),
    ReturnType (FireAndForget, Return),
    cast,
    handleMessage,
  )
import Protocol.Fresh
  ( CounterVar,
    HasCounterVar (..),
    newCounterVar,
  )
import Protocol.MessageBoxClass (IsMessageBox (..))
import Protocol.UnboundedMessageBox (InBoxConfig (UnboundedMessageBox))
import RIO
  ( MonadIO (liftIO),
    MonadUnliftIO,
    conc,
    runConc,
    runRIO,
    unless,
  )

mkExampleBook :: Int -> Book
mkExampleBook !i =
  MkBook
    ( "The not so very very very very very very very very very very very very very very very very very very very very very very very very " ++ show i,
      "large",
      "meeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeessssssssssssssssssssssssssssssssss" ++ show i,
      ( "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
        even i,
        123423421111111111111111111123234 * toInteger i
      )
    )

newtype Book = MkBook ([Char], [Char], [Char], ([Char], [Char], Bool, Integer))
  deriving stock (Show, Eq, Ord)

data BookStore

data instance Command BookStore _ where
  Donate :: Book -> Command BookStore 'FireAndForget
  GetBooks :: Command BookStore ( 'Return [Book])

deriving stock instance Eq (Command BookStore 'FireAndForget)

deriving stock instance Show (Command BookStore 'FireAndForget)

deriving stock instance Eq (Command BookStore ( 'Return [Book]))

deriving stock instance Show (Command BookStore ( 'Return [Book]))

newtype BookStoreEnv = MkBookStoreEnv
  {_fresh :: CounterVar CallId}

instance HasCounterVar CallId BookStoreEnv where
  getCounterVar MkBookStoreEnv {_fresh} = _fresh
  putCounterVar newFresh MkBookStoreEnv {_fresh} = MkBookStoreEnv {_fresh = newFresh}

unidirectionalMessagePassing ::
  (MonadUnliftIO m, IsMessageBox inbox outbox) =>
  (Int -> Book) ->
  InBoxConfig inbox ->
  (Int, Int, Int) ->
  m ()
unidirectionalMessagePassing !msgGen !impl (!nP, !nMTotal, !nC) = do
  freshCounter <- newCounterVar
  runRIO (MkBookStoreEnv {_fresh = freshCounter}) $ do
    bookStoreInBox <- newInBox impl
    bookStoreOutBox <- newOutBox bookStoreInBox
    let producer 0 = pure ()
        producer workLeft = do
          ok <- cast bookStoreOutBox (Donate $ msgGen 1)
          unless ok (error "cast failed!")
          producer (workLeft - 1)
    let consumer 0 = pure ()
        consumer workLeft =
          let handler =
                handleMessage bookStoreInBox $
                  \case
                    NonBlocking _actual -> do
                      return Nothing
                    a -> do
                      liftIO $ putStrLn "blocking case called"
                      pure (Just ("did not expect message: " <> show a))
           in handler
                >>= maybe
                  (error "HandleMessage failed!")
                  ( maybe
                      (consumer (workLeft - 1))
                      error
                  )
    let perProducerWork = nMTotal `div` nP -- books to donate per producer
        perConsumerWork = nMTotal `div` nC -- books to receive per consumer
    let consumers = stimes nC (conc $ consumer perConsumerWork)
    let producers = stimes nP (conc $ producer perProducerWork)
    runConc (producers <> consumers)

benchmark =
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
          [ let x = BoundedMessageBox 16 in (show x, unidirectionalMessagePassing mkExampleBook x),
            let x = UnboundedMessageBox in (show x, unidirectionalMessagePassing mkExampleBook x),
            let x = BoundedMessageBox 4096 in (show x, unidirectionalMessagePassing mkExampleBook x)
          ],
        (senderNo, receiverNo) <-
          [ (1, 1000),
            (10, 100),
            (1, 1),
            (1000, 1)
          ]
    ]
