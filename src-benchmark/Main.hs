{-# LANGUAGE StandaloneDeriving #-}
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
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Monad (replicateM)
import Criterion.Main (defaultMain)
import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import qualified Data.Map.Strict as Map
import Data.Semigroup (Semigroup (stimes))
import Data.Set (Set)
import qualified Data.Set as Set
import Protocol.BoundedMessageBox (InBoxConfig (BoundedMessageBox))
import qualified Protocol.BoundedMessageBox as Bounded
import Protocol.Command as Command
import Protocol.Fresh
import Protocol.MessageBoxClass (IsMessageBox (..), deliver, receive)
import Protocol.UnboundedMessageBox (InBoxConfig (UnboundedMessageBox))
import qualified Protocol.UnboundedMessageBox as Unbounded
import RIO
import UnliftIO (MonadUnliftIO, conc, runConc)

main =
  defaultMain
    [ bgroup
        "command"
        [ bench
            ( "fetchDsps: "
                ++ show nF
                ++ " total fetches "
                ++ show nC
                ++ " clients"
            )
            (nfAppIO fetchDspsBench cfg)
          | cfg@(nF, nC) <-
              [(100000, 1), (100000, 1000)]
        ],
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
              [ let x = BoundedMessageBox 16 in (show x, unidirectionalMessagePassing mkTestMessage x),
                let x = UnboundedMessageBox in (show x, unidirectionalMessagePassing mkTestMessage x),
                let x = BoundedMessageBox 4096 in (show x, unidirectionalMessagePassing mkTestMessage x)
              ],
            (senderNo, receiverNo) <-
              [ (1, 1000),
                (10, 100),
                (1, 1),
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
  (MonadUnliftIO m, IsMessageBox inbox outbox) =>
  (Int -> TestMessage) ->
  InBoxConfig inbox ->
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
      cs <- traverse newOutBox cis
      return (ccs, cs)
      where
        consume 0 _inBox = return ()
        consume workLeft inBox = do
          !_msg <- receive inBox
          consume (workLeft - 1) inBox

--------------------------------------------------------------------------------
-- Command Benchmarks

fetchDspsBench :: (Int, Int) -> IO ()
fetchDspsBench (nFetchesTotal, nClients) =
  let startClients serverOut =
        stimes nClients (conc (client perClientWork))
        where
          client 0 = return ()
          client !workLeft =
            call serverOut FetchDsps 5_000_000
              >>= either
                (error . show)
                (const (client (workLeft - 1)))

      startServer ::
        RIO
          (CounterVar CallId)
          ( Unbounded.OutBox (Message MediaApi),
            Conc (RIO (CounterVar CallId)) ()
          )
      startServer = do
        serverIn <- newInBox UnboundedMessageBox
        serverOut <- newOutBox serverIn
        let dspSet = Set.fromList [0 .. 4096]
        return (serverOut, conc (server serverWork serverIn dspSet))
        where
          server ::
            Int ->
            Unbounded.InBox (Message MediaApi) ->
            Set DspId ->
            RIO (CounterVar CallId) ()
          server 0 _ _ = return ()
          server !workLeft !serverIn !dspSet =
            handleMessage
              serverIn
              ( \case
                  Blocking FetchDsps replyBox -> do
                    replyTo replyBox dspSet
                    server (workLeft - 1) serverIn dspSet
                  Blocking other _ ->
                    error ("unexpected command: " ++ show other)

                  NonBlocking other ->
                    error ("unexpected command: " ++ show other)
              )
             >>= maybe (error "handleMessage failed") return

      perClientWork = nFetchesTotal `div` nClients
      serverWork = perClientWork * nClients -- not the same as nFetchesTotal
   in do
        callIdCounter <- newCounterVar
        runRIO callIdCounter $ do
          (serverOut, serverConc) <- startServer
          runConc (serverConc <> startClients serverOut)

-- internals

data MediaApi

data MediaClientApi

data MixingTreeApi

type DspId = Int

type MixerId = Int

type MediaStreamId = Int

type MixingGroupId = Int

data instance Command MediaApi _ where
  FetchDsps :: Command MediaApi ( 'Return (Set DspId))
  CreateMixer :: DspId -> Command MediaApi ( 'Return (Maybe MixerId))
  DestroyMixer :: MixerId -> Command MediaApi 'FireAndForget
  AddToMixer :: DspId -> MixerId -> MediaStreamId -> Command MediaApi ( 'Return Bool)
  RemoveFromMixer :: DspId -> MixerId -> MediaStreamId -> Command MediaApi ( 'Return ())

deriving instance Show (Command MediaApi ( 'Return (Set DspId)))
deriving instance Show (Command MediaApi ( 'Return (Maybe MixerId)))
deriving instance Show (Command MediaApi ( 'Return Bool))
deriving instance Show (Command MediaApi ( 'Return ()))
deriving instance Show (Command MediaApi 'FireAndForget)

data instance Command MediaClientApi _ where
  OnCallConnected :: MixingGroupId -> DspId -> MediaStreamId -> Command MediaClientApi 'FireAndForget
  OnCallDisconnected :: MixingGroupId -> DspId -> MediaStreamId -> Command MediaClientApi 'FireAndForget
  MemberJoined :: MixingGroupId -> MediaStreamId -> Command MediaClientApi 'FireAndForget
  MemberUnJoined :: MixingGroupId -> MediaStreamId -> Command MediaClientApi 'FireAndForget

data instance Command MixingTreeApi _ where
  CreateMixingGroup :: MixingGroupId -> Command MixingTreeApi ( 'Return ())
  DestroyMixingGroup :: MixingGroupId -> Command MixingTreeApi ( 'Return ())
  Join :: MixingGroupId -> MediaStreamId -> Command MixingTreeApi 'FireAndForget
  UnJoin :: MixingGroupId -> MediaStreamId -> Command MixingTreeApi 'FireAndForget

type Capacity = Int

newtype MemberState = MemberState {isMemberJoined :: Bool}

data MediaEnv = MediaEnv
  { dsps :: IORef (Map DspId Capacity),
    mixerIdCounter :: CounterVar MixerId
  }

instance HasCounterVar MixerId MediaEnv where
  getCounterVar = mixerIdCounter

-- Run a MediaApi message handler, that exists when no message was received for 
-- more than one second.
mediaSim = error "TODO"

-- Core of the media simulation: Handle MediaApi requests and manage a map of dsps.
mediaSimHandleMessage :: Message MediaApi -> RIO MediaEnv ()
mediaSimHandleMessage =
  \case
    Blocking FetchDsps replyBox -> do
      dspsVar <- asks dsps
      allDsps <- readIORef dspsVar
      let goodDsps = Set.filter (>= 2) (Map.keysSet allDsps)
      replyTo replyBox goodDsps
    Blocking (CreateMixer dspId) replyBox -> do
      dspsVar <- asks dsps
      allDsps <- readIORef dspsVar
      let capacities = Map.lookup dspId allDsps
      r <- case capacities of
        Just capacity | capacity > 0 -> do
          writeIORef
            dspsVar
            (Map.insert dspId (capacity - 1) allDsps)
          Just <$> fresh
        _ ->
          pure Nothing
      replyTo replyBox r
    Blocking AddToMixer {} replyBox -> do
      replyTo replyBox (error "TODO")
    Blocking RemoveFromMixer {} replyBox -> do
      replyTo replyBox (error "TODO")
    NonBlocking (DestroyMixer dspId) -> do
      dspsVar <- asks dsps
      allDsps <- readIORef dspsVar
      let capacities = Map.lookup dspId allDsps
      traverse_
        ( \capacity ->
            writeIORef
              dspsVar
              (Map.insert dspId (max 0 (capacity - 1)) allDsps)
        )
        capacities
