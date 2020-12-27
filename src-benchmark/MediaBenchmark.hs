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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | More complex benchmarks, mimiking real world applications
-- from the media domain.
module MediaBenchmark (benchmark) where

import Control.Monad
import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import qualified Data.Map.Strict as Map
import Data.Semigroup (Semigroup (stimes))
import qualified Data.Set as Set
import Protocol.Command as Command
  ( CallId,
    Command,
    Message (..),
    ReturnType (FireAndForget, Return),
    call,
    replyTo,
  )
import Protocol.Fresh
  ( CounterVar,
    HasCounterVar (getCounterVar),
    fresh,
    newCounterVar,
  )
import Protocol.MessageBoxClass
  ( IsMessageBox (..),
    IsOutBox,
    handleMessage,
  )
import Protocol.UnboundedMessageBox (InBoxConfig (UnboundedMessageBox))
import qualified Protocol.UnboundedMessageBox as Unbounded
import RIO
  ( Conc,
    IORef,
    Map,
    RIO,
    Set,
    asks,
    conc,
    readIORef,
    runConc,
    runRIO,
    traverse_,
    writeIORef,
  )

benchmark =
  bgroup
    "Media"
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
    ]

-- | Run @nClients@ processes that each call 'FetchDsps' so many times
-- that globally @nFetchesTotal@ times 'FetchDsps' is called.
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

-- | A benchmark on a simulated application workload.
--
-- The applicaton has three layers:
--
-- 1. Media
--    * MediaApi processes
-- 2. Media Stream Grouping
--    * A process per media stream group
-- 3. Client
--    * A process that processes a benchmark specific set of
--      random requests for grouping media
--
-- Startup and shutdown:
--  The MediaApi will handle requests for ever.
--  The Client will processes a list of
mediaAppBenchmark ::
  Map DspId Capacity ->
  Map MixingGroupId (DspId, Set MediaStreamId) ->
  IO ()
mediaAppBenchmark !availableDsps !testMixingGroups = do
  (mediaOutBox, mediaConc) <- spawnMediaApi
  error "TODO"
  where
    spawnMediaApi =
          error "TODO"
     
-- * Types for the domain of the benchmarks in this module

data MediaApi

data MixingApi

type DspId = Int

type MixerId = Int

type MediaStreamId = Int

type MixingGroupId = Int

data instance Command MediaApi _ where
  FetchDsps :: Command MediaApi ( 'Return (Set DspId))
  CreateMixer :: DspId -> Command MediaApi ( 'Return (Maybe MixerId))
  DestroyMixer :: MixerId -> Command MediaApi 'FireAndForget
  AddToMixer :: MixerId -> MediaStreamId -> Command MediaApi ( 'Return Bool)
  RemoveFromMixer :: MediaStreamId -> Command MediaApi ( 'Return ())

deriving stock instance Show (Command MediaApi ( 'Return (Set DspId)))

deriving stock instance Show (Command MediaApi ( 'Return (Maybe MixerId)))

deriving stock instance Show (Command MediaApi ( 'Return Bool))

deriving stock instance Show (Command MediaApi ( 'Return ()))

deriving stock instance Show (Command MediaApi 'FireAndForget)

data MediaClientEvent where
  OnMediaStreamCreated :: MixingGroupId -> DspId -> MediaStreamId -> MediaClientEvent
  OnMediaStreamDestroyed :: MixingGroupId -> DspId -> MediaStreamId -> MediaClientEvent
  deriving stock (Show, Eq)

data MixingGroupEvent where
  MemberJoined :: MixingGroupId -> MediaStreamId -> MixingGroupEvent
  MemberUnJoined :: MixingGroupId -> MediaStreamId -> MixingGroupEvent
  deriving stock (Show, Eq)

data instance Command MixingApi _ where
  CreateMixingGroup ::
    MixingGroupId ->
    Command MixingApi ( 'Return ())
  DestroyMixingGroup ::
    MixingGroupId ->
    Command MixingApi ( 'Return ())
  Join ::
    IsOutBox outBox =>
    MixingGroupId ->
    MediaStreamId ->
    outBox MixingGroupEvent ->
    Command MixingApi 'FireAndForget
  UnJoin ::
    IsOutBox outBox =>
    MixingGroupId ->
    MediaStreamId ->
    outBox MixingGroupEvent ->
    Command MixingApi 'FireAndForget

type Capacity = Int

newtype MemberState = MemberState {isMemberJoined :: Bool}

-- | MixingApi dispatcher
dispatchMixingApi ::
  (IsOutBox outBox, HasCounterVar CallId env) =>
  DispatcherSt outBox ->
  MediaClientEvent ->
  RIO env (DispatcherSt outBox)
dispatchMixingApi st =
  \case
    OnMediaStreamCreated _ _ _ -> error "TODO"
    OnMediaStreamDestroyed _ _ _ -> error "TODO"


data DispatcherSt outBox = DispatcherSt
  { dMediaServer :: outBox MediaApi,
    mixingServers :: Map MixingGroupId (outBox MixingApi)
  }

-- | Mix media streams using 'MediaApi'

-- | Media server simulation
--
-- Handle 'MediaApi' requests and manage a map of
-- dsps and mixers.
handleMediaApi ::
  HasCounterVar CallId env =>
  MediaSimSt ->
  Message MediaApi ->
  RIO env MediaSimSt
handleMediaApi st =
  \case
    Blocking FetchDsps replyBox -> do
      let goodDsps = Set.filter (>= 2) (Map.keysSet (allDsps st))
      replyTo replyBox goodDsps
      return st
    Blocking (CreateMixer dspId) replyBox ->
      case Map.lookup dspId (allDsps st) of
        Just capacity | capacity > 0 -> do
          let theNewMixerId = nextMixerId st
          replyTo replyBox (Just theNewMixerId)
          return
            ( st
                { allDsps =
                    Map.insert
                      dspId
                      (capacity - 1)
                      (allDsps st),
                  allMixers =
                    Map.insert
                      theNewMixerId
                      (dspId, Set.empty)
                      (allMixers st),
                  nextMixerId = theNewMixerId + 1
                }
            )
        _ -> do
          replyTo replyBox Nothing
          return st
    Blocking (AddToMixer theMixer newMember) replyBox ->
      case Map.lookup theMixer (allMixers st) of
        Nothing ->
          replyTo replyBox False >> return st
        Just (dsp, streams) ->
          if Set.member newMember streams
            then replyTo replyBox True >> return st
            else case Map.lookup dsp (allDsps st) of
              Just capacity
                | capacity >= 1 ->
                  replyTo replyBox True
                    >> return
                      ( st
                          { allMixers =
                              Map.insert
                                theMixer
                                (dsp, Set.insert newMember streams)
                                (allMixers st),
                            allDsps =
                              Map.insert
                                dsp
                                (capacity - 1)
                                (allDsps st)
                          }
                      )
              _ ->
                replyTo replyBox False >> return st
    Blocking (RemoveFromMixer theMember) replyBox ->
      replyTo replyBox ()
        >> pure
        $ case Map.foldrWithKey'
          ( \theMixer (theDsp, theMembers) acc ->
              if Set.member theMember theMembers
                then Just (theMixer, (theDsp, theMembers))
                else acc
          )
          Nothing
          (allMixers st) of
          Nothing ->
            st
          Just (theMixer, (theDsp, theMembers)) ->
            st
              { allDsps =
                  Map.update
                    (Just . (+ 1))
                    theDsp
                    (allDsps st),
                allMixers =
                  Map.insert
                    theMixer
                    (theDsp, Set.delete theMember theMembers)
                    (allMixers st)
              }
    NonBlocking (DestroyMixer theMixerId) ->
      let foundMixer (mixersDsp, mediaStreams) =
            st
              { allMixers =
                  Map.delete theMixerId (allMixers st),
                allDsps =
                  Map.update
                    (Just . (+ (1 + Set.size mediaStreams)))
                    mixersDsp
                    (allDsps st)
              }
       in pure $
            maybe
              st
              foundMixer
              (Map.lookup theMixerId (allMixers st))

data MediaSimSt = MediaSimSt
  { allDsps :: Map DspId Capacity,
    nextMixerId :: MixerId,
    allMixers :: Map MixerId (DspId, Set MediaStreamId)
  }
