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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | More complex benchmarks, mimiking real world applications
-- from the media domain.
module MediaBenchmark (benchmark) where

import Control.Monad (forM, forM_, replicateM_, unless, void)
import Criterion.Types
  ( Benchmark,
    bench,
    bgroup,
    nfAppIO,
  )
import qualified Data.Map.Strict as Map
import Data.Semigroup (Semigroup (stimes))
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Protocol.Command as Command
  ( Command,
    Message (..),
    ReturnType (FireAndForget, Return),
    call,
    cast,
    delegateCall,
    replyTo,
  )
import Protocol.Command.CallId as CallId
import Protocol.Fresh
  ( CounterVar,
    HasCounterVar (getCounterVar),
    newCounterVar,
  )
import Protocol.MessageBox.Class
  ( IsInput (..),
    IsMessageBox (..),
    IsMessageBoxFactory (..),
    handleMessage,
    newInput,
    receiveAfter,
  )
import RIO
  (fix,  Map,
    RIO,
    Set,
    asks,
    runRIO,
  )
import UnliftIO
  ( Conc,
    MonadIO (liftIO),
    SomeException,
    conc,
    runConc,
    try,
  )
import UnliftIO.Concurrent (forkIO)

benchmark :: (IsMessageBoxFactory cfg) => cfg -> Benchmark
benchmark cfg =
  bgroup
    "Media"
    $ [ bench ("Mixing/" ++ show p) (nfAppIO (mediaAppBenchmark cfg) p)
        | p <-
            [ Param 100 500 4,
              Param 100 1000 4
            ]
      ]
      ++ [ bench
             ( "fetchDsps/"
                 ++ show nF
                 ++ "fetches/"
                 ++ show nC
                 ++ "clients"
             )
             (nfAppIO (fetchDspsBench cfg) (nF, nC))
           | (nF, nC) <-
               [(100000, 1), (100000, 1000)]
         ]

-- | Run @nClients@ processes that each call 'FetchDsps' so many times
-- that globally @nFetchesTotal@ times 'FetchDsps' is called.
fetchDspsBench ::
  forall cfg.
  (IsMessageBoxFactory cfg) =>
  cfg ->
  (Int, Int) ->
  IO ()
fetchDspsBench cfg (nFetchesTotal, nClients) =
  let startClients serverOut =
        stimes nClients (conc (client perClientWork))
        where
          client 0 = return ()
          client !workLeft =
            call serverOut FetchDsps 500_000
              >>= either
                (error . show)
                (const (client (workLeft - 1)))

      startServer ::
        RIO
          (CounterVar CallId)
          ( Input (MessageBox cfg) (Message MediaApi),
            Conc (RIO (CounterVar CallId)) ()
          )
      startServer = do
        serverIn <- newMessageBox cfg
        serverOut <- newInput serverIn
        let dspSet = Set.fromList [0 .. 4096]
        return (serverOut, conc (server serverWork serverIn dspSet))
        where
          server ::
            Int ->
            MessageBox cfg (Message MediaApi) ->
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

-- | A benchmark of a fictional media mixing application.
--
-- The applicaton has these layers:
--
-- 1. Media resources (DSPs) for mixing media streams (aka members)
--    * One MediaApi process with @i@ Dsps
--    * Exits after a stop message is received
--      and all media resources are removed.
--
-- 2. Logical Mixing Groups
--    * one process per group
--    * use the media resources to mix groups
--      spanning several DSPs
--    * create bridges between low-level mixers
--
-- 3. Mixing Group Broker
--    * Starts/Stops the processes for the
--      logical mixing groups.
--    * Delegates the MixingApi to the
--      logical mixing group processes
--    * When all groups are destroyed:
--       - Send the media resources process a stop message
--       - exit
--
-- 4. Mixing Group Applications (aka clients)
--    * uses the Mixing Group Broker
--    * corresponds to a single exclusive mixing group
--    * Follows this procedure:
--      - create the mixing group
--      - add members
--      - remove members
--      - destroy the group
--      - exit
--
-- This whole process is repeated @m@ times.
--
-- When all clients have finished, send a shutdown message to the media process,
-- and wait for every process to finish.
mediaAppBenchmark ::
  forall cfg.
  (HasCallStack, IsMessageBoxFactory cfg) =>
  cfg ->
  Param ->
  IO ()
mediaAppBenchmark cfg param = do
  (mixingOut, c1) <- do
    (mediaInput, mediaConc) <- spawnMediaApi
    (mixingOut, mixingConc) <- spawnMixingBroker mediaInput
    return (mixingOut, mediaConc <> mixingConc)
  appCounters <- AppCounters <$> newCounterVar <*> newCounterVar
  let clients = spawnMixingApps mixingOut
  runRIO appCounters (runConc (c1 <> clients))
  where
    spawnMediaApi = do
      mediaOutput <- newMessageBox cfg
      mediaInput <- newInput mediaOutput
      let startMediaServer =
            void $ mediaServerLoop (mkMediaSimSt (toDspConfig param))
          mediaServerLoop st = do
            let !isFinished =
                  shuttingDown st && Map.null (allMixers st)
            unless isFinished $
              try
                (handleMessage mediaOutput (handleMediaApi st))
                >>= either
                  ( liftIO
                      . putStrLn
                      . ("media server failed to receive next message: " ++)
                      . (show :: SomeException -> String)
                  )
                  (maybe (error "media server loop premature exit") mediaServerLoop)
      return (mediaInput, conc startMediaServer)

    spawnMixingBroker ::
      Input (MessageBox cfg) (Message MediaApi) ->
      IO (Input (MessageBox cfg) (Message MixingApi), Conc (RIO AppCounters) ())
    spawnMixingBroker mediaBoxOut = do
      mixingOutput <- newMessageBox cfg
      mixingInput <- newInput mixingOutput
      let startMixingServer =
            let groupMap :: Map MixingGroupId (Input (MessageBox cfg) (Message MixingApi))
                groupMap = Map.empty
             in mixingServerLoop (0, groupMap)
          mixingServerLoop !groupMap =
            try
              ( handleMessage
                  mixingOutput
                  (dispatchMixingApi groupMap)
              )
              >>= either
                ( liftIO
                    . putStrLn
                    . ("mixingServerLoop failed: " ++)
                    . (show :: SomeException -> String)
                )
                ( maybe
                    (error "mixing server loop premature exit")
                    ( \(!nDestroyed', !groupMap') ->
                        if Map.null groupMap' && nDestroyed' == nGroups param
                          then void $ cast mediaBoxOut MediaShutdown
                          else mixingServerLoop (nDestroyed', groupMap')
                    )
                )
          dispatchMixingApi ::
            (Int, Map MixingGroupId (Input (MessageBox cfg) (Message MixingApi))) ->
            Message MixingApi ->
            RIO AppCounters (Int, Map MixingGroupId (Input (MessageBox cfg) (Message MixingApi)))
          dispatchMixingApi (!nDestroyed, !st) =
            \case
              Blocking cm@(CreateMixingGroup !mgId) !r -> do
                unless
                  (Map.notMember mgId st)
                  (error ("Mixing group ID conflict: " ++ show mgId))
                !mgInput <- spawnMixingGroup mediaBoxOut
                !ok <- delegateCall mgInput cm r
                unless ok (error ("delegation failed: " ++ show cm))
                return (nDestroyed, Map.insert mgId mgInput st)
              Blocking (DestroyMixingGroup !mgId) !r ->
                case Map.lookup mgId st of
                  Nothing ->
                    error ("DestroyMixingGroup: Mixing group doesn't exist: " ++ show mgId)
                  Just !mgInput -> do
                    !ok <- delegateCall mgInput (DestroyMixingGroup mgId) r
                    unless ok (error ("delegation failed: " ++ show (DestroyMixingGroup mgId)))
                    return (nDestroyed + 1, Map.delete mgId st)
              NonBlocking m@(Join !mgId _ _) ->
                case Map.lookup mgId st of
                  Nothing ->
                    error ("Mixing group doesn't exist: " ++ show mgId ++ " in: " ++ show m)
                  Just !mgInput -> do
                    !ok <- cast mgInput m
                    unless ok (error ("delegation failed: " ++ show m))
                    return (nDestroyed, st)
              NonBlocking m@(UnJoin !mgId _ _) ->
                case Map.lookup mgId st of
                  Nothing ->
                    error ("Mixing group doesn't exist: " ++ show mgId ++ " in: " ++ show m)
                  Just !mgInput -> do
                    !ok <- cast mgInput m
                    unless ok (error ("delegation failed: " ++ show m))
                    return (nDestroyed, st)
      return (mixingInput, conc startMixingServer)
    spawnMixingGroup ::
      Input (MessageBox cfg) (Message MediaApi) ->
      RIO AppCounters (Input (MessageBox cfg) (Message MixingApi))
    spawnMixingGroup !mediaInput = do
      !mgOutput <- newMessageBox cfg
      !mgInput <- newInput mgOutput
      let mgLoop (!mgId, !groupMap) =
            try
              ( handleMessage
                  mgOutput
                  (handleCmd (mgId, groupMap))
              )
              >>= either
                ( liftIO
                    . putStrLn
                    . (("mixingGroup " ++ show mgId ++ " exception: ") ++)
                    . (show :: SomeException -> String)
                )
                ( maybe
                    (error ("mixingGroup loop " ++ show mgId ++ " premature exit"))
                    (maybe (return ()) mgLoop)
                )
          handleCmd ::
            (MixingGroupId, Map DspId (MixerId, Set MemberId)) ->
            Message MixingApi ->
            RIO AppCounters (Maybe (MixingGroupId, Map DspId (MixerId, Set MemberId)))
          handleCmd (!mgId, !st) =
            \case
              Blocking (CreateMixingGroup !mgId') !r -> do
                replyTo r ()
                return (Just (mgId', st))
              Blocking (DestroyMixingGroup _) !r -> do
                replyTo r ()
                return Nothing -- exit
              NonBlocking (Join !_mgId !memberId !callBack) ->
                call mediaInput FetchDsps 50_000
                  >>= either
                    ( \ !mErr -> do
                        void $ deliver callBack (MemberUnJoined mgId memberId)
                        error (show mErr)
                    )
                    ( \ !dsps ->
                        let selectedDspId =
                              let !ks = Set.toList dsps
                                  !nDsps = length ks
                                  !ki = memberId `rem` nDsps
                               in ks !! ki
                            doAdd !theMixerId !theMembers =
                              let !m = AddToMixer theMixerId memberId
                               in call mediaInput m 200_000
                                    >>= \case
                                      Left !err ->
                                        error (show m ++ " failed: " ++ show err)
                                      Right False -> do
                                        void $ deliver callBack (MemberUnJoined mgId memberId)
                                        error (show m ++ " did not succeed")
                                      Right True -> do
                                        let st' =
                                              Map.insert
                                                selectedDspId
                                                (theMixerId, Set.insert memberId theMembers)
                                                st
                                        void $ deliver callBack (MemberJoined mgId memberId)
                                        return (Just (mgId, st'))
                         in if Set.null dsps
                              then error "Not enough DSP capacity"
                              else case Map.lookup selectedDspId st of
                                Nothing -> do
                                  call mediaInput (CreateMixer selectedDspId) 500_000
                                    >>= \case
                                      Left !err ->
                                        error (show err)
                                      Right Nothing ->
                                        error ("create mixer failed on: " ++ show selectedDspId)
                                      Right (Just !theMixerId) ->
                                        doAdd theMixerId Set.empty
                                Just (!mixerId, !members) -> doAdd mixerId members
                    )
              NonBlocking (UnJoin _ !memberId !callBack) ->
                case Map.toList (Map.filter (Set.member memberId . snd) st) of
                  ((!theDspId, (!theMixerId, !theMembers)) : _) -> do
                    call mediaInput (RemoveFromMixer theMixerId memberId) 500_000
                      >>= either (error . show) (const (return ()))
                    let theMembers' = Set.delete memberId theMembers
                    if Set.null theMembers'
                      then do
                        !ok <- cast mediaInput (DestroyMixer theMixerId)
                        unless ok (error (show (DestroyMixer theMixerId) ++ " failed!"))
                        void $ deliver callBack (MemberUnJoined mgId memberId)
                        return (Just (mgId, Map.delete theDspId st))
                      else do
                        void $ deliver callBack (MemberUnJoined mgId memberId)
                        return (Just (mgId, Map.insert theDspId (theMixerId, theMembers') st))
                  [] ->
                    return (Just (mgId, st))
      void $ forkIO (mgLoop (-1, Map.empty))
      return mgInput

    spawnMixingApps mixingInput =
      let !clients = foldMap spawnClient [0 .. nGroups param - 1]

          spawnClient !mixingGroupId = conc $ do
            eventsIn <- newMessageBox cfg
            eventsOut <- newInput eventsIn
            -- create conference,
            call mixingInput (CreateMixingGroup mixingGroupId) 50_000_000
              >>= either
                (error . ((show (CreateMixingGroup mixingGroupId) ++ " failed: ") ++) . show)
                (const (return ()))
            -- add participants and wait for the joined event
            !members <- forM [0 .. nMembers param - 1] $ \i -> do
              let !memberId = nMembers param * mixingGroupId + i
              !castSuccessful <- cast mixingInput (Join mixingGroupId memberId eventsOut)
              unless
                castSuccessful
                (error ("Failed to cast: " ++ show (Join mixingGroupId memberId eventsOut)))
              return memberId
            replicateM_
              (nMembers param)
              (fix $ \ ~again ->
                 receive eventsIn
                  >>= \case
                    Just (MemberJoined _ _) ->
                      return ()
                    Just unexpected ->
                      error ("Unexpected mixing group event: " ++ show unexpected ++ " expected MemberJoined")
                    Nothing ->
                      again
              )
            -- remove participants and wait for unjoined
            forM_ members $ \ !memberId -> do
              !castSuccessful <- cast mixingInput (UnJoin mixingGroupId memberId eventsOut)
              unless
                castSuccessful
                (error ("Failed to cast: " ++ show (UnJoin mixingGroupId memberId eventsOut)))
            replicateM_
              (nMembers param)
              (fix $ \ ~again ->
                 receiveAfter eventsIn 500_000
                  >>= \case
                    Just (MemberUnJoined _ _) ->
                      return ()
                    Just unexpected ->
                      error ("Unexpected mixing group event: " ++ show unexpected ++ " expected MemberUnJoined")
                    Nothing ->
                      again
              )
            -- destroy the conference,
            call mixingInput (DestroyMixingGroup mixingGroupId) 500_000
              >>= either
                (error . ((show (DestroyMixingGroup mixingGroupId) ++ " failed: ") ++) . show)
                (const (return ()))
       in clients

data AppCounters = AppCounters
  { callIdCounter :: !(CounterVar CallId),
    idCounter :: !(CounterVar Int)
  }

instance CallId.HasCallIdCounter AppCounters where
  getCallIdCounter = asks callIdCounter

instance HasCounterVar Int AppCounters where
  getCounterVar = asks idCounter

data Param = Param
  { nDsps :: !Int,
    nGroups :: !Int,
    nMembers :: !Int
  }

instance Show Param where
  showsPrec _ Param {nDsps, nGroups, nMembers} =
    shows nDsps . showString "DSPs/"
      . shows nGroups
      . showString "GRPs/"
      . shows nMembers
      . showString "MEMBERS"

toDspConfig :: Param -> Map DspId Capacity
toDspConfig p =
  let perDspCapacity =
        max 2 (2 * requiredTotal) -- (2 * (ceiling (fromIntegral requiredTotal / fromIntegral (nDsps p) :: Double)))
      requiredTotal = nGroups p + nGroups p * nMembers p
   in Map.fromList ([0 ..] `zip` replicate (nDsps p) perDspCapacity)

-- * Types for the domain of the benchmarks in this module

data MediaApi

data MixingApi

type DspId = Int

type MixerId = Int

type MemberId = Int

type MixingGroupId = Int

data instance Command MediaApi _ where
  FetchDsps :: Command MediaApi ( 'Return (Set DspId))
  CreateMixer :: DspId -> Command MediaApi ( 'Return (Maybe MixerId))
  DestroyMixer :: MixerId -> Command MediaApi 'FireAndForget
  AddToMixer :: MixerId -> MemberId -> Command MediaApi ( 'Return Bool)
  RemoveFromMixer :: MixerId -> MemberId -> Command MediaApi ( 'Return ())
  MediaShutdown :: Command MediaApi 'FireAndForget

deriving stock instance Show (Command MediaApi ( 'Return (Set DspId)))

deriving stock instance Show (Command MediaApi ( 'Return (Maybe MixerId)))

deriving stock instance Show (Command MediaApi ( 'Return Bool))

deriving stock instance Show (Command MediaApi ( 'Return ()))

deriving stock instance Show (Command MediaApi 'FireAndForget)

data MixingGroupEvent where
  MemberJoined :: MixingGroupId -> MemberId -> MixingGroupEvent
  MemberUnJoined :: MixingGroupId -> MemberId -> MixingGroupEvent
  deriving stock (Show, Eq)

data instance Command MixingApi _ where
  CreateMixingGroup ::
    MixingGroupId ->
    Command MixingApi ( 'Return ())
  DestroyMixingGroup ::
    MixingGroupId ->
    Command MixingApi ( 'Return ())
  Join ::
    IsInput outBox =>
    MixingGroupId ->
    MemberId ->
    outBox MixingGroupEvent ->
    Command MixingApi 'FireAndForget
  UnJoin ::
    IsInput outBox =>
    MixingGroupId ->
    MemberId ->
    outBox MixingGroupEvent ->
    Command MixingApi 'FireAndForget

instance Show (Command MixingApi ( 'Return ())) where
  showsPrec d (CreateMixingGroup i) =
    showParen (d >= 9) (showString "CreateMixingGroup " . shows i)
  showsPrec d (DestroyMixingGroup i) =
    showParen (d >= 9) (showString "DestroyMixingGroup " . shows i)

instance Show (Command MixingApi 'FireAndForget) where
  showsPrec d (Join i j _) =
    showParen (d >= 9) (showString "Join " . shows i . showChar ' ' . shows j)
  showsPrec d (UnJoin i j _) =
    showParen (d >= 9) (showString "UnJoin " . shows i . showChar ' ' . shows j)

type Capacity = Int

-- | Mix media streams using 'MediaApi'

-- | Media server simulation
--
-- Handle 'MediaApi' requests and manage a map of
-- dsps and mixers.
handleMediaApi ::
  MediaSimSt ->
  Message MediaApi ->
  RIO env MediaSimSt
handleMediaApi !st =
  \case
    Blocking FetchDsps replyBox -> do
      let !goodDsps = Map.keysSet (Map.filter (> 1) (allDsps st))
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
        Just (!dsp, !streams) ->
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
    Blocking (RemoveFromMixer theMixer theMember) replyBox -> do
      replyTo replyBox ()
      case Map.lookup theMixer (allMixers st) of
        Just (theDsp, theMembers)
          | Set.member theMember theMembers ->
            return
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
        _ ->
          return st
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
    NonBlocking MediaShutdown ->
      pure st {shuttingDown = True}

data MediaSimSt = MediaSimSt
  { allDsps :: !(Map DspId Capacity),
    nextMixerId :: !MixerId,
    allMixers :: !(Map MixerId (DspId, Set MemberId)),
    shuttingDown :: !Bool
  }

mkMediaSimSt :: Map DspId Capacity -> MediaSimSt
mkMediaSimSt x =
  MediaSimSt
    { allDsps = x,
      nextMixerId = 0,
      allMixers = Map.empty,
      shuttingDown = False
    }