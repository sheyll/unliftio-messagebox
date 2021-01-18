{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module MessageBoxCommon (testContentionRobustness) where

import Control.Exception (ErrorCall (ErrorCall))
import Control.Monad (forM_, unless, void)
import Data.Foldable (traverse_)
import Data.Function (fix)
import Data.Functor ((<&>))
import qualified Data.IntSet as Set
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe,
  )
import UnliftIO.MessageBox.Class
  ( IsInput (deliver),
    IsMessageBox (newInput, receive),
  )
import QCOrphans ()
import Test.Tasty as Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as Tasty
  ( assertEqual,
    testCase,
  )
import UnliftIO (throwTo, timeout)
import UnliftIO.Concurrent (forkIO, myThreadId, threadDelay)

testContentionRobustness ::
  IsMessageBox msgBox =>
  IO (msgBox (Int, Set.Key)) ->
  Int ->
  Set.Key ->
  TestTree
testContentionRobustness !mkReceiverIn !nSenders !nMsgs =
  Tasty.testGroup
    "lock contention robustness"
    [ Tasty.testCase
        "when many concurrent senders send many messages to one receiver,\
        \ and when every failed deliver is retried indefinitely, \
        \ all messages will be received"
        $ do
          !receiverIn <- mkReceiverIn
          let allMessages =
                let msgIds = Set.fromList [0 .. nMsgs - 1]
                 in Map.fromList [(sId, msgIds) | sId <- [0 .. nSenders - 1]]
          let doReceive = go Map.empty
                where
                  go acc =
                    ( timeout 2_000_000 (receive receiverIn)
                        <&> fromMaybe Nothing
                    )
                      >>= maybe
                        (return acc)
                        ( \(sId, msg) ->
                            -- liftIO (print (sId, msg)) >>
                            go
                              ( Map.alter
                                  ( Just
                                      . maybe
                                        (Set.singleton msg)
                                        (Set.insert msg)
                                  )
                                  sId
                                  acc
                              )
                        )

          receiverOut <- newInput receiverIn
          mainThread <- myThreadId
          forM_
            (Map.assocs allMessages)
            ( \(sId, msgs) ->
                void $
                  forkIO $ do
                    -- printf "Sender %i starting!\n" sId
                    -- printf "Sender %i started!\n" sId
                    traverse_
                      ( \msgId ->
                          timeout
                            30_000_000
                            ( fix
                                ( \again -> do
                                    ok <- deliver receiverOut (sId, msgId)
                                    if ok
                                      then return True
                                      else do
                                        threadDelay (msgId + sId)
                                        again
                                )
                            )
                            >>= maybe
                              ( throwTo
                                  mainThread
                                  ( ErrorCall
                                      ( "deliver for "
                                          <> show (sId, msgId)
                                          <> " timed out!"
                                      )
                                  )
                              )
                              ( `unless`
                                  throwTo
                                    mainThread
                                    ( ErrorCall
                                        ( "deliver for "
                                            <> show (sId, msgId)
                                            <> " failed!"
                                        )
                                    )
                              )
                      )
                      (Set.toList msgs)
                      -- printf "Sender %i done!\n" sId
            )
          receivedMessages <- doReceive
          assertEqual
            "failed to correctly receive all test messages"
            allMessages
            receivedMessages
    ]