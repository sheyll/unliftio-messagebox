{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module ClosableMessageBoxTest (test) where

import Control.Monad (forM, forever, replicateM, void)
import Data.Either (isLeft, isRight)
import Data.Function (fix)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe ()
import Data.Semigroup ()
import Protocol.ClosableMessageBox as MessageBox
import System.Mem (performGC)
import Test.QuickCheck
import Test.Tasty as Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as Tasty
  ( assertBool,
    assertEqual,
    testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck as Tasty (testProperty)
import Text.Printf (printf)
import UnliftIO
import UnliftIO.Concurrent (forkFinally, forkIO, threadDelay)

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocols.ClosableMessageBox"
    [ Tasty.testGroup
        "single threaded"
        [ Tasty.testGroup
            "createInBox and createOutBoxForInbox"
            [ testProperty "when createBox is applied to a limit > 0, then limit messages can be enqueued" $ \limit ->
                limit > 0 && limit <= 100
                  ==> ioProperty
                  $ do
                    i <- MessageBox.createInBox limit :: IO (InBox String)
                    o <- MessageBox.createOutBoxForInbox i
                    results <- replicateM limit (trySend o "test message")
                    return $ case last results of
                      Right _ -> True
                      Left _ -> False,
              testProperty "when createBox is applied to a limit <= 0, then no messages can be enqueued" $ \limit ->
                limit <= 0
                  ==> ioProperty
                  $ do
                    i <- MessageBox.createInBox limit :: IO (InBox String)
                    o <- MessageBox.createOutBoxForInbox i
                    results <- replicateM limit (trySend o "test message")
                    return $ isLeft `all` results,
              testProperty "when the message box is 'full' not more then one more messages can be enqueued" $
                \(Positive (Small noOfMessagesThatCanBeWritten)) (Positive (Small x)) ->
                  ioProperty $ do
                    let noOfMessagesAttemptedToWrite = noOfMessagesThatCanBeWritten + x
                    i <- MessageBox.createInBox noOfMessagesThatCanBeWritten
                    o <- MessageBox.createOutBoxForInbox i
                    let messages = replicate noOfMessagesAttemptedToWrite "some test message"
                    results <- traverse (trySend o) messages
                    let (expectedSuccesses, rest) = splitAt noOfMessagesThatCanBeWritten results
                        (_ambuiguos, expectedFailures) = splitAt noOfMessagesThatCanBeWritten rest
                    let expectSomeFailures = noOfMessagesAttemptedToWrite > 2 * noOfMessagesThatCanBeWritten

                    return
                      ( label "all isRight expectedSuccesses" (all isRight expectedSuccesses)
                          .&&. label "all isLeft expectedSuccesses" (all isLeft expectedFailures)
                          .&&. cover
                            100.0
                            expectSomeFailures
                            "noOfMessagesAttemptedToWrite > 2 * noOfMessagesThatCanBeWritten"
                            (expectSomeFailures ==> not (null expectedFailures))
                      )
            ],
          Tasty.testGroup
            "create and send messages without receiving"
            [ Tasty.testGroup
                "trySend"
                [ Tasty.testCase "When the message limit is 2, two messages can be enqueued,and after the first insertion the caller is warned that the queue is almost full" $ do
                    i <- MessageBox.createInBox 2
                    o <- MessageBox.createOutBoxForInbox i
                    r1 <- trySend o "Messge 1"
                    r1 @?= Right OutBoxCriticallyFull
                    r2 <- trySend o "Messge 2"
                    r2 @?= Right OutBoxCriticallyFull,
                  Tasty.testCase "when writing into an empty OutBox, OutBoxOk is returned" $ do
                    i <- createInBox 16
                    o <- createOutBoxForInbox i
                    trySend o "Stop the climate crisis" >>= (@?= Right OutBoxOk),
                  Tasty.testCase "when writing into a full OutBox trySend returns Left OutBoxFull" $ do
                    i <- createInBox 16
                    o <- createOutBoxForInbox i
                    fix $ \next ->
                      trySend o "Stop the climate crisis" >>= \case
                        Left e -> e @?= OutBoxFull
                        Right !_ -> next,
                  Tasty.testCase "when writing into a closed OutBox, Left OutBoxClosed is returned" $ do
                    i <- createInBox 16
                    o <- createOutBoxForInbox i
                    closeInBox i
                    trySend o "Stop the climate crisis" >>= (@?= Left OutBoxClosed)
                ],
              Tasty.testGroup
                "trySendAndWait"
                [ Tasty.testCase "When the message limit is 2, two messages can be enqueued,and after the first insertion the caller is warned that the queue is almost full" $ do
                    i <- MessageBox.createInBox 2
                    o <- MessageBox.createOutBoxForInbox i
                    r1 <- trySendAndWait 1 o "Messge 1"
                    r1 @?= Right OutBoxCriticallyFull
                    r2 <- trySendAndWait 1 o "Messge 2"
                    r2 @?= Right OutBoxCriticallyFull,
                  Tasty.testCase "when writing into an empty OutBox, Left OutBoxOk is returned" $ do
                    i <- createInBox 16
                    o <- createOutBoxForInbox i
                    trySendAndWait 1 o "Stop the climate crisis" >>= (@?= Right OutBoxOk),
                  Tasty.testCase "when writing into a closed OutBox, Left OutBoxClosed is returned" $ do
                    i <- createInBox 16
                    o <- createOutBoxForInbox i
                    closeInBox i
                    trySendAndWait 1 o "Stop the climate crisis" >>= (@?= Left OutBoxClosed)
                ]
            ],
          Tasty.testGroup
            "create, send and receive messages"
            [ testProperty "all messages that were succecssfully sent are received in order" $
                \(NonEmpty (ms :: [(Bool, Double, [String])])) (Positive (Small noOfMessagesThatCanBeWritten)) ->
                  withMaxSuccess 20 $
                    ioProperty $ do
                      i <- MessageBox.createInBox noOfMessagesThatCanBeWritten
                      o <- MessageBox.createOutBoxForInbox i
                      allFine <- forM ms $ \message -> do
                        response <- trySend o message
                        receiveResult <- tryReceive i
                        return (isRight response && receiveResult == Just message)
                      return (and allFine),
              testProperty
                "At least 'queueSize' messages can be written before without loosing\
                \ any message, and after n messages have been read, upto n messages \
                \ can be written again, all messages are in FIFO order"
                $ \(Positive (Small queueSize)) (Positive (Small n)) ->
                  n < queueSize
                    ==> withMaxSuccess 20
                    $ ioProperty $
                      do
                        let valueGenerator = [0 ..]
                        i <- MessageBox.createInBox queueSize
                        o <- MessageBox.createOutBoxForInbox i

                        -- write queueSize messages
                        let (messages1, ~valueGenerator1) = splitAt queueSize valueGenerator
                        results1 <- traverse (trySend o) messages1
                        let sendSuccessfully1 = isRight `all` results1
                        -- read n messages
                        receivedMessages1 <- replicateM n (tryReceive i)

                        -- write n messages
                        let (messages2, ~_valueGenerator2) = splitAt n valueGenerator1
                        results2 <- traverse (trySend o) messages2
                        let sendSuccessfully2 = isRight `all` results2
                        -- read queueSize messages
                        receivedMessages2 <- replicateM queueSize (tryReceive i)

                        let receivedInOrder =
                              receivedMessages1 ++ receivedMessages2
                                == (Just <$> take (queueSize + n) valueGenerator)

                        return $
                          sendSuccessfully1
                            .&&. sendSuccessfully2
                            .&&. receivedInOrder
            ]
            --TODO: Test for queue is empty after all values were read
        ],
      Tasty.testGroup
        "multi threaded"
        [ Tasty.testGroup
            "sending into outBoxes with closed inBoxes"
            [ Tasty.testCase "001" $
                -- no messages are sent, no receiver is started, the closeInBox is immediately called after createInBox
                do
                  o <- do
                    i <- createInBox 100
                    closeInBox i
                    createOutBoxForInbox i
                  r <- trySend o "Stop the climate crisis"
                  r @?= Left OutBoxClosed,
              Tasty.testCase "002" $
                -- no messages are sent, no receiver is started, the closeInBox is called after createOutBoxFromInBox
                do
                  o <- do
                    i <- createInBox 100
                    o <- createOutBoxForInbox i
                    closeInBox i
                    return o
                  r <- trySend o "Stop the climate crisis"
                  r @?= Left OutBoxClosed,
              Tasty.testProperty "003" $
                -- no messages are sent, the receiver process is delayed by a random amount,
                -- then exists and closes the InBox, trySend eventually returns
                -- an error
                \(Positive (Small delay)) -> withMaxSuccess 10 $
                  ioProperty $ do
                    sync <- newEmptyMVar
                    o <- do
                      !i <- createInBox 100
                      !o <- createOutBoxForInbox i
                      void $
                        forkFinally
                          (threadDelay delay)
                          (const (closeInBox i >> putMVar sync ()))
                      return o
                    () <- takeMVar sync
                    res <-
                      fix
                        ( \next !maxRounds -> do
                            r <- trySend o "Stop the climate crisis"
                            case r of
                              Left e ->
                                return (Just e)
                              _ ->
                                if maxRounds <= 0
                                  then return Nothing
                                  else do
                                    threadDelay delay
                                    next (maxRounds - 1)
                        )
                        10000
                    return (res === Just OutBoxClosed),              
              Tasty.testProperty "004" $
                -- messages are sent until trySend returns OutBoxFull,
                -- then a process is forked that delays a bit and closes the InBox,
                -- in the mean time the test process uses trySendAndWait, to
                -- enqueue a message. It should be blocked by Unagi.writeChan.
                -- After the forked process has closed the InBox, the blocking
                -- trySendAndWait invokation should eventually return
                -- 'Left OutBoxClosed'.
                \(Positive (Small delay')) (Positive (Small queueSize)) ->
                  withMaxSuccess 20 $
                    ioProperty $ do
                      startSendingSoon <- newEmptyMVar
                      doGC <- newEmptyMVar
                      let delay = (4 + (delay' `rem` 13)) * 500
                      o <- do
                        i <- createInBox queueSize
                        o <- createOutBoxForInbox i
                        fix $ \next ->
                          trySend o "Stop the climate crisis" >>= \case
                            Left OutBoxFull -> return ()
                            Left e -> error (show e)
                            Right !_ -> next
                        void $
                          forkFinally
                            (putMVar startSendingSoon () >> threadDelay delay)
                            (const (closeInBox i >> putMVar doGC ()))
                        return o
                      void (forkIO (takeMVar doGC >> liftIO performGC))
                      takeMVar startSendingSoon                      
                      r <- trySendAndWait (100 * delay) o "Stop the climate crisis"
                      return (
                              cover 50 (r == Left OutBoxClosed) "OutBoxClosed" (r === Left OutBoxClosed) 
                        .||.  cover 1 (r == Left OutBoxFull) "OutBoxFull" (r === Left OutBoxFull) 
                        )
            ],
          Tasty.testGroup
            "1 Writer x receivers y messages"
            [ Tasty.testProperty
                "when using InBoxes with a limit greater than the number\
                \ of messsages in the test, trySend always succeeds"
                $ \(Positive (Small nMsgs')) (Positive (Small nReceivers')) ->
                  withMaxSuccess 20 $
                    ioProperty $ do
                      let nMsgs = nMsgs' `rem` 100
                      let nReceivers = nReceivers' `rem` 100
                      !outBoxesAndReaders <- replicateM nReceivers $ do
                        !ib <- createInBox (max 128 nMsgs)
                        !ob <- createOutBoxForInbox ib
                        pure
                          ( ob,
                            fix
                              ( \ !next !noMsgs ->
                                  receive ib
                                    >>= ( \case
                                            (!_testMsg, Just _) -> next (noMsgs + 1)
                                            (!_testMsg, Nothing) -> return noMsgs
                                        )
                              )
                              0
                          )

                      let (!outBoxes, !receivers) = unzip outBoxesAndReaders
                      let ms = [("Test-Message", Just n) | n <- [1 .. nMsgs :: Int]] ++ [("Stop-Message", Nothing)]
                      let doSend = traverse (fmap isRight . uncurry trySend) ((,) <$> outBoxes <*> ms)

                      result <-
                        concurrently
                          (forConcurrently receivers id)
                          doSend

                      return
                        ( result
                            === ( replicate nReceivers nMsgs,
                                  replicate ((nMsgs + 1) * nReceivers) True
                                )
                        ),
              Tasty.testCase
                "when using InBoxes with a limit smaller than half of the number\
                \ of messsages in the test, trySend first succeeds and then fails"
                $ do
                  let nGood = 128
                      nAmbigous = 2 * nGood
                      nBad = 123

                  receiverOut <- createInBox nGood >>= createOutBoxForInbox
                  let doReceive = threadDelay 300_000_000

                  let good = [("Test-Message GOOD", Just n) | n <- [1 .. nGood :: Int]]
                      ambigous = [("Test-Message ambigous", Just n) | n <- [1 .. nAmbigous :: Int]]
                      bad = [("Test-Message Base", Just n) | n <- [1 .. nBad :: Int]]

                  let doSend =
                        traverse (fmap isRight . trySend receiverOut) good
                          <> traverse (fmap (const True) . trySend receiverOut) ambigous
                          <> traverse (fmap isRight . trySend receiverOut) bad
                  result <-
                    race
                      doReceive
                      doSend

                  result
                    @?= Right
                      ( replicate nGood True
                          <> replicate nAmbigous True
                          <> replicate nBad False
                      ),
              Tasty.testCase
                "when sending 5 times faster then receiving, then trySend \
                \will begin to fail after some time"
                $ do
                  let nGood = 256
                      nAmbigous = 2 * nGood + 1
                      nBad = nGood
                      tSend = 500
                      tRecv = 5 * tSend

                  receiverIn <- createInBox nGood
                  receiverOut <- createOutBoxForInbox receiverIn
                  let doReceive = void (forever (tryReceive receiverIn >> threadDelay tRecv))

                  let good = [("Test-Message GOOD", Just n) | n <- [1 .. nGood :: Int]]
                      ambigous = [("Test-Message ambigous", Just n) | n <- [1 .. nAmbigous :: Int]]
                      bad = [("Test-Message Base", Just n) | n <- [1 .. nBad :: Int]]

                  let doSend =
                        traverse (\m -> threadDelay tSend >> isRight <$> trySend receiverOut m) good
                          <> traverse (\m -> threadDelay tSend >> trySend receiverOut m >> return True) ambigous
                          <> traverse (\m -> threadDelay tSend >> isRight <$> trySend receiverOut m) bad
                  Right result <-
                    race
                      doReceive
                      doSend
                  let resultGoodPart = take nGood result
                      resultBad = drop (nGood + nAmbigous) result

                  assertBool "first messages succeed" (all (== True) resultGoodPart)
                  assertBool "last messages fail" (False `elem` resultBad),
              Tasty.testProperty
                "when sending faster than receiving using trySendAndWait\
                \ with a very high timeout will succeed\
                \ to send all messages, irrespective of the queue size"
                $ \(Positive (Small queueSize)) ->  withMaxSuccess 20 $ ioProperty $
                  do
                    let nGood = 256
                        tSend = 200
                        tRecv = 5 * tSend
                        sendTimeout = extraSlack * (tRecv * nGood + tSend)
                          where
                            extraSlack = 32

                    receiverIn <- createInBox queueSize
                    receiverOut <- createOutBoxForInbox receiverIn
                    let doReceive =
                          fix $ \continue -> do
                            threadDelay tRecv
                            receive receiverIn
                              >>= maybe
                                (return ())
                                (const continue)

                    let ms = replicate nGood (Just "Test-Message")
                    let doSend = do
                          res <- forM ms $ \m -> do
                            threadDelay tSend
                            trySendAndWait sendTimeout receiverOut m
                          threadDelay tSend
                          lr <- trySendAndWait sendTimeout receiverOut Nothing
                          return (lr : res)

                    concurrently
                      doReceive
                      doSend
                      >>= \case
                        (_, result) -> do
                          let expected =
                                Right OutBoxOk :
                                replicate (nGood + 1 - 1) (Right OutBoxOk)
                              actual =
                                map (either Left (const (Right OutBoxOk))) result
                          assertEqual "messages succeed" expected actual,
              Tasty.testCase
                "when 1000 senders send 100 messages to 1 receiver using \
                \trySendAndWait, all messages will be received"
                $ do
                  let queueSize = nSenders
                      nMsgs = 100
                      nSenders = 1000 :: Int
                  !receiverIn <- createInBox queueSize
                  !receiverOut <- createOutBoxForInbox receiverIn
                  let doReceive = flip fix Map.empty $ \continue !resultMap ->
                        if nSenders == Map.size resultMap
                          && all (== nMsgs) (length <$> Map.elems resultMap)
                          then return resultMap
                          else do
                            (!senderId, !msg) <- receive receiverIn
                            let !nextResultMap =
                                  Map.alter
                                    (maybe (Just [msg]) (Just . (msg :)))
                                    senderId
                                    resultMap
                            continue nextResultMap

                  let mkMsgs !senderId =
                        [ (senderId, printf "%i" mId :: String)
                          | !mId <- [0 .. nMsgs - 1]
                        ]

                  let mkSender !senderId = do
                        forM (mkMsgs senderId) $ \ !m -> do
                          trySendAndWait 5_000_000 receiverOut m

                  (!receiverResult, !_senderResults) <-
                    concurrently
                      doReceive
                      (mapConcurrently mkSender [0 .. nSenders - 1])
                  assertEqual "expect that messages from all senders are received" nSenders (Map.size receiverResult)
                  mapM_
                    ( \(!senderId, !msgs) ->
                        assertEqual
                          ("expect that message receiving for sender " ++ show senderId ++ " succeeded")
                          (sort (mkMsgs senderId))
                          (sort ((senderId,) <$> msgs))
                    )
                    (Map.toList receiverResult)
            ]
        ]
        -- [0 -> 2, 1 -> 3, 2 -> 4, 2 -> 5, 3 -> 5]
        -- y = mx + b
    ]

{-

manyReadersOneWriter = error "TODO"

manyWriterOneReader = error "TODO"

manyOneToOne = error "TODO"

-}