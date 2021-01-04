{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module LimitedMessageBoxTest (test) where

import Control.Monad (forM, forever, replicateM, void)
import Data.Function (fix)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Semigroup (All (All, getAll))
import Protocol.MessageBox.Limited as MessageBox
  ( MessageBox,
    InputNB (..),
    create,
    newInput,
    deliver,
    receive,
    tryReceive,
    tryToDeliver,
    tryToDeliverAndWait,
  )
import qualified Protocol.MessageBox.Class as Class
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
import UnliftIO.Concurrent (threadDelay)

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocol.MessageBox.Limited"
    [ Tasty.testGroup
        "Non-Blocking"
        [ Tasty.testCase "receive from an empty queue" $ do
            i <- MessageBox.create 10
            f <- Class.tryReceive i
            timeout 1_000_000 (Class.tryNow f)
              >>= assertEqual "the future must not block and should return be emtpy" (Just (Nothing @Int)),
          Tasty.testCase "send to full queue" $ do
            i <- MessageBox.create 10
            o <- MessageBox.newInput i
            let sendWhileNotFull = do
                  success <- Class.deliver (InputNB o) 2
                  if not success then sendWhileNotFull else pure ()
            timeout 1_000_000 sendWhileNotFull
              >>= assertBool "deliver must not block" . isJust
        ],
      Tasty.testGroup
        "single threaded"
        [ Tasty.testGroup
            "create and newInput"
            [ testProperty "when createBox is applied to a limit > 0, then limit messages can be enqueued" $ \limit ->
                limit > 0 && limit <= 100
                  ==> ioProperty
                  $ do
                    i <- MessageBox.create limit :: IO (MessageBox String)
                    o <- MessageBox.newInput i
                    results <- replicateM limit (tryToDeliver o "test message")
                    pure $ last results,
              testProperty "when createBox is applied to a limit <= 0, then no messages can be enqueued" $ \limit ->
                limit <= 0
                  ==> ioProperty
                  $ do
                    i <- MessageBox.create limit :: IO (MessageBox String)
                    o <- MessageBox.newInput i
                    results <- replicateM limit (tryToDeliver o "test message")
                    return $ and results,
              testProperty "when the message box is 'full' not more then one more messages can be enqueued" $
                \(Positive (Small nOfMessagesThatCanBeWritten)) (Positive (Small x)) ->
                  ioProperty $ do
                    let noOfMessagesAttemptedToWrite = nOfMessagesThatCanBeWritten + x
                    i <- MessageBox.create nOfMessagesThatCanBeWritten
                    o <- MessageBox.newInput i
                    let messages = replicate noOfMessagesAttemptedToWrite "some test message"
                    results <- traverse (tryToDeliver o) messages
                    let (expectedSuccesses, rest) = splitAt nOfMessagesThatCanBeWritten results
                        (_ambuiguos, expectedFailures) = splitAt nOfMessagesThatCanBeWritten rest
                    let expectSomeFailures = noOfMessagesAttemptedToWrite > 2 * nOfMessagesThatCanBeWritten

                    return
                      ( label "all isRight expectedSuccesses" (and expectedSuccesses)
                          .&&. label "all isLeft expectedSuccesses" (not $ or expectedFailures)
                          .&&. cover
                            100.0
                            expectSomeFailures
                            "noOfMessagesAttemptedToWrite > 2 * nOfMessagesThatCanBeWritten"
                            (expectSomeFailures ==> not (null expectedFailures))
                      )
            ],
          Tasty.testGroup
            "create and send messages without receiving"
            [ Tasty.testGroup
                "tryToDeliver"
                [ Tasty.testCase "When the message limit is 2, two messages can be enqueued" $ do
                    i <- MessageBox.create 2
                    o <- MessageBox.newInput i
                    r1 <- tryToDeliver o "Messge 1"
                    r1 @?= True
                    r2 <- tryToDeliver o "Messge 2"
                    r2 @?= True,
                  Tasty.testCase "when writing into an empty Input, True is returned" $ do
                    i <- create 16
                    o <- newInput i
                    tryToDeliver o "Stop the climate crisis" >>= (@?= True),
                  Tasty.testCase "when writing into a full Input tryToDeliver returns False" $ do
                    i <- create 16
                    o <- newInput i
                    fix $ \next ->
                      tryToDeliver o "Stop the climate crisis" >>= \case
                        False -> assertBool "expected 'False' when the queue is full" True
                        True -> next
                ],
              Tasty.testGroup
                "tryToDeliverAndWait"
                [ Tasty.testCase "When the message limit is 2, two messages can be enqueued" $ do
                    i <- MessageBox.create 2
                    o <- MessageBox.newInput i
                    r1 <- tryToDeliverAndWait 1 o "Messge 1"
                    r1 @?= True
                    r2 <- tryToDeliverAndWait 1 o "Messge 2"
                    r2 @?= True,
                  Tasty.testCase "when writing into a full Input tryToDeliver returns False after the timeout" $ do
                    i <- create 16
                    o <- newInput i
                    fix $ \next ->
                      tryToDeliver o "Stop the climate crisis" >>= \case
                        False -> return ()
                        True -> next
                    tryToDeliverAndWait 123 o "foo bar"
                      >>= assertBool "expect tryToDeliverAndWait to return False (timeout)" . not
                ]
            ],
          Tasty.testGroup
            "create, send and receive messages"
            [ testProperty "all messages that were succecssfully sent are received in order" $
                \(NonEmpty (ms :: [(Bool, Double, [String])])) (Positive (Small nOfMessagesThatCanBeWritten)) ->
                  withMaxSuccess 20 $
                    ioProperty $ do
                      i <- MessageBox.create nOfMessagesThatCanBeWritten
                      o <- MessageBox.newInput i
                      allFine <- forM ms $ \message -> do
                        response <- tryToDeliver o message
                        f <- tryReceive i
                        receiveResult <- Class.tryNow f
                        return (response && receiveResult == Just message)
                      return (and allFine),
              testProperty "the future for the first message, receives only the first message" $
                \(NonEmpty (ms :: [(Bool, Double, [String])])) (Positive (Small nOfMessagesThatCanBeWritten)) ->
                  withMaxSuccess 20 $
                    ioProperty $ do
                      i <- MessageBox.create nOfMessagesThatCanBeWritten
                      o <- MessageBox.newInput i
                      futures <- replicateM (length ms) (tryReceive i)
                      allDelivered <- forM ms (fmap All . tryToDeliver o)
                      allReceived <-
                        mapConcurrently
                          ( \(message, future) -> fix $ \notNow -> do
                              Class.tryNow future >>= maybe notNow (return . All . (== message))
                          )
                          (ms `zip` futures)
                      return (getAll (mconcat allDelivered <> mconcat allReceived)),
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
                        i <- MessageBox.create queueSize
                        o <- MessageBox.newInput i

                        -- write queueSize messages
                        let (messages1, ~valueGenerator1) = splitAt queueSize valueGenerator
                        results1 <- traverse (tryToDeliver o) messages1
                        let sendSuccessfully1 = and results1
                        -- read n messages
                        receivedMessages1 <- replicateM n (tryReceive i >>= Class.tryNow)

                        -- write n messages
                        let (messages2, ~_valueGenerator2) = splitAt n valueGenerator1
                        results2 <- traverse (tryToDeliver o) messages2
                        let sendSuccessfully2 = and results2
                        -- read queueSize messages
                        receivedMessages2 <- replicateM queueSize (tryReceive i >>= Class.tryNow)

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
            "1 Writer x receivers y messages"
            [ Tasty.testProperty
                "when using Outputes with a limit greater than the number\
                \ of messsages in the test, tryToDeliver always succeeds"
                $ \(Positive (Small nMsgs')) (Positive (Small nReceivers')) ->
                  withMaxSuccess 20 $
                    ioProperty $ do
                      let nMsgs = nMsgs' `rem` 100
                      let nReceivers = nReceivers' `rem` 100
                      !outBoxesAndReaders <- replicateM nReceivers $ do
                        !ib <- create (max 128 nMsgs)
                        !ob <- newInput ib
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
                      let doSend = traverse (uncurry tryToDeliver) ((,) <$> outBoxes <*> ms)

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
                "when using Outputes with a limit smaller than half of the number\
                \ of messsages in the test, tryToDeliver first succeeds and then fails"
                $ do
                  let nGood = 128
                      nAmbigous = 2 * nGood
                      nBad = 123

                  receiverOut <- create nGood >>= newInput
                  let doReceive = threadDelay 300_000_000

                  let good = [("Test-Message GOOD", Just n) | n <- [1 .. nGood :: Int]]
                      ambigous = [("Test-Message ambigous", Just n) | n <- [1 .. nAmbigous :: Int]]
                      bad = [("Test-Message Base", Just n) | n <- [1 .. nBad :: Int]]

                  let doSend =
                        traverse (tryToDeliver receiverOut) good
                          <> traverse (fmap (const True) . tryToDeliver receiverOut) ambigous
                          <> traverse (tryToDeliver receiverOut) bad
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
                "when sending 5 times faster then receiving, then tryToDeliver \
                \will begin to fail after some time"
                $ do
                  let nGood = 256
                      nAmbigous = 2 * nGood + 1
                      nBad = nGood
                      tSend = 500
                      tRecv = 5 * tSend

                  receiverIn <- create nGood
                  receiverOut <- newInput receiverIn
                  let doReceive = void (forever (receive receiverIn >> threadDelay tRecv))

                  let !good = [("Test-Message GOOD", Just n) | !n <- [1 .. nGood :: Int]]
                      !ambigous = [("Test-Message ambigous", Just n) | !n <- [1 .. nAmbigous :: Int]]
                      !bad = [("Test-Message Base", Just n) | !n <- [1 .. nBad :: Int]]

                  let doSend =
                        traverse (\ !m -> threadDelay tSend >> tryToDeliver receiverOut m) good
                          <> traverse (\ !m -> threadDelay tSend >> tryToDeliver receiverOut m >> return True) ambigous
                          <> traverse (\ !m -> threadDelay tSend >> tryToDeliver receiverOut m) bad
                  Right result <-
                    race
                      doReceive
                      doSend
                  let !resultGoodPart = take nGood result
                      !resultBad = drop (nGood + nAmbigous) result

                  assertBool "first messages succeed" (and resultGoodPart)
                  assertBool "last messages fail" (not $ and resultBad),
              Tasty.testProperty
                "when sending faster than receiving using deliver\
                \ with a very high timeout will succeed\
                \ to send all messages, irrespective of the queue size"
                $ \(Positive (Small queueSize)) -> withMaxSuccess 20 $
                  ioProperty $
                    do
                      let nGood = 256
                          tSend = 200
                          tRecv = 5 * tSend

                      receiverIn <- create queueSize
                      receiverOut <- newInput receiverIn
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
                              tryToDeliverAndWait 5_000_000 receiverOut m
                            threadDelay tSend
                            lr <- tryToDeliverAndWait 5_000_000 receiverOut Nothing
                            return (lr : res)

                      concurrently
                        doReceive
                        doSend
                        >>= \case
                          (_, actual) -> do
                            let expected =
                                  True :
                                  replicate (nGood + 1 - 1) True
                            assertEqual "messages succeed" expected actual,
              Tasty.testCase
                "when 1000 senders send 100 messages to 1 receiver using \
                \tryToDeliverAndWait, all messages will be received"
                $ do
                  let queueSize = nSenders
                      nMsgs = 100
                      nSenders = 1000 :: Int
                  !receiverIn <- create queueSize
                  !receiverOut <- newInput receiverIn
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
                          deliver receiverOut m

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
