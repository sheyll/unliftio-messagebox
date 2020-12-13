{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MessageBoxTests (tests) where

import Control.Monad
import Data.Either
import Data.Function
import Data.Semigroup
import Protocol.MessageBox as MessageBox
import Test.QuickCheck
import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
import Test.Tasty.QuickCheck as Tasty
import UnliftIO
import UnliftIO.Concurrent

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Protocols.MessageBox"
    [ Tasty.testGroup
        "single threaded"
        [ Tasty.testGroup
            "create and send messages without receiving"
            [ Tasty.testCase "When the message limit is 2, two messages can be enqueued,and after the first insertion the caller is warned that the queue is almost full" $ do
                inBox <- MessageBox.createInBox 2
                outBox <- MessageBox.createOutBoxForInbox inBox
                result1 <- trySend outBox "Messge 1"
                result1 @?= Right OutBoxCriticallyFull
                result2 <- trySend outBox "Messge 2"
                result2 @?= Right OutBoxCriticallyFull,
              Tasty.testGroup
                "Queue Sizes"
                [ testProperty "when createBox is applied to a limit > 0, then limit messages can be enqueued" $ \limit ->
                    limit > 0 && limit <= 100
                      ==> ioProperty
                      $ do
                        inBox <- MessageBox.createInBox limit :: IO (InBox String)
                        outBox <- MessageBox.createOutBoxForInbox inBox
                        results <- replicateM limit (trySend outBox "test message")
                        return $ case last results of
                          Right _ -> True
                          Left _ -> False,
                  testProperty "when createBox is applied to a limit <= 0, then no messages can be enqueued" $ \limit ->
                    limit <= 0
                      ==> ioProperty
                      $ do
                        inBox <- MessageBox.createInBox limit :: IO (InBox String)
                        outBox <- MessageBox.createOutBoxForInbox inBox
                        results <- replicateM limit (trySend outBox "test message")
                        return $ isLeft `all` results,
                  testProperty "when the message box is 'full' not more then one more messages can be enqueued" $
                    \(Positive (Small noOfMessagesThatCanBeWritten)) (Positive (Small x)) ->
                      ioProperty $ do
                        let noOfMessagesAttemptedToWrite = noOfMessagesThatCanBeWritten + x
                        inBox <- MessageBox.createInBox noOfMessagesThatCanBeWritten
                        outBox <- MessageBox.createOutBoxForInbox inBox
                        let messages = replicate noOfMessagesAttemptedToWrite "some test message"
                        results <- traverse (trySend outBox) messages
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
                ]
            ],
          Tasty.testGroup
            "create, send and receive messages"
            [ testProperty "all messages that were succecssfully sent are received in order" $
                \(NonEmpty (testMessages :: [(Bool, Double, [String])])) (Positive (Small noOfMessagesThatCanBeWritten)) ->
                  ioProperty $ do
                    inBox <- MessageBox.createInBox noOfMessagesThatCanBeWritten
                    outBox <- MessageBox.createOutBoxForInbox inBox
                    allFine <- forM testMessages $ \message -> do
                      response <- trySend outBox message
                      receiveResult <- tryReceive inBox
                      return (isRight response && receiveResult == Just message)
                    return (and allFine),
              testProperty
                "At least 'queueSize' messages can be written before without loosing\
                \ any message, and after n messages have been read, upto n messages \
                \ can be written again, all messages are in FIFO order"
                $ \(Positive (Small queueSize)) (Positive (Small n)) ->
                  n < queueSize
                    ==> ioProperty
                    $ do
                      let valueGenerator = [0 ..]
                      inBox <- MessageBox.createInBox queueSize
                      outBox <- MessageBox.createOutBoxForInbox inBox

                      -- write queueSize messages
                      let (messages1, valueGenerator1) = splitAt queueSize valueGenerator
                      results1 <- traverse (trySend outBox) messages1
                      let sendSuccessfully1 = isRight `all` results1
                      -- read n messages
                      receivedMessages1 <- replicateM n (tryReceive inBox)

                      -- write n messages
                      let (messages2, _valueGenerator2) = splitAt n valueGenerator1
                      results2 <- traverse (trySend outBox) messages2
                      let sendSuccessfully2 = isRight `all` results2
                      -- read queueSize messages
                      receivedMessages2 <- replicateM queueSize (tryReceive inBox)

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
            "closing a message box"
            [],
          Tasty.testGroup
            "1 Writer x receivers y messages"
            [ Tasty.testProperty "when using InBoxes with a limit greater than the number of messsages in the test, trySend always succeeds" $
                \(Positive (Small noTestMessages')) (Positive (Small noReceivers')) ->
                  ioProperty $ do
                    let noTestMessages = noTestMessages' `rem` 100
                    let noReceivers = noReceivers' `rem` 100
                    !outBoxesAndReaders <- replicateM noReceivers $ do
                      !ib <- createInBox (max 128 noTestMessages)
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
                    let testMessages = [("Test-Message", Just n) | n <- [1 .. noTestMessages :: Int]] ++ [("Stop-Message", Nothing)]
                    let sendTestMessages = traverse (fmap isRight . uncurry trySend) ((,) <$> outBoxes <*> testMessages)

                    result <-
                      concurrently
                        (forConcurrently receivers id)
                        sendTestMessages

                    return
                      ( result
                          === ( replicate noReceivers noTestMessages,
                                replicate ((noTestMessages + 1) * noReceivers) True
                              )
                      ),
              Tasty.testCase "when using InBoxes with a limit smaller than half of the number of messsages in the test, trySend first succeeds and then fails" $
                do
                  let noTestMessagesGood = 128
                      noTestMessagesAmbigous = 2 * noTestMessagesGood
                      noTestMessagesBad = 123

                  receiverOutBox <- createInBox noTestMessagesGood >>= createOutBoxForInbox
                  let receiverAction = threadDelay 300_000_000

                  let testMessagesGood = [("Test-Message GOOD", Just n) | n <- [1 .. noTestMessagesGood :: Int]]
                      testMessagesAmbigous = [("Test-Message Ambigous", Just n) | n <- [1 .. noTestMessagesAmbigous :: Int]]
                      testMessagesBad = [("Test-Message Base", Just n) | n <- [1 .. noTestMessagesBad :: Int]]

                  let sendTestMessages =
                        traverse (fmap isRight . trySend receiverOutBox) testMessagesGood
                          <> traverse (fmap (const True) . trySend receiverOutBox) testMessagesAmbigous
                          <> traverse (fmap isRight . trySend receiverOutBox) testMessagesBad
                  result <-
                    race
                      receiverAction
                      sendTestMessages

                  result
                    @?= Right
                      ( replicate noTestMessagesGood True
                          <> replicate noTestMessagesAmbigous True
                          <> replicate noTestMessagesBad False
                      ),
              Tasty.testCase "when sending 5 times faster then receiving, then trySend will begin to fail after some time" $
                do
                  let noTestMessagesGood = 256
                      noTestMessagesAmbigous = 2 * noTestMessagesGood + 1
                      noTestMessagesBad = 12
                      delaySender = 500
                      delayReceiver = 5 * delaySender

                  receiverInBox <- createInBox noTestMessagesGood
                  receiverOutBox <- createOutBoxForInbox receiverInBox
                  let receiverAction = void (forever (tryReceive receiverInBox >> threadDelay delayReceiver))

                  let testMessagesGood = [("Test-Message GOOD", Just n) | n <- [1 .. noTestMessagesGood :: Int]]
                      testMessagesAmbigous = [("Test-Message Ambigous", Just n) | n <- [1 .. noTestMessagesAmbigous :: Int]]
                      testMessagesBad = [("Test-Message Base", Just n) | n <- [1 .. noTestMessagesBad :: Int]]

                  let sendTestMessages =
                        traverse (\m -> threadDelay delaySender >> isRight <$> trySend receiverOutBox m) testMessagesGood
                          <> traverse (\m -> threadDelay delaySender >> trySend receiverOutBox m >> return True) testMessagesAmbigous
                          <> traverse (\m -> threadDelay delaySender >> isRight <$> trySend receiverOutBox m) testMessagesBad
                  Right result <-
                    race
                      receiverAction
                      sendTestMessages
                  let resultGoodPart = take noTestMessagesGood result
                      resultBad = drop (noTestMessagesGood + noTestMessagesAmbigous) result

                  assertBool "first messages succeed" (all (== True) resultGoodPart)
                  assertBool "last messages fail" (False `elem` resultBad),
              Tasty.testProperty
                "when sending faster then receiving using trySendAndWait with a very high timeout will succeed\
                \ to send all messages, irrespective of the queue size"
                $ \(Positive (Small queueSize)) -> ioProperty $
                  do
                    let noTestMessagesGood = 256
                        delaySender = 20
                        delayReceiver = 5 * delaySender

                    receiverInBox <- createInBox queueSize
                    receiverOutBox <- createOutBoxForInbox receiverInBox
                    let receiverAction =
                          fix $ \continue -> do
                            m <- try @_ @SomeException $
                              do
                                threadDelay delayReceiver
                                receive receiverInBox
                            either
                              (return . Just . show)
                              ( maybe
                                  (return Nothing)
                                  (const continue)
                              )
                              m

                    let testMessages = replicate noTestMessagesGood (Just "Test-Message")

                    let sendTestMessages = do
                          res <- forM testMessages $ \m -> do
                            threadDelay delaySender
                            trySendAndWait 100_000 receiverOutBox m
                          threadDelay delaySender
                          lr <- trySendAndWait 100_000 receiverOutBox Nothing
                          return (lr : res)

                    concurrently
                      receiverAction
                      sendTestMessages
                      >>= \case
                        (Just err, _) ->
                          assertFailure err
                        (Nothing, result) -> do
                          let expected =
                                Right OutBoxOk :
                                replicate (noTestMessagesGood + 1 - 1) (Right OutBoxOk)
                              actual =
                                map (either Left (const (Right OutBoxOk))) result
                          assertEqual "messages succeed" expected actual
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