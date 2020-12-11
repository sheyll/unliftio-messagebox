{-# LANGUAGE ScopedTypeVariables #-}

module MessageBoxTests where

import Control.Monad
import Data.Either
import Data.Semigroup
import Protocol.MessageBox as MessageBox
import Test.QuickCheck
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as Tasty
import Test.Tasty.QuickCheck
import UnliftIO

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
        [ -- 1x writer
          -- many reader
          -- kill one reader
          -- other readers don't starve

          -- no starvation:
          --   * timeout timeItTakesToStarve (tryRead ...) => Right _
          --
          --
          -- writer:
          --    kill reader-3
          --    trySend reader-1 => OK
          --    trySend reader-2 => OK
          --    trySend reader-3 => blocked
          --    trySend reader-4 => is subject to starvation
          --    trySend reader-5 => is subject to starvation
          --
          --
          Tasty.testCase "1 Writer 5 Reader 10 messages, all messages get delivered" $ do
            (resultAggregatorOutBox, resultAggregator) <- do
              ib <- createInBox 16
              ob <- createOutBoxForInbox ib
              pure
                ( ob,
                  conc $ replicateM 4 receive
                )
            resultAggregator <- createOutBoxForInbox ib
            outBoxesAndReaders <- forM [1 .. 5] $ \readerIndex -> do
              ib <- createInBox 16
              ob <- createOutBoxForInbox ib
              pure
                ( ob,
                  conc $ do
                    _msg <- receive ib
                    if readerIndex == 3
                      then error "Test-User-Error"
                      else do
                        trySend resultAggregatorOutBox readerIndex
                )
            mconcat $ forM outBoxesAndReaders $ \(readerInbox, reader) -> do
              reader
              trySend () readerInbox
              pure reader

            

            (outBoxWriter, writerAction) <- do
              ib <- createInBox 16
              ob <- createOutBoxForInbox ib
              let action = do

              return (ob, conc action)
            runConc
              ()
        ]
        -- [0 -> 2, 1 -> 3, 2 -> 4, 2 -> 5, 3 -> 5]
        -- y = mx + b
    ]

{-

manyReadersOneWriter = error "TODO"

manyWriterOneReader = error "TODO"

manyOneToOne = error "TODO"

-}