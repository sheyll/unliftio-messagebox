{-# LANGUAGE NoOverloadedStrings #-}

module LimitedMessageBoxTest (test) where

import Control.Monad
  ( forever,
    replicateM,
    void,
    when,
  )
import Data.Function (fix)
import Data.Maybe
  ( fromMaybe,
    isNothing,
  )
import Data.Proxy (Proxy (Proxy))
import MessageBoxCommon (testContentionRobustness)
import UnliftIO.MessageBox.Util.Future (tryNow)
import UnliftIO.MessageBox.Class
  ( IsInput (deliver),
    IsMessageBox (newInput, receive, tryReceive),
    IsMessageBoxArg (MessageBox, newMessageBox),
    receiveAfter,
  )
import UnliftIO.MessageBox.Limited
  ( BlockingBoxLimit (..),
    MessageLimit (..),
    NonBlockingBoxLimit (..),
    WaitingBoxLimit (..),
    messageLimitToInt,
  )
import QCOrphans ()
import Test.QuickCheck
  ( Positive (Positive),
    Small (Small),
    ioProperty,
    withMaxSuccess,
    (==>),
  )
import Test.Tasty as Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as Tasty
  ( assertBool,
    assertEqual,
    testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck as Tasty (testProperty)
import UnliftIO (concurrently, race, timeout)
import UnliftIO.Concurrent (threadDelay)
import Utils (allEnumMethodsImplemented, allEqOrdShowMethodsImplemented)

test :: Tasty.TestTree
test =
  testGroup
    "UnliftIO.MessageBox.Limited"
    [ testBlockingBox,
      testWaitingBox,
      testNonBlockingBox,
      testCommon BlockingBoxLimit,
      testCommon NonBlockingBoxLimit,
      testCommon (WaitingBoxLimit Nothing 5_000_000),
      testCommon (WaitingBoxLimit (Just (48 * 3600_000_000)) 5_000_000),
      derivedInstances
    ]

testCommon ::
  forall boxCfg cfg.
  ( IsMessageBoxArg boxCfg,
    Show boxCfg,
    cfg ~ MessageBox boxCfg
  ) =>
  (MessageLimit -> boxCfg) ->
  Tasty.TestTree
testCommon mkCfg =
  let mkBox :: MessageLimit -> IO (cfg a)
      mkBox = newMessageBox . mkCfg
   in Tasty.testGroup
        ("Common Functionality for " <> show (mkCfg MessageLimit_64) <> " Note: Tests may vary the limit")
        [ testProperty "when createBox is applied to a limit > 0, then limit messages can be enqueued" $ \limit ->
            limit <= MessageLimit_128
              ==> ioProperty
              $ do
                i <- mkBox limit :: IO (cfg String)
                o <- newInput i
                results <- replicateM (messageLimitToInt limit) (deliver o "test message")
                pure $ last results,
          testProperty "when the message box is 'full' not more then one more messages can be enqueued" $
            \limit ->
              ioProperty $ do
                let nOfMessagesThatCanBeWritten = messageLimitToInt limit
                i <- mkBox limit
                o <- newInput i
                let countDeliveries n =
                      timeout 100_000 (deliver o "some test message")
                        >>= maybe (return n) (\ok -> if ok then countDeliveries (n + 1) else return n)
                nDelivered <- countDeliveries 0
                assertBool
                  ( "at least "
                      <> show nDelivered
                      <> " messages should be delivered, in a queue of minimum size "
                      <> show nOfMessagesThatCanBeWritten
                  )
                  (nDelivered >= nOfMessagesThatCanBeWritten)
                assertBool
                  ( "at most "
                      <> show nDelivered
                      <> " messages may be delivered, in a queue of minimum size "
                      <> show nOfMessagesThatCanBeWritten
                  )
                  (nDelivered <= 2 * nOfMessagesThatCanBeWritten),
          Tasty.testCase "When the message limit is 2, two messages can be enqueued" $ do
            i <- mkBox MessageLimit_2
            o <- newInput i
            r1 <- deliver o "Messge 1"
            r1 @?= True
            r2 <- deliver o "Messge 2"
            r2 @?= True,
          Tasty.testCase "when writing into an empty Input, True is returned" $ do
            i <- mkBox MessageLimit_32
            o <- newInput i
            deliver o "Stop the climate crisis" >>= (@?= True),
          Tasty.testCase "when writing into a full Input deliver returns False or blocks forever" $ do
            i <- mkBox MessageLimit_64
            o <- newInput i
            fix $ \next ->
              timeout 100_000 (deliver o "Stop the climate crisis") >>= \case
                Just False -> assertBool "expected 'False' when the queue is full" True
                Just True -> next
                Nothing -> return (),
          testProperty
            "when messages are send until the limit is reached,\
            \and when x, x < limit messages are received, then \
            \x more messages can be delivered again, and all \
            \messages are in FIFO order"
            $ \limit (Positive (Small x)) ->
              x < messageLimitToInt limit
                ==> withMaxSuccess 20
                $ ioProperty $
                  do
                    i <- mkBox limit
                    -- write until the real limit is reached
                    nFirstMessages <- do
                      o <- newInput i
                      let go msgId
                            | msgId < messageLimitToInt limit = do
                              success <-
                                fromMaybe False
                                  <$> timeout
                                    50_000
                                    (deliver o msgId)

                              if success
                                then go (msgId + 1)
                                else return msgId
                            | otherwise = return msgId
                      go 0

                    assertBool
                      ( "internal error less than 'limit' messages could be delivered: "
                          <> show nFirstMessages
                          <> " < "
                          <> show limit
                      )
                      (nFirstMessages >= messageLimitToInt limit)

                    firstXReceived <- replicateM x (receive i)
                    assertEqual "could not receive x messages" (Just <$> [0 .. x -1]) firstXReceived
                    do
                      nNextMessages <- do
                        o <- newInput i
                        maybe 0 length
                          <$> timeout
                            5_000_000
                            (mapM (deliver o) [nFirstMessages .. nFirstMessages + x - 1])
                      assertEqual
                        "unexpected number of messages delivered"
                        x
                        nNextMessages
                    nextReceived <- replicateM nFirstMessages (receive i)
                    assertEqual
                      "could not receive x messages"
                      (Just <$> [x .. x + nFirstMessages -1])
                      nextReceived,
          let queueSize = MessageLimit_512
           in testContentionRobustness (mkBox queueSize :: IO (cfg (Int, Int))) (messageLimitToInt queueSize) 50
        ]

testBlockingBox :: TestTree
testBlockingBox =
  Tasty.testGroup
    "BlockingBox"
    [ Tasty.testCase "receive on an empty queue blocks forever" $ do
        i <- newMessageBox (BlockingBoxLimit MessageLimit_16)
        let foreverInMicros = 100_000
        receiveAfter i foreverInMicros
          >>= assertEqual
            "timeout expected"
            (Nothing :: Maybe Int),
      Tasty.testCase "tryReceive from an empty queue returns future that returns Nothing" $ do
        i <- newMessageBox (BlockingBoxLimit MessageLimit_16)
        f <- tryReceive i
        timeout 1_000_000 (tryNow f)
          >>= assertEqual "the future must not block and should return Nothing" (Just (Nothing @Int)),
      Tasty.testCase "when a message box is full, deliver will block forever" $ do
        i <- newMessageBox (BlockingBoxLimit MessageLimit_16)
        o <- newInput i
        let sendWhileNotFull = do
              success <- deliver o (666 :: Int)
              when success sendWhileNotFull
        timeout 1_000_000 sendWhileNotFull
          >>= assertBool "deliver must block" . isNothing,
      Tasty.testProperty
        "when sending faster than receiving all messages must be received"
        $ \queueSize -> withMaxSuccess 20 $
          ioProperty $
            do
              let nGood = 256
                  tSend = 200
                  tRecv = 5 * tSend

              receiverIn <- newMessageBox (BlockingBoxLimit queueSize)
              receiverOut <- newInput receiverIn
              let doReceive =
                    replicateM
                      nGood
                      ( threadDelay tRecv
                          *> receive receiverIn
                      )

              let doSend =
                    replicateM
                      nGood
                      ( threadDelay tSend
                          *> deliver receiverOut ("Test Message" :: String)
                      )

              (received, sendResults) <- concurrently doReceive doSend
              assertEqual "not all messages could be sent" (replicate nGood True) sendResults
              assertEqual "not all messages could be received" (replicate nGood (Just "Test Message")) received
    ]

testWaitingBox :: TestTree
testWaitingBox =
  testGroup
    "WaitingBox"
    [ Tasty.testCase "when the receive timeout is Nothing receive blocks forever" $ do
        i <- newMessageBox (WaitingBoxLimit Nothing 123 MessageLimit_16)
        let foreverInMicros = 100_000
        timeout foreverInMicros (receive i)
          >>= assertEqual
            "receive did not block"
            (Nothing :: Maybe (Maybe Int)),
      Tasty.testCase "when the receive timeout is Just x, receive returns Nothing after x micro seconds" $ do
        i <- newMessageBox (WaitingBoxLimit (Just 234) 123 MessageLimit_16)
        let foreverInMicros = 100_000
        timeout foreverInMicros (receive i)
          >>= assertEqual
            "receive did not timeout"
            (Just (Nothing :: Maybe Int)),
      Tasty.testCase
        "when the receive timeout is Just x, and another thread waits 2*x and delivers a message, receive returns Nothing"
        $ do
          let m1 = "Message 1" :: String
          mbox <- newMessageBox (WaitingBoxLimit (Just 150_000) 1_000 MessageLimit_16)
          input <- newInput mbox
          (senderOk, receiverOK) <-
            concurrently
              ( do
                  threadDelay 300_000
                  s1Ok <- deliver input m1
                  return
                    ( assertBool "delivering message failed" s1Ok
                    )
              )
              ( do
                  r <- receive mbox
                  return $
                    assertEqual "wrong message received!" Nothing r
              )
          senderOk
          receiverOK,
      Tasty.testCase
        "when the receive timeout is Just 150_000, and another thread waits 1000 and delivers a message, receive returns Just that message"
        $ do
          let m1 = "Message 1" :: String
          mbox <- newMessageBox (WaitingBoxLimit (Just 150_000) 1_000 MessageLimit_16)
          input <- newInput mbox
          (senderOk, receiverOK) <-
            concurrently
              ( do
                  threadDelay 1000
                  s1Ok <- deliver input m1
                  return
                    ( assertBool "delivering message failed" s1Ok
                    )
              )
              ( do
                  r <- receive mbox
                  return $
                    assertEqual "wrong message received!" (Just m1) r
              )
          senderOk
          receiverOK,
      Tasty.testCase "tryReceive from an empty queue returns a future that returns Nothing" $ do
        i <- newMessageBox (WaitingBoxLimit (Just 234) 123 MessageLimit_16)
        f <- tryReceive i
        timeout 1_000_000 (tryNow f)
          >>= assertEqual "the future must not block and should return Nothing" (Just (Nothing @Int)),
      Tasty.testCase "when a message box is full, deliver return False after the timeout elapsed" $ do
        i <- newMessageBox (WaitingBoxLimit (Just 234) 123 MessageLimit_16)
        o <- newInput i
        let sendWhileNotFull = do
              success <- deliver o (666 :: Int)
              when success sendWhileNotFull
        timeout 1_000_000 (sendWhileNotFull >> deliver o (666 :: Int))
          >>= assertEqual "deliver must return False after the timeout has elapsed" (Just False),
      Tasty.testProperty
        "when sending faster than receiving\
        \ and when the timeout is sufficiently large, all messages must be received"
        $ \limit -> withMaxSuccess 20 $
          ioProperty $
            do
              let nGood = 256
                  tSend = 200
                  tRecv = 5 * tSend

              receiverIn <- newMessageBox (WaitingBoxLimit Nothing 5_000_000 limit)
              receiverOut <- newInput receiverIn
              let doReceive =
                    replicateM
                      nGood
                      ( threadDelay tRecv
                          *> receive receiverIn
                      )

              let doSend =
                    replicateM
                      nGood
                      ( threadDelay tSend
                          *> deliver receiverOut ("Test Message" :: String)
                      )

              (received, sendResults) <- concurrently doReceive doSend
              assertEqual "not all messages could be sent" (replicate nGood True) sendResults
              assertEqual "not all messages could be received" (replicate nGood (Just "Test Message")) received,
      Tasty.testCase "When the message limit is 2, two messages can be enqueued" $ do
        i <- newMessageBox (WaitingBoxLimit Nothing 5_000_000 MessageLimit_2)
        o <- newInput i
        r1 <- deliver o "Messge 1"
        r1 @?= True
        r2 <- deliver o "Messge 2"
        r2 @?= True,
      Tasty.testCase "when writing into a full box, deliver returns False after the timeout" $ do
        i <- newMessageBox (WaitingBoxLimit Nothing 123 MessageLimit_32)
        o <- newInput i
        fix $ \next ->
          deliver o "Stop the climate crisis" >>= \case
            False -> return ()
            True -> next
        deliver o "foo bar"
          >>= assertBool "expect tryToDeliverAndWait to return False (timeout)" . not,
      testGroup
        "receive with/without timeout"
        [ Tasty.testCase "when a receive timeout is Just t, and no message is delivered, receive returns Nothing after the timeout" $ do
            i <- newMessageBox (WaitingBoxLimit (Just 10) 123 MessageLimit_8)
            receive i
              >>= assertEqual "timeout expected" (Nothing :: Maybe String),
          Tasty.testCase "when a receive timeout is Nothing, and no message is delivered, receive never returns" $ do
            i <- newMessageBox (WaitingBoxLimit Nothing 123 MessageLimit_8)
            let foreverInMicros = 100_000
            timeout foreverInMicros (receive i)
              >>= assertEqual "expected receive to block forever" (Nothing :: Maybe (Maybe String))
        ]
    ]

testNonBlockingBox :: TestTree
testNonBlockingBox =
  Tasty.testGroup
    "NonBlockingBox"
    [ Tasty.testCase
        "only delivery is non-blocking, when a message box has no inputs then receive blocks forever"
        $ do
          mbox <- newMessageBox (NonBlockingBoxLimit MessageLimit_16)
          r <- timeout 100_000 (receive mbox)
          assertEqual "receive must not return" (Nothing :: Maybe (Maybe String)) r,
      Tasty.testCase "when writing into a full Input deliver returns False " $ do
        i <- newMessageBox (NonBlockingBoxLimit MessageLimit_16)
        o <- newInput i
        fix $ \next ->
          deliver o "Stop the climate crisis" >>= \case
            False -> return ()
            True -> next
        deliver o "foo bar"
          >>= assertBool "deliver should not have succeeded" . not,
      Tasty.testCase
        "when using a limit smaller than half of the number\
        \ of messsages in the test, deliver first succeeds and then fails"
        $ do
          let lGood = MessageLimit_128
              nGood = messageLimitToInt lGood
              nAmbigous = 2 * nGood
              nBad = 123

          receiverOut <- newMessageBox (NonBlockingBoxLimit lGood) >>= newInput
          let doReceive = threadDelay 300_000_000

          let good = [("Test-Message GOOD", Just n) | n <- [1 .. nGood :: Int]]
              ambigous = [("Test-Message ambigous", Just n) | n <- [1 .. nAmbigous :: Int]]
              bad = [("Test-Message Base", Just n) | n <- [1 .. nBad :: Int]]

          let doSend =
                traverse (deliver receiverOut) good
                  <> traverse (fmap (const True) . deliver receiverOut) ambigous
                  <> traverse (deliver receiverOut) bad
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
        "when sending 5 times faster then receiving, then deliver \
        \will begin to fail after some time"
        $ do
          let lGood = MessageLimit_128
              nGood = messageLimitToInt lGood
              nAmbigous = 2 * nGood + 1
              nBad = nGood
              tSend = 500
              tRecv = 5 * tSend

          receiverIn <- newMessageBox (NonBlockingBoxLimit lGood)
          receiverOut <- newInput receiverIn
          let doReceive = void (forever (receive receiverIn >> threadDelay tRecv))

          let !good = [("Test-Message GOOD", Just n) | !n <- [1 .. nGood :: Int]]
              !ambigous = [("Test-Message ambigous", Just n) | !n <- [1 .. nAmbigous :: Int]]
              !bad = [("Test-Message Base", Just n) | !n <- [1 .. nBad :: Int]]

          let doSend =
                traverse (\ !m -> threadDelay tSend >> deliver receiverOut m) good
                  <> traverse (\ !m -> threadDelay tSend >> deliver receiverOut m >> return True) ambigous
                  <> traverse (\ !m -> threadDelay tSend >> deliver receiverOut m) bad
          Right result <-
            race
              doReceive
              doSend
          let !resultGoodPart = take nGood result
              !resultBad = drop (nGood + nAmbigous) result

          assertBool "first messages succeed" (and resultGoodPart)
          assertBool "last messages fail" (not $ and resultBad)
    ]

-- This ONLY exists because I wanted the test-code coverage to be 100%
derivedInstances :: TestTree
derivedInstances =
  testGroup
    "Derived instances"
    [ -- this is just to get to 100% code coverage for
      -- even for derived instances
      testProperty
        "MessageLimit code coverage"
        (allEnumMethodsImplemented (Proxy @MessageLimit)),

      testProperty
        "MessageLimit code coverage"
        (allEqOrdShowMethodsImplemented (Proxy @MessageLimit)),

      testProperty
        "BlockingBoxLimit code coverage"
        (allEqOrdShowMethodsImplemented (Proxy @BlockingBoxLimit)),

      testProperty
        "NonBlockingBoxLimit code coverage"
        (allEqOrdShowMethodsImplemented (Proxy @NonBlockingBoxLimit)),

      testProperty
        "WaitingBoxLimit code coverage"
        (allEqOrdShowMethodsImplemented (Proxy @WaitingBoxLimit))      
    ]