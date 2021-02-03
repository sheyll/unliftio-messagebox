module MessageBoxClassTest (test) where

import Control.Monad (forM, replicateM, when)
import Data.List (sort)
import Test.Tasty as Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import qualified Test.Tasty.HUnit as Tasty
import UnliftIO
  ( conc,
    concurrently,
    runConc,
    timeout,
  )
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.MessageBox.CatchAll
  ( CatchAllArg (CatchAllArg),
  )
import UnliftIO.MessageBox.Class
  ( IsInput (..),
    IsMessageBox (..),
    IsMessageBoxArg (..),
    deliver,
    handleMessage,
    newInput,
    receive,
  )
import qualified UnliftIO.MessageBox.Limited as B
import qualified UnliftIO.MessageBox.Unlimited as U
import UnliftIO.MessageBox.Util.Future (tryNow)
import Utils
  ( NoOpBox (OnReceive),
    untilJust,
    untilM,
  )

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "UnliftIO.MessageBox.Class"
    [ utilTest,
      testWith U.BlockingUnlimited,
      testWith $ CatchAllArg U.BlockingUnlimited,
      testWith (B.BlockingBoxLimit B.MessageLimit_256),
      testWith $ CatchAllArg (B.BlockingBoxLimit B.MessageLimit_256),
      testWith (B.BlockingBoxLimit B.MessageLimit_1),
      testWith (B.BlockingBoxLimit B.MessageLimit_2),
      testWith (B.NonBlockingBoxLimit B.MessageLimit_1),
      testWith (B.NonBlockingBoxLimit B.MessageLimit_64),
      testWith (B.WaitingBoxLimit Nothing 60_000_000 B.MessageLimit_1),
      testWith (B.WaitingBoxLimit Nothing 60_000_000 B.MessageLimit_64),
      testWith (B.WaitingBoxLimit (Just 60_000_000) 60_000_000 B.MessageLimit_64)
    ]

utilTest :: TestTree
utilTest =
  testGroup
    "Utilities"
    [ testCase
        "when receive returns Nothing, then handleMessage\
        \ does not execute the callback and returns Nothing"
        $ handleMessage (OnReceive Nothing Nothing) (const (return ()))
          >>= assertEqual "handleMessage should return Nothing" Nothing,
      testCase
        "when receive returns Just x, then handleMessage\
        \ applies the callback to x and returns Just the result"
        $ handleMessage (OnReceive Nothing (Just ("123" :: String))) return
          >>= assertEqual "handleMessage should return Just the result" (Just "123")
    ]

testWith ::
  (IsMessageBoxArg cfg, Show cfg) =>
  cfg ->
  TestTree
testWith arg =
  Tasty.testGroup
    (show arg)
    [ commonFunctionality arg
    ]

-- standard tests
commonFunctionality ::
  (IsMessageBoxArg cfg) =>
  cfg ->
  TestTree
commonFunctionality arg =
  let futureLoop f = tryNow f >>= maybe (threadDelay 10 >> futureLoop f) return
   in testGroup
        "Basics"
        [ testGroup
            "deliver and receive"
            [ Tasty.testCase "tryReceive from an empty queue, without any writer threads" $ do
                i <- newMessageBox arg
                f <- tryReceive i
                timeout 1_000_000 (tryNow f)
                  >>= assertEqual
                    "the future must not block and should return be emtpy"
                    (Just (Nothing @Int)),
              testCase
                "when a process delivers a message into an input successfully, \
                \and then calls receiveAfter, the message is returned"
                $ do
                  let m1 = "Message 1" :: String
                  mbox <- newMessageBox arg
                  input <- newInput mbox
                  do
                    s1Ok <- deliver input m1
                    assertBool "delivering message 1 failed" s1Ok
                  r <- receiveAfter mbox 200_000
                  case r of
                    Nothing -> assertFailure "No message received!"
                    Just m -> assertEqual "wrong message received!" m1 m,
              testCase
                "when receiveAfter is called, and two different messages are delivered late but not too late, \
                \then the first message is returned"
                $ do
                  let m1 = "Message 1" :: String
                      m2 = "Message 2" :: String
                  mbox <- newMessageBox arg
                  input <- newInput mbox
                  (senderOk, receiverOK) <-
                    concurrently
                      ( do
                          threadDelay 10_000
                          s1Ok <- deliver input m1
                          s2Ok <- deliver input m2
                          return
                            ( assertBool "delivering message 1 failed" s1Ok
                                >> assertBool "delivering message 2 failed" s2Ok
                            )
                      )
                      ( do
                          r <- receiveAfter mbox 200_000
                          return $
                            case r of
                              Nothing -> assertFailure "No message received!"
                              Just m -> assertEqual "wrong message received!" m1 m
                      )
                  senderOk
                  receiverOK,
              testCase
                "when one process delivers a message into an input successfully, \
                \while another process calls receive, the message is returned"
                $ do
                  let m1 = "Message 1" :: String
                  mbox <- newMessageBox arg
                  input <- newInput mbox
                  (senderOk, receiverOK) <-
                    concurrently
                      ( do
                          s1Ok <- deliver input m1
                          return $ do
                            assertBool "delivering message 1 failed" s1Ok
                      )
                      ( do
                          r1 <- receive mbox
                          return $ do
                            case r1 of
                              Nothing -> assertFailure "No message received!"
                              Just m -> assertEqual "wrong message received!" m1 m
                      )
                  senderOk
                  receiverOK,
              testCase
                "when one process delivers two messages into an input successfully, \
                \while another process calls receive twice, the messages are returned in FIFO order"
                $ when (maybe True (> 1) (getConfiguredMessageLimit arg)) $ do
                  let m1 = "Message 1" :: String
                      m2 = "Message 2" :: String
                  mbox <- newMessageBox arg
                  input <- newInput mbox
                  (senderOk, receiverOK) <-
                    concurrently
                      ( do
                          s1Ok <- deliver input m1
                          s2Ok <- deliver input m2
                          return $ do
                            assertBool "delivering message 1 failed" s1Ok
                            assertBool "delivering message 2 failed" s2Ok
                      )
                      ( do
                          r1 <- receive mbox
                          r2 <- receive mbox
                          return $ do
                            case r1 of
                              Nothing -> assertFailure "No message received!"
                              Just m -> assertEqual "wrong message received!" m1 m
                            case r2 of
                              Nothing -> assertFailure "No message received!"
                              Just m -> assertEqual "wrong message received!" m2 m
                      )
                  senderOk
                  receiverOK,
              testCase "all 113 messages of all 237 outBoxes are received by the messageBox" $ do
                let n = 113
                    k = 237
                    expected =
                      [ ("test message: " :: String, i, j)
                        | i <- [0 .. k - 1],
                          j <- [0 .. n - 1]
                      ]
                res <-
                  timeout
                    60_000_000
                    ( do
                        messageBox <- newMessageBox arg
                        runConc
                          ( foldMap
                              ( \i ->
                                  conc $ do
                                    input <- newInput messageBox
                                    forM
                                      [0 .. n - 1]
                                      ( \j ->
                                          untilM
                                            (deliver input ("test message: ", i, j) <* threadDelay 10)
                                      )
                              )
                              [0 .. k - 1]
                              *> conc
                                ( replicateM
                                    (n * k)
                                    ( untilJust
                                        (receive messageBox <* threadDelay 10)
                                    )
                                )
                          )
                    )
                assertEqual "bad result" (Just expected) (fmap sort res)
            ],
          testGroup
            "tryReceive"
            [ testCase
                "when one process delivers one messages into an input successfully, \
                \while another process calls tryReceive, then the future returns the \
                \message, and a second tryReceives returns a future that returns Nothing"
                $ do
                  let m1 = "Message 1" :: String
                  mbox <- newMessageBox arg
                  input <- newInput mbox
                  (senderOk, receiverOK) <-
                    concurrently
                      ( do
                          s1Ok <- deliver input m1
                          return $ do
                            assertBool "delivering message 1 failed" s1Ok
                      )
                      ( do
                          f1 <- tryReceive mbox
                          f2 <- tryReceive mbox
                          rm1 <- futureLoop f1
                          rm2 <- tryNow f2
                          return $ do
                            assertEqual "wrong message 1 received!" m1 rm1
                            assertEqual "wrong message 2 received!" Nothing rm2
                      )
                  senderOk
                  receiverOK,
              testCase
                "when one process delivers one messages into an input successfully, \
                \while another process calls tryReceive, then the future returns the \
                \message, and a second tryReceives returns a future that returns Nothing, \
                \then a second message is deliverd, and the second future returns that value,\
                \and the first future still returns the first value(immediately)."
                $ do
                  let m1 = "Message 1" :: String
                      m2 = "Message 2" :: String
                  mbox <- newMessageBox arg
                  input <- newInput mbox
                  (senderOk, (f1, f2, receiverOK)) <-
                    concurrently
                      ( do
                          s1Ok <- deliver input m1
                          return $ do
                            assertBool "delivering message 1 failed" s1Ok
                      )
                      ( do
                          f1 <- tryReceive mbox
                          f2 <- tryReceive mbox
                          rm1 <- futureLoop f1
                          rm2 <- tryNow f2
                          return
                            ( f1,
                              f2,
                              do
                                assertEqual "wrong message 1 received!" m1 rm1
                                assertEqual "wrong message 2 received!" Nothing rm2
                            )
                      )
                  senderOk
                  receiverOK
                  deliver input m2 >>= assertBool "delivering message 2 failed"
                  tryNow f1 >>= assertEqual "first future contains in invalid message" (Just m1)
                  futureLoop f2 >>= assertEqual "second future contains in invalid message" m2,
              testCase
                "when one process delivers two messages (with random delays) into an input successfully, \
                \while another process calls tryReceive twice, then the futures each return the \
                \correct message in FIFO order"
                $ when (maybe True (> 1) (getConfiguredMessageLimit arg)) $ do
                  let m1 = "Message 1" :: String
                      m2 = "Message 2" :: String
                  mbox <- newMessageBox arg
                  input <- newInput mbox
                  (senderOk, receiverOK) <-
                    concurrently
                      ( do
                          threadDelay 20_000
                          s1Ok <- deliver input m1
                          threadDelay 20_000
                          s2Ok <- deliver input m2
                          return $ do
                            assertBool "delivering message 1 failed" s1Ok
                            assertBool "delivering message 2 failed" s2Ok
                      )
                      ( do
                          f1 <- tryReceive mbox
                          f2 <- tryReceive mbox
                          (rm1, rm2) <- concurrently (futureLoop f1) (futureLoop f2)
                          return $ do
                            assertEqual "wrong message 1 received!" m1 rm1
                            assertEqual "wrong message 2 received!" m2 rm2
                      )
                  senderOk
                  receiverOK
            ]
        ]
