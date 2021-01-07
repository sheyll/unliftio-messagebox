{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MessageBoxClassTest (test) where

import Control.Monad (forM, replicateM)
import Data.Foldable (Foldable (fold))
import Data.Maybe (isJust)
import Data.Monoid (All (All, getAll))
import Protocol.MessageBox.Class
  (receiveAfter,  IsInput (..),
    IsMessageBox (..),
    IsMessageBoxFactory (..),
    tryNow,
  )
import qualified Protocol.MessageBox.Limited as B
import qualified Protocol.MessageBox.Unlimited as U
import Test.QuickCheck
  ( Positive (Positive),
    Small (Small),
    ioProperty,
  )
import Test.Tasty as Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as Tasty (testProperty)
import UnliftIO (conc, concurrently, runConc)
import UnliftIO.Concurrent (threadDelay)

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocol.MessageBox.Class"
    [ -- TODO receive tests
      -- TODO deliver tests
      -- TODO Timeout tests
      -- TODO CatchExceptions tests

      testWith U.UnlimitedMessageBox,
      testWith (B.BlockingBoxLimit B.MessageLimit_64)
    ]

testWith ::
  (IsMessageBoxFactory cfg, Show cfg) =>
  cfg ->
  TestTree
testWith arg =
  Tasty.testGroup
    (show arg)
    [ commonFunctionality arg,
      Tasty.testGroup
        "MessageBox Wrapper"
        [ withTimeoutTest arg
        ]
    ]

realWorldTest ::
  (IsMessageBoxFactory cfg) =>
  cfg ->
  TestTree
realWorldTest arg =
  testProperty "all n messages of all k outBoxes are received by the output" $
    \(Positive (Small n)) (Positive (Small k)) ->
      ioProperty $
        getAll . fold <$> do
          output <- newMessageBox arg
          runConc
            ( (<>)
                <$> foldMap
                  ( \i ->
                      conc
                        ( do
                            input <- newInput output
                            forM [0 .. n - 1] (\j -> All <$> deliver input ("test message: ", i, j))
                        )
                  )
                  [0 .. k - 1]
                <*> conc (replicateM (n * k) (All . isJust <$> receive output))
            )

withTimeoutTest ::
  forall cfg.
  (IsMessageBoxFactory cfg, Show cfg) =>
  cfg ->
  TestTree
withTimeoutTest arg =
  testGroup
    (show arg)
    [ testGroup
        "WithTimeout"
        [ testCase
            "when receive is called, and two different messages are delivered late but not too late, \
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
                      r <- receiveAfter mbox 200_000 (pure Nothing)
                      return $
                        case r of
                          Nothing -> assertFailure "No message received!"
                          Just m -> assertEqual "wrong message received!" m1 m
                  )
              senderOk
              receiverOK
        ]
    ]

-- standard tests
commonFunctionality ::
  (IsMessageBoxFactory cfg) =>
  cfg ->
  TestTree
commonFunctionality arg =
  let futureLoop f = tryNow f >>= maybe (threadDelay 10 >> futureLoop f) return
   in testGroup
        "Common Funcionality"
        [ testGroup
            "deliver and receive"
            [ testCase
                "when a process delivers a message into an input successfully, \
                \and then calls receive, the message is returned"
                $ do
                  let m1 = "Message 1" :: String
                  mbox <- newMessageBox arg
                  input <- newInput mbox
                  do
                    s1Ok <- deliver input m1
                    assertBool "delivering message 1 failed" s1Ok
                  r <- receiveAfter mbox 200_000 (pure Nothing)
                  case r of
                    Nothing -> assertFailure "No message received!"
                    Just m -> assertEqual "wrong message received!" m1 m,
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
                $ do
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
                  receiverOK
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
                $ do
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
            ],
          realWorldTest arg
        ]