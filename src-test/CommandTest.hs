{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CommandTest where

import Control.Applicative
  ( Applicative (pure, (<*>)),
    (<$),
    (<$>),
  )
import Data.Functor (void, ($>))
import Data.Maybe (Maybe (), fromJust, fromMaybe, isJust, isNothing, maybe)
import Data.Semigroup (All (All, getAll))
import GHC.IO.Exception (userError)
import Protocol.Command
  ( Command,
    CommandError (..),
    DuplicateReply (..),
    Message (Blocking, NonBlocking),
    ReturnType (FireAndForget, Return),
    call,
    callAsync,
    cast,
    delegateCall,
    replyTo,
    tryTakeReply,
    waitForReply,
  )
import Protocol.Command.CallId (CallId (MkCallId))
import qualified Protocol.Command.CallId as CallId
import Protocol.Fresh (CounterVar, fresh, newCounterVar)
import Protocol.Future (Future (Future))
import Protocol.MessageBox.Class
  ( IsInput (..),
    IsMessageBox (..),
    IsMessageBoxFactory (..),
    handleMessage,
  )
import Protocol.MessageBox.Limited
  ( BlockingBoxLimit (BlockingBoxLimit),
    MessageLimit (..),
  )
import Protocol.MessageBox.Unlimited (UnlimitedMessageBox (UnlimitedMessageBox))
import RIO
  ( Bool (..),
    Either (Left, Right),
    Eq ((==)),
    Foldable (foldMap),
    Int,
    Maybe (..),
    Monad (return, (>>=)),
    MonadIO (liftIO),
    MonadUnliftIO,
    Num ((*), (+)),
    Ord ((>=)),
    Semigroup ((<>)),
    Show (..),
    String,
    Traversable (traverse),
    conc,
    concurrently,
    const,
    error,
    flip,
    newEmptyMVar,
    putMVar,
    readTVarIO,
    registerDelay,
    runConc,
    runRIO,
    threadDelay,
    throwTo,
    timeout,
    traverse_,
    tryReadMVar,
    ($),
    (.),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Large (getLarge),
    NonEmptyList (getNonEmpty),
    Positive (..),
    PrintableString (..),
    Property,
    ioProperty,
    testProperty,
  )
import UnliftIO
  ( takeMVar,
    try,
  )
import UnliftIO.Concurrent (forkIO)
import Utils (untilJust, withCallIds)
import Prelude (Show (showsPrec), showParen, showString)

test :: TestTree
test =
  testGroup
    "Protocol.Command"
    [ unitTests,
      integrationTests
    ]

integrationTests :: TestTree
integrationTests =
  testGroup
    "integration tests"
    [ testCase "Show instance of the Message data family works" $
        CallId.newCallIdCounter
          >>= \cv -> runRIO cv $ do
            bookStoreOutput <- newMessageBox UnlimitedMessageBox
            bookStoreInput <- newInput bookStoreOutput
            let blockingMsg = GetBooks
                nonblockingMsg =
                  Donate
                    (Donor (PersonName "A" "B") 1)
                    ( Book
                        "Unsere Welt neu denken: Eine Einladung"
                        (Author (PersonName "Maya" "Goepel") 208)
                        (Publisher "Ullstein")
                        (Year 2020)
                        []
                    )
            (_, shownBlockingMsg) <-
              concurrently
                (call bookStoreInput blockingMsg 1_000_000)
                (handleMessage bookStoreOutput (return . show))
            (_, shownNonBlockingMsg) <-
              concurrently
                (cast bookStoreInput nonblockingMsg)
                (handleMessage bookStoreOutput (return . show))
            liftIO $
              assertEqual
                "bad Show instance"
                (Just ("B: " <> showsPrec 9 blockingMsg " 1"))
                shownBlockingMsg
            liftIO $
              assertEqual
                "bad Show instance"
                (Just ("NB: " <> showsPrec 9 nonblockingMsg ""))
                shownNonBlockingMsg,
      testProperty
        "all books that many donors concurrently donate into the book store end up in the bookstore"
        allDonatedBooksAreInTheBookStore,
      testCase "handling a cast succeeds" $ do
        bookStoreOutput <- newMessageBox UnlimitedMessageBox
        bookStoreInput <- newInput bookStoreOutput
        let expected =
              Donate
                (Donor (PersonName "Mueller" "Hans") 1)
                ( Book
                    "Wann wenn nicht wir."
                    (Author (PersonName "Schmitt" "Agent") 477)
                    (Publisher "Kletten Verlag")
                    (Year 2019)
                    []
                )
        wasHandleMessageCalled <- newEmptyMVar
        void (forkIO (void (cast bookStoreInput expected)))
        result <- handleMessage bookStoreOutput $
          \case
            NonBlocking actual -> do
              putMVar wasHandleMessageCalled ()
              assertEqual "correct message" expected actual
            a ->
              assertFailure ("unexpected message: " <> show a)
        tryReadMVar wasHandleMessageCalled >>= assertBool "handle message must be called" . isJust
        assertBool "handling the message was successful" (isJust result),
      testCase "handling a call succeeds" $ do
        freshCounter <- newCounterVar
        runRIO (MkBookStoreEnv {_fresh = freshCounter}) $ do
          let expectedBooks =
                [ Book
                    "Wann wenn nicht wir."
                    (Author (PersonName "Schmitt" "Agent") 477)
                    (Publisher "Kletten Verlag")
                    (Year 2019)
                    []
                ]

          bookStoreOutput <- newMessageBox UnlimitedMessageBox
          bookStoreInput <- newInput bookStoreOutput
          let concurrentCallAction = call bookStoreInput GetBooks 100
              concurrentBookStore = handleMessage bookStoreOutput $ \case
                Blocking GetBooks replyBox -> do
                  Nothing <$ replyTo replyBox expectedBooks
                NonBlocking a ->
                  pure (Just ("unexpected message: " <> show a))
          (callResult, handleResult) <- concurrently concurrentCallAction concurrentBookStore
          liftIO $ do
            case callResult of
              Left err ->
                assertFailure $ "failed unexpectedly: " <> show err
              Right actualBooks ->
                assertEqual "call should return the right books" actualBooks expectedBooks
            case handleResult of
              Nothing ->
                assertFailure "could not receive message in handleMessage"
              Just Nothing -> return ()
              Just (Just err) ->
                assertFailure err,
      testCase "reply to dead caller does not crash" $ do
        let delayDuration = 1000
        freshCounter <- newCounterVar
        runRIO (MkBookStoreEnv {_fresh = freshCounter}) $ do
          bookStoreOutput <- newMessageBox UnlimitedMessageBox
          bookStoreInput <- newInput bookStoreOutput
          let concurrentCallAction = do
                tId <- forkIO (void $ call bookStoreInput GetBooks (delayDuration * 3))
                threadDelay (1 * delayDuration)
                throwTo tId $ userError "uh-oh"

              concurrentBookStore = handleMessage bookStoreOutput $ \case
                Blocking GetBooks replyBox -> do
                  threadDelay (2 * delayDuration)
                  replyTo replyBox []
                  pure True
                NonBlocking a ->
                  error (show a)
          (_callResult, handleResult) <- concurrently concurrentCallAction concurrentBookStore
          liftIO $
            assertEqual "expect bookStore to survive a failed reply" (Just True) handleResult,
      testCase
        "call to died thread returns Nothing"
        $ do
          let delayDuration = 1000_000
          freshCounter <- newCounterVar
          runRIO (MkBookStoreEnv {_fresh = freshCounter}) $ do
            bookStoreOutput <- newMessageBox UnlimitedMessageBox
            bookStoreInput <- newInput bookStoreOutput
            let concurrentCallAction = call bookStoreInput GetBooks (delayDuration * 2)
                concurrentBookStore = handleMessage bookStoreOutput $ \case
                  Blocking GetBooks _replyBox -> do
                    threadDelay delayDuration
                    pure Nothing
                  NonBlocking a ->
                    pure (Just ("unexpected message: " <> show a))
            (callResult, handleResult) <- concurrently concurrentCallAction concurrentBookStore
            liftIO $ do
              callId' <- runRIO freshCounter fresh
              assertEqual "no message other than GetBooks received" (Just Nothing) handleResult
              case callResult of
                (Left (BlockingCommandTimedOut (MkCallId c)))
                  | MkCallId (c + 1) == callId' -> pure ()
                _ -> assertFailure "call result should match (Left (BlockingCommandTimedOut _))",
      testCase "replying to call within the given timeout is successful" $ do
        freshCounter <- newCounterVar
        runRIO (MkBookStoreEnv {_fresh = freshCounter}) $ do
          let oneMilliSecond = 1_000 -- one milli second is 1000 micro seconds
          serverOutput <- newMessageBox UnlimitedMessageBox
          serverInput <- newInput serverOutput
          let bookstoreClient = call serverInput GetBooks (20 * oneMilliSecond)
              bookstoreServer = handleMessage serverOutput $ \case
                Blocking GetBooks replyBox -> do
                  void (replyTo replyBox [])
                  return Nothing
                NonBlocking a ->
                  pure (Just ("unexpected message: " <> show a))
          (clientResult, serverResult) <- concurrently bookstoreClient bookstoreServer
          liftIO $ do
            assertEqual "unexpected message received: " (Just Nothing) serverResult
            case clientResult of
              (Right actualBooks) -> assertEqual "call should return the right books: " actualBooks []
              other -> assertFailure $ "unexpected call result: " <> show other,
      testCase
        "when the server sends a reply after the call timeout has elapsed, the call function \
        \returns 'Left BlockingCommandTimedOut', and the replyTo function returns False"
        $ do
          freshCounter <- newCounterVar
          runRIO (MkBookStoreEnv {_fresh = freshCounter}) $ do
            bookStoreOutput <- newMessageBox UnlimitedMessageBox
            bookStoreInput <- newInput bookStoreOutput
            let client = do
                  result <- call bookStoreInput GetBooks 100_000
                  case result of
                    Left (BlockingCommandTimedOut _) ->
                      -- this is the error we expected
                      return $ return ()
                    unexpectedOther ->
                      return $
                        assertFailure
                          ( "call result should match (Left (BlockingCommandTimedOut _)), unexpected: "
                              <> show unexpectedOther
                          )
                server =
                  handleMessage bookStoreOutput $ \case
                    Blocking GetBooks replyBox -> do
                      threadDelay 500_000 -- artificial delay to cause the client to
                      -- loose patience and timeout
                      void (replyTo replyBox [])
                      return (return ())
                    NonBlocking a ->
                      return (assertFailure ("unexpected message: " <> show a))
            (clientAssertion, serverAssertion) <-
              concurrently client server
            liftIO $ do
              clientAssertion
              fromMaybe (assertFailure "book store server died unexpectedly") serverAssertion,
      testCase
        "when the server receives the call and does not send a reply and immediately \
        \exits, the call function returns a timeout error"
        $ do
          freshCounter <- newCounterVar
          let env = MkBookStoreEnv {_fresh = freshCounter}
          (serverResult, clientResult) <- runRIO env $ do
            bookStoreOutput <- newMessageBox (BlockingBoxLimit MessageLimit_16)
            bookStoreInput <- newInput bookStoreOutput
            concurrently
              ( handleMessage bookStoreOutput $ \case
                  Blocking GetBooks _replyBox -> return Nothing
                  u -> return (Just ("unexpected message: " <> show u))
              )
              ( timeout
                  1_000_000
                  (call bookStoreInput GetBooks 100_000)
              )
          assertEqual
            "unexpected serverResult"
            (Just Nothing)
            serverResult
          assertEqual
            "unexpected clientResult"
            (Just (Left (BlockingCommandTimedOut (MkCallId 1))))
            clientResult
    ]

newtype BookStoreEnv = MkBookStoreEnv
  {_fresh :: CounterVar CallId}

instance CallId.HasCallIdCounter BookStoreEnv where
  getCallIdCounter MkBookStoreEnv {_fresh} = _fresh
  putCallIdCounter newFresh MkBookStoreEnv {_fresh} = MkBookStoreEnv {_fresh = newFresh}

allDonatedBooksAreInTheBookStore :: [(Donor, Book)] -> Property
allDonatedBooksAreInTheBookStore donorsAndBooks = ioProperty $ do
  bookStoreIn <- newMessageBox UnlimitedMessageBox
  bookStoreOut <- newInput bookStoreIn
  getAll
    <$> runConc
      ( foldMap
          ( \(donor, book) ->
              conc (cast bookStoreOut (Donate donor book) $> All True)
          )
          donorsAndBooks
          <> pure (All True)
      )

data BookStore

data instance Command BookStore _ where
  Donate :: Donor -> Book -> Command BookStore 'FireAndForget
  GetBooks :: Command BookStore ( 'Return [Book])

deriving stock instance Eq (Command BookStore 'FireAndForget)

deriving stock instance Show (Command BookStore 'FireAndForget)

deriving stock instance Eq (Command BookStore ( 'Return [Book]))

deriving stock instance Show (Command BookStore ( 'Return [Book]))

newtype Year = Year Int
  deriving newtype (Show, Eq, Ord)
  deriving (Arbitrary) via (Positive Int)

newtype Page = Page String
  deriving newtype (Show, Eq, Ord)
  deriving (Arbitrary) via PrintableString

data PersonName = PersonName
  { lastName :: String,
    surName :: String
  }
  deriving stock (Show, Eq, Ord)

instance Arbitrary PersonName where
  arbitrary =
    PersonName
      <$> (getPrintableString <$> arbitrary)
        <*> (getPrintableString <$> arbitrary)

data Donor = Donor
  { donorName :: PersonName,
    donorId :: Int
  }
  deriving stock (Show, Eq, Ord)

instance Arbitrary Donor where
  arbitrary =
    Donor
      <$> arbitrary
      <*> (getLarge . getPositive <$> arbitrary)

data Author = Author
  { authorName :: PersonName,
    authorId :: Int
  }
  deriving stock (Show, Eq, Ord)

instance Arbitrary Author where
  arbitrary =
    Author
      <$> arbitrary
      <*> (getLarge . getPositive <$> arbitrary)

newtype Publisher = Publisher String
  deriving newtype (Show, Eq, Ord)
  deriving (Arbitrary) via PrintableString

data Book = Book
  { bookTitle :: String,
    bookAuthor :: Author,
    bookPublisher :: Publisher,
    bookYear :: Year,
    bookContent :: [Page]
  }
  deriving stock (Show, Eq, Ord)

instance Arbitrary Book where
  arbitrary =
    Book
      <$> (getPrintableString <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (getNonEmpty <$> arbitrary)

-- --------------------------------------------------------
-- Unit tests
-- --------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "unit tests"
    [ testCase "Message Show instance" $ do
        withCallIds
          ( callAsync
              ( OnDeliver $ \m -> do
                  liftIO (assertEqual "bad show result" "B: (Echo \"123\") 1" (show m))
                  return True
              )
              (Echo "123")
          )
          >>= assertBool "should not fail" . isJust
        withCallIds
          ( cast
              ( OnDeliver $ \m -> do
                  liftIO (assertEqual "bad show result" "NB: (Tell 123)" (show m))
                  return True
              )
              (Tell "123")
          )
          >>= assertBool "should not fail",
      testCase "CommandError Show instance" $ do
        assertEqual
          "Show instance broken"
          "CouldNotEnqueueCommand 1"
          (show (CouldNotEnqueueCommand (MkCallId 1)))
        assertEqual
          "Show instance broken"
          "BlockingCommandFailure 1"
          (show (BlockingCommandFailure (MkCallId 1)))
        assertEqual
          "Show instance broken"
          "BlockingCommandTimedOut 1"
          (show (BlockingCommandTimedOut (MkCallId 1))),
      testCase
        "when deliver returns False, BlockingCommandFailed\
        \ should be returned"
        $ withCallIds
          ( call
              (OnDeliver (const (return False)))
              (Echo "123")
              2_000_000
          )
          >>= assertEqual
            "bad call result"
            (Left (CouldNotEnqueueCommand (MkCallId 1))),
      testCase
        "when deliver blocks forever, call also blocks forever\
        \ regardles of the timeout parameter"
        $ do
          res <-
            withCallIds $
              timeout 10_000 $
                call
                  ( OnDeliver
                      ( const $ do
                          threadDelay 3600_000_000
                          return False
                      )
                  )
                  (Echo "123")
                  1
          assertEqual "bad call result" Nothing res,
      testCase
        "when deliver succeeds, but the reply box is ignored \
        \and no replyTo invokation is done, BlockingCommandTimedOut\
        \ should be returned"
        $ do
          res <-
            withCallIds $
              call
                (OnDeliver (const (return True)))
                (Echo "123")
                10_000
          assertEqual
            "bad call result"
            (Left (BlockingCommandTimedOut (MkCallId 1)))
            res,
      testCase
        "when the reply box is passed to a new process, but\
        \ replyTo is delayed very much,\
        \ BlockingCommandTimedOut should be returned"
        $ do
          res <-
            withCallIds $
              call
                ( OnDeliver
                    ( \(Blocking (Echo x) rbox) -> do
                        void $
                          forkIO $ do
                            threadDelay 3600_000_000
                            replyTo rbox x
                        return True
                    )
                )
                (Echo "123")
                10_000
          assertEqual
            "bad call result"
            (Left (BlockingCommandTimedOut (MkCallId 1)))
            res,
      testCase
        "when the reply box is passed to a new process, that calls\
        \ replyTo after x seconds, where x < rt (the receive timeou),\
        \ then before y seconds have passed, where x <= y < rt,\
        \ call returns the value passed to the reply box."
        $ do
          let x = 20_000
              rt = 1_000_000
              y = 500_000
          res <-
            withCallIds $
              timeout y $
                call
                  ( OnDeliver
                      ( \(Blocking (Echo value) rbox) -> do
                          void $
                            forkIO $ do
                              threadDelay x
                              replyTo rbox value
                          return True
                      )
                  )
                  (Echo "123")
                  rt
          assertEqual
            "bad call result"
            (Just (Right "123"))
            res,
      testCase
        "when a call is delegated to, and replied by, another process, \
        \ call should return that reply"
        $ do
          res <-
            withCallIds $
              call
                ( OnDeliver
                    ( \(Blocking (Echo value) rbox) ->
                        delegateCall
                          ( OnDeliver
                              ( \(Blocking (Echo value') rbox') -> do
                                  replyTo rbox' value'
                                  return True
                              )
                          )
                          (Echo value)
                          rbox
                    )
                )
                (Echo "123")
                1_000
          assertEqual
            "bad call result"
            (Right "123")
            res,
      testCase
        "when deliver has a delay, callAsync \
        \ has at least the same delay"
        $ do
          res <-
            withCallIds $
              timeout
                90_000
                ( callAsync
                    ( OnDeliver
                        ( \(Blocking (Echo _value) _rbox) ->
                            do
                              threadDelay 100_000
                              return False
                        )
                    )
                    (Echo "123")
                )
          assertBool
            "callAsync returned too soon"
            (isNothing res),
      testCase
        "when deliver fails, callAsync returns Nothing"
        $ do
          res <-
            withCallIds $
              callAsync
                ( OnDeliver
                    (\(Blocking (Echo _value) _rbox) -> return False)
                )
                (Echo "123")
          assertBool
            "callAsync should fail"
            (isNothing res),
      testCase
        "when deliver succeeds, callAsync returns Just the pending reply"
        $ do
          res <-
            withCallIds $
              callAsync
                ( OnDeliver
                    (\(Blocking (Echo _value) _rbox) -> return True)
                )
                (Echo "123")
          assertBool
            "callAsync should succeed"
            (isJust res),
      testCase
        "when deliver succeeds, but no reply is sent, \
        \ waitForReply returns Left BlockingCommandTimedOut"
        $ withCallIds
          ( callAsync
              ( OnDeliver
                  (\(Blocking (Echo _value) _rbox) -> return True)
              )
              (Echo "123")
              >>= traverse (waitForReply 1_000)
          )
          >>= assertEqual
            "timeout error expected"
            (Just (Left (BlockingCommandTimedOut (MkCallId 1)))),
      testCase
        "when deliver succeeds, but no reply is sent, \
        \ tryTakeReply will return Nothing"
        $ withCallIds
          ( callAsync
              ( OnDeliver
                  (\(Blocking (Echo _value) _rbox) -> return True)
              )
              (Echo "123")
              >>= traverse tryTakeReply
          )
          >>= assertBool "tryTakeReply should return Nothing" . isNothing . fromJust,
      testCase
        "when deliver succeeds, and a reply is sent, \
        \ waitForReply will return Right with the reply"
        $ withCallIds
          ( callAsync
              ( OnDeliver
                  ( \(Blocking (Echo value) rbox) -> do
                      replyTo rbox value
                      return True
                  )
              )
              (Echo "123")
              >>= traverse (waitForReply 1_000_000)
          )
          >>= assertEqual "waitForReply should return the reply" (Just (Right "123")),
      testCase
        "when deliver has succeeded, and a reply has been sent, \
        \ tryTakeReply will eventually return Just Right the reply"
        $ withCallIds
          ( callAsync
              ( OnDeliver
                  ( \(Blocking (Echo value) rbox) -> do
                      replyTo rbox value
                      return True
                  )
              )
              (Echo "123")
              >>= ( \x -> do
                      threadDelay 10_000
                      traverse tryTakeReply x
                  )
          )
          >>= assertEqual
            "tryTakeReply should return the reply"
            (Just (Just (Right "123"))),
      testCase
        "after waitForReply returns Right the reply,\
        \ any further call to waitForReply will return\
        \ the same result immediately."
        $ withCallIds $
          callAsync
            ( OnDeliver
                ( \(Blocking (Echo value) rbox) -> do
                    replyTo rbox value
                    return True
                )
            )
            (Echo "123")
            >>= maybe
              (liftIO $ assertFailure "callAsync failed unexpectedly")
              ( \pendingReply -> do
                  waitForReply 1_000_000 pendingReply
                    >>= liftIO
                      . assertEqual
                        "waitForReply should return the reply"
                        (Right "123")
                  timeout 1_000 (waitForReply 1_000_000 pendingReply)
                    >>= liftIO
                      . maybe
                        (assertFailure "the second waitForReply should return immediately")
                        ( assertEqual
                            "the second waitForReply should return the reply"
                            (Right "123")
                        )
              ),
      testCase
        "when waitForReply has returned, tryTakeReply will\
        \ also return Just the same result"
        $ withCallIds $
          callAsync
            ( OnDeliver
                ( \(Blocking (Echo value) rbox) -> do
                    replyTo rbox value
                    return True
                )
            )
            (Echo "123")
            >>= maybe
              (liftIO $ assertFailure "callAsync failed unexpectedly")
              ( \pendingReply -> do
                  waitForReply 1_000_000 pendingReply
                    >>= liftIO
                      . assertEqual
                        "waitForReply should return the reply"
                        (Right "123")
                  tryTakeReply pendingReply
                    >>= liftIO
                      . maybe
                        (assertFailure "the second tryTakeReply should return Just (...)")
                        ( assertEqual
                            "tryTakeReply should return the reply"
                            (Right "123")
                        )
              ),
      testCase
        "when tryTakeReply has returned Just a result, waitForReply will\
        \ also return that same result"
        $ withCallIds $
          callAsync
            ( OnDeliver
                ( \(Blocking (Echo value) rbox) -> do
                    replyTo rbox value
                    return True
                )
            )
            (Echo "123")
            >>= maybe
              (liftIO $ assertFailure "callAsync failed unexpectedly")
              ( \pendingReply -> do
                  timeout 1_000_000 (untilJust (tryTakeReply pendingReply))
                    >>= maybe
                      (liftIO $ assertFailure "tryTakeReply should return a result")
                      ( liftIO
                          . assertEqual
                            "tryTakeReply should return the reply"
                            (Right "123")
                      )
                  waitForReply 1_000 pendingReply
                    >>= liftIO
                      . assertEqual
                        "waitForReply should return the same reply"
                        (Right "123")
              ),
      testCase "DuplicateReply Eq and Show instance" $ do
        assertEqual "Eq instance" (DuplicateReply (MkCallId 1)) (DuplicateReply (MkCallId 1))
        assertEqual "Show instance" "more than one reply sent for: 1" (show (DuplicateReply (MkCallId 1))),
      testCase
        "when replyTo is called again on the same replyBox, an error is thrown\
        \ and the waitForResult will still return the first reply"
        $ withCallIds
          ( do
              reply <-
                timeout 1_000_000 $
                  try $
                    callAsync
                      ( OnDeliver
                          ( \(Blocking (Echo value) rbox) -> do
                              replyTo rbox value
                              replyTo rbox value
                              return True
                          )
                      )
                      (Echo "123")

              liftIO $ case reply of
                (Just (Left (DuplicateReply cId))) ->
                  assertEqual "wrong callId in DuplicateReply exception" (MkCallId 1) cId
                (Just (Right _)) ->
                  assertFailure "expected an exception to be thrown in second `replyTo` call"
                Nothing ->
                  assertFailure "unexpectedly indefinitely blocked on `replyTo` call"
          ),
      testCase
        "tryTakeReply should return Nothing until the reply is sent, and\
        \ from the moment the reply is available, it will always and\
        \ immediately return the reply, it will never return Nothing again"
        $ withCallIds
          ( do
              replyNow <- newEmptyMVar
              replySent <- newEmptyMVar
              mPendingReply <-
                callAsync
                  ( OnDeliver
                      ( \(Blocking (Echo value) rbox) -> do
                          void $
                            forkIO $ do
                              () <- takeMVar replyNow
                              replyTo rbox value
                              putMVar replySent ()
                          return True
                      )
                  )
                  (Echo "")
              case mPendingReply of
                Nothing ->
                  liftIO $ assertFailure "callAsync failed"
                Just pendingReply -> do
                  tryTakeReply pendingReply
                    >>= liftIO . assertEqual "tryTakeReply failed" Nothing
                  putMVar replyNow ()
                  takeMVar replySent
                  tryTakeReply pendingReply
                    >>= liftIO
                      . assertEqual "tryTakeReply failed" (Just (Right ""))
          ),
      testCase
        "PendingResult's show instance contains the call Id and the \
        \ name of the result type"
        $ do
          res <-
            withCallIds $
              callAsync
                ( OnDeliver
                    (const (return True))
                )
                (Echo (Just "123"))
          case res of
            Nothing -> assertFailure "unexpected delivery failure"
            Just pendingReply ->
              assertEqual
                "bad show instance for PendingResult"
                "AR: 1"
                (show pendingReply)
    ]

-- Echo Protocol for callTest

data Echo

data instance Command Echo _ where
  Echo :: a -> Command Echo ( 'Return a)

instance (Show a) => Show (Command Echo ( 'Return a)) where
  showsPrec !d (Echo x) = showParen (d >= 9) (showString "Echo " . showsPrec 9 x)

data Tell

data instance Command Tell _ where
  Tell :: String -> Command Tell 'FireAndForget

instance Show (Command Tell 'FireAndForget) where
  showsPrec !d (Tell x) = showParen (d >= 9) (showString "Tell " . showString x)

-- IsMessageBox.* instance(s) for callTest

data NoOpFactory
  = NoOpFactory
  deriving stock (Show)

newtype NoOpInput a
  = OnDeliver (forall m. MonadUnliftIO m => a -> m Bool)

data NoOpBox a
  = OnReceive (Maybe Int) (Maybe a)
  deriving stock (Show)

instance IsMessageBoxFactory NoOpFactory where
  type MessageBox NoOpFactory = NoOpBox
  newMessageBox NoOpFactory = return (OnReceive Nothing Nothing)
  getConfiguredMessageLimit _ = Nothing

instance IsMessageBox NoOpBox where
  type Input NoOpBox = NoOpInput
  newInput _ = return $ OnDeliver (const (return False))
  receive (OnReceive t r) = do
    traverse_ threadDelay t
    return r
  tryReceive (OnReceive t r) = do
    timeoutVar <- traverse registerDelay t
    return
      ( Future
          ( do
              isOver <- fromMaybe True <$> traverse readTVarIO timeoutVar
              if isOver
                then return r
                else return Nothing
          )
      )

instance IsInput NoOpInput where
  deliver (OnDeliver react) m =
    react m
