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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CommandTest where

import Data.Functor (Functor ((<$)), void, ($>), (<$>))
import Data.Semigroup (All (All, getAll))
import GHC.IO.Exception (userError)
import qualified Protocol.Command.CallId as CallId
import Protocol.Command.CallId (CallId(MkCallId))
import Protocol.Command
  ( Command,
    CommandError (BlockingCommandTimedOut),
    Message (Blocking, NonBlocking),
    ReturnType (FireAndForget, Return),
    call,
    cast,
    replyTo,
  )
import Protocol.Fresh (CounterVar, fresh, newCounterVar)
import Protocol.MessageBox.Class
  ( IsMessageBoxFactory (newMessageBox),
    handleMessage,
    newInput,
  )
import Protocol.MessageBox.Unlimited (UnlimitedMessageBox (UnlimitedMessageBox))
import RIO
  ( Applicative (pure, (<*>)),
    Bool (True),
    Either (Left, Right),
    Eq ((==)),
    Foldable (foldMap),
    Int,
    Monad (return, (>>=)),
    MonadIO (liftIO),
    Num ((*), (+)),
    Ord,
    Semigroup ((<>)),
    Show (..),
    String,
    conc,
    concurrently,
    error,
    newEmptyMVar,
    putMVar,
    runConc,
    runRIO,
    threadDelay,
    throwTo,
    tryReadMVar,
    ($),
    (.),
  )
import Test.Tasty as Tasty (TestTree, testGroup)
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
import UnliftIO.Concurrent (forkIO)
import Data.Maybe

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocol.Command"
    [ testProperty
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
                (Left (BlockingCommandTimedOut (MkCallId c))) | MkCallId (c + 1) == callId' -> pure ()
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
      testCase "when the server sends a reply after the call timeout has elapsed, the call function \
              \returns 'Left BlockingCommandTimedOut', and the replyTo function returns False" $ do
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
                    return $ assertFailure
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
            fromMaybe (assertFailure "book store server died unexpectedly") serverAssertion
         
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
