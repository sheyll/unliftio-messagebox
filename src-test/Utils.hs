{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Utils
  ( untilJust,
    untilM,
    withCallIds,
    allEqOrdShowMethodsImplemented,
    allEnumMethodsImplemented,
    NoOpFactory (..),
    NoOpBox (..),
    NoOpInput (..),
  )
where

import qualified Protocol.Command.CallId as CallId
import Protocol.Fresh (CounterVar)
import Protocol.Future (Future (Future))
import Protocol.MessageBox.Class
  ( IsInput (..),
    IsMessageBox (Input, newInput, receive, tryReceive),
    IsMessageBoxFactory (..),
  )
import RIO (MonadIO, MonadUnliftIO, RIO, fromMaybe, readTVarIO, registerDelay, runRIO, threadDelay, traverse_)
import Test.QuickCheck
  ( Arbitrary,
    Property,
    Testable (property),
    (.&&.),
    (.||.),
    (=/=),
    (===),
    (==>),
  )

untilJust :: (Monad m) => m (Maybe a) -> m a
untilJust loopBody = do
  !res <- loopBody
  case res of
    Nothing ->
      untilJust loopBody
    Just r ->
      return r

untilM :: Monad m => m Bool -> m ()
untilM loopBody = do
  !isOk <- loopBody
  if isOk
    then return ()
    else untilM loopBody

withCallIds ::
  MonadIO m => RIO (CounterVar CallId.CallId) b -> m b
withCallIds f =
  CallId.newCallIdCounter >>= flip runRIO f

allEqOrdShowMethodsImplemented :: forall a proxy. (Arbitrary a, Ord a, Show a) => proxy a -> Property
allEqOrdShowMethodsImplemented _ = property $ \(x :: a) (y :: a) ->
  ( x < y
      ==> ( compare x y === LT
              .&&. compare y x === GT
              .&&. x /= y
              .&&. (y >= x)
              .&&. x < y
              .&&. (y > x)
              .&&. x <= y
              .&&. max x y === y
              .&&. min x y === x
              .&&. show x /= show y
              .&&. show (Just x) /= show (Just y)
          )
  )
    .&&. ( (x =/= y)
             .||. ( compare x y === EQ
                      .&&. compare y x === EQ
                      .&&. x == y
                      .&&. y == x
                      .&&. show x === show y
                      .&&. show (Just x) === show (Just y)
                      .&&. x == y
                      .&&. x >= y
                      .&&. y >= x
                      .&&. x <= y
                      .&&. y <= x
                      .&&. x <= y
                      .&&. x >= y
                      .&&. max x y === x
                      .&&. min x y === x
                  )
         )

allEnumMethodsImplemented ::
  forall a proxy.
  (Show a, Ord a, Enum a, Bounded a, Arbitrary a) =>
  proxy a ->
  Property
allEnumMethodsImplemented _ =
  property $ \(x :: a) (y :: a) ->
    ( not (x > minBound && x < maxBound)
        .||. succ (pred x) === pred (succ x)
    )
      .&&. ( x /= y
               .||. ( fromEnum x === fromEnum y
                        .&&. ( x >= maxBound
                                 .||. succ x === succ y
                             )
                        .&&. ( x <= minBound
                                 .||. pred x === pred y
                             )
                    )
           )
      .&&. toEnum (fromEnum x) === x
      .&&. head (enumFrom x) === x
      .&&. (x >= maxBound .||. enumFrom x !! 1 === succ x)
      .&&. ( x < y
               .||. fromEnum x >= fromEnum y
           )
      .&&. head (enumFromThen x y) === x
      .&&. if x > y
        then enumFromTo x y === []
        else
          head (enumFromTo x y)
            === x
              .&&. last (enumFromTo x y)
            === y
              .&&.
            ( x >= maxBound .||.  enumFromThenTo x (succ x) y
            === enumFromTo x y)

-- message box dummy implementation

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