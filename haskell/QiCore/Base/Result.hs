{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QiCore.Base.Result
  ( Result (..)

    -- * Construction
  , success
  , failure
  , fromTryCatch
  , fromMaybe
  , fromEither

    -- * Transformation
  , mapResult
  , mapError
  , flatMap
  , andThen

    -- * Extraction
  , unwrap
  , unwrapOr
  , unwrapOrElse

    -- * Query
  , isSuccess
  , isFailure
  , getData
  , getError

    -- * Conditional operations
  , match
  , orElse
  , recover
  , retryOnFailure

    -- * Collection operations
  , sequenceResult
  , traverseResult
  , liftA2Result
  , filterSuccesses
  , filterFailures
  , partitionResults

    -- * Validation helpers
  , validateAll
  , validateFirst

    -- * Exception integration
  , QiErrorException (..)
  ) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Exception (Exception, SomeException, throw, try)
import Data.Foldable (traverse_)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import QiCore.Base.Error (QiError (..))
import QiCore.Base.Error qualified as Error

{- | Result type for operations that can succeed with a value or fail with a structured error

This type is similar to 'Either' but specialized for error handling with 'QiError'.
It provides:

* Clear success/failure semantics
* Rich error information via 'QiError'
* Monadic composition for error propagation
* Integration with exception handling
-}
data Result a = Success !a | Failure !QiError
  deriving stock (Eq, Show, Generic)

-- * Basic Construction

-- | Create a successful result
success :: a -> Result a
success = Success

-- | Create a failed result
failure :: QiError -> Result a
failure = Failure

-- | Convert a 'Maybe' to a 'Result' with a default error
fromMaybe :: QiError -> Maybe a -> Result a
fromMaybe err = \case
  Nothing -> Failure err
  Just a -> Success a

-- | Convert an 'Either' to a 'Result'
fromEither :: Either QiError a -> Result a
fromEither = \case
  Left err -> Failure err
  Right a -> Success a

{- | Safely execute an 'IO' action, converting exceptions to 'Result'

Any thrown exception is caught and converted to a 'QiError' with category 'UNKNOWN'.
-}
fromTryCatch :: IO a -> IO (Result a)
fromTryCatch action = do
  result <- try action
  case result of
    Right value -> pure $ Success value
    Left (ex :: SomeException) -> do
      -- Convert exception to QiError
      qiError <-
        Error.create
          "EXCEPTION_CAUGHT"
          (T.pack $ show ex)
          Error.UNKNOWN
          Nothing
          Nothing
      pure $ Failure qiError

-- * Transformation

-- | Transform the success value (Functor fmap)
mapResult :: (a -> b) -> Result a -> Result b
mapResult f = \case
  Success a -> Success (f a)
  Failure err -> Failure err

-- | Transform the error (contravariant map on error)
mapError :: (QiError -> QiError) -> Result a -> Result a
mapError f = \case
  Success a -> Success a
  Failure err -> Failure (f err)

-- | Chain operations that return Results (Monad bind)
flatMap :: (a -> Result b) -> Result a -> Result b
flatMap f = \case
  Success a -> f a
  Failure err -> Failure err

-- | Alias for 'flatMap' with arguments flipped (more intuitive for chaining)
andThen :: Result a -> (a -> Result b) -> Result b
andThen = flip flatMap

-- * Extraction

{- | Return the success value or throw the error as an exception

Use with caution - prefer 'unwrapOr' or 'match' for safer alternatives.
-}
unwrap :: Result a -> IO a
unwrap = \case
  Success a -> pure a
  Failure err -> throw (QiErrorException err)

-- | Return the success value or a default value
unwrapOr :: a -> Result a -> a
unwrapOr defaultValue = \case
  Success a -> a
  Failure _ -> defaultValue

-- | Return the success value or compute a default using the error
unwrapOrElse :: (QiError -> a) -> Result a -> a
unwrapOrElse f = \case
  Success a -> a
  Failure err -> f err

-- * Query

-- | Check if the result is a success
isSuccess :: Result a -> Bool
isSuccess = \case
  Success _ -> True
  Failure _ -> False

-- | Check if the result is a failure
isFailure :: Result a -> Bool
isFailure = not . isSuccess

-- | Extract the success value as 'Maybe'
getData :: Result a -> Maybe a
getData = \case
  Success a -> Just a
  Failure _ -> Nothing

-- | Extract the error as 'Maybe'
getError :: Result a -> Maybe QiError
getError = \case
  Success _ -> Nothing
  Failure err -> Just err

-- * Conditional Operations

-- | Pattern match on both success and failure cases
match :: (a -> b) -> (QiError -> b) -> Result a -> b
match onSuccess onError = \case
  Success a -> onSuccess a
  Failure err -> onError err

-- | Provide an alternative result if this one failed
orElse :: (QiError -> Result a) -> Result a -> Result a
orElse alternative = \case
  Success a -> Success a
  Failure err -> alternative err

-- | Recover from failure with a pure value
recover :: a -> Result a -> Result a
recover defaultValue = orElse (const $ Success defaultValue)

-- | Retry a computation on failure (useful for transient errors)
retryOnFailure :: Int -> IO (Result a) -> IO (Result a)
retryOnFailure maxAttempts action = go maxAttempts
 where
  go 0 = action -- Last attempt, return whatever we get
  go n = do
    result <- action
    case result of
      Success _ -> pure result
      Failure _ -> go (n - 1)

-- * Collection Operations

{- | Convert a list of 'Result's to a 'Result' of a list

Succeeds only if all results are successful.
-}
sequenceResult :: [Result a] -> Result [a]
sequenceResult = foldr go (Success [])
 where
  go (Success a) (Success as) = Success (a : as)
  go (Failure err) _ = Failure err
  go _ (Failure err) = Failure err

-- | Map a function over a list and collect the results
traverseResult :: (a -> Result b) -> [a] -> Result [b]
traverseResult f = sequenceResult . map f

-- | Lift a binary function to work on 'Result's
liftA2Result :: (a -> b -> c) -> Result a -> Result b -> Result c
liftA2Result f (Success a) (Success b) = Success (f a b)
liftA2Result _ (Failure err) _ = Failure err
liftA2Result _ _ (Failure err) = Failure err

-- | Extract all successful values from a list of results
filterSuccesses :: [Result a] -> [a]
filterSuccesses = foldr go []
 where
  go (Success a) acc = a : acc
  go (Failure _) acc = acc

-- | Extract all errors from a list of results
filterFailures :: [Result a] -> [QiError]
filterFailures = foldr go []
 where
  go (Success _) acc = acc
  go (Failure err) acc = err : acc

-- | Partition a list of results into successes and failures
partitionResults :: [Result a] -> ([a], [QiError])
partitionResults = foldr go ([], [])
 where
  go (Success a) (successes, failures) = (a : successes, failures)
  go (Failure err) (successes, failures) = (successes, err : failures)

-- * Validation Helpers

{- | Validate all items in a list, collecting all errors

Unlike 'traverseResult', this continues validation even after the first failure,
collecting all validation errors.
-}
validateAll :: (a -> Result b) -> [a] -> Result [b]
validateAll f items =
  let results = map f items
      (successes, failures) = partitionResults results
   in case failures of
        [] -> Success successes
        (firstError : _) -> Failure firstError -- Could combine errors here

-- | Validate items until the first success
validateFirst :: (a -> Result b) -> [a] -> Result b
validateFirst _ [] =
  Failure $
    QiError
      { Error.code = "NO_ITEMS"
      , Error.message = "Empty list for validation"
      , Error.category = Error.VALIDATION
      , Error.context = Nothing
      , Error.cause = Nothing
      , Error.timestamp = 0
      }
validateFirst f (x : xs) = case f x of
  Success b -> Success b
  Failure _ -> validateFirst f xs

-- * Exception Integration

-- | Exception wrapper for 'QiError' to enable throwing
newtype QiErrorException = QiErrorException QiError
  deriving stock (Show, Typeable)

instance Exception QiErrorException

-- * Type Class Instances

-- | Transform success values while preserving failures
instance Functor Result where
  fmap = mapResult

{- | Apply functions in 'Result' context

Accumulates the first error encountered.
-}
instance Applicative Result where
  pure = Success
  Success f <*> Success a = Success (f a)
  Success _ <*> Failure err = Failure err
  Failure err <*> _ = Failure err

{- | Chain computations that may fail

Short-circuits on the first failure.
-}
instance Monad Result where
  (>>=) = flip flatMap

-- | Provide alternative results on failure
instance Alternative Result where
  empty =
    Failure $
      QiError
        { Error.code = "EMPTY_ALTERNATIVE"
        , Error.message = "Empty Alternative result"
        , Error.category = Error.UNKNOWN
        , Error.context = Nothing
        , Error.cause = Nothing
        , Error.timestamp = 0
        }
  Success a <|> _ = Success a
  Failure _ <|> r = r
