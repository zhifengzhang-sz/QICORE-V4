{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module QiCore.Base.Error
  ( QiError (..)
  , ErrorCategory (..)
  , create
  , toString
  , toStructuredData
  , getCategory
  , withContext
  , withCause
  , fromStructuredData

    -- * Smart constructors for common error types
  , validationError
  , networkError
  , configurationError
  , timeoutError

    -- * Context helpers
  , withField
  , withFields
  ) where

import Control.Monad (mzero, when)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.:?), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)

{- | Error categories for systematic classification and handling

Each category enables different handling strategies:

* 'VALIDATION' - User input errors, recoverable with correction
* 'NETWORK' - Connectivity issues, often retryable
* 'FILESYSTEM' - File operations, check permissions/existence
* 'CONFIGURATION' - Setup errors, usually require environment fixes
* 'CACHE' - Temporary storage issues, often recoverable
* 'TIMEOUT' - Time-based failures, may be retryable
* 'PERMISSION' - Access control, requires authorization changes
* 'UNKNOWN' - Unclassified errors, require investigation
-}
data ErrorCategory
  = VALIDATION
  | NETWORK
  | FILESYSTEM
  | CONFIGURATION
  | CACHE
  | TIMEOUT
  | PERMISSION
  | UNKNOWN
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Standardized error representation with rich context and debugging information

Design principles:

* Immutable once created for thread safety
* Rich context for debugging without exposing sensitive data
* Cause chains for error propagation tracking
* Timestamps for temporal debugging
* JSON serializable for structured logging
-}
data QiError = QiError
  { code :: !T.Text
  -- ^ Unique error identifier
  , message :: !T.Text
  -- ^ Human-readable description
  , category :: !ErrorCategory
  -- ^ Classification for handling
  , context :: !(Maybe (Map.Map T.Text Value))
  -- ^ Debugging context
  , cause :: !(Maybe QiError)
  -- ^ Root cause chain
  , timestamp :: !Integer
  -- ^ Creation time (milliseconds since epoch)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Create a new QiError with validation and circular reference protection

Validates input parameters and prevents circular cause chains.
Timestamps are automatically generated.

>>> create "INVALID_INPUT" "User ID cannot be empty" VALIDATION Nothing Nothing
QiError { code = "INVALID_INPUT", message = "User ID cannot be empty", ... }
-}
create
  :: T.Text
  -- ^ Error code (must be non-empty)
  -> T.Text
  -- ^ Error message
  -> ErrorCategory
  -- ^ Error classification
  -> Maybe (Map.Map T.Text Value)
  -- ^ Optional context
  -> Maybe QiError
  -- ^ Optional cause
  -> IO QiError
create errorCode errorMessage errorCategory errorContext errorCause = do
  -- Validate inputs
  when (T.null errorCode) $
    error "Error code must be non-empty"

  -- Check for circular cause chain and depth limit
  validateCauseChain errorCause 0

  -- Generate timestamp
  now <- getCurrentTime
  let timestampMs = round (realToFrac (utcTimeToPOSIXSeconds now) * 1_000)

  pure
    QiError
      { code = errorCode
      , message = errorMessage
      , category = errorCategory
      , context = errorContext
      , cause = errorCause
      , timestamp = timestampMs
      }
 where
  validateCauseChain :: Maybe QiError -> Int -> IO ()
  validateCauseChain Nothing _ = pure ()
  validateCauseChain (Just err) depth
    | depth >= 10 = error "Cause chain too deep (max 10)"
    | otherwise = validateCauseChain (cause err) (depth + 1)

{- | Convert QiError to a formatted string for display

Produces a hierarchical representation showing the full error chain:

@
[VALIDATION] INVALID_INPUT: User ID cannot be empty (context: {...}) | Caused by: [NETWORK] CONNECTION_FAILED: ...
@
-}
toString :: QiError -> T.Text
toString err =
  T.concat
    [ "["
    , T.pack (show (category err))
    , "] "
    , code err
    , ": "
    , message err
    , contextStr
    , causeStr
    ]
 where
  contextStr = case context err of
    Nothing -> ""
    Just ctx -> " (context: " <> T.pack (show ctx) <> ")"

  causeStr = case cause err of
    Nothing -> ""
    Just c -> " | Caused by: " <> toString c

-- | Convert QiError to structured data for logging and transmission
toStructuredData :: QiError -> Value
toStructuredData = toJSON

-- | Extract the error category for programmatic handling
getCategory :: QiError -> ErrorCategory
getCategory = category

{- | Create a new QiError with additional context merged

New context takes precedence over existing context for duplicate keys.
-}
withContext :: QiError -> Map.Map T.Text Value -> IO QiError
withContext err additionalContext = do
  let mergedContext = case context err of
        Nothing -> Just additionalContext
        Just existingContext -> Just (Map.union additionalContext existingContext)

  pure err {context = mergedContext}

{- | Create a new QiError with a cause chain

Validates that no circular references are created in the cause chain.
-}
withCause :: QiError -> QiError -> IO QiError
withCause err causeError = do
  -- Validate cause chain doesn't create cycles
  validateNoCycle err causeError
  pure err {cause = Just causeError}
 where
  validateNoCycle :: QiError -> QiError -> IO ()
  validateNoCycle original new =
    when (containsError original new) $
      error "Circular cause chain detected"

  containsError :: QiError -> QiError -> Bool
  containsError target current =
    code target == code current
      || case cause current of
        Nothing -> False
        Just c -> containsError target c

-- | Create QiError from structured data (for deserialization)
fromStructuredData :: Value -> Maybe QiError
fromStructuredData = parseMaybe parseJSON

-- * Smart constructors for common error patterns

-- | Create a validation error with optional field context
validationError :: T.Text -> T.Text -> IO QiError
validationError code msg = create code msg VALIDATION Nothing Nothing

-- | Create a network error with optional URL context
networkError :: T.Text -> T.Text -> IO QiError
networkError code msg = create code msg NETWORK Nothing Nothing

-- | Create a configuration error
configurationError :: T.Text -> T.Text -> IO QiError
configurationError code msg = create code msg CONFIGURATION Nothing Nothing

-- | Create a timeout error with optional duration context
timeoutError :: T.Text -> T.Text -> IO QiError
timeoutError code msg = create code msg TIMEOUT Nothing Nothing

-- * Context building helpers

-- | Add a single field to error context
withField :: QiError -> T.Text -> Value -> IO QiError
withField err key value = withContext err (Map.singleton key value)

-- | Add multiple fields to error context
withFields :: QiError -> [(T.Text, Value)] -> IO QiError
withFields err fields = withContext err (Map.fromList fields)
