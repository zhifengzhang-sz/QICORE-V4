# QiCore Base - Haskell Implementation

This is the Haskell implementation of QiCore Base components, providing standardized error handling and result types with functional composition support.

## Components

### QiError

A standardized error representation with rich context and debugging information:

- **Immutable**: All properties are readonly after construction
- **Serializable**: Can be converted to/from JSON for logging and transmission  
- **Chainable**: Supports error cause chains for debugging complex failures
- **Language-Neutral**: No language-specific error types or stack traces

**Properties:**
- `code`: Unique error identifier
- `message`: Human-readable error description
- `category`: Error classification (VALIDATION, NETWORK, etc.)
- `context`: Additional error context as key-value structure
- `cause`: Underlying error that caused this error (for error chaining)
- `timestamp`: When the error occurred (epoch milliseconds)

**Operations:**
- `create`: Factory method for creating errors
- `toString`: Return formatted error string for display
- `toStructuredData`: Return serializable structure for logging/transmission
- `getCategory`: Return error category for programmatic handling
- `withContext`: Create new error with merged context
- `withCause`: Create new error with cause chain

### Result<T>

A type-safe way to handle operations that can succeed or fail, eliminating exceptions in normal control flow:

- **Functional**: Supports functional composition through transformation methods
- **Monadic**: Implements Monad, Functor, and Applicative typeclasses
- **Thread-Safe**: Immutable value objects safe for concurrent access
- **Composable**: Chain operations that may fail using monadic bind

**Constructors:**
- `Success a`: Represents successful operation with result value
- `Failure QiError`: Represents failed operation with error details

**Operations:**
- `success`: Factory method for successful result
- `failure`: Factory method for failed result
- `fromTryCatch`: Wrap function call in try-catch, return Result
- `mapResult`: Transform success value, pass through failures unchanged (Functor map)
- `flatMap`: Chain operations that return Results (Monad bind)
- `unwrap`: Return data if success, throw error if failure
- `unwrapOr`: Return data if success, default value if failure
- `match`: Pattern matching for handling both success and failure cases
- `orElse`: Provide alternative Result if this one failed

## Usage Examples

### Basic Error Creation

```haskell
import QiCore.Base
import qualified Data.Map.Strict as Map
import Data.Aeson (Value(..))

-- Create a validation error
validationError <- create 
  "INVALID_INPUT" 
  "Username must be at least 3 characters long"
  VALIDATION
  (Just $ Map.fromList [("input", String "ab"), ("minLength", Number 3)])
  Nothing
```

### Result Composition

```haskell
-- Chain operations that may fail
parseAndDivide :: Text -> Text -> IO (Result Integer)
parseAndDivide x y = do
  xResult <- parseNumber x
  yResult <- parseNumber y
  case (xResult, yResult) of
    (Success nx, Success ny) -> divideBy nx ny
    (Failure err, _) -> return $ Failure err
    (_, Failure err) -> return $ Failure err

-- Using monadic composition (more idiomatic)
parseAndDivideM :: Text -> Text -> IO (Result Integer)  
parseAndDivideM x y = do
  xResult <- parseNumber x
  case xResult of
    Success nx -> do
      yResult <- parseNumber y
      case yResult of
        Success ny -> divideBy nx ny
        Failure err -> return $ Failure err
    Failure err -> return $ Failure err
```

### Pattern Matching

```haskell
handleResult :: Result Text -> Text
handleResult = match
  (\value -> "Success: " <> value)
  (\err -> "Error: " <> Error.toString err)
```

### Error Context and Cause Chains

```haskell
-- Create base error
baseError <- create "FILE_NOT_FOUND" "Config file not found" FILESYSTEM 
  (Just $ Map.fromList [("path", String "/etc/config.yaml")]) Nothing

-- Add context
enrichedError <- withContext baseError $ Map.fromList
  [("user", String "myuser"), ("attempted_paths", Array [...])]

-- Create higher-level error with cause
appError <- create "CONFIG_LOAD_FAILED" "Failed to load config" CONFIGURATION
  (Just $ Map.fromList [("component", String "ConfigManager")])
  (Just enrichedError)
```

## Mathematical Properties

The Result type implements the mathematical laws required for functional composition:

### Functor Laws
- **Identity**: `fmap id = id`
- **Composition**: `fmap (f . g) = fmap f . fmap g`

### Monad Laws  
- **Left Identity**: `return a >>= f = f a`
- **Right Identity**: `m >>= return = m`
- **Associativity**: `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`

## Building

```bash
# Build the library
cabal build

# Run tests
cabal test

# Run benchmarks
cabal bench

# Run the example
cabal run example
```

## Dependencies

- `base`: Haskell standard library
- `text`: Efficient Unicode text processing
- `containers`: Efficient container types (Map, Set, etc.)
- `time`: Time library for timestamps
- `aeson`: JSON parsing and generation

## Performance

The Haskell implementation targets sub-50 microsecond performance for all operations, meeting the functional language tier requirements specified in the contracts.

## Error Categories

The following error categories are supported:

- `VALIDATION`: Input validation failures
- `NETWORK`: HTTP/network related errors
- `FILESYSTEM`: File/IO operation failures  
- `CONFIGURATION`: Config loading/parsing errors
- `CACHE`: Cache operation failures
- `TIMEOUT`: Operation timeout errors
- `PERMISSION`: Access/authorization failures
- `UNKNOWN`: Unclassified errors

## Type Safety

The Haskell implementation provides strong type safety guarantees:

- **Compile-time Error Detection**: Invalid operations are caught at compile time
- **No Null Pointer Exceptions**: Maybe types prevent null reference errors
- **Exhaustive Pattern Matching**: Compiler ensures all cases are handled
- **Immutability**: All types are immutable by default
- **Referential Transparency**: Pure functions with no side effects (except for IO operations)