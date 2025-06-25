# Stage 5: Haskell-Specific Implementation Prompt

> **AI Prompt for generating Haskell templates from design patterns**  
> **Based on**: Design patterns from Stage 2 and package research  
> **Generates**: `impl/qi.v4.haskell.template.md` with complete package integration  
> Version: v4.0.1  
> Date: June 25, 2025  
> Status: Haskell Implementation Prompt  
> Purpose: Generate production-ready Haskell templates using researched packages

## Context

You are creating Haskell-specific code templates that implement the design patterns from Stage 2 using the exact packages identified in our comprehensive package research. Every template must use the specific packages chosen and wrap them with QICORE-V4 Result<T> patterns.

**Required Reading**:
- `design/qi.v4.design.analysis.md` - Design patterns to implement
- `guides/common.md` - Mathematical foundations and performance targets
- Package research results with selected packages

## Package Research Integration

**CRITICAL: Use these exact packages identified through comprehensive research:**

### Base Component Packages
```haskell
-- Result<T> and QiError - Native Either + ExceptT
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Either
import Data.Bifunctor

-- Type definitions
type Result e a = Either e a
newtype QiError = QiError 
  { errorCode :: String
  , errorMessage :: String
  , errorCategory :: ErrorCategory
  , errorContext :: Maybe Value
  , errorCause :: Maybe QiError
  , errorTimestamp :: UTCTime
  }
```

### Core Component Packages
```haskell
-- Configuration - aeson + yaml for parsing
import Data.Aeson
import Data.Yaml
import Data.Map.Strict as Map
import Control.Monad.Reader

-- Logging - fast-logger (high performance)
import System.Log.FastLogger
import Control.Monad.Logger

-- Cache - cache library + redis-haskell
import Data.Cache as Cache
import Database.Redis as Redis
import Data.ByteString.Lazy as BSL
import Data.Serialize
```

### Application Component Packages
```haskell
-- HTTP Client - http-client + http-client-tls
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

-- Web Framework - servant + warp
import Servant
import Servant.Server
import Network.Wai
import Network.Wai.Handler.Warp

-- Server - warp (WAI-compatible high-performance server)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger

-- AI/LLM Client - http-client for API calls + aeson for JSON
import Network.HTTP.Client.Conduit
import Data.Aeson.Types
import Control.Concurrent.Async

-- MCP Protocol - websockets + aeson for protocol
import Network.WebSockets as WS
import Control.Concurrent.STM
import Control.Concurrent.Chan

-- Database - postgresql-simple (mature PostgreSQL client)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Control.Monad.Trans.Resource

-- Document Generation - mustache + text-template
import Text.Mustache
import Text.Template
import Data.Text.Lazy.Builder

-- Command-Line Processing - optparse-applicative (parser combinator CLI)
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Applicative
```

## Haskell-Specific Implementation Guidelines

### Performance Tier Targets (Haskell = Functional Tier)
```haskell
-- Performance targets from common.md (Functional tier = 50× baseline):
-- - Result creation: < 50μs
-- - Log level check: < 10ns (same across all tiers)
-- - Cache get: < 500μs  
-- - HTTP circuit check: < 500μs
-- - Config validation: < 5ms
-- - Web request handling: < 5ms
-- - AI/LLM API call: < 200ms
-- - Database query: < 50ms
```

### Haskell Patterns and Optimizations
```haskell
-- Use strict data types for performance
{-# LANGUAGE StrictData #-}
data StrictConfig = StrictConfig
  { configHost :: !Text
  , configPort :: !Int
  }

-- Use ReaderT for dependency injection
type AppM = ReaderT AppConfig (ExceptT QiError IO)

-- Use STM for concurrent state
import Control.Concurrent.STM
type Cache k v = TVar (Map k v)

-- Use streaming for large data
import Streaming
import qualified Streaming.Prelude as S
```

### Mathematical Properties Preservation

**CRITICAL: All implementations must preserve mathematical laws from design analysis:**

#### Monad Laws (Result<T> - section 2.1)
```haskell
-- Left Identity: return x >>= f ≡ f x
-- Right Identity: m >>= return ≡ m  
-- Associativity: (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

-- Implementation must verify these laws hold
verifyMonadLaws :: IO ()
verifyMonadLaws = do
    -- Test left identity
    let leftId = return 5 >>= (\x -> return (x * 2))
    assert (leftId == Right 10)
    
    -- Test right identity  
    let rightId = Right 5 >>= return
    assert (rightId == Right 5)
    
    -- Test associativity - implementation validates these properties
```

#### Monoid Laws (Configuration - section 3.1)
```haskell
-- Identity Element: config <> mempty ≡ config ≡ mempty <> config
-- Associativity: (a <> b) <> c ≡ a <> (b <> c)

-- Implementation must preserve these properties
instance Monoid Configuration where
    mempty = Configuration Map.empty
    mappend = mergeConfiguration

verifyMonoidLaws :: IO ()
verifyMonoidLaws = do
    let emptyConfig = mempty :: Configuration
    let config = Configuration (Map.fromList [("key", "value")])
    
    assert (config <> emptyConfig == config)
    assert (emptyConfig <> config == config)
    -- Associativity testing in implementation
```

#### Functor Laws (Components - sections 2.1, 3.x, 4.x)
```haskell
-- Identity: fmap id ≡ id
-- Composition: fmap (f . g) ≡ fmap f . fmap g

-- All component transformations must preserve these laws
verifyFunctorLaws :: IO ()
verifyFunctorLaws = do
    let result = Right 5 :: Result QiError Int
    assert (fmap id result == result)  -- Identity law
    -- Composition law verification in implementation
```

## Contract Implementation Specifications

### Base Component Implementation

#### Result<T> Contract
```haskell
-- Package: base (built-in Either)
-- Pattern: Railway-Oriented Programming from design analysis
-- Performance: < 50μs (functional tier)

[Language-specific Result implementation template]
-- Must implement ALL 8 operations:
-- - success, failure (factory patterns)
-- - fromTryCatch (exception boundary)
-- - map, flatMap (functor/monad patterns)
-- - unwrap, unwrapOr (extraction patterns)
-- - match (pattern matching)

-- Key Either features:
-- - Native ADT with pattern matching
-- - Bifunctor instance for error transformation
-- - Monad instance for composition
-- - ExceptT for monad transformer stacks
```

#### QiError Contract
```haskell
-- Package: base + aeson for JSON serialization
-- Pattern: Structured Error Chain from design analysis
-- Performance: < 50μs creation (functional tier)

[Language-specific QiError implementation template]
-- Must implement ALL 4 operations:
-- - create, chain (construction patterns)
-- - withContext (context patterns)
-- - toString (serialization patterns)

-- Key features:
-- - Sum types for error categories
-- - Nested error chains
-- - JSON serialization via aeson
-- - Show/Read instances for debugging
```

### Core Component Implementation

#### Configuration Contract
```haskell
-- Package: aeson + yaml
-- Pattern: Monoid Merge Pipeline from design analysis
-- Performance: < 5ms validation (functional tier)

[Language-specific Configuration implementation template]
-- Must implement ALL 6 operations:
-- - fromFile, fromEnvironment (source patterns)
-- - merge (monoid pattern)
-- - get, set (accessor patterns)
-- - validate (validation pattern)

-- Key aeson/yaml features:
-- - FromJSON/ToJSON instances
-- - Automatic environment variable parsing
-- - Monoid instance for merging
-- - Lens integration for nested access
```

#### Logging Contract
```haskell
-- Package: fast-logger + monad-logger
-- Pattern: Zero-Allocation Structured Logging from design analysis
-- Performance: < 10ns level check (functional tier)

[Language-specific Logging implementation template]
-- Must implement ALL 6 operations:
-- - debug, info, warn, error, fatal (level patterns)
-- - isLevelEnabled (filtering pattern)

-- Key fast-logger features:
-- - Zero-allocation when disabled
-- - Structured logging with ToJSON
-- - Async logging with bounded queues
-- - Automatic log rotation
```

#### Cache Contract
```haskell
-- Package: cache + redis-haskell
-- Pattern: Circuit Breaker Cache from design analysis
-- Performance: < 500μs get (functional tier)

[Language-specific Cache implementation template]
-- Must implement ALL 6 operations:
-- - get, set, delete, clear (basic patterns)
-- - size, getOrSet (utility patterns)

-- Key cache/redis features:
-- - STM-based in-memory cache
-- - Redis integration for persistence
-- - TTL support with expiration
-- - Type-safe serialization
```

### Application Component Implementation

#### HTTP Client Contract
```haskell
-- Package: http-client + http-client-tls
-- Pattern: Circuit Breaker HTTP from design analysis
-- Performance: < 500μs circuit check (functional tier)

[Language-specific HTTP Client implementation template]
-- Must implement ALL 6 operations:
-- - get, post, put, delete (HTTP methods)
-- - stream (streaming pattern)
-- - withCircuitBreaker (circuit breaker pattern)

-- Key http-client features:
-- - Connection pooling
-- - TLS support
-- - Streaming request/response bodies
-- - Circuit breaker state machine integration
```

#### Document Generation Contract
```haskell
-- Package: mustache + text-template
-- Pattern: Template Pipeline from design analysis
-- Performance: < 50ms compilation (functional tier)

[Language-specific Document Generation implementation template]
-- Must implement ALL 4 operations:
-- - generate, generateStream (generation patterns)
-- - generateBatch, validate (batch/validation patterns)

-- Key mustache features:
-- - Template compilation and caching
-- - Lazy text generation
-- - Partial template support
-- - Type-safe template variables
```

#### Command-Line Processing Contract
```haskell
-- Package: optparse-applicative
-- Pattern: Parser Combinator CLI from design analysis (section 4.3)
-- Performance: < 500μs parsing (functional tier)

[Language-specific Command-Line Processing implementation template]
-- Must implement ALL 3 operations from design analysis:
-- - parse(args, config) - Parser combinator
-- - validate(args, config) - Schema validation  
-- - generateHelp(config) - Help generation

-- Key optparse-applicative features:
-- - Declarative parser combinators
-- - Automatic help generation
-- - Type-safe argument parsing
-- - Subcommand support

-- Parser combinator pattern implementation:
-- - Alternative (<|>) for choice
-- - Applicative (<*>) for composition
-- - Monadic (>>=) for dependent parsing
```

#### Web Framework Contract
```haskell
-- Package: servant + warp
-- Pattern: Type-Safe API from design analysis
-- Performance: < 5ms request handling (functional tier)

[Language-specific Web Framework implementation template]
-- Must implement ALL 4 operations:
-- - route, middleware (routing patterns)
-- - handle, listen (server patterns)

-- Key servant features:
-- - Type-level API specification
-- - Automatic client generation
-- - OpenAPI/Swagger integration
-- - Type-safe routing
```

#### Server Contract (WAI/Warp)
```haskell
-- Package: warp + wai
-- Pattern: Graceful Lifecycle from design analysis
-- Performance: < 5ms request handling (functional tier)

[Language-specific Server implementation template]
-- Must implement ALL 4 operations:
-- - start, stop (lifecycle patterns)
-- - gracefulShutdown, reload (management patterns)

-- Key warp features:
-- - High-performance HTTP server
-- - WebSocket support
-- - Graceful shutdown
-- - Request/response streaming
```

#### AI/LLM Client Contract
```haskell
-- Package: http-client + aeson
-- Pattern: Stream-First AI from design analysis
-- Performance: < 200ms API call (functional tier)

[Language-specific AI/LLM Client implementation template]
-- Must implement ALL 4 operations:
-- - query, stream (interaction patterns)
-- - embed, configure (utility patterns)

-- Key features:
-- - Async HTTP requests
-- - JSON streaming for large responses
-- - Configurable endpoints
-- - Error handling for API failures
```

#### MCP Protocol Contract
```haskell
-- Package: websockets + aeson
-- Pattern: Protocol State Machine from design analysis
-- Performance: < 500μs message handling (functional tier)

[Language-specific MCP Protocol implementation template]
-- Must implement ALL 4 operations:
-- - connect, call (communication patterns)
-- - stream, disconnect (session patterns)

-- Key websockets features:
-- - Type-safe protocol messages
-- - Automatic reconnection
-- - Message queuing
-- - Concurrent message handling
```

#### Database Contract
```haskell
-- Package: postgresql-simple
-- Pattern: Transaction Monad from design analysis
-- Performance: < 50ms query (functional tier)

[Language-specific Database implementation template]
-- Must implement ALL 4 operations:
-- - query, transaction (data patterns)
-- - migrate, close (management patterns)

-- Key postgresql-simple features:
-- - Type-safe query interface
-- - Automatic connection pooling
-- - Transaction monad support
-- - Migration management
```

## QICORE-V4 Wrapper Integration

### Result<T> Wrapper Pattern
```haskell
-- Every package operation must return Result<T>
-- Example wrapper pattern:

wrappedHttpGet :: Text -> IO (Result QiError Response)
wrappedHttpGet url = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest (T.unpack url)
    result <- try $ httpLbs request manager
    case result of
        Left err -> return $ Left $ QiError "HTTP_REQUEST_FAILED" (show err) NetworkError Nothing Nothing =<< getCurrentTime
        Right response -> return $ Right response
```

### Circuit Breaker Integration
```haskell
-- All external calls (HTTP, AI/LLM, Database) must integrate circuit breaker
-- Use state machine pattern from design analysis

data CircuitState = Closed | Open | HalfOpen
newtype CircuitBreaker = CircuitBreaker (TVar CircuitState)

withCircuitBreaker :: CircuitBreaker -> IO (Result e a) -> IO (Result e a)
withCircuitBreaker breaker action = do
    state <- readTVarIO (cbState breaker)
    case state of
        Open -> return $ Left $ QiError "CIRCUIT_OPEN" "Circuit breaker is open" CircuitBreakerError Nothing Nothing =<< getCurrentTime
        _ -> do
            result <- action
            updateCircuitState breaker result
            return result
```

### Performance Optimization
```haskell
-- Use strict evaluation for hot paths
{-# LANGUAGE BangPatterns #-}

-- Use unboxed types for primitives
import Data.Vector.Unboxed as U

-- Use streaming for large datasets
import Streaming.Prelude as S

-- Profile with GHC profiling
{-# OPTIONS_GHC -fprof-auto #-}
```

## Base Component Templates

## Core Component Templates  

## Application Component Templates

[Templates for each component using the packages above, implementing the exact operations specified in the design analysis, with Haskell-specific optimizations and mathematical law preservation]

## Output Requirements

Generate two files:
1. `build/impl/qi.v4.haskell.template.md` - Complete Haskell code templates
2. `build/impl/qi.v4.haskell.impl.md` - Implementation guide with package integration examples

Each template must:
- Use exact packages identified in research
- Implement ALL operations from design analysis
- Preserve mathematical properties (monad/functor/monoid laws)
- Meet functional tier performance targets (50× baseline)
- Include property-based testing with QuickCheck
- Provide complete type signatures
- Handle all error cases with Result<T>