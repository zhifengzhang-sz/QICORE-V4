# QiCore v4.0 Common Context

> **Shared Context for All Development Stages**  
> **Purpose**: Mathematical foundations, performance models, and common patterns  
> Version: v4.0.1  
> Date: June 17, 2025

## Mathematical Foundations

### Category Theory Knowledge (Simplified)

**Key Concepts to Apply:**
- **Monads**: For Result<T> error handling and async operations
- **Monoids**: For Configuration merging with associative operations and identity  
- **Functors**: For data structure transformations and component boundaries
- **Natural Transformations**: For cross-language behavioral consistency
- **Simple Effects**: For logging and other side effects (not free monads)

**Avoid Over-Engineering:**
- Use monoids instead of sheaves for configuration
- Use simple effect interfaces instead of free monads for logging
- Focus on practical patterns over theoretical purity

**Source References:**
- **Spivak's "Polynomial Functors and Optics"**: https://arxiv.org/abs/1909.07668
- **Fong & Spivak's "Seven Sketches in Compositionality"**: https://arxiv.org/abs/1803.05316
- **Bartosz Milewski's "Category Theory for Programmers"**: https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/

### Mathematical Model Contracts

> **Abstract Interface Layer for Design Stage Derivation**  
> **Purpose**: Formal mathematical contracts that design patterns must satisfy  
> **Usage**: Referenced by design.prompt.md to create concrete design patterns

The following abstract mathematical models define interface contracts that all implementations must satisfy. These contracts bridge formal specifications to practical design patterns.

#### Abstract Monad Contract
```haskell
class Monad m where
  return :: a -> m a           -- Unit operation (pure value injection)
  (>>=) :: m a -> (a -> m b) -> m b  -- Bind operation (sequential composition)
  
  -- Laws that must be preserved by all implementations:
  -- Left Identity:   return a >>= f  ≡  f a
  -- Right Identity:  m >>= return   ≡  m  
  -- Associativity:   (m >>= f) >>= g  ≡  m >>= (\x -> f x >>= g)
```

**Design Derivation**: Error handling (Result), async operations (Promise), state management (IO)

#### Abstract Functor Contract
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b   -- Structure-preserving transformation
  
  -- Laws that must be preserved:
  -- Identity:     fmap id ≡ id
  -- Composition:  fmap (f . g) ≡ fmap f . fmap g
```

**Design Derivation**: Data transformations, component boundaries, API mappings

#### Abstract Monoid Contract
```haskell
class Monoid a where
  mempty :: a                  -- Identity element
  mappend :: a -> a -> a       -- Associative binary operation
  
  -- Laws that must be preserved:
  -- Left Identity:   mappend mempty x ≡ x
  -- Right Identity:  mappend x mempty ≡ x
  -- Associativity:   mappend x (mappend y z) ≡ mappend (mappend x y) z
```

**Design Derivation**: Configuration merging, accumulation patterns, parallel composition

#### Abstract Effect Interface Contract
```haskell
class Effect e where
  perform :: e a -> IO a       -- Execute effect in IO context
  pure :: a -> e a            -- Lift pure value to effect
  
  -- Laws for effect composition:
  -- Effect order preservation
  -- Resource cleanup guarantees
  -- Error propagation rules
```

**Design Derivation**: Logging operations, IO operations, side effect management

#### Abstract State Machine Contract
```haskell
data StateMachine s i o where
  Transition :: s -> i -> (s, o)  -- State transition function
  Initial :: s                     -- Initial state
  
  -- Laws for state transitions:
  -- Deterministic: same input + state → same output + state
  -- Total: defined for all valid state/input combinations
```

**Design Derivation**: Circuit breaker pattern, connection states, protocol handlers

#### Abstract Stream Contract
```haskell
data Stream a where
  Empty :: Stream a
  Cons :: a -> Stream a -> Stream a
  
  -- Laws for stream operations:
  -- Lazy evaluation: elements computed on demand
  -- Backpressure: consumer controls production rate
  -- Resource safety: cleanup on termination
```

**Design Derivation**: HTTP streaming, document processing, large data handling

### Contract Hierarchy

```
Abstract Contracts (mathematical-contracts)
    ↓ (specialized by formal.spec.md)
Concrete Mathematical Models  
    ↓ (implemented via design.prompt.md)
Design Patterns
    ↓ (realized in impl.prompt.md)
Language-Agnostic Implementation Templates
    ↓ (specialized via impl.[LANG].prompt.md)
Language-Specific Code
```

### Contract Compliance Verification

All design patterns derived from these contracts must:

1. **Preserve Mathematical Laws**: Monad laws, monoid laws, functor laws
2. **Maintain Type Safety**: Proper categorical structure
3. **Enable Composition**: Natural transformations between components
4. **Support Performance Tiers**: Meet language-specific performance requirements

### Categorical Structures

The specification employs the following categorical constructions:
- **Monads**: For error handling (Result) and stateful operations (Cache, Database)
- **Functors**: For type transformations and component boundaries
- **Natural Transformations**: For cross-language behavioral consistency
- **Monoids**: For configuration merging with precedence
- **Effect Interfaces**: For logging and other side effects
- **Monad Transformers**: For async operations composition
- **Stream Coalgebras**: For streaming operations in HTTP, Document generation, and Web responses
- **State Machines**: For circuit breaker pattern and connection management
- **IO Monads**: For web framework request/response handling and ASGI server lifecycle
- **Reader Monad**: For dependency injection in web applications and AI client configuration
- **Protocol Functors**: For MCP message transformation and serialization
- **Transaction Monads**: For database operations with ACID properties
- **Continuation Monads**: For async/await patterns in server operations

## Performance Tier Model

Performance requirements are specified per language tier:

**Tier Definitions:**
- **Functional** (Haskell): 50× baseline
- **Interpreted** (Python, TypeScript/Node.js): 100× baseline

**Currently Supported Languages:**
- **Haskell**: Functional tier with excellent mathematical properties
- **Python**: Interpreted tier with comprehensive ecosystem  
- **TypeScript**: Interpreted tier with strong type safety

**Example Performance Targets:**
If baseline Result creation is 1μs, then:
- Functional (Haskell): < 50μs
- Interpreted (Python/TypeScript): < 100μs

**Critical Operation Targets:**
```
Operation               Functional (Haskell)  Interpreted (Python/TypeScript)
-------------------------------------------------------------------------
Result creation         50μs                   100μs
Log level check         10ns                   10ns
Cache get (memory)      500μs                  1ms
HTTP circuit check      <1ms                   <1ms
Config validation       5ms                    10ms
Template compilation    50ms                   100ms
Web request handling    5ms                    10ms
ASGI connection         500μs                  1ms
AI/LLM API call         100ms                  200ms
MCP message parsing     500μs                  1ms
Database query          50ms                   100ms
```

## Component Architecture

### Base Component (Foundation)
- **Result<T>**: Monad for error handling with 8 operations
- **QiError**: Product type with 8 categories and 6 operations
- **No Dependencies**: Forms foundational category

### Core Component (Infrastructure)
- **Configuration**: Monoid with 9 operations (4 loading + 1 merge + 4 validation)
- **Logger**: Effect interface with 7 operations (1 factory + 5 levels + 1 performance)
- **Cache**: State monad with 9 operations (2 factories + 7 cache operations)
- **Depends on**: Base Component only

### Application Components (Domain-Specific)
- **HTTP**: 7 operations (5 basic + 1 streaming + 1 circuit breaker)
- **Document**: 6 operations (3 basic + 1 streaming + 1 batch + 1 validation)
- **CLP**: 5 operations (2 parsing + 1 validation + 2 help)
- **Web Framework**: 8 operations (4 routing + 2 middleware + 1 static + 1 error handling)
- **ASGI Server**: 6 operations (2 lifecycle + 2 connection + 1 worker + 1 monitoring)
- **AI/LLM Client**: 7 operations (3 chat + 1 streaming + 1 embedding + 1 config + 1 circuit breaker)
- **MCP Protocol**: 6 operations (2 connection + 2 messaging + 1 resource + 1 tool)
- **Database**: 8 operations (4 CRUD + 2 transaction + 1 migration + 1 connection pool)
- **Depends on**: Base + Core Components

## Required Patterns

### Error Recovery Patterns
All operations must support error recovery:
1. **Result.orElse**: Provide alternative on failure
2. **Retry with Backoff**: Exponential backoff for transient failures
3. **Circuit Breaking**: Fail fast when service is down
4. **Fallback Values**: Configuration defaults, cached data

### Circuit Breaker Pattern
State machine with three states:
- **CLOSED(failures)**: Normal operation, counting failures
- **OPEN(timestamp)**: Failing fast, waiting for timeout
- **HALF_OPEN(successes)**: Testing recovery, counting successes

### Stream Processing Pattern
For HTTP, Document, Web response, and AI streaming:
- **Lazy Evaluation**: Elements produced on demand
- **Backpressure**: Consumer controls flow rate
- **Resource Management**: Proper cleanup on termination
- **Error Propagation**: Errors terminate stream gracefully

### Request/Response Pattern
For Web Framework operations:
- **IO Monad**: Request processing in functional context
- **Middleware Composition**: Function composition for request pipeline
- **Error Handling**: Unified error responses via Result<T>
- **Resource Cleanup**: Automatic cleanup via RAII patterns

### Connection Management Pattern
For ASGI Server and Database:
- **Connection Pooling**: Resource sharing with lifecycle management
- **Health Monitoring**: Connection state validation
- **Graceful Shutdown**: Clean termination of active connections
- **Backpressure**: Flow control for incoming connections

### Protocol Message Pattern
For MCP Protocol operations:
- **Message Serialization**: Bidirectional protocol transformations
- **Type Safety**: Protocol compliance via type system
- **Error Recovery**: Protocol-level error handling
- **Versioning**: Backward-compatible protocol evolution

## Operation Coverage Requirements

### Total Operation Count: 99 Operations

**Base Component (22 operations):**
- Result: 8 operations (unit, bind, map, flatMap, unwrap, unwrapOr, match, orElse)
- QiError: 6 operations (create, toString, toStructuredData, getCategory, withContext, withCause)
- Error Categories: 8 categories (VALIDATION, NETWORK, FILESYSTEM, CONFIGURATION, CACHE, TIMEOUT, PERMISSION, UNKNOWN)

**Core Component (25 operations):**
- Configuration: 9 operations (4 loading + 1 merge + 4 validation)
- Logger: 7 operations (1 factory + 5 levels + 1 performance)
- Cache: 9 operations (2 factories + 7 cache operations)

**Application Components (52 operations):**
- HTTP: 7 operations (5 basic + 1 streaming + 1 circuit breaker)
- Document: 6 operations (3 basic + 1 streaming + 1 batch + 1 validation)
- CLP: 5 operations (2 parsing + 1 validation + 2 help)
- Web Framework: 8 operations (4 routing + 2 middleware + 1 static + 1 error handling)
- ASGI Server: 6 operations (2 lifecycle + 2 connection + 1 worker + 1 monitoring)
- AI/LLM Client: 7 operations (3 chat + 1 streaming + 1 embedding + 1 config + 1 circuit breaker)
- MCP Protocol: 6 operations (2 connection + 2 messaging + 1 resource + 1 tool)
- Database: 8 operations (4 CRUD + 2 transaction + 1 migration + 1 connection pool)

## Language-Specific Context

### TypeScript
- **Packages**: fp-ts, zod, fastify, drizzle-orm, modern async patterns
- **Patterns**: Union types, async/await, functional programming
- **Performance**: JIT optimization, async boundaries
- **Tier**: Interpreted (100× baseline performance)

### Python
- **Packages**: returns, cytoolz, fastapi, drizzle-equivalent, asyncio
- **Patterns**: Type hints, dataclasses, async/await
- **Performance**: GIL considerations, asyncio event loop
- **Tier**: Interpreted (100× baseline performance)

### Haskell
- **Packages**: category-theory native, IHP, servant, persistent, STM
- **Patterns**: Type classes, lazy evaluation, pure functions
- **Performance**: Lazy evaluation, GHC optimization
- **Tier**: Functional (50× baseline performance)

## Common Pitfalls to Avoid

### Over-Engineering
- ❌ Using sheaf theory for configuration
- ❌ Free monads for simple logging
- ❌ Complex operad compositions
- ✅ Use simple, practical mathematical models

### Unrealistic Performance
- ❌ "< 100ns for all operations"
- ❌ Same performance across all languages
- ✅ Language-tier based targets
- ✅ Acknowledge hardware/runtime variations

### Missing Implementations
- ❌ Interface definitions without examples
- ❌ "TODO: implement circuit breaker"
- ✅ Complete, runnable code
- ✅ All patterns fully implemented

### Ignoring Practical Concerns
- ❌ No error recovery patterns
- ❌ No streaming for large data
- ✅ Resilience patterns throughout
- ✅ Memory-efficient implementations

## Verification Principles

### Complete Coverage
Every operation from formal specification must be addressed in design and implementation.

### Mathematical Consistency
All categorical laws must be preserved:
- Monad laws for Result and async operations
- Monoid laws for Configuration
- Functor laws for component boundaries
- Natural transformation coherence

### Performance Validation
Designs must meet tier-specific targets and include realistic performance expectations.

### Pattern Implementation
All required patterns must be included:
- Error recovery via Result.orElse
- Circuit breaker state machine
- Stream coalgebras for large data
- Performance tier awareness 