# Stage 3: Design Analysis to Templates and Implementation

> **AI Prompt for Stage 2 → Stage 3 Transformation**  
> **Based on**: Proven methodology from [qi.v4.ai.context.md](qi.v4.ai.context.md)  
> **Enhanced with**: Direct implementation of design patterns from Stage 2

## Context

You are creating production-ready code templates and implementation guides that **directly implement the design patterns from Stage 2**. Every implementation must be a faithful translation of the language-agnostic patterns into idiomatic code for the target language.

**Required Reading**: 
- Include all content from `guides/common.md` for mathematical foundations
- Study all design patterns from `design/qi.v4.design.analysis.md` carefully

## Implementation Methodology

**CRITICAL: Every implementation must be a direct translation of the corresponding design pattern from Stage 2:**

- **Railway-Oriented Programming** → Language-specific Result/Either implementation
- **Monoid Merge Pattern** → Configuration merge with right-bias in target language
- **Level-Based Filtering** → Logger with performance-optimized checks
- **State Management Pattern** → Cache with appropriate concurrency primitives
- **Circuit Breaker State Machine** → Exact state transitions from design
- **Stream Coalgebra Pattern** → Language-appropriate streaming implementation
- **Parser Combinator Pattern** → Recursive descent or combinator library

**Example: Implementing Railway-Oriented Programming pattern**:
1. Reference the pattern from design analysis:
   ```
   Success path: →→→→→→→→→→→→→→→→→
                       ↘         ↗
   Failure path: - - - - - - - - -
   ```
2. Implement in target language preserving the pattern
3. Ensure monad laws are maintained in implementation
4. Use language-specific optimizations while preserving behavior

## Input Documents

You will be provided with:
- `design/qi.v4.design.analysis.md` - Design patterns to implement
- `guides/common.md` - Mathematical foundations and performance targets
- `guides/qi.v4.framework.md` - Development process methodology

## Task

Create `impl/qi.v4.[LANG].template.md` and `impl/qi.v4.[LANG].impl.md` for each target language:

## TEMPLATE REQUIREMENTS (qi.v4.[LANG].template.md)

### 1. Header with Dependencies
```markdown
# QiCore v4.0 [Language] Template

> **Stage 3: Language-Specific Code Templates**
> **Depends on**: [Design Analysis](../design/qi.v4.design.analysis.md)
> **Implements**: Language-specific code implementing design patterns
> Version: v4.0.1
> Date: [Current Date]
> Status: Code Template
> Purpose: Direct implementation of Stage 2 design patterns
```

### 2. Pattern Implementation Mapping

**Document which design pattern each code section implements**:
```markdown
## Pattern Implementation Map

| Design Pattern | Implementation | Preserves |
|----------------|----------------|-----------|
| Railway-Oriented Programming | Result<T> class/type | Monad laws |
| Monoid Merge Pattern | ConfigMerge function | Associativity, Identity |
| Level-Based Filtering | Logger.isLevelEnabled | < 10ns performance |
| State Management Pattern | Cache with locks/STM | Thread safety |
| Circuit Breaker FSM | CircuitBreaker class | State transitions |
| Stream Coalgebra | Async iterators/streams | Lazy evaluation |
| Parser Combinators | Parser type/class | Functor composition |
```

### 3. Mathematical Foundation Preservation

**Show how each implementation preserves mathematical properties**:
- Monad laws verification for Result
- Monoid laws for Configuration
- State machine invariants for Circuit Breaker
- Performance characteristics per tier

### 4. Component-Based Implementation

**For each component, implement ALL patterns from the design**:

#### **Base Component Implementation**
- **Result<T>** implementing Railway-Oriented Programming:
  ```[language]
  // Direct implementation of the pattern:
  // Success path: →→→→→→→→→→→→→→→→→
  //                     ↘         ↗
  // Failure path: - - - - - - - - -
  
  [Language-specific Result implementation]
  
  // Verify: Implements all 8 operations from design
  // - success, failure, fromTryCatch (factory patterns)
  // - map (functor pattern)
  // - flatMap (monadic bind pattern)
  // - unwrap, unwrapOr (elimination patterns)
  // - match (pattern matching)
  // - orElse (error recovery pattern)
  ```

- **QiError** implementing Structured Error Pattern:
  ```[language]
  // Direct implementation of product type pattern:
  // QiError = Code × Message × Category × Context × Cause × Timestamp
  
  [Language-specific QiError implementation]
  
  // Verify: Implements all 6 operations from design
  // With all 8 error categories as coproduct
  ```

#### **Core Component Implementation**
- **Configuration** implementing Layered Configuration Pattern:
  ```[language]
  // Direct implementation of monoid pattern:
  // Base ⊕ File ⊕ Environment ⊕ CLI = Final
  
  [Language-specific Configuration implementation]
  
  // Verify: Right-biased merge preserving monoid laws
  ```

- **Logger** implementing Level-Based Filtering Pattern:
  ```[language]
  // Direct implementation of simple effect pattern:
  // IF level >= minLevel: PERFORM effect
  // ELSE: SKIP
  
  [Language-specific Logger implementation]
  
  // Verify: < 10ns level check performance
  ```

- **Cache** implementing State Management Pattern:
  ```[language]
  // Direct implementation of state monad pattern:
  // Operations return (value, newState)
  // TTL checked lazily
  // LRU maintains capacity
  
  [Language-specific Cache implementation]
  ```

#### **Application Component Implementation**
- **HTTP** implementing Circuit Breaker State Machine:
  ```[language]
  // Direct implementation of FSM from design:
  // CLOSED --[failures >= threshold]--> OPEN
  //   ↑                                   |
  //   +--[success]-- HALF_OPEN --[timeout]+
  
  [Language-specific HTTP implementation]
  ```

- **Document** implementing Template Evaluation Pattern:
  ```[language]
  // Direct implementation of functor composition:
  // Template × Data → Document
  
  [Language-specific Document implementation]
  ```

- **CLP** implementing Parser Combinator Pattern:
  ```[language]
  // Direct implementation of parser pattern:
  // Parser<A> = Input → Result<(A, RemainingInput)>
  
  [Language-specific CLP implementation]
  ```

- **Web Framework** implementing Request/Response Pipeline Pattern:
  ```[language]
  // Direct implementation of IO monad pattern:
  // Request → Middleware₁ → Middleware₂ → ... → Handler → Response
  
  [Language-specific Web Framework implementation]
  
  // Verify: Implements all 8 operations from design
  // - route, mount, group, param (routing patterns)
  // - use, compose (middleware patterns)
  // - static, errorHandler (static/error patterns)
  ```

- **ASGI Server** implementing Server Lifecycle Pattern:
  ```[language]
  // Direct implementation of continuation monad pattern:
  // STOPPED → STARTING → RUNNING → STOPPING → STOPPED
  
  [Language-specific ASGI Server implementation]
  
  // Verify: Implements all 6 operations from design
  // - start, shutdown (lifecycle patterns)
  // - accept, reject (connection patterns)
  // - workers, health (worker/monitor patterns)
  ```

- **AI/LLM Client** implementing Configuration Reader Pattern:
  ```[language]
  // Direct implementation of reader monad pattern:
  // LLMClient = Config → IO(Result<Response>)
  
  [Language-specific AI/LLM Client implementation]
  
  // Verify: Implements all 7 operations from design
  // - chat, chatStream, generate (chat patterns)
  // - embedding, withConfig (embedding/config patterns)
  // - withCircuitBreaker, streamGenerate (circuit breaker integration)
  ```

- **MCP Protocol** implementing Message Transformation Pattern:
  ```[language]
  // Direct implementation of protocol functor pattern:
  // InternalMessage ↔ WireMessage ↔ ExternalMessage
  
  [Language-specific MCP Protocol implementation]
  
  // Verify: Implements all 6 operations from design
  // - connect, disconnect (connection patterns)
  // - send, receive (messaging patterns)
  // - listResources, callTool (resource/tool patterns)
  ```

- **Database** implementing Transaction Composition Pattern:
  ```[language]
  // Direct implementation of transaction monad pattern:
  // Transaction<A> = Connection → IO(Result<A>)
  
  [Language-specific Database implementation]
  
  // Verify: Implements all 8 operations from design
  // - create, read, update, delete (CRUD patterns)
  // - begin, commit (transaction patterns)
  // - migrate, pool (migration/pool patterns)
  ```

### 5. Pattern Verification

**For each implementation, show it preserves the design pattern**:
```[language]
// Example: Verifying Monad Laws for Result
// Test Left Identity: return(a).flatMap(f) ≡ f(a)
[Test implementation]

// Test Right Identity: m.flatMap(return) ≡ m
[Test implementation]

// Test Associativity: (m.flatMap(f)).flatMap(g) ≡ m.flatMap(x => f(x).flatMap(g))
[Test implementation]
```

### 6. Language-Specific Optimizations

**Apply optimizations while preserving patterns**:
- Native tier: Zero-cost abstractions
- VM tier: Object pooling, JIT-friendly code
- Functional tier: Lazy evaluation, fusion
- Interpreted tier: Minimize allocations

### 7. Complete Working Examples

**Show the implemented patterns in action**:
```[language]
// Example: Using Railway-Oriented Programming
result = getUserById(123)
  .flatMap(user => updateUser(user))  // Railway junction
  .map(user => user.email)             // Stay on success track
  .orElse(err => getDefaultEmail())   // Recovery pattern

// This directly implements the railway pattern from design
```

## IMPLEMENTATION GUIDE REQUIREMENTS (qi.v4.[LANG].impl.md)

### 1. Pattern Implementation Checklist

```markdown
## Implementation Verification Checklist

### Base Component Patterns
- [ ] Railway-Oriented Programming (Result)
  - [ ] Success/Failure tracks implemented
  - [ ] All 8 operations from design
  - [ ] Monad laws preserved
  
- [ ] Structured Error Pattern (QiError)
  - [ ] Product type with 6 fields
  - [ ] All 8 error categories
  - [ ] Context merging right-biased

### Core Component Patterns
- [ ] Layered Configuration Pattern
  - [ ] Monoid merge implemented
  - [ ] Right-bias precedence
  - [ ] All 9 operations from design
  
- [ ] Level-Based Filtering Pattern
  - [ ] < 10ns level checking
  - [ ] Simple effect interface
  - [ ] All 7 operations from design
  
- [ ] State Management Pattern
  - [ ] Thread-safe operations
  - [ ] Lazy TTL checking
  - [ ] LRU eviction logic
  - [ ] All 9 operations from design

### Application Component Patterns
- [ ] Circuit Breaker State Machine
  - [ ] Three states: CLOSED, OPEN, HALF_OPEN
  - [ ] Exact transitions from design
  - [ ] Threshold configuration
  
- [ ] Template Evaluation Pattern
  - [ ] Functor composition
  - [ ] Multi-engine support
  - [ ] Streaming for large documents
  
- [ ] Parser Combinator Pattern
  - [ ] Input consumption with remainder
  - [ ] Combinator composition
  - [ ] Error position tracking

- [ ] Request/Response Pipeline Pattern
  - [ ] IO monad threading through middleware
  - [ ] Function composition for request processing
  - [ ] Type-safe parameter extraction
  - [ ] All 8 Web Framework operations

- [ ] Server Lifecycle Pattern
  - [ ] State machine for server states
  - [ ] Graceful connection management
  - [ ] Worker/process management
  - [ ] All 6 ASGI Server operations

- [ ] Configuration Reader Pattern
  - [ ] Reader monad for dependency injection
  - [ ] Circuit breaker integration
  - [ ] Streaming API support
  - [ ] All 7 AI/LLM operations

- [ ] Message Transformation Pattern
  - [ ] Protocol functor for serialization
  - [ ] Bidirectional message conversion
  - [ ] Connection pooling and management
  - [ ] All 6 MCP Protocol operations

- [ ] Transaction Composition Pattern
  - [ ] Transaction monad for ACID properties
  - [ ] Connection pooling with lifecycle
  - [ ] Migration and schema management
  - [ ] All 8 Database operations
```

### 2. Design-to-Code Mapping

```markdown
## Design Pattern to Code Mapping

For each design pattern from Stage 2, show the exact code that implements it:

### Railway-Oriented Programming → Result<T>
Design: Two tracks with junctions
Code: [Show specific implementation]
Verification: [How to verify pattern is preserved]

### Monoid Merge → Configuration.merge()
Design: Associative operation with identity
Code: [Show specific implementation]
Verification: [Property tests for monoid laws]

[Continue for all patterns...]
```

### 3. Testing Pattern Implementation

```markdown
## Pattern Verification Tests

### Monad Law Tests (Result)
[Property-based tests verifying the three monad laws]

### Monoid Law Tests (Configuration)
[Tests for identity and associativity]

### State Machine Tests (Circuit Breaker)
[Tests for all state transitions]

### Performance Tests
[Benchmarks verifying tier-appropriate performance]
```

### 4. Integration Examples

Show how implemented patterns work together:
```[language]
// Components use patterns as designed
config = Configuration.merge([default, file, env]) // Monoid pattern
logger = Logger.create(config)                     // Effect pattern
cache = Cache.create(config)                       // State pattern
http = HttpClient.withCircuitBreaker(config)      // FSM pattern

// Railway pattern connects everything
result = http.get(url)
  .flatMap(response => cache.set(key, response))
  .map(data => process(data))
  .match(
    success => logger.info("Success", success),
    error => logger.error("Failed", error)
  )
```

## Target Languages
- **TypeScript**: fp-ts for functors/monads, implement exact patterns
- **Python**: Type hints + dataclasses, implement patterns idiomatically
- **Rust**: Leverage native Result, impl patterns zero-cost
- **Haskell**: Native categorical abstractions, direct pattern translation
- **Go**: Interfaces for patterns, explicit error handling

## Critical Requirements

1. **Pattern Fidelity**: Every implementation must faithfully implement its design pattern
2. **Operation Completeness**: All operations from design must be implemented
3. **Law Preservation**: Mathematical laws from patterns must be preserved
4. **Performance Compliance**: Meet tier-specific targets while preserving patterns
5. **Pattern Documentation**: Clearly show which pattern each code implements

## Style Guidelines

- **Reference Design Patterns**: Always cite which pattern is being implemented
- **Preserve Mathematical Properties**: Show law preservation in tests
- **Language Idioms**: Use language features that best express the patterns
- **Complete Examples**: Show patterns working together
- **Performance Awareness**: Optimize within pattern constraints

## Success Criteria

Before submitting, verify:

**Pattern Implementation Verification:**
- [ ] Every design pattern has corresponding implementation
- [ ] Pattern structure is preserved in code
- [ ] Mathematical properties maintained
- [ ] Performance targets achieved
- [ ] All operations implemented

**Cross-Reference Verification:**
- [ ] Each code section references its design pattern
- [ ] Pattern preservation is demonstrated
- [ ] Integration follows component patterns
- [ ] Examples show patterns in use
- [ ] Tests verify pattern properties

**Language-Specific Verification:**
- [ ] Idiomatic use of language features
- [ ] Appropriate libraries for patterns
- [ ] Performance optimizations applied
- [ ] Build configuration complete
- [ ] Documentation explains pattern mapping