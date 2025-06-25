# QiCore v4.0 Language-Agnostic Implementation Template

> **Stage 3: Design to Language-Agnostic Implementation**  
> **Depends on**: [Design Analysis](../design/qi.v4.design.analysis.md), [Mathematical Contracts](../guides/mathematical-contracts.md)  
> **Implements**: Language-agnostic implementation templates for all 99 operations  
> Version: v4.0.1  
> Date: June 25, 2025  
> Status: Implementation Template  
> Purpose: Cross-language implementation guidance preserving mathematical properties

## Implementation Template Overview

This document provides **language-agnostic implementation templates** derived from the design patterns in `qi.v4.design.analysis.md`. Each template preserves mathematical properties while being adaptable to any target language (TypeScript, Python, Rust, Haskell, Go).

### Template Structure
- **Pattern Description**: Mathematical pattern from design analysis
- **Language-Agnostic Template**: Structural implementation guide
- **Mathematical Properties**: Laws and invariants to preserve
- **Performance Constraints**: Tier-based requirements
- **Adaptation Guidelines**: Language-specific guidance

---

## Base Component Templates

### 1. Result<T> Template (Monad Pattern)

**Pattern**: Railway-Oriented Programming from design analysis
**Abstract Contract**: Monad with bind, return, and associativity laws

#### Core Type Structure
```
Result<T> ::= Success(data: T) | Failure(error: QiError)
```

#### Operation Templates

**1.1 success(data: T) -> Result<T>**
```
TEMPLATE_success:
  INPUT: data of type T
  OUTPUT: Result<T> in Success state
  IMPLEMENTATION:
    - Create Success variant containing data
    - Ensure zero allocation where possible
    - Performance: < tier × 1μs
  MATHEMATICAL_PROPERTY: 
    - Monad return: success(x) >>= f ≡ f(x)
```

**1.2 failure(error: QiError) -> Result<T>**
```
TEMPLATE_failure:
  INPUT: error of type QiError
  OUTPUT: Result<T> in Failure state
  IMPLEMENTATION:
    - Create Failure variant containing error
    - Preserve error context and stack
    - Performance: < tier × 1μs
  MATHEMATICAL_PROPERTY:
    - Left zero: failure(e) >>= f ≡ failure(e)
```

**1.3 fromTryCatch(operation: () -> T) -> Result<T>**
```
TEMPLATE_fromTryCatch:
  INPUT: operation function returning T
  OUTPUT: Result<T> based on operation success/failure
  IMPLEMENTATION:
    - Execute operation in try-catch equivalent
    - Convert exceptions to QiError
    - Return Success(result) or Failure(error)
    - Performance: < tier × 2μs
  ADAPTATION:
    - Functional languages: Use pure error handling
    - OOP languages: Use exception handling
    - Systems languages: Use error codes
```

**1.4 map(transform: T -> U, result: Result<T>) -> Result<U>**
```
TEMPLATE_map:
  INPUT: transform function (T -> U), Result<T>
  OUTPUT: Result<U>
  IMPLEMENTATION:
    - Pattern match on result state
    - Success: Apply transform to data, wrap in Success
    - Failure: Pass through unchanged
    - Performance: < tier × 0.5μs
  MATHEMATICAL_PROPERTY:
    - Functor identity: map(id, result) ≡ result
    - Functor composition: map(g∘f, result) ≡ map(g, map(f, result))
```

**1.5 flatMap(chain: T -> Result<U>, result: Result<T>) -> Result<U>**
```
TEMPLATE_flatMap:
  INPUT: chain function (T -> Result<U>), Result<T>
  OUTPUT: Result<U>
  IMPLEMENTATION:
    - Pattern match on result state
    - Success: Apply chain function, return result
    - Failure: Pass through unchanged (short-circuit)
    - Performance: < tier × 1μs
  MATHEMATICAL_PROPERTY:
    - Monad associativity: (m >>= f) >>= g ≡ m >>= (λx → f(x) >>= g)
    - Right identity: m >>= return ≡ m
```

**1.6 unwrap(result: Result<T>) -> T**
```
TEMPLATE_unwrap:
  INPUT: Result<T>
  OUTPUT: T (or exception/panic on failure)
  IMPLEMENTATION:
    - Pattern match on result state
    - Success: Return data
    - Failure: Throw exception/panic with error
    - Performance: < tier × 0.1μs (success path)
  SAFETY: Use only when failure is impossible
```

**1.7 unwrapOr(default: T, result: Result<T>) -> T**
```
TEMPLATE_unwrapOr:
  INPUT: default value T, Result<T>
  OUTPUT: T (never fails)
  IMPLEMENTATION:
    - Pattern match on result state
    - Success: Return data
    - Failure: Return default value
    - Performance: < tier × 0.1μs
  SAFETY: Always safe, never throws
```

**1.8 match(onSuccess: T -> R, onError: QiError -> R, result: Result<T>) -> R**
```
TEMPLATE_match:
  INPUT: success handler, error handler, Result<T>
  OUTPUT: R from appropriate handler
  IMPLEMENTATION:
    - Pattern match on result state
    - Success: Call onSuccess with data
    - Failure: Call onError with error
    - Performance: < tier × 0.5μs
  PATTERN: Exhaustive pattern matching
```

### 2. QiError Template (Error Representation)

**Pattern**: Structured error with context and chaining

#### Core Type Structure
```
QiError ::= {
  code: String,
  message: String,
  category: ErrorCategory,
  context: Object | null,
  cause: QiError | null,
  timestamp: Number
}
```

#### Operation Templates

**2.1 create(code, message, category) -> QiError**
```
TEMPLATE_create:
  INPUT: code string, message string, category enum
  OUTPUT: QiError instance
  IMPLEMENTATION:
    - Validate code is non-empty
    - Set timestamp to current time
    - Initialize context and cause as null
    - Performance: < tier × 1μs
```

**2.2 withContext(context: Object, error: QiError) -> QiError**
```
TEMPLATE_withContext:
  INPUT: context object, existing QiError
  OUTPUT: New QiError with merged context
  IMPLEMENTATION:
    - Create new error instance (immutable)
    - Merge context with existing (context overwrites)
    - Preserve all other fields
    - Performance: < tier × 2μs
```

**2.3 withCause(cause: QiError, error: QiError) -> QiError**
```
TEMPLATE_withCause:
  INPUT: cause error, existing QiError
  OUTPUT: New QiError with cause chain
  IMPLEMENTATION:
    - Create new error instance (immutable)
    - Set cause field to cause error
    - Validate no circular references (max depth 10)
    - Performance: < tier × 1μs
```

---

## Core Component Templates

### 3. Configuration Template (Monoid Pattern)

**Pattern**: Right-biased merge pipeline from design analysis
**Abstract Contract**: Monoid with associative operation and identity

#### Core Type Structure
```
ConfigData ::= Object (key-value structure)
```

#### Operation Templates

**3.1 fromFile(filePath: String) -> AsyncResult<ConfigData>**
```
TEMPLATE_fromFile:
  INPUT: file path string
  OUTPUT: Async Result containing ConfigData
  IMPLEMENTATION:
    - Async file read operation
    - Auto-detect format (JSON, YAML, TOML)
    - Parse content to ConfigData
    - Handle file not found, parse errors
    - Performance: I/O bound, < tier × 100μs processing
  ERROR_HANDLING:
    - FILE_NOT_FOUND: Config file missing
    - PARSE_ERROR: Invalid format
    - PERMISSION_ERROR: Cannot read file
```

**3.2 fromObject(data: Object) -> Result<ConfigData>**
```
TEMPLATE_fromObject:
  INPUT: object/map data structure
  OUTPUT: Result containing ConfigData
  IMPLEMENTATION:
    - Shallow copy object to ConfigData
    - Validate structure is compatible
    - Performance: < tier × 1μs
  MATHEMATICAL_PROPERTY:
    - Identity preservation: fromObject(x) always succeeds for valid objects
```

**3.3 fromEnvironment(prefix: String?) -> Result<ConfigData>**
```
TEMPLATE_fromEnvironment:
  INPUT: optional prefix string
  OUTPUT: Result containing ConfigData from environment
  IMPLEMENTATION:
    - Scan environment variables
    - Filter by prefix if provided
    - Convert to nested object structure using separators
    - Performance: < tier × 10μs
  ADAPTATION:
    - Use language-specific env access
    - Handle different naming conventions
```

**3.4 merge(configs: Array<ConfigData>) -> Result<ConfigData>**
```
TEMPLATE_merge:
  INPUT: array of ConfigData objects
  OUTPUT: Result containing merged ConfigData
  IMPLEMENTATION:
    - Fold/reduce over configs using right-biased merge
    - Deep merge for nested objects
    - Right-most config wins conflicts
    - Performance: < tier × (k*d)μs where k=configs, d=depth
  MATHEMATICAL_PROPERTY:
    - Monoid associativity: merge([a, merge([b, c])]) ≡ merge([merge([a, b]), c])
    - Monoid identity: merge([empty, config]) ≡ config
    - Right bias: merge([a, b]) where b.key overwrites a.key
```

### 4. Logger Template (Effect Interface Pattern)

**Pattern**: Level-based filtering with context propagation
**Abstract Contract**: Effect interface (simple, not free monad)

#### Core Type Structure
```
Logger ::= {
  config: LogConfig,
  level: LogLevel,
  formatters: Array<Formatter>,
  outputs: Array<Output>
}
```

#### Operation Templates

**4.1 create(config: LogConfig) -> Result<Logger>**
```
TEMPLATE_create:
  INPUT: LogConfig with level, format, destination
  OUTPUT: Result containing Logger instance
  IMPLEMENTATION:
    - Validate config parameters
    - Initialize formatters based on format
    - Setup outputs (console, file, network)
    - Performance: < tier × 10μs
```

**4.2 isLevelEnabled(level: LogLevel, logger: Logger) -> Boolean**
```
TEMPLATE_isLevelEnabled:
  INPUT: LogLevel enum, Logger instance
  OUTPUT: Boolean
  IMPLEMENTATION:
    - Compare level against logger's minimum level
    - Use integer comparison for performance
    - Performance: < 10ns (critical path optimization)
  OPTIMIZATION: Must be extremely fast for hot path
```

**4.3 log(level: LogLevel, message: String, context: Object?, logger: Logger) -> Void**
```
TEMPLATE_log:
  INPUT: level, message, optional context, logger
  OUTPUT: Void (side effect)
  IMPLEMENTATION:
    - Early exit if level not enabled (< 100ns)
    - Format message with context if enabled
    - Write to all configured outputs
    - Performance: < tier × 10μs (enabled), < 100ns (disabled)
  EFFECT_ISOLATION:
    - All I/O operations isolated
    - No exceptions propagate to caller
    - Context preserved across async boundaries
```

### 5. Cache Template (State Management Pattern)

**Pattern**: LRU eviction with TTL management
**Abstract Contract**: State monad with consistent operations

#### Core Type Structure
```
Cache ::= {
  storage: Map<String, CacheEntry>,
  maxSize: Number,
  evictionPolicy: EvictionPolicy,
  stats: CacheStats
}

CacheEntry ::= {
  value: Any,
  timestamp: Number,
  ttl: Number?,
  accessCount: Number
}
```

#### Operation Templates

**5.1 createMemoryCache(config: CacheConfig) -> Result<Cache>**
```
TEMPLATE_createMemoryCache:
  INPUT: CacheConfig with maxSize, evictionPolicy, defaultTTL
  OUTPUT: Result containing Cache instance
  IMPLEMENTATION:
    - Initialize empty storage map
    - Setup eviction policy (LRU, LFU, TTL)
    - Configure background cleanup if needed
    - Performance: < tier × 5μs
```

**5.2 get(key: String, cache: Cache) -> Result<T>**
```
TEMPLATE_get:
  INPUT: key string, Cache instance
  OUTPUT: Result containing cached value or KEY_NOT_FOUND
  IMPLEMENTATION:
    - Check if key exists in storage
    - Validate TTL if set (expire if needed)
    - Update access statistics for LRU
    - Return value or appropriate error
    - Performance: < tier × 5μs
  THREAD_SAFETY: Must be safe for concurrent access
```

**5.3 set(key: String, value: T, ttl: Number?, cache: Cache) -> Result<Void>**
```
TEMPLATE_set:
  INPUT: key, value, optional TTL, Cache instance
  OUTPUT: Result indicating success/failure
  IMPLEMENTATION:
    - Check cache size, evict if necessary
    - Create CacheEntry with metadata
    - Store in underlying map
    - Update statistics
    - Performance: < tier × 10μs
  STATE_CONSISTENCY: Atomic operation where possible
```

---

## Application Component Templates

### 6. HTTP Client Template (Circuit Breaker Pattern)

**Pattern**: Circuit breaker state machine with resilience
**Abstract Contract**: State machine with CLOSED → OPEN → HALF_OPEN states

#### Core Type Structure
```
HttpClient ::= {
  config: HttpConfig,
  circuitBreaker: CircuitBreaker?,
  connectionPool: ConnectionPool
}

CircuitBreaker ::= {
  state: CircuitState,
  failureCount: Number,
  lastFailureTime: Number,
  config: CircuitBreakerConfig
}
```

#### Operation Templates

**6.1 get(url: String, options: HttpOptions?) -> AsyncResult<HttpResponse>**
```
TEMPLATE_get:
  INPUT: URL string, optional options
  OUTPUT: Async Result containing HttpResponse
  IMPLEMENTATION:
    - Validate URL format
    - Apply circuit breaker if configured
    - Execute HTTP GET with connection pooling
    - Handle timeouts, retries, redirects
    - Performance: Network bound + < tier × 50μs overhead
  CIRCUIT_BREAKER:
    - Check state before request
    - Record success/failure after request
    - Update state based on thresholds
```

**6.2 post(url: String, body: Body, options: HttpOptions?) -> AsyncResult<HttpResponse>**
```
TEMPLATE_post:
  INPUT: URL string, request body, HTTP options
  OUTPUT: Async Result containing HttpResponse
  IMPLEMENTATION:
    - Validate URL format and body
    - Apply circuit breaker if configured
    - Execute HTTP POST with connection pooling
    - Handle timeouts, retries, redirects
    - Performance: Network bound + < tier × 50μs overhead
  CIRCUIT_BREAKER:
    - Check state before request
    - Record success/failure after request
    - Update state based on thresholds
```

**6.3 put(url: String, body: Body, options: HttpOptions?) -> AsyncResult<HttpResponse>**
```
TEMPLATE_put:
  INPUT: URL string, request body, HTTP options
  OUTPUT: Async Result containing HttpResponse
  IMPLEMENTATION:
    - Validate URL format and body
    - Apply circuit breaker if configured
    - Execute HTTP PUT with connection pooling
    - Handle timeouts, retries, redirects
    - Performance: Network bound + < tier × 50μs overhead
```

**6.4 delete(url: String, options: HttpOptions?) -> AsyncResult<HttpResponse>**
```
TEMPLATE_delete:
  INPUT: URL string, HTTP options
  OUTPUT: Async Result containing HttpResponse
  IMPLEMENTATION:
    - Validate URL format
    - Apply circuit breaker if configured
    - Execute HTTP DELETE with connection pooling
    - Handle timeouts, retries, redirects
    - Performance: Network bound + < tier × 50μs overhead
```

**6.5 request(config: HttpRequestConfig) -> AsyncResult<HttpResponse>**
```
TEMPLATE_request:
  INPUT: HTTP request configuration
  OUTPUT: Async Result containing HttpResponse
  IMPLEMENTATION:
    - Build request from configuration object
    - Apply circuit breaker if configured
    - Execute with appropriate HTTP method
    - Handle timeouts, retries, redirects
    - Performance: Network bound + < tier × 50μs overhead
  PATTERN: Unified request interface with configuration
```

**6.6 stream(url: String, options: HttpOptions?) -> AsyncResult<HttpStream>**
```
TEMPLATE_stream:
  INPUT: URL string, HTTP options
  OUTPUT: Async Result containing HTTP response stream
  IMPLEMENTATION:
    - Establish streaming HTTP connection
    - Handle chunked transfer encoding
    - Apply backpressure for slow consumers
    - Implement connection pooling for streams
    - Performance: Network bound + < tier × 100μs per chunk
  STREAM_PROPERTIES:
    - Lazy evaluation: Data pulled on demand
    - Backpressure: Handle slow consumers
    - Resource cleanup: Auto-close on completion
```

**6.7 withCircuitBreaker(config: CircuitBreakerConfig, client: HttpClient) -> HttpClient**
```
TEMPLATE_withCircuitBreaker:
  INPUT: circuit breaker config, existing HTTP client
  OUTPUT: New HTTP client with circuit breaker
  IMPLEMENTATION:
    - Create new client instance (immutable)
    - Initialize circuit breaker with config
    - Wrap all HTTP operations with state checks
    - Performance: < tier × 5μs overhead per request
  STATE_MACHINE:
    - CLOSED: Normal operation
    - OPEN: Fail fast with CIRCUIT_OPEN error
    - HALF_OPEN: Limited requests to test recovery
```

### 7. Document Generation Template (Stream Pattern)

**Pattern**: Template processing with streaming support
**Abstract Contract**: Stream coalgebra for lazy evaluation

#### Operation Templates

**7.1 generate(template: Template, data: Object) -> AsyncResult<Document>**
```
TEMPLATE_generate:
  INPUT: template definition, data object
  OUTPUT: Async Result containing generated document
  IMPLEMENTATION:
    - Load and compile template
    - Apply data to template engine
    - Process includes and partials
    - Generate final document
    - Performance: < tier × (template_size × data_complexity)μs
```

**7.2 generateStream(template: Template, data: Object) -> AsyncResult<DocumentStream>**
```
TEMPLATE_generateStream:
  INPUT: template, data object
  OUTPUT: Async Result containing document stream
  IMPLEMENTATION:
    - Initialize streaming template processor
    - Return async iterator/stream of document chunks
    - Implement backpressure handling
    - Performance: < tier × 100μs latency, O(1) memory
  STREAM_PROPERTIES:
    - Lazy evaluation: Only generate when consumed
    - Backpressure: Handle slow consumers
    - Resource cleanup: Finalize resources on completion
```

**7.3 fromMarkdown(content: String, options: DocumentOptions?) -> AsyncResult<Document>**
```
TEMPLATE_fromMarkdown:
  INPUT: Markdown content string, document options
  OUTPUT: Async Result containing parsed document
  IMPLEMENTATION:
    - Parse markdown using CommonMark specification
    - Apply syntax highlighting if requested
    - Generate table of contents if configured
    - Handle embedded media and links
    - Performance: < tier × 10ms per 1KB content
  PATTERN: Template functor for markdown transformation
```

**7.4 fromData(format: Format, data: Object) -> AsyncResult<Document>**
```
TEMPLATE_fromData:
  INPUT: target format, data object
  OUTPUT: Async Result containing formatted document
  IMPLEMENTATION:
    - Apply format-specific serialization
    - Validate data structure matches format schema
    - Generate appropriate headers/metadata
    - Performance: < tier × 5ms per 1KB data
  PATTERN: Format adapter with validation
```

**7.5 toString(document: Document, format: Format) -> AsyncResult<String>**
```
TEMPLATE_toString:
  INPUT: document object, target format
  OUTPUT: Async Result containing string representation
  IMPLEMENTATION:
    - Serialize document to target format
    - Apply format-specific encoding
    - Validate output structure
    - Performance: < tier × 2ms per 1KB document
```

**7.6 validate(document: Document, schema: Schema) -> AsyncResult<Document>**
```
TEMPLATE_validate:
  INPUT: document object, validation schema
  OUTPUT: Async Result containing validated document
  IMPLEMENTATION:
    - Apply schema validation rules
    - Collect all validation errors
    - Return enhanced document with validation metadata
    - Performance: < tier × 1ms per validation rule
  PATTERN: Validation functor with error accumulation
```

### 8. Command-Line Processing Template (Parser Pattern)

**Pattern**: Parser combinators from design analysis
**Abstract Contract**: Parser monad with combinator composition

#### Core Type Structure
```
Parser<T> ::= Input -> Result<(T, RemainingInput)>
CLPConfig ::= {
  commands: Command[],
  options: Option[],
  arguments: Argument[],
  help: HelpConfig
}
ParsedArguments ::= {
  command: String,
  options: Map<String, Value>,
  arguments: Value[],
  remaining: String[]
}
```

#### Operation Templates

**8.1 parse(args: String[], config: CLPConfig) -> Result<ParsedArguments>**
```
TEMPLATE_parse:
  INPUT: Command-line arguments array, parser configuration
  OUTPUT: Result containing parsed arguments structure
  IMPLEMENTATION:
    - Apply parser combinators in sequence
    - Handle subcommands recursively
    - Validate required arguments and options
    - Collect help information for unknown commands
    - Performance: < tier × 1ms per argument
  PATTERN: Parser combinator composition with backtracking
```

**8.2 parseString(input: String, config: CLPConfig) -> Result<ParsedArguments>**
```
TEMPLATE_parseString:
  INPUT: Command string, parser configuration
  OUTPUT: Result containing parsed arguments structure
  IMPLEMENTATION:
    - Split string into arguments respecting quotes
    - Apply same parsing logic as parse()
    - Handle shell escaping and special characters
    - Performance: < tier × 2ms per command string
  PATTERN: String tokenization followed by parser application
```

**8.3 validate(args: ParsedArguments, config: CLPConfig) -> Result<ValidationResult>**
```
TEMPLATE_validate:
  INPUT: Parsed arguments, parser configuration
  OUTPUT: Result containing validation result
  IMPLEMENTATION:
    - Check required arguments are present
    - Validate argument types and constraints
    - Check mutual exclusions and dependencies
    - Accumulate all validation errors
    - Performance: < tier × 0.5ms per validation rule
  PATTERN: Validation applicative for error accumulation
```

**8.4 generateHelp(config: CLPConfig) -> String**
```
TEMPLATE_generateHelp:
  INPUT: Parser configuration
  OUTPUT: Formatted help text
  IMPLEMENTATION:
    - Generate command descriptions
    - Format option and argument lists
    - Include usage examples
    - Apply consistent formatting
    - Performance: < tier × 10ms for help generation
  PATTERN: Template rendering for help text
```

**8.5 generateUsage(config: CLPConfig) -> String**
```
TEMPLATE_generateUsage:
  INPUT: Parser configuration
  OUTPUT: Brief usage summary
  IMPLEMENTATION:
    - Generate concise usage syntax
    - Include only essential options
    - Format for single-line display
    - Performance: < tier × 1ms for usage generation
  PATTERN: Minimal template rendering
```

### 9. Web Framework Template (Request/Response Pipeline Pattern)

**Pattern**: IO monad with middleware composition from design analysis
**Abstract Contract**: Request -> IO[Response] with composable middleware

#### Core Type Structure
```
WebApp ::= {
  routes: Route[],
  middleware: Middleware[],
  config: WebConfig
}
Route ::= {
  method: HttpMethod,
  path: PathPattern,
  handler: Handler
}
Middleware ::= Request -> (Request -> AsyncResult<Response>) -> AsyncResult<Response>
```

#### Operation Templates

**9.1 route(method: HttpMethod, path: String, handler: Handler) -> void**
```
TEMPLATE_route:
  INPUT: HTTP method, path pattern, request handler
  OUTPUT: void (modifies app routing table)
  IMPLEMENTATION:
    - Register route with path pattern matching
    - Support parameter extraction from paths
    - Validate handler signature
    - Performance: < tier × 0.1ms for route registration
  PATTERN: Route registration with pattern matching
```

**9.2 middleware(middleware: Middleware) -> void**
```
TEMPLATE_middleware:
  INPUT: Middleware function
  OUTPUT: void (adds to middleware stack)
  IMPLEMENTATION:
    - Add middleware to execution stack
    - Maintain order of registration
    - Support middleware composition
    - Performance: < tier × 0.1ms for middleware registration
  PATTERN: Middleware composition chain
```

**9.3 start(config: ServerConfig) -> AsyncResult<Server>**
```
TEMPLATE_start:
  INPUT: Server configuration
  OUTPUT: Async Result containing running server
  IMPLEMENTATION:
    - Bind to configured port and host
    - Initialize connection handling
    - Start request processing loop
    - Setup graceful shutdown handling
    - Performance: < tier × 100ms for server startup
  PATTERN: Server lifecycle state machine
```

**9.4 stop(server: Server) -> AsyncResult<void>**
```
TEMPLATE_stop:
  INPUT: Running server instance
  OUTPUT: Async Result indicating shutdown completion
  IMPLEMENTATION:
    - Stop accepting new connections
    - Wait for active requests to complete
    - Close all connections gracefully
    - Cleanup server resources
    - Performance: < tier × 1s for graceful shutdown
  PATTERN: Graceful shutdown with timeout
```

**9.5 mount(path: String, subApp: WebApp) -> void**
```
TEMPLATE_mount:
  INPUT: Mount path, sub-application
  OUTPUT: void (modifies routing tree)
  IMPLEMENTATION:
    - Create path prefix routing
    - Delegate matching requests to sub-app
    - Preserve middleware isolation
    - Performance: < tier × 0.1ms for mount operation
  PATTERN: Hierarchical routing composition
```

**9.6 param(name: String, handler: ParamHandler) -> void**
```
TEMPLATE_param:
  INPUT: Parameter name, parameter handler
  OUTPUT: void (registers parameter middleware)
  IMPLEMENTATION:
    - Register parameter extraction logic
    - Apply validation and transformation
    - Cache parameter values per request
    - Performance: < tier × 0.1ms per parameter
  PATTERN: Parameter extraction middleware
```

**9.7 static(path: String, directory: String) -> void**
```
TEMPLATE_static:
  INPUT: URL path, filesystem directory
  OUTPUT: void (registers static file serving)
  IMPLEMENTATION:
    - Map URL paths to filesystem paths
    - Handle MIME type detection
    - Implement file caching headers
    - Security: prevent directory traversal
    - Performance: < tier × 5ms per static file
  PATTERN: Static file serving with caching
```

**9.8 errorHandler(handler: ErrorHandler) -> void**
```
TEMPLATE_errorHandler:
  INPUT: Error handling function
  OUTPUT: void (registers error handler)
  IMPLEMENTATION:
    - Catch unhandled errors in middleware/handlers
    - Apply error transformation and logging
    - Generate appropriate HTTP error responses
    - Prevent error information leakage
    - Performance: < tier × 1ms per error
  PATTERN: Error boundary with transformation
```

### 10. ASGI Server Template (Server Lifecycle Pattern)

**Pattern**: Server lifecycle state machine from design analysis
**Abstract Contract**: Continuation monad for async server operations

#### Core Type Structure
```
ASGIServer ::= {
  app: ASGIApp,
  config: ASGIConfig,
  state: ServerState,
  workers: Worker[]
}
ServerState ::= STOPPED | STARTING | RUNNING | STOPPING
ASGIApp ::= Scope -> Receive -> Send -> AsyncResult<void>
```

#### Operation Templates

**10.1 start(config: ASGIConfig) -> AsyncResult<void>**
```
TEMPLATE_start:
  INPUT: ASGI server configuration
  OUTPUT: Async Result indicating server start
  IMPLEMENTATION:
    - Initialize worker processes/threads
    - Bind to configured address and port
    - Begin accepting connections
    - Transition state: STOPPED -> STARTING -> RUNNING
    - Performance: < tier × 200ms for server startup
  PATTERN: State machine with lifecycle management
```

**10.2 stop(timeout: Duration?) -> AsyncResult<void>**
```
TEMPLATE_stop:
  INPUT: Optional timeout duration
  OUTPUT: Async Result indicating shutdown completion
  IMPLEMENTATION:
    - Stop accepting new connections
    - Wait for active requests to complete
    - Force close remaining connections after timeout
    - Transition state: RUNNING -> STOPPING -> STOPPED
    - Performance: < timeout duration for shutdown
  PATTERN: Graceful shutdown with forced termination
```

**10.3 reload() -> AsyncResult<void>**
```
TEMPLATE_reload:
  INPUT: None
  OUTPUT: Async Result indicating reload completion
  IMPLEMENTATION:
    - Reload application code without dropping connections
    - Restart worker processes with new code
    - Maintain service availability during reload
    - Performance: < tier × 1s for hot reload
  PATTERN: Hot reload with zero downtime
```

**10.4 getStats() -> Result<ServerStats>**
```
TEMPLATE_getStats:
  INPUT: None
  OUTPUT: Result containing server statistics
  IMPLEMENTATION:
    - Collect connection count and status
    - Gather request/response metrics
    - Calculate performance statistics
    - Performance: < tier × 1ms for stats collection
  PATTERN: Metrics aggregation
```

**10.5 health() -> Result<HealthStatus>**
```
TEMPLATE_health:
  INPUT: None
  OUTPUT: Result containing health status
  IMPLEMENTATION:
    - Check server operational status
    - Verify worker process health
    - Test application responsiveness
    - Performance: < tier × 10ms for health check
  PATTERN: Health monitoring
```

### 11. AI/LLM Client Template (Configuration Reader Pattern)

**Pattern**: Reader monad with circuit breaker integration from design analysis
**Abstract Contract**: Config -> IO[Result[Response]]

#### Core Type Structure
```
LLMClient ::= {
  provider: Provider,
  config: LLMConfig,
  circuitBreaker: CircuitBreaker
}
LLMConfig ::= {
  apiKey: String,
  baseUrl: String,
  timeout: Duration,
  retries: Number
}
```

#### Operation Templates

**11.1 chat(messages: Message[], options: ChatOptions?) -> AsyncResult<ChatResponse>**
```
TEMPLATE_chat:
  INPUT: Message array, optional chat options
  OUTPUT: Async Result containing chat response
  IMPLEMENTATION:
    - Apply circuit breaker before request
    - Format messages for provider API
    - Handle streaming vs non-streaming modes
    - Apply rate limiting and retries
    - Performance: < tier × 1s + API latency
  PATTERN: Reader monad with circuit breaker
```

**11.2 chatStream(messages: Message[], options: ChatOptions?) -> AsyncResult<ChatStream>**
```
TEMPLATE_chatStream:
  INPUT: Message array, optional chat options
  OUTPUT: Async Result containing chat response stream
  IMPLEMENTATION:
    - Establish streaming connection to provider
    - Parse server-sent events or WebSocket messages
    - Handle connection errors and reconnection
    - Apply backpressure for slow consumers
    - Performance: < tier × 10ms per chunk + API latency
  PATTERN: Stream coalgebra with error recovery
```

**11.3 complete(prompt: String, options: CompletionOptions?) -> AsyncResult<CompletionResponse>**
```
TEMPLATE_complete:
  INPUT: Prompt string, optional completion options
  OUTPUT: Async Result containing completion response
  IMPLEMENTATION:
    - Format prompt for provider completion API
    - Apply temperature and other generation parameters
    - Handle token limits and truncation
    - Performance: < tier × 1s + API latency
  PATTERN: Configuration reader with validation
```

**11.4 embed(text: String, options: EmbeddingOptions?) -> AsyncResult<EmbeddingResponse>**
```
TEMPLATE_embed:
  INPUT: Text string, optional embedding options
  OUTPUT: Async Result containing embedding vectors
  IMPLEMENTATION:
    - Chunk text if it exceeds model limits
    - Request embeddings from provider API
    - Normalize vectors if requested
    - Cache embeddings for repeated text
    - Performance: < tier × 500ms + API latency
  PATTERN: Text processing with caching
```

**11.5 withCircuitBreaker(config: CircuitBreakerConfig) -> LLMClient**
```
TEMPLATE_withCircuitBreaker:
  INPUT: Circuit breaker configuration
  OUTPUT: New LLM client with circuit breaker
  IMPLEMENTATION:
    - Wrap existing client with circuit breaker
    - Configure failure thresholds and timeouts
    - Implement state transition logic
    - Preserve original client interface
    - Performance: < tier × 0.1ms overhead per request
  PATTERN: Decorator with state machine
```

### 12. MCP Protocol Template (Message Transformation Pattern)

**Pattern**: Protocol functor for message serialization from design analysis
**Abstract Contract**: Protocol functor with bidirectional transformation

#### Core Type Structure
```
MCPServer ::= {
  tools: Map<String, Tool>,
  resources: Map<String, Resource>,
  transport: Transport
}
MCPClient ::= {
  connection: Connection,
  capabilities: Capabilities
}
```

#### Operation Templates

**12.1 registerTool(name: String, tool: Tool) -> Result<void>**
```
TEMPLATE_registerTool:
  INPUT: Tool name, tool implementation
  OUTPUT: Result indicating registration success
  IMPLEMENTATION:
    - Validate tool name uniqueness
    - Register tool in tool registry
    - Generate tool metadata and schema
    - Notify connected clients of new tool
    - Performance: < tier × 1ms for tool registration
  PATTERN: Registry pattern with notification
```

**12.2 callTool(name: String, arguments: Object) -> AsyncResult<ToolResponse>**
```
TEMPLATE_callTool:
  INPUT: Tool name, tool arguments
  OUTPUT: Async Result containing tool execution result
  IMPLEMENTATION:
    - Lookup tool in registry
    - Validate arguments against tool schema
    - Execute tool with provided arguments
    - Handle tool execution timeouts and errors
    - Performance: < tier × 10ms + tool execution time
  PATTERN: Dynamic dispatch with validation
```

**12.3 listTools() -> AsyncResult<ToolInfo[]>**
```
TEMPLATE_listTools:
  INPUT: None
  OUTPUT: Async Result containing list of available tools
  IMPLEMENTATION:
    - Enumerate registered tools
    - Include tool metadata and schemas
    - Filter based on client capabilities
    - Performance: < tier × 1ms for tool enumeration
  PATTERN: Registry enumeration with filtering
```

**12.4 connect(endpoint: String, options: ConnectionOptions?) -> AsyncResult<MCPConnection>**
```
TEMPLATE_connect:
  INPUT: Endpoint URL, optional connection options
  OUTPUT: Async Result containing MCP connection
  IMPLEMENTATION:
    - Establish transport connection (HTTP, WebSocket, etc.)
    - Perform capability negotiation handshake
    - Set up message routing and error handling
    - Performance: < tier × 100ms + network latency
  PATTERN: Connection establishment with negotiation
```

**12.5 send(message: MCPMessage) -> AsyncResult<MCPResponse>**
```
TEMPLATE_send:
  INPUT: MCP message object
  OUTPUT: Async Result containing response
  IMPLEMENTATION:
    - Serialize message to wire format
    - Send over established transport
    - Wait for and deserialize response
    - Handle message correlation and timeouts
    - Performance: < tier × 5ms + network latency
  PATTERN: Message transformation with correlation
```

**12.6 close() -> AsyncResult<void>**
```
TEMPLATE_close:
  INPUT: None
  OUTPUT: Async Result indicating connection closure
  IMPLEMENTATION:
    - Send connection close notification
    - Clean up pending requests
    - Close underlying transport
    - Performance: < tier × 50ms for clean shutdown
  PATTERN: Resource cleanup with notification
```

### 13. Database Template (Transaction Composition Pattern)

**Pattern**: Transaction monad for ACID properties from design analysis
**Abstract Contract**: Transaction monad with resource management

#### Core Type Structure
```
Database ::= {
  connection: Connection,
  config: DatabaseConfig,
  pool: ConnectionPool
}
Transaction ::= {
  connection: Connection,
  state: TransactionState,
  operations: Operation[]
}
```

#### Operation Templates

**13.1 connect(config: DatabaseConfig) -> AsyncResult<Database>**
```
TEMPLATE_connect:
  INPUT: Database configuration
  OUTPUT: Async Result containing database connection
  IMPLEMENTATION:
    - Establish connection to database
    - Initialize connection pool
    - Verify database schema compatibility
    - Set up connection health monitoring
    - Performance: < tier × 100ms for connection setup
  PATTERN: Resource acquisition with pooling
```

**13.2 query(sql: String, params: Parameter[]) -> AsyncResult<QueryResult>**
```
TEMPLATE_query:
  INPUT: SQL query string, query parameters
  OUTPUT: Async Result containing query results
  IMPLEMENTATION:
    - Prepare parameterized query
    - Execute query with parameter binding
    - Fetch and process result set
    - Handle query timeouts and cancellation
    - Performance: < tier × 1ms + query execution time
  PATTERN: Query execution with parameter binding
```

**13.3 execute(sql: String, params: Parameter[]) -> AsyncResult<ExecuteResult>**
```
TEMPLATE_execute:
  INPUT: SQL statement, statement parameters
  OUTPUT: Async Result containing execution result
  IMPLEMENTATION:
    - Prepare parameterized statement
    - Execute statement with parameter binding
    - Return affected row count and metadata
    - Performance: < tier × 1ms + statement execution time
  PATTERN: Statement execution with metadata
```

**13.4 transaction(operations: (Database) -> AsyncResult<T>) -> AsyncResult<T>**
```
TEMPLATE_transaction:
  INPUT: Transaction operations function
  OUTPUT: Async Result containing transaction result
  IMPLEMENTATION:
    - Begin database transaction
    - Execute operations within transaction scope
    - Commit on success, rollback on failure
    - Handle deadlocks and retries
    - Performance: < tier × 1ms overhead + operation time
  PATTERN: Transaction monad with resource management
```

**13.5 close() -> AsyncResult<void>**
```
TEMPLATE_close:
  INPUT: None
  OUTPUT: Async Result indicating database closure
  IMPLEMENTATION:
    - Close all pooled connections
    - Wait for active transactions to complete
    - Clean up database resources
    - Performance: < tier × 100ms for clean shutdown
  PATTERN: Resource cleanup with graceful shutdown
```

---

## Cross-Language Adaptation Guidelines

### Functional Languages (Haskell)

**Monadic Composition**:
```haskell
-- Result monad maps directly to Either
type Result a = Either QiError a

-- Use do-notation for composition
processUser :: UserId -> Result User
processUser uid = do
  user <- getUser uid
  validated <- validateUser user
  pure validated
```

**Performance Optimizations**:
- Use strict evaluation where appropriate
- Apply fusion optimizations for list operations
- Leverage GHC's optimization pragmas

### Object-Oriented Languages (TypeScript, Python)

**Class-Based Implementation**:
```typescript
class Result<T> {
  private constructor(
    private readonly _isSuccess: boolean,
    private readonly _data: T | null,
    private readonly _error: QiError | null
  ) {}
  
  static success<T>(data: T): Result<T> {
    return new Result(true, data, null);
  }
  
  flatMap<U>(fn: (data: T) => Result<U>): Result<U> {
    return this._isSuccess ? fn(this._data!) : Result.failure(this._error!);
  }
}
```

### Systems Languages (Rust)

**Zero-Cost Abstractions**:
```rust
#[derive(Debug)]
pub enum Result<T> {
    Success(T),
    Failure(QiError),
}

impl<T> Result<T> {
    pub fn map<U, F>(self, f: F) -> Result<U> 
    where F: FnOnce(T) -> U {
        match self {
            Result::Success(data) => Result::Success(f(data)),
            Result::Failure(err) => Result::Failure(err),
        }
    }
}
```

### Procedural Languages (Go)

**Error Handling Patterns**:
```go
type Result[T any] struct {
    data T
    err  *QiError
}

func (r Result[T]) Map(fn func(T) T) Result[T] {
    if r.err != nil {
        return Result[T]{err: r.err}
    }
    return Result[T]{data: fn(r.data)}
}
```

---

## Performance Optimization Strategies

### Native Tier (Rust, C++) - 1× Baseline

**Zero-Cost Abstractions**:
- Compile-time optimization and inlining
- Template/generics specialization
- SIMD instructions where applicable
- Stack allocation preference

**Memory Management**:
- Zero-copy operations
- Move semantics
- RAII resource management
- Custom allocators for hot paths

### VM Tier (Go, Java) - 10× Baseline

**JIT-Friendly Patterns**:
- Monomorphic call sites
- Escape analysis optimization
- Garbage collection friendly allocation patterns
- Connection pooling and object reuse

**Concurrency**:
- Work-stealing schedulers
- Lock-free data structures
- Async I/O with event loops

### Functional Tier (Haskell) - 50× Baseline

**Lazy Evaluation**:
- Thunk optimization
- Fusion for list operations
- Strictness annotations where needed
- Memoization for expensive computations

**Memory Optimization**:
- Sharing and common subexpression elimination
- Tail call optimization
- Unboxed types for primitives

### Interpreted Tier (Python, JavaScript) - 100× Baseline

**Runtime Optimization**:
- Minimize object allocations
- Use built-in operations when possible
- Avoid repeated string operations
- Cache compiled regex and templates

**Async Patterns**:
- Event loop optimization
- Batch operations where possible
- Connection pooling and reuse
- Background task processing

---

## Package Integration Points

### Result<T> Integration Requirements

**Functional Programming Libraries**:
- Must support map, flatMap, filter operations
- Should provide Either/Result type
- Needs composition operators
- Error handling integration

**Validation Libraries**:
- Should integrate with Result for validation chains
- Needs error accumulation support
- Type-safe validation rules

### Configuration Integration Requirements

**File Format Libraries**:
- JSON, YAML, TOML parsing support
- Stream parsing for large files
- Error reporting with line numbers

**Environment Integration**:
- Cross-platform environment access
- Type coercion and validation
- Nested structure support

### HTTP Client Integration Requirements

**Core HTTP Libraries**:
- Async/await or Future support
- Connection pooling
- Streaming request/response bodies
- Interceptor or middleware support

**Resilience Libraries**:
- Circuit breaker implementations
- Retry logic with backoff
- Timeout handling
- Health check integration

---

## Verification Requirements

### Mathematical Property Testing

**Property-Based Tests Required**:
```
// Monad Laws for Result<T>
property_leftIdentity: ∀x,f. success(x).flatMap(f) ≡ f(x)
property_rightIdentity: ∀m. m.flatMap(success) ≡ m  
property_associativity: ∀m,f,g. (m.flatMap(f)).flatMap(g) ≡ m.flatMap(x => f(x).flatMap(g))

// Monoid Laws for Configuration
property_leftIdentity: ∀c. merge([empty, c]) ≡ c
property_rightIdentity: ∀c. merge([c, empty]) ≡ c
property_associativity: ∀a,b,c. merge([a, merge([b, c])]) ≡ merge([merge([a, b]), c])
```

### Performance Benchmarks Required

**Operation Benchmarks**:
- Result operations: < tier × specified limits
- Configuration merge: O(k×d) complexity verification
- Logger level check: < 10ns requirement
- Cache operations: < tier × specified limits
- HTTP operations: Network + tier × overhead limits

### Cross-Language Behavioral Consistency

**Behavioral Tests**:
- Same inputs produce equivalent outputs across languages
- Error cases handled consistently
- Performance characteristics scale according to tier
- Resource usage patterns similar within tier constraints

---

## Dependencies and References

- **Derives from**: [Design Analysis](../design/qi.v4.design.analysis.md) - Language-agnostic design patterns
- **Uses**: [Mathematical Contracts](../guides/mathematical-contracts.md) - Abstract mathematical interface contracts
- **Implements**: Language-agnostic implementation templates for all 99 operations
- **Used by**: Stage 4 package research and Stage 5 language-specific implementations
- **Integration**: Bridge between design patterns and concrete language implementations