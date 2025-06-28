# QiCore v4.0 Base Component Contracts

> **Experiment: Base Component Only**  
> **Scope**: Result<T> and QiError classes only  
> **Purpose**: Test incremental tree-traversal implementation approach

## Base Component Dependency Analysis

**Dependency Tree Position**: Leaf nodes (no dependencies)
- **QiError** - no dependencies  
- **Result<T>** - depends only on QiError

**Component Relationship**:
```
Result<T> → QiError
```

This represents the minimal complete component that can be implemented and validated independently.

## QiError Contract

**Purpose**: Standardized error representation across all QiCore library functions with rich context and debugging information for cross-language compatibility.

**Behavior**:
- **Immutable**: All properties are readonly after construction
- **Serializable**: Can be converted to/from structured data for logging and transmission
- **Chainable**: Supports error cause chains for debugging complex failures
- **Language-Neutral**: No language-specific error types or stack traces

**Required Properties**:
- `code: string` - Unique error identifier (e.g., "CONFIG_NOT_FOUND", "NETWORK_TIMEOUT")
- `message: string` - Human-readable error description
- `category: ErrorCategory` - Error classification for handling ("VALIDATION", "NETWORK", etc.)
- `context: object | null` - Additional error context as key-value structure (file paths, request IDs, etc.)
- `cause: QiError | null` - Underlying error that caused this error (for error chaining)
- `timestamp: number` - When the error occurred (epoch milliseconds)

**Required Operations**:
- `create(code, message, category, context?, cause?)` - Factory method for creating errors
- `toString()` - Return formatted error string for display
- `toStructuredData()` - Return serializable structure for logging/transmission
- `getCategory()` - Return error category for programmatic handling
- `withContext(additionalContext)` - Create new error with merged context
- `withCause(causeError)` - Create new error with cause chain

**Input/Output Contracts**:
- **create()**: Takes code, message, category, optional context and cause, returns QiError
- **toString()**: Returns string representation suitable for user display
- **toStructuredData()**: Returns structured object/map containing all error information
- **getCategory()**: Returns ErrorCategory enum value
- **withContext(context)**: Takes object, returns new QiError with merged context
- **withCause(error)**: Takes QiError, returns new QiError with cause chain

**Error Conditions**: 
- **Invalid Code**: Code must be non-empty string
- **Invalid Category**: Category must be valid ErrorCategory value
- **Circular Cause Chain**: Cause chain must not be circular (max depth: 10)

**Side Effects**: None - Errors are immutable value objects

**Performance**: O(1) for property access, O(n) for cause chain traversal where n is chain depth

**Error Categories**:
- `VALIDATION`: Input validation failures
- `NETWORK`: HTTP/network related errors  
- `FILESYSTEM`: File/IO operation failures
- `CONFIGURATION`: Config loading/parsing errors
- `CACHE`: Cache operation failures
- `TIMEOUT`: Operation timeout errors
- `PERMISSION`: Access/authorization failures
- `UNKNOWN`: Unclassified errors

## Result<T> Contract

**Purpose**: Provides a type-safe way to handle operations that can succeed or fail, eliminating exceptions in normal control flow while enabling functional composition.

**Behavior**: 
- **Synchronous**: Immutable value object representing operation outcome
- **Thread-Safe**: All properties are readonly, safe for concurrent access
- **Functional**: Supports functional composition through transformation methods
- **No Lifecycle**: Pure value objects with no resource management

**Required Properties**:
- `isSuccess: boolean` - True if operation succeeded, false if failed
- `data: T | null` - Contains result data on success, null on failure  
- `error: QiError | null` - Contains error details on failure, null on success

**Required Operations**:
- `success(data)` - Factory method for successful result
- `failure(error)` - Factory method for failed result  
- `fromTryCatch(function)` - Wrap function call in try-catch, return Result
- `map(transformFunction)` - Transform success value, pass through failures unchanged
- `flatMap(chainFunction)` - Chain operations that return Results
- `unwrap()` - Return data if success, throw error if failure
- `unwrapOr(defaultValue)` - Return data if success, default value if failure
- `match(onSuccess, onError)` - Pattern matching for handling both success and failure cases
- `orElse(alternativeFunction)` - Provide alternative Result if this one failed

**Input/Output Contracts**:
- **success(data)**: Takes value of type T, returns Result<T>
- **failure(error)**: Takes QiError, returns Result<T>
- **fromTryCatch(fn)**: Takes function that may throw, returns Result<T>
- **map(fn)**: Takes transformation function `(T) -> U`, returns `Result<U>`
- **flatMap(fn)**: Takes chain function `(T) -> Result<U>`, returns `Result<U>`  
- **unwrap()**: Returns `T` on success, throws `QiError` on failure
- **unwrapOr(defaultValue)**: Takes default value `T`, returns `T` (never fails)
- **match(onSuccess, onError)**: Takes two functions, returns result of called function
- **orElse(fn)**: Takes function `(QiError) -> Result<T>`, returns `Result<T>`

**Mathematical Properties**:
- **Monad Laws**: 
  - Left identity: `Result.success(a).flatMap(f) === f(a)`
  - Right identity: `m.flatMap(Result.success) === m`
  - Associativity: `(m.flatMap(f)).flatMap(g) === m.flatMap(x => f(x).flatMap(g))`
- **Functor Laws**:
  - Identity: `m.map(x => x) === m`
  - Composition: `m.map(f).map(g) === m.map(x => g(f(x)))`

**Error Conditions**:
- **Invariant Violation**: Exactly one of `data` or `error` must be non-null
- **unwrap() on Failure**: Must throw the contained QiError when called on failed Result
- **Invalid Function Arguments**: map/flatMap functions must not be null

**Side Effects**: None - Results are immutable value objects

**Performance**: 
- **Complexity**: O(1) for all operations
- **Object Creation**: Language-specific targets:
  - Native compiled (Rust, C++): < 1 microsecond
  - VM-based (Go, Java): < 10 microseconds
  - Interpreted (Python, JavaScript): < 100 microseconds
  - Functional (Haskell): < 50 microseconds

## Validation Criteria

**QiError Validation**:
- [ ] All 6 operations implemented
- [ ] All 8 error categories supported
- [ ] Immutability enforced
- [ ] Context merging works correctly
- [ ] Cause chaining prevents cycles
- [ ] Serialization round-trip preserves data

**Result<T> Validation**:
- [ ] All 9 operations implemented
- [ ] Monad laws verified with property tests
- [ ] Functor laws verified with property tests
- [ ] Error propagation works correctly
- [ ] Performance targets met
- [ ] Type safety enforced

**Integration Validation**:
- [ ] Result<T> correctly uses QiError for failures
- [ ] Error context preserved through Result operations
- [ ] Functional composition chains work correctly
- [ ] No memory leaks or resource issues

## Success Criteria

This base component experiment succeeds if:

1. **Complete Implementation**: Both QiError and Result<T> fully implemented
2. **Mathematical Compliance**: All monad and functor laws verified
3. **Performance Targets**: Meet interpreted tier requirements (< 100μs)
4. **Quality Assurance**: Pass all linting, type checking, and tests
5. **No Dependencies**: Implementation uses only language standard library
6. **Composable Foundation**: Ready to support higher-level components

**Ready for Next**: When base component is complete and validated, proceed to core components (Configuration, Logger, Cache) that depend on this foundation.