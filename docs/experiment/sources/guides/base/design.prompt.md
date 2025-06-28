# Base Component Design Prompt (Language-Agnostic)

> **Stage 2: Design Patterns for Base Components**  
> **Scope**: QiError and Result<T> design patterns  
> **Language**: Agnostic - pure mathematical and structural patterns

## Design Derivation Context

**Mathematical Foundation**: Result<T> implements monad pattern, QiError implements structured error pattern
**Dependencies**: No external dependencies beyond language standard library
**Performance Tier**: Language-agnostic baseline definitions

## QiError Design Pattern

**Pattern**: Immutable Builder with Context Accumulation
**Mathematical Structure**: Product type with chaining

**Core Design**:
```pseudocode
ErrorCategory = VALIDATION | NETWORK | FILESYSTEM | CONFIGURATION | 
               CACHE | TIMEOUT | PERMISSION | UNKNOWN

QiError:
  code: String (required, non-empty)
  message: String (required)
  category: ErrorCategory (required)
  context: Map[String, Any] | null (optional)
  cause: QiError | null (optional)
  timestamp: Number (auto-generated)
```

**Construction Pattern**:
```pseudocode
// Factory methods for each category
create_validation_error(code, message, context?) → QiError
create_network_error(code, message, context?) → QiError
create_filesystem_error(code, message, context?) → QiError
// ... for each category

// Builder pattern for context/cause
with_context(error, newContext) → QiError (immutable update)
with_cause(error, causeError) → QiError (immutable update)
```

**Structural Invariants**:
- **Immutability**: All properties readonly after construction
- **Completeness**: Code, message, category always present
- **Context Merging**: New context merged with existing, conflicts overwritten
- **Cause Chain Limits**: Maximum depth 10 to prevent infinite chains
- **Timestamp Precision**: Milliseconds since epoch

**Operations Design**:

1. **create(code, message, category, context?, cause?)**
   - Pattern: Factory method with validation
   - Validation: Non-empty code, valid category
   - Performance: O(1) object creation

2. **toString()**
   - Pattern: Lazy string formatting
   - Format: "[CODE] message (category)"
   - Include: Cause chain if present

3. **toStructuredData()**
   - Pattern: Serialization visitor
   - Output: Nested structure preserving all data
   - Include: Full cause chain expansion

4. **getCategory()**
   - Pattern: Direct property accessor
   - Performance: O(1) field access

5. **withContext(additionalContext)**
   - Pattern: Immutable builder
   - Merge: Object union, new values override existing
   - Performance: O(k) where k is context size

6. **withCause(causeError)**
   - Pattern: Immutable chain builder
   - Validation: Prevent circular references
   - Performance: O(1) object creation

## Result<T> Design Pattern

**Pattern**: Railway-Oriented Programming (Monad)
**Mathematical Structure**: Sum type Either<Error, Success>

**Core Design**:
```pseudocode
Result<T> = Success<T> | Failure<QiError>

// Algebraic data type representation
Success(value: T)
Failure(error: QiError)
```

**Mathematical Properties**:
```pseudocode
// Monad Laws
left_identity: return(a) >>= f ≡ f(a)
right_identity: m >>= return ≡ m
associativity: (m >>= f) >>= g ≡ m >>= (λx → f(x) >>= g)

// Functor Laws  
identity: map(id) ≡ id
composition: map(f ∘ g) ≡ map(f) ∘ map(g)
```

**Operations Design**:

1. **Factory Operations**
   ```pseudocode
   success(value: T) → Result<T>
     return Success(value)
   
   failure(error: QiError) → Result<T>
     return Failure(error)
   
   from_try_catch(operation: () → T) → Result<T>
     try:
       value ← operation()
       return Success(value)
     catch exception:
       error ← convert_exception_to_qierror(exception)
       return Failure(error)
   ```

2. **Transformation Operations**
   ```pseudocode
   map(transform: T → U): Result<T> → Result<U>
     Success(value) → Success(transform(value))
     Failure(error) → Failure(error)
   
   flat_map(chain: T → Result<U>): Result<T> → Result<U>
     Success(value) → chain(value)
     Failure(error) → Failure(error)
   ```

3. **Elimination Operations**
   ```pseudocode
   unwrap(): Result<T> → T | throws QiError
     Success(value) → value
     Failure(error) → throw error
   
   unwrap_or(default: T): Result<T> → T
     Success(value) → value
     Failure(error) → default
   
   match(on_success: T → U, on_error: QiError → U): Result<T> → U
     Success(value) → on_success(value)
     Failure(error) → on_error(error)
   
   or_else(alternative: QiError → Result<T>): Result<T> → Result<T>
     Success(value) → Success(value)
     Failure(error) → alternative(error)
   ```

## Integration Patterns

**Error Creation in Result Context**:
```pseudocode
wrap_operation(operation: () → T, error_context: Map) → Result<T>:
  try:
    value ← operation()
    return success(value)
  catch exception:
    error ← QiError.unknown("OPERATION_FAILED", exception.message, error_context)
    return failure(error)
```

**Validation Chains**:
```pseudocode
validate_user(userData: Map) → Result<User>:
  return success(userData)
    |> flat_map(validate_required)
    |> flat_map(validate_email)
    |> flat_map(validate_age)
    |> map(create_user)

validate_required(data: Map) → Result<Map>:
  if data.name == null:
    return failure(QiError.validation("MISSING_NAME", "Name is required"))
  return success(data)
```

**Error Recovery Patterns**:
```pseudocode
with_fallback(primary: () → Result<T>, fallback: () → Result<T>) → Result<T>:
  result ← primary()
  return result.or_else(λerror → fallback())

retry_with_backoff(operation: () → Result<T>, max_attempts: Int) → Result<T>:
  for attempt in 1..max_attempts:
    result ← operation()
    if result.is_success:
      return result
    if attempt < max_attempts:
      wait(exponential_backoff(attempt))
  return result  // Return last failure
```

## Performance Considerations

**Language Tier Multipliers**:
- **Native (Rust, C++)**: 1× baseline
- **VM-Based (Go, Java)**: 10× baseline  
- **Functional (Haskell)**: 50× baseline
- **Interpreted (Python, JS)**: 100× baseline

**Operation Performance Targets**:
```pseudocode
// All times scaled by language tier multiplier
result_creation: < 1μs
error_creation: < 1μs
map_operation: < 0.5μs
flat_map_operation: < 1μs
unwrap_operation: < 0.1μs
context_merge: < 0.1μs
```

**Memory Optimization Patterns**:
- **Object Pooling**: Reuse Result instances where safe
- **Lazy Evaluation**: Defer expensive operations until needed
- **Structural Sharing**: Share immutable parts between instances
- **Compact Representation**: Minimize memory overhead per instance

## Cross-Language Adaptation Strategies

**Static Type Systems** (Rust, Haskell, TypeScript):
- Use native algebraic data types (enum, union types)
- Leverage compile-time type checking
- Zero-cost abstractions where possible

**Dynamic Type Systems** (Python, JavaScript):
- Use class hierarchies or tagged objects
- Runtime type validation
- Performance optimizations through caching

**Object-Oriented Languages** (Java, C#):
- Sealed class hierarchies
- Visitor pattern for operations
- Generic type parameters for type safety

**Functional Languages** (Haskell, OCaml):
- Native sum types and pattern matching
- Leverage existing Either/Maybe types
- Compose with existing monad infrastructure

## Validation Requirements

**Mathematical Law Verification**:
```pseudocode
// Property-based tests for each law
property_left_identity(a: T, f: T → Result<U>):
  assert success(a).flat_map(f) == f(a)

property_right_identity(m: Result<T>):
  assert m.flat_map(success) == m

property_associativity(m: Result<T>, f: T → Result<U>, g: U → Result<V>):
  left ← m.flat_map(f).flat_map(g)
  right ← m.flat_map(λx → f(x).flat_map(g))
  assert left == right
```

**Performance Benchmarks**:
```pseudocode
benchmark_suite:
  - operation: "result_creation"
    iterations: 1000
    target: "< tier_multiplier × 1μs"
  
  - operation: "error_creation"
    iterations: 1000
    target: "< tier_multiplier × 1μs"
  
  - operation: "map_chain_10_ops"
    iterations: 100
    target: "< tier_multiplier × 5μs"
```

## Implementation Strategy

**Phase 1: Core Structures**
1. Define ErrorCategory enumeration
2. Implement QiError with immutable properties
3. Define Result<T> algebraic data type
4. Implement basic factory methods

**Phase 2: Operations**
1. Implement all QiError operations with validation
2. Implement all Result<T> transformation operations
3. Ensure mathematical law compliance
4. Add comprehensive error handling

**Phase 3: Integration**
1. Test QiError and Result<T> integration
2. Verify functional composition patterns
3. Validate performance characteristics
4. Create usage examples and documentation

**Success Criteria**:
- All mathematical laws verified through property testing
- Performance targets met for target language tier
- Clean integration between QiError and Result<T>
- Ready for consumption by higher-level components