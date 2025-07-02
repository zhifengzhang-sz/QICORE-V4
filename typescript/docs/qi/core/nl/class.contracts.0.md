# QiCore v4.0 Mathematical Class Contracts

> **Purpose**: Complete algebraic specifications that enable AI pattern matching to derive ergonomic APIs  
> **Audience**: AI code generators, compiler designers, mathematical verification  
> **Method**: Minimal mathematical properties that encode maximal API guidance  

## Contract Philosophy

These contracts specify **complete algebraic properties** for each type. AI systems pattern match on these mathematical structures to derive:
- Compositional APIs (monoid laws → fluent chaining)
- Error handling patterns (monad laws → safe operation chaining) 
- Immutable operations (algebraic laws → builder patterns)
- Performance characteristics (complexity bounds → efficient implementations)

---

## QiError Contract

### Algebraic Structure
**Semi-group with Context Composition**
- **Type**: `QiError`
- **Context Operation**: `⊕ : (QiError, Context) → QiError` 
- **Cause Operation**: `∘ : (QiError, QiError) → QiError`

### Mathematical Properties

**Context Composition Laws**:
```
Identity: e ⊕ ∅ = e
Associativity: (e ⊕ c₁) ⊕ c₂ = e ⊕ (c₁ ∪ c₂)
Immutability: e ⊕ c ≠ e (returns new instance)
```

**Cause Chain Laws**:
```
Acyclic: e₁ ∘ e₂ ∘ ... ∘ eₙ where eᵢ ≠ eⱼ for i ≠ j
Bounded: depth(e₁ ∘ e₂ ∘ ... ∘ eₙ) ≤ 10
Preservation: category(e₁ ∘ e₂) = category(e₁)
```

**Serialization Laws**:
```
Isomorphism: deserialize(serialize(e)) = e
JSON Compatible: serialize(e) ∈ ValidJSON
```

### Required Operations
```
create: (Code, Message, Category, Context?) → QiError
from: (Error, Category?) → QiError
withContext: (QiError, Context) → QiError  
causedBy: (QiError, QiError) → QiError
isRetryable: QiError → Boolean
toError: QiError → Error
```

### Error Categories
```
ErrorCategory = VALIDATION | NETWORK | SYSTEM | BUSINESS | SECURITY | TIMEOUT | PARSING | UNKNOWN
```

**Category Properties**:
- **Retryable**: `{NETWORK, TIMEOUT} ⊆ Retryable`
- **Non-Retryable**: `{VALIDATION, SECURITY} ⊆ Non-Retryable` 
- **Context-Dependent**: `{SYSTEM, BUSINESS, PARSING, UNKNOWN}`

---

## Result<T> Contract

### Algebraic Structure
**Monad with Error Context**
- **Type**: `Result<T> = Success<T> | Failure<T>`
- **Unit**: `η : T → Result<T>`
- **Bind**: `>>=: Result<T> → (T → Result<U>) → Result<U>`

### Mathematical Properties

**Functor Laws**:
```
Identity: map(id) = id
Composition: map(f ∘ g) = map(f) ∘ map(g)
```

**Monad Laws**:
```
Left Identity: η(a) >>= f = f(a)
Right Identity: m >>= η = m  
Associativity: (m >>= f) >>= g = m >>= (λx. f(x) >>= g)
```

**Error Propagation Laws**:
```
Failure Preservation: Failure(e) >>= f = Failure(e)
Success Transform: Success(a) >>= f = f(a)
Context Preservation: error context never lost in chains
```

**Alternative Laws** (orElse operation):
```
Left Identity: failure(e) `orElse` f = f(e)
Right Identity: success(a) `orElse` f = success(a)
Associativity: (a `orElse` b) `orElse` c = a `orElse` (b `orElse` c)
```

### Required Operations
```
success: T → Result<T>
failure: QiError → Result<T>
map: Result<T> → (T → U) → Result<U>
flatMap: Result<T> → (T → Result<U>) → Result<U>
orElse: Result<T> → (QiError → Result<T>) → Result<T>
match: Result<T> → (T → U) → (QiError → U) → U
unwrapOr: Result<T> → T → T
```

### Composition Properties
```
Chain Length: Unlimited (stack-safe implementation required)
Error Short-Circuit: First failure stops chain execution
Memory: O(1) per operation (no accumulation)
```

---

## Configuration Contract

### Algebraic Structure
**Commutative Monoid with Validation**
- **Type**: `ConfigData`
- **Operation**: `⊕ : (ConfigData, ConfigData) → ConfigData`
- **Identity**: `∅ : ConfigData`

### Mathematical Properties

**Monoid Laws**:
```
Associativity: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
Left Identity: ∅ ⊕ a = a
Right Identity: a ⊕ ∅ = a
```

**Merge Semantics**:
```
Right Bias: (a ⊕ b)[k] = b[k] if k ∈ dom(b), else a[k]
Domain Union: dom(a ⊕ b) = dom(a) ∪ dom(b)
Idempotence: a ⊕ a = a
```

**Validation Properties**:
```
Schema Compliance: validate(config, schema) → Result<ConfigData>
Type Safety: get(config, key) returns typed values
Path Resolution: supports nested key access (a.b.c)
```

### Required Operations
```
empty: () → ConfigData
merge: (ConfigData, ConfigData) → ConfigData
fromFile: FilePath → Async<Result<ConfigData>>
fromObject: Object → Result<ConfigData>
fromEnvironment: Prefix? → Result<ConfigData>
get: (ConfigData, KeyPath) → Result<Value>
validate: (ConfigData, Schema) → Result<ConfigData>
```

### Loading Properties
```
Format Agnostic: JSON, YAML, TOML, ENV support
Async Safe: File operations return Async<Result<T>>
Security: Path traversal protection, size limits
```

---

## Logger Contract

### Algebraic Structure
**Effect System with Structured Output**
- **Type**: `Logger`
- **Effect**: `log : (Level, Message, Context?) → IO<()>`
- **Filter**: `Level → Boolean`

### Mathematical Properties

**Level Ordering**:
```
Ordering: ERROR > WARN > INFO > DEBUG > TRACE
Monotonic: if level ≥ threshold then log(level, msg) executes
Filtering: isEnabled(level) ↔ level ≥ currentLevel
```

**Context Composition**:
```
Merge: log(level, msg, ctx₁ ∪ ctx₂) includes all context
Serialization: context must be JSON-serializable
Structured: context preserves type information
```

**Output Properties**:
```
Atomic: Each log call is atomic operation
Ordered: Log entries maintain temporal ordering
Async Safe: Thread-safe in concurrent environments
```

### Required Operations
```
create: LogConfig → Result<Logger>
log: (Logger, Level, Message, Context?) → IO<()>
isEnabled: (Logger, Level) → Boolean
withContext: (Logger, Context) → Logger
```

### Performance Requirements
```
Latency: < 1ms per log call (excluding I/O)
Memory: Bounded buffers, no memory leaks
I/O: Configurable sync/async output strategies
```

---

## Cache Contract

### Algebraic Structure
**Key-Value Store with TTL Semantics**
- **Type**: `Cache<K,V>`
- **Get**: `K → Async<Result<V?>>`
- **Set**: `(K, V, TTL?) → Async<Result<()>>`

### Mathematical Properties

**Storage Invariants**:
```
Deterministic: get(k) after set(k,v) returns v (within TTL)
TTL Monotonic: value expires after TTL seconds
Eviction: LRU eviction when capacity exceeded
```

**Consistency Properties**:
```
Read-Your-Writes: set(k,v) then get(k) returns v
Monotonic Reads: repeated get(k) returns same value until update/expire
Bounded Staleness: TTL provides upper bound on staleness
```

**Concurrency Properties**:
```
Thread Safe: All operations safe under concurrent access
Atomic Updates: set/delete operations are atomic
Isolation: Operations don't interfere with each other
```

### Required Operations
```
create: CacheConfig → Async<Result<Cache>>
get: (Cache, Key) → Async<Result<Value?>>
set: (Cache, Key, Value, TTL?) → Async<Result<()>>
delete: (Cache, Key) → Async<Result<Boolean>>
clear: Cache → Async<Result<()>>
stats: Cache → Async<Result<CacheStats>>
```

### Performance Requirements
```
Memory Cache: < 100μs per operation
Distributed Cache: < 10ms per operation  
Hit Rate: Configurable eviction policies
Memory: Bounded by maxSize configuration
```

---

## Mathematical Derivation Examples

These complete algebraic properties enable AI to derive:

**From Monoid Laws** → **Fluent Configuration**:
```
config.merge(file).merge(env).merge(defaults)
// Associativity enables flexible composition
```

**From Monad Laws** → **Safe Operation Chaining**:
```
loadUser(id).flatMap(validateUser).flatMap(saveUser)
// Error propagation without explicit checking
```

**From Semi-group Laws** → **Context Enrichment**:
```
error.withContext(requestInfo).withContext(userInfo).causedBy(rootCause)
// Immutable context building
```

**From Effect Laws** → **Structured Logging**:
```
logger.withContext(traceId).info("Operation completed", metrics)
// Context preservation with type safety
```

The mathematical properties are **sufficient and necessary** for AI to pattern match into ergonomic, composable APIs.