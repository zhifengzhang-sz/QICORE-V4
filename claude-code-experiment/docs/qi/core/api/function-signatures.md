# QiCore v4.0 TypeScript API Reference

## Base Components

### Result<T> Operations

```typescript
// Type Definition
type Result<T> = Either<QiError, T>

// Core Operations
success<T>(value: T): Result<T>
failure<T>(error: QiError): Result<T>

// Functor Operations  
map<T, U>(f: (value: T) => U): (result: Result<T>) => Result<U>

// Monad Operations
flatMap<T, U>(f: (value: T) => Result<U>): (result: Result<T>) => Result<U>

// Utility Operations
fold<T, U>(
  onError: (error: QiError) => U,
  onSuccess: (value: T) => U
): (result: Result<T>) => U

unwrap<T>(result: Result<T>): T  // Throws on failure
unwrapOr<T>(defaultValue: T): (result: Result<T>) => T

// Composition Operations
orElse<T>(
  alternative: (error: QiError) => Result<T>
): (result: Result<T>) => Result<T>

fromTryCatch<T>(operation: () => T): Result<T>
fromAsyncTryCatch<T>(operation: () => Promise<T>): Promise<Result<T>>
```

**Performance Specifications:**
- `success()`: ~5μs
- `failure()`: ~5μs  
- `map()`: ~20μs
- `flatMap()`: ~40μs
- All operations < 100μs (TypeScript tier compliance)

### QiError Operations

```typescript
// Type Definition
interface QiError {
  readonly code: string
  readonly message: string
  readonly category: ErrorCategory
  readonly context: Map<string, unknown>
  readonly cause?: QiError
  readonly timestamp: number
}

// Creation Operations
createQiError(
  code: string,
  message: string, 
  category: ErrorCategory,
  context?: Record<string, unknown>
): QiError

// Context Operations
withContext(
  error: QiError,
  additionalContext: Record<string, unknown>
): QiError

withCause(error: QiError, cause: QiError): QiError

// Serialization Operations
toString(error: QiError): string
toStructuredData(error: QiError): Record<string, unknown>
getCategory(error: QiError): ErrorCategory

// Error Categories
enum ErrorCategory {
  VALIDATION = "VALIDATION"
  NETWORK = "NETWORK" 
  FILESYSTEM = "FILESYSTEM"
  CONFIGURATION = "CONFIGURATION"
  CACHE = "CACHE"
  TIMEOUT = "TIMEOUT"
  PERMISSION = "PERMISSION"
  SYSTEM = "SYSTEM"
}
```

**Performance Specifications:**
- `createQiError()`: ~25μs
- `withContext()`: ~15μs
- `withCause()`: ~15μs
- `toString()`: ~30μs

## Core Components

### Configuration Operations

```typescript
// Type Definition
interface ConfigData {
  readonly data: Map<string, unknown>
}

// Monoid Operations
empty(): ConfigData
merge(a: ConfigData, b: ConfigData): ConfigData

// Loading Operations
fromObject(obj: Record<string, unknown>): Result<ConfigData>
fromEnvironment(prefix: string): Result<ConfigData>
fromFile(path: string): Promise<Result<ConfigData>>

// Access Operations
get<T>(config: ConfigData, key: string): Result<T>
has(config: ConfigData, key: string): boolean
keys(config: ConfigData): string[]

// Validation Operations
validate<T>(
  config: ConfigData,
  schema: Schema<T>
): Result<T>
```

**Performance Specifications:**
- `merge()`: ~200-500μs (< 1ms requirement)
- `fromObject()`: ~100μs
- `get()`: ~10μs
- `has()`: ~5μs

### Logger Operations

```typescript
// Type Definition
interface Logger {
  readonly log: (level: LogLevel, message: string, context?: Record<string, unknown>) => void
  readonly isLevelEnabled: (level: LogLevel) => boolean
}

// Creation Operations
createDefault(): Result<Logger>
createWithConfig(config: LoggerConfig): Result<Logger>

// Log Level Operations
enum LogLevel {
  DEBUG = "debug"
  INFO = "info"
  WARN = "warn"
  ERROR = "error"
  FATAL = "fatal"
}

// Logging Operations
log(level: LogLevel, message: string, context?: Record<string, unknown>): void
debug(message: string, context?: Record<string, unknown>): void
info(message: string, context?: Record<string, unknown>): void
warn(message: string, context?: Record<string, unknown>): void
error(message: string, context?: Record<string, unknown>): void
fatal(message: string, context?: Record<string, unknown>): void

// Level Check Operations
isLevelEnabled(level: LogLevel): boolean
setLevel(level: LogLevel): void
getLevel(): LogLevel
```

**Performance Specifications:**
- `isLevelEnabled()`: ~0.1-0.5μs (< 1μs requirement)
- `log()`: ~5-10μs per message
- `createDefault()`: ~3ms (one-time initialization)

### Cache Operations

```typescript
// Type Definition
interface Cache<K, V> {
  get(key: K): Promise<Result<V>>
  set(key: K, value: V, ttl?: number): Promise<Result<void>>
  has(key: K): Promise<Result<boolean>>
  delete(key: K): Promise<Result<boolean>>
  clear(): Promise<Result<void>>
  size(): Promise<Result<number>>
  keys(): Promise<Result<K[]>>
}

// Creation Operations
createMemoryCache<K, V>(
  maxSize?: number,
  defaultTTL?: number
): Result<Cache<K, V>>

createRedisCache<K, V>(
  connectionString: string,
  options?: RedisCacheOptions
): Promise<Result<Cache<K, V>>>

// Batch Operations
mget<K, V>(cache: Cache<K, V>, keys: K[]): Promise<Result<Map<K, V>>>
mset<K, V>(cache: Cache<K, V>, entries: Map<K, V>, ttl?: number): Promise<Result<void>>

// Statistics Operations
getStats<K, V>(cache: Cache<K, V>): Promise<Result<CacheStats>>

interface CacheStats {
  hitCount: number
  missCount: number
  evictionCount: number
  size: number
  maxSize: number
}
```

**Performance Specifications:**
- Memory cache `get()`/`set()`: ~10-30μs (< 50μs requirement)
- Redis cache `get()`/`set()`: ~1-2ms (including network)
- `has()`: ~5-15μs (memory), ~1ms (Redis)
- `size()`: ~1μs (memory), ~0.5ms (Redis)

### Performance Operations

```typescript
// Type Definition
interface PerformanceMeasurement {
  readonly operation: string
  readonly duration: number  // microseconds
  readonly timestamp: number
}

interface PerformanceStats {
  readonly operation: string
  readonly sampleCount: number
  readonly mean: number      // microseconds
  readonly median: number    // microseconds
  readonly p95: number       // 95th percentile
  readonly p99: number       // 99th percentile
  readonly min: number
  readonly max: number
  readonly tierCompliant: boolean
}

// Measurement Operations
measure<T>(operation: string, fn: () => T): T
measureAsync<T>(operation: string, fn: () => Promise<T>): Promise<T>

// Benchmarking Operations
benchmark(
  operation: string,
  fn: () => void | Promise<void>,
  iterations?: number
): Promise<Result<PerformanceStats>>

// Statistics Operations
getStats(operation: string): Result<PerformanceStats>
getOperations(): string[]
generateReport(): Result<string>

// Monitoring Operations
recordMeasurement(measurement: PerformanceMeasurement): void
clearMeasurements(): void
createMonitor(maxSamples?: number): PerformanceMonitor

// Performance Requirements (TypeScript Tier)
const PERFORMANCE_REQUIREMENTS = {
  RESULT_OPERATIONS: 100,    // μs
  CONFIG_MERGE: 1000,        // μs
  LOGGER_LEVEL_CHECK: 1,     // μs
  CACHE_OPERATIONS: 50,      // μs
  HTTP_OVERHEAD: 5000,       // μs
  DB_OVERHEAD: 1000          // μs
} as const
```

**Performance Specifications:**
- `measure()`: ~5-10μs overhead (< 10μs requirement)
- `measureAsync()`: ~5-15μs overhead
- `getStats()`: ~1-5ms for statistical calculation
- `benchmark()`: Variable based on iterations and operation complexity

## Integration Patterns

### Component Composition

```typescript
// Service Creation Pattern
createUserService(): Promise<Result<UserService>>
createDataService(config: ConfigData): Promise<Result<DataService>>

// Configuration Chain Pattern
loadConfiguration(): Promise<Result<ConfigData>>
// Loads from: defaults → file → environment → overrides

// Error Recovery Pattern
withRetry<T>(
  operation: () => Promise<Result<T>>,
  maxRetries?: number,
  backoffMs?: number
): Promise<Result<T>>

// Performance Monitoring Pattern
withPerformanceMonitoring<T>(
  operationName: string,
  operation: () => T | Promise<T>
): T | Promise<T>
```

### Testing Utilities

```typescript
// Property Testing Support
verifyMonadLaws<T, U>(
  value: T,
  f: (a: T) => Result<U>,
  g: (b: U) => Result<unknown>
): boolean

verifyMonoidLaws(
  values: ConfigData[]
): boolean

verifyFunctorLaws<T, U>(
  container: Container<T>,
  f: (a: T) => U,
  g: (b: U) => unknown
): boolean

// Performance Testing
benchmarkAllOperations(): Promise<Result<Map<string, PerformanceStats>>>
verifyTierCompliance(): Promise<Result<boolean>>

// Mock Utilities
createMockLogger(): Logger
createMockCache<K, V>(): Cache<K, V>
createTestConfig(overrides?: Record<string, unknown>): ConfigData
```

## Usage Examples

### Basic Error Handling
```typescript
const processUser = (userData: unknown): Result<User> =>
  validateUserData(userData)
    .flatMap(createUser)
    .flatMap(persistUser)
    .map(addDefaultPermissions)
```

### Configuration with Error Recovery
```typescript
const loadAppConfig = async (): Promise<Result<AppConfig>> => {
  const configChain = await loadConfiguration()
  if (configChain._tag === "Left") {
    return withRetry(() => loadConfiguration(), 3)
  }
  return configChain
}
```

### Caching with Performance Monitoring
```typescript
const getCachedUser = async (id: string): Promise<Result<User>> =>
  measureAsync("get_cached_user", async () => {
    const cached = await cache.get(id)
    if (cached._tag === "Right") return cached
    
    const user = await fetchUser(id)
    if (user._tag === "Right") {
      await cache.set(id, user.right, 300000) // 5 minutes TTL
    }
    return user
  })
```

---

**All operations are designed for production use with comprehensive error handling, performance monitoring, and mathematical correctness.**