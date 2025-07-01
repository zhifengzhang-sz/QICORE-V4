# Function Signatures for QiCore v4 Components

## Result<T> Component

```typescript
interface Result<T> extends Either<QiError, T> {}

namespace Result {
  // Factory functions
  function success<T>(value: T): Result<T>;
  function failure<T>(error: QiError): Result<T>;
  function fromTryCatch<T>(operation: () => T): Result<T>;
  function fromAsync<T>(operation: () => Promise<T>): Promise<Result<T>>;

  // Monad operations
  function map<T, U>(result: Result<T>, f: (value: T) => U): Result<U>;
  function flatMap<T, U>(result: Result<T>, f: (value: T) => Result<U>): Result<U>;
  function fold<T, U>(result: Result<T>, onError: (error: QiError) => U, onSuccess: (value: T) => U): U;

  // Utility functions
  function isSuccess<T>(result: Result<T>): boolean;
  function isFailure<T>(result: Result<T>): boolean;
  function getOrElse<T>(result: Result<T>, defaultValue: T): T;
  function getOrElse<T>(result: Result<T>, defaultValue: () => T): T;
}
```

## QiError Component

```typescript
enum ErrorCategory {
  VALIDATION = 'VALIDATION',
  NETWORK = 'NETWORK',
  BUSINESS = 'BUSINESS',
  SYSTEM = 'SYSTEM'
}

interface QiError {
  readonly code: string;
  readonly message: string;
  readonly category: ErrorCategory;
  readonly context: Record<string, unknown>;
  readonly cause?: QiError;
  readonly timestamp: number;
}

namespace QiError {
  // Factory functions
  function create(code: string, message: string, category: ErrorCategory): QiError;
  function fromError(error: Error, category?: ErrorCategory): QiError;

  // Context operations
  function withContext(error: QiError, context: Record<string, unknown>): QiError;
  function withCause(error: QiError, cause: QiError): QiError;

  // Serialization
  function serialize(error: QiError): string;
  function deserialize(json: string): Result<QiError>;

  // Utility functions
  function getFullContext(error: QiError): Record<string, unknown>;
  function getCauseChain(error: QiError): QiError[];
}
```

## Configuration Component

```typescript
interface ConfigData extends Record<string, unknown> {}

interface Configuration {
  readonly data: ConfigData;
}

namespace Configuration {
  // Factory functions
  function fromObject(data: ConfigData): Result<Configuration>;
  function fromFile(filePath: string): Promise<Result<Configuration>>;
  function fromEnvironment(prefix?: string): Result<Configuration>;
  function empty(): Configuration;

  // Monoid operations
  function merge(configs: Configuration[]): Result<Configuration>;
  function merge(a: Configuration, b: Configuration): Result<Configuration>;

  // Access operations
  function get<T>(config: Configuration, key: string): Result<T>;
  function getOptional<T>(config: Configuration, key: string): Result<T | undefined>;
  function has(config: Configuration, key: string): boolean;

  // Validation
  function validate<T>(config: Configuration, schema: ValidationSchema<T>): Result<T>;

  // Utility functions
  function keys(config: Configuration): string[];
  function size(config: Configuration): number;
}
```

## Logger Component

```typescript
enum LogLevel {
  DEBUG = 0,
  INFO = 1,
  WARN = 2,
  ERROR = 3
}

interface LogContext extends Record<string, unknown> {}

interface LogEntry {
  readonly level: LogLevel;
  readonly message: string;
  readonly context?: LogContext;
  readonly timestamp: number;
}

interface LoggerConfig {
  readonly level: LogLevel;
  readonly transports: string[];
  readonly format?: string;
}

interface Logger {
  // Logging operations
  debug(message: string, context?: LogContext): void;
  info(message: string, context?: LogContext): void;
  warn(message: string, context?: LogContext): void;
  error(message: string, context?: LogContext): void;
  
  // Generic log method
  log(level: LogLevel, message: string, context?: LogContext): void;

  // Level checking (optimized for performance)
  isDebugEnabled(): boolean;
  isInfoEnabled(): boolean;
  isWarnEnabled(): boolean;
  isErrorEnabled(): boolean;

  // Configuration
  setLevel(level: LogLevel): void;
  getLevel(): LogLevel;

  // Cleanup
  close(): Promise<void>;
}

namespace Logger {
  // Factory functions
  function create(config: LoggerConfig): Result<Logger>;
  function createDefault(): Logger;
  function createInMemory(): Logger;

  // Configuration from Configuration component
  function fromConfiguration(config: Configuration): Result<Logger>;
}
```

## Cache Component

```typescript
interface CacheEntry<T> {
  readonly value: T;
  readonly ttl?: number;
  readonly createdAt: number;
  readonly accessedAt: number;
}

interface CacheStats {
  readonly hits: number;
  readonly misses: number;
  readonly size: number;
  readonly hitRate: number;
}

interface CacheConfig {
  readonly maxSize?: number;
  readonly defaultTtl?: number;
  readonly logger?: Logger;
}

interface Cache<T = unknown> {
  // Basic operations
  get(key: string): Result<T | undefined>;
  set(key: string, value: T, ttl?: number): Result<void>;
  delete(key: string): Result<boolean>;
  clear(): Result<void>;

  // Batch operations
  getMany(keys: string[]): Result<Map<string, T>>;
  setMany(entries: Map<string, T>, ttl?: number): Result<void>;
  deleteMany(keys: string[]): Result<number>;

  // Information
  has(key: string): boolean;
  size(): number;
  keys(): string[];
  
  // Statistics
  stats(): CacheStats;
  resetStats(): void;

  // Cleanup
  close(): Promise<Result<void>>;
}

namespace Cache {
  // Factory functions
  function createMemory<T>(config?: CacheConfig): Result<Cache<T>>;
  function createRedis<T>(config: CacheConfig & { redisUrl: string }): Promise<Result<Cache<T>>>;

  // Configuration from Configuration component
  function fromConfiguration<T>(config: Configuration): Result<Cache<T>>;
}
```

## Cross-Component Integration Signatures

```typescript
// System initialization
interface QiCoreSystem {
  readonly config: Configuration;
  readonly logger: Logger;
  readonly cache: Cache;
}

namespace QiCoreSystem {
  function create(configPath: string): Promise<Result<QiCoreSystem>>;
  function createFromConfig(config: Configuration): Result<QiCoreSystem>;
  
  // Graceful shutdown
  function shutdown(system: QiCoreSystem): Promise<Result<void>>;
}

// Error handling utilities
namespace ErrorHandling {
  function enrichError(error: QiError, component: string, operation: string): QiError;
  function logError(logger: Logger, error: QiError): void;
  function handleSystemError(error: QiError, logger: Logger): void;
}

// Performance monitoring
namespace Performance {
  function measureOperation<T>(operation: () => T, name: string): { result: T; duration: number };
  function measureAsync<T>(operation: () => Promise<T>, name: string): Promise<{ result: T; duration: number }>;
  function benchmark(operation: () => void, iterations: number): { mean: number; median: number; min: number; max: number };
}
```

## Type Guards and Utility Types

```typescript
// Type guards
function isResult<T>(value: unknown): value is Result<T>;
function isQiError(value: unknown): value is QiError;
function isConfiguration(value: unknown): value is Configuration;
function isLogger(value: unknown): value is Logger;
function isCache<T>(value: unknown): value is Cache<T>;

// Utility types
type ResultType<T> = T extends Result<infer U> ? U : never;
type ErrorType<T> = T extends Result<infer U> ? QiError : never;

// Configuration schema types
interface ValidationSchema<T> {
  validate(data: unknown): Result<T>;
}

// Async result helpers
type AsyncResult<T> = Promise<Result<T>>;

// Component composition types
type ComponentDependencies = {
  config?: Configuration;
  logger?: Logger;
  cache?: Cache;
};
``` 