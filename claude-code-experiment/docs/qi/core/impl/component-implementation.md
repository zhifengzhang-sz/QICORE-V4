# QiCore v4.0 Component Implementation Guide

## Implementation Strategy

**Mathematical Thinking at Implementation Level**: Apply category theory patterns through TypeScript idioms, not formal specifications.

## Component Implementation Order

### 1. Base Components (Foundation)

#### Step 1.1: Result<T> Implementation
```typescript
// File: src/qicore/base/result.ts
import { Either, left, right } from "fp-ts/Either";

// Mathematical insight: Error handling is Either monad
export type Result<T> = Either<QiError, T>;

// Core operations preserving monad laws
export const success = <T>(value: T): Result<T> => right(value);
export const failure = <T>(error: QiError): Result<T> => left(error);

// Functor: map preserves structure
export const map = <T, U>(f: (value: T) => U) => 
  (result: Result<T>): Result<U> => 
    result._tag === "Right" ? success(f(result.right)) : result;

// Monad: flatMap enables composition
export const flatMap = <T, U>(f: (value: T) => Result<U>) =>
  (result: Result<T>): Result<U> =>
    result._tag === "Right" ? f(result.right) : result;

// Utility operations
export const fold = <T, U>(
  onError: (error: QiError) => U,
  onSuccess: (value: T) => U
) => (result: Result<T>): U =>
  result._tag === "Left" ? onError(result.left) : onSuccess(result.right);
```

**Key Implementation Decisions:**
- ✅ Use fp-ts Either to ensure monad laws automatically
- ✅ Curried functions for better composition
- ✅ Type-safe pattern matching with fold

#### Step 1.2: QiError Implementation
```typescript
// File: src/qicore/base/error.ts

// Mathematical insight: Error is product type with context accumulation
export interface QiError {
  readonly code: string;
  readonly message: string;
  readonly category: ErrorCategory;
  readonly context: Map<string, unknown>;
  readonly cause?: QiError;
  readonly timestamp: number;
}

export const createQiError = (
  code: string,
  message: string,
  category: ErrorCategory,
  context?: Record<string, unknown>
): QiError => ({
  code,
  message,
  category,
  context: new Map(Object.entries(context || {})),
  timestamp: Date.now()
});

// Context chaining (monoid-like operation)
export const withContext = (
  error: QiError,
  additionalContext: Record<string, unknown>
): QiError => ({
  ...error,
  context: new Map([...error.context, ...Object.entries(additionalContext)])
});

// Cause chaining for error composition
export const withCause = (error: QiError, cause: QiError): QiError => ({
  ...error,
  cause
});
```

### 2. Core Components (Built on Base)

#### Step 2.1: Configuration Implementation
```typescript
// File: src/qicore/core/config.ts

// Mathematical insight: Configuration merge is monoid operation
export interface ConfigData {
  readonly data: Map<string, unknown>;
}

// Identity element
export const empty = (): ConfigData => ({
  data: new Map()
});

// Associative operation (right-biased merge)
export const merge = (a: ConfigData, b: ConfigData): ConfigData => ({
  data: new Map([...a.data, ...b.data])
});

// Object to ConfigData conversion
export const fromObject = (obj: Record<string, unknown>): Result<ConfigData> => {
  try {
    return success({
      data: new Map(Object.entries(obj))
    });
  } catch (error) {
    return failure(createQiError(
      "CONFIG_PARSE_ERROR",
      `Failed to parse configuration: ${error}`,
      "CONFIGURATION"
    ));
  }
};

// Environment variable loading
export const fromEnvironment = (prefix: string): Result<ConfigData> => {
  try {
    const envVars = Object.entries(process.env)
      .filter(([key]) => key.startsWith(prefix))
      .map(([key, value]) => [key.slice(prefix.length).toLowerCase(), value]);
    
    return success({
      data: new Map(envVars)
    });
  } catch (error) {
    return failure(createQiError(
      "ENV_LOAD_ERROR",
      `Failed to load environment variables: ${error}`,
      "CONFIGURATION"
    ));
  }
};
```

#### Step 2.2: Logger Implementation
```typescript
// File: src/qicore/core/logger.ts
import winston from "winston";

// Mathematical insight: Logging is simple effect interface
export interface Logger {
  readonly log: (level: LogLevel, message: string, context?: Record<string, unknown>) => void;
  readonly isLevelEnabled: (level: LogLevel) => boolean;
}

export const createDefault = (): Result<Logger> => {
  try {
    const winstonLogger = winston.createLogger({
      level: "info",
      format: winston.format.combine(
        winston.format.timestamp(),
        winston.format.json()
      ),
      transports: [new winston.transports.Console()]
    });

    return success({
      log: (level, message, context) => {
        if (winstonLogger.isLevelEnabled(level)) {
          winstonLogger.log(level, message, context);
        }
      },
      isLevelEnabled: (level) => winstonLogger.isLevelEnabled(level)
    });
  } catch (error) {
    return failure(createQiError(
      "LOGGER_INIT_ERROR",
      `Failed to initialize logger: ${error}`,
      "SYSTEM"
    ));
  }
};
```

#### Step 2.3: Cache Implementation
```typescript
// File: src/qicore/core/cache.ts

// Mathematical insight: Cache is state monad with LRU + TTL
interface CacheEntry<V> {
  value: V;
  expiry: number;
  lastUsed: number;
}

export class MemoryCache<K, V> {
  private data = new Map<K, CacheEntry<V>>();
  private maxSize: number;
  private defaultTTL: number;

  constructor(maxSize = 1000, defaultTTL = 300000) { // 5 minutes default
    this.maxSize = maxSize;
    this.defaultTTL = defaultTTL;
  }

  async get(key: K): Promise<Result<V>> {
    const entry = this.data.get(key);
    
    if (!entry) {
      return failure(createQiError("CACHE_MISS", `Key not found: ${key}`, "CACHE"));
    }
    
    if (Date.now() > entry.expiry) {
      this.data.delete(key);
      return failure(createQiError("CACHE_EXPIRED", `Key expired: ${key}`, "CACHE"));
    }
    
    // Update LRU
    this.data.set(key, { ...entry, lastUsed: Date.now() });
    return success(entry.value);
  }

  async set(key: K, value: V, ttl?: number): Promise<Result<void>> {
    const expiry = Date.now() + (ttl ?? this.defaultTTL);
    
    // LRU eviction if at capacity
    if (this.data.size >= this.maxSize && !this.data.has(key)) {
      this.evictLRU();
    }
    
    this.data.set(key, { value, expiry, lastUsed: Date.now() });
    return success(undefined);
  }

  private evictLRU(): void {
    let oldestKey: K | undefined;
    let oldestTime = Date.now();
    
    for (const [key, entry] of this.data.entries()) {
      if (entry.lastUsed < oldestTime) {
        oldestTime = entry.lastUsed;
        oldestKey = key;
      }
    }
    
    if (oldestKey !== undefined) {
      this.data.delete(oldestKey);
    }
  }
}

export const createMemoryCache = <K, V>(
  maxSize?: number,
  defaultTTL?: number
): Result<MemoryCache<K, V>> => {
  try {
    return success(new MemoryCache<K, V>(maxSize, defaultTTL));
  } catch (error) {
    return failure(createQiError(
      "CACHE_INIT_ERROR",
      `Failed to create memory cache: ${error}`,
      "CACHE"
    ));
  }
};
```

#### Step 2.4: Performance Implementation
```typescript
// File: src/qicore/core/performance.ts

// Mathematical insight: Performance measurement is function composition with timing
export interface PerformanceMeasurement {
  readonly operation: string;
  readonly duration: number; // microseconds
  readonly timestamp: number;
}

export const measure = <T>(
  operation: string,
  fn: () => T
): T => {
  const start = performance.now();
  const result = fn();
  const end = performance.now();
  
  recordMeasurement({
    operation,
    duration: (end - start) * 1000, // Convert to microseconds
    timestamp: Date.now()
  });
  
  return result;
};

export const measureAsync = async <T>(
  operation: string,
  fn: () => Promise<T>
): Promise<T> => {
  const start = performance.now();
  const result = await fn();
  const end = performance.now();
  
  recordMeasurement({
    operation,
    duration: (end - start) * 1000,
    timestamp: Date.now()
  });
  
  return result;
};

// Benchmark with statistical analysis
export const benchmark = async (
  operation: string,
  fn: () => void | Promise<void>,
  iterations = 1000
): Promise<Result<PerformanceStats>> => {
  const measurements: number[] = [];
  
  try {
    for (let i = 0; i < iterations; i++) {
      const start = performance.now();
      await fn();
      const end = performance.now();
      measurements.push((end - start) * 1000);
    }
    
    const sorted = measurements.sort((a, b) => a - b);
    const mean = measurements.reduce((sum, m) => sum + m, 0) / measurements.length;
    const median = sorted[Math.floor(sorted.length / 2)];
    const p95 = sorted[Math.floor(sorted.length * 0.95)];
    
    return success({
      operation,
      sampleCount: iterations,
      mean,
      median,
      p95,
      min: sorted[0],
      max: sorted[sorted.length - 1]
    });
  } catch (error) {
    return failure(createQiError(
      "BENCHMARK_ERROR",
      `Benchmark failed: ${error}`,
      "SYSTEM"
    ));
  }
};
```

## Component Integration Patterns

### Pattern 1: Component Composition
```typescript
// Mathematical insight: Components compose through Result<T> monad
export const createUserService = async (): Promise<Result<UserService>> => {
  // Sequential composition with error short-circuiting
  const configResult = fromObject({ users: { cacheSize: 100 } });
  if (configResult._tag === "Left") return configResult;
  
  const loggerResult = createDefault();
  if (loggerResult._tag === "Left") return loggerResult;
  
  const cacheResult = createMemoryCache<string, User>();
  if (cacheResult._tag === "Left") return cacheResult;
  
  return success({
    getUser: (id: string) => getUserWithCache(id, cacheResult.right, loggerResult.right)
  });
};
```

### Pattern 2: Error Recovery
```typescript
// Mathematical insight: Error recovery preserves monad structure
export const withRetry = <T>(
  operation: () => Promise<Result<T>>,
  maxRetries = 3
): Promise<Result<T>> => {
  const attempt = async (retriesLeft: number): Promise<Result<T>> => {
    const result = await operation();
    
    if (result._tag === "Right" || retriesLeft === 0) {
      return result;
    }
    
    await new Promise(resolve => setTimeout(resolve, 100 * (maxRetries - retriesLeft)));
    return attempt(retriesLeft - 1);
  };
  
  return attempt(maxRetries);
};
```

## Testing Implementation Strategy

### Property Tests for Mathematical Laws
```typescript
import { describe, it, expect } from "vitest";

describe("Mathematical Law Verification", () => {
  it("verifies Result<T> monad laws", () => {
    // Left identity: return(a) >>= f ≡ f(a)
    const f = (x: number) => success(x.toString());
    expect(flatMap(f)(success(42))).toEqual(f(42));
    
    // Right identity: m >>= return ≡ m
    const m = success(42);
    expect(flatMap(success)(m)).toEqual(m);
    
    // Associativity: (m >>= f) >>= g ≡ m >>= (x => f(x) >>= g)
    const g = (x: string) => success(x.length);
    expect(flatMap(g)(flatMap(f)(m))).toEqual(flatMap(x => flatMap(g)(f(x)))(m));
  });
  
  it("verifies Configuration monoid laws", () => {
    const a = fromObject({ x: 1 }).right!;
    const b = fromObject({ y: 2 }).right!;
    const c = fromObject({ z: 3 }).right!;
    
    // Associativity: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
    expect(merge(merge(a, b), c)).toEqual(merge(a, merge(b, c)));
    
    // Identity: ∅ ⊕ a = a ⊕ ∅ = a
    expect(merge(empty(), a)).toEqual(a);
    expect(merge(a, empty())).toEqual(a);
  });
});
```

## Performance Verification
```typescript
describe("Performance Tier Compliance", () => {
  it("verifies TypeScript tier requirements", async () => {
    // Result operations < 100μs
    const resultStats = await benchmark("result_operations", () => {
      const result = success(42);
      return flatMap(x => success(x * 2))(map(x => x + 1)(result));
    });
    expect(resultStats.right!.mean).toBeLessThan(100);
    
    // Config merge < 1ms
    const configStats = await benchmark("config_merge", () => {
      merge(fromObject({ a: 1 }).right!, fromObject({ b: 2 }).right!);
    });
    expect(configStats.right!.mean).toBeLessThan(1000);
  });
});
```

---

**This implementation guide ensures mathematical correctness through TypeScript patterns while maintaining production quality and performance compliance.**