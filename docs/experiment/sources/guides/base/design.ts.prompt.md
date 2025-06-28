# Base Component TypeScript Design Prompt

> **Stage 2: TypeScript-Specific Design Patterns for Base Components**  
> **Scope**: QiError and Result<T> TypeScript implementation patterns  
> **Target**: TypeScript with fp-ts integration

## TypeScript-Specific Design Context

**Performance Tier**: Interpreted (TypeScript/Node.js) - 100× baseline
**Target Runtime**: Node.js with V8 engine optimizations
**Dependencies**: fp-ts for functional programming abstractions
**Type System**: Structural typing with strict null checks

## TypeScript Type System Design

**Error Category Definition**:
```typescript
enum ErrorCategory {
  VALIDATION = 'validation',
  NETWORK = 'network',
  FILESYSTEM = 'filesystem',
  CONFIGURATION = 'configuration',
  CACHE = 'cache',
  TIMEOUT = 'timeout',
  PERMISSION = 'permission',
  UNKNOWN = 'unknown'
}
```

**QiError Interface Design**:
```typescript
interface QiErrorData {
  readonly code: string
  readonly message: string
  readonly category: ErrorCategory
  readonly context?: Record<string, unknown>
  readonly cause?: QiError
  readonly timestamp: number
}

// Branded type for error codes for additional type safety
type ErrorCode = string & { readonly __brand: 'ErrorCode' }
```

**Result<T> Type Definition**:
```typescript
import { Either } from 'fp-ts/Either'

// Use fp-ts Either as foundation
type Result<T> = Either<QiError, T>

// Type guards for runtime checking
const isSuccess = <T>(result: Result<T>): result is Right<T> => isRight(result)
const isFailure = <T>(result: Result<T>): result is Left<QiError> => isLeft(result)
```

## QiError TypeScript Implementation Pattern

**Class Design with Immutability**:
```typescript
class QiError implements QiErrorData {
  readonly code: string
  readonly message: string
  readonly category: ErrorCategory
  readonly context?: Record<string, unknown>
  readonly cause?: QiError
  readonly timestamp: number

  private constructor(data: QiErrorData) {
    this.code = data.code
    this.message = data.message
    this.category = data.category
    this.context = data.context ? { ...data.context } : undefined
    this.cause = data.cause
    this.timestamp = data.timestamp
  }

  // Factory method with validation
  static create(
    code: string,
    message: string,
    category: ErrorCategory,
    context?: Record<string, unknown>,
    cause?: QiError
  ): QiError {
    // Validation
    if (code.length === 0) {
      throw new Error('Error code cannot be empty')
    }
    
    // Check for circular cause chains
    if (cause && this.wouldCreateCircularChain(cause, code)) {
      throw new Error('Circular cause chain detected')
    }

    return new QiError({
      code,
      message,
      category,
      context: context ? { ...context } : undefined,
      cause,
      timestamp: Date.now()
    })
  }

  private static wouldCreateCircularChain(cause: QiError, newCode: string): boolean {
    let current: QiError | undefined = cause
    let depth = 0
    const maxDepth = 10

    while (current && depth < maxDepth) {
      if (current.code === newCode) return true
      current = current.cause
      depth++
    }

    return depth >= maxDepth
  }
}
```

**Builder Pattern Methods**:
```typescript
class QiError {
  // Immutable context addition
  withContext(additionalContext: Record<string, unknown>): QiError {
    return QiError.create(
      this.code,
      this.message,
      this.category,
      { ...this.context, ...additionalContext },
      this.cause
    )
  }

  // Immutable cause chain
  withCause(causeError: QiError): QiError {
    return QiError.create(
      this.code,
      this.message,
      this.category,
      this.context,
      causeError
    )
  }

  // String representation
  toString(): string {
    const causeStr = this.cause ? ` (caused by: ${this.cause.toString()})` : ''
    return `[${this.code}] ${this.message} (${this.category})${causeStr}`
  }

  // Structured data for serialization
  toStructuredData(): Record<string, unknown> {
    return {
      code: this.code,
      message: this.message,
      category: this.category,
      context: this.context,
      cause: this.cause?.toStructuredData(),
      timestamp: this.timestamp
    }
  }

  getCategory(): ErrorCategory {
    return this.category
  }
}
```

**Category-Specific Factory Methods**:
```typescript
class QiError {
  static validation(code: string, message: string, context?: Record<string, unknown>): QiError {
    return QiError.create(code, message, ErrorCategory.VALIDATION, context)
  }

  static network(code: string, message: string, context?: Record<string, unknown>): QiError {
    return QiError.create(code, message, ErrorCategory.NETWORK, context)
  }

  static filesystem(code: string, message: string, context?: Record<string, unknown>): QiError {
    return QiError.create(code, message, ErrorCategory.FILESYSTEM, context)
  }

  static configuration(code: string, message: string, context?: Record<string, unknown>): QiError {
    return QiError.create(code, message, ErrorCategory.CONFIGURATION, context)
  }

  static cache(code: string, message: string, context?: Record<string, unknown>): QiError {
    return QiError.create(code, message, ErrorCategory.CACHE, context)
  }

  static timeout(code: string, message: string, context?: Record<string, unknown>): QiError {
    return QiError.create(code, message, ErrorCategory.TIMEOUT, context)
  }

  static permission(code: string, message: string, context?: Record<string, unknown>): QiError {
    return QiError.create(code, message, ErrorCategory.PERMISSION, context)
  }

  static unknown(code: string, message: string, context?: Record<string, unknown>): QiError {
    return QiError.create(code, message, ErrorCategory.UNKNOWN, context)
  }
}
```

## Result<T> TypeScript Implementation Pattern

**fp-ts Integration**:
```typescript
import { Either, left, right, isLeft, isRight } from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import * as E from 'fp-ts/Either'

// Type alias for clarity
export type Result<T> = Either<QiError, T>
```

**Factory Functions**:
```typescript
// Success factory
export const success = <T>(data: T): Result<T> => right(data)

// Failure factory
export const failure = <T>(error: QiError): Result<T> => left(error)

// Try-catch wrapper
export const fromTryCatch = <T>(operation: () => T): Result<T> => {
  try {
    const result = operation()
    return success(result)
  } catch (error) {
    const qiError = error instanceof QiError 
      ? error 
      : QiError.unknown(
          'UNEXPECTED_ERROR',
          error instanceof Error ? error.message : String(error),
          { originalError: error }
        )
    return failure(qiError)
  }
}

// Async try-catch wrapper
export const fromAsyncTryCatch = async <T>(
  operation: () => Promise<T>
): Promise<Result<T>> => {
  try {
    const result = await operation()
    return success(result)
  } catch (error) {
    const qiError = error instanceof QiError 
      ? error 
      : QiError.unknown(
          'ASYNC_OPERATION_FAILED',
          error instanceof Error ? error.message : String(error),
          { originalError: error }
        )
    return failure(qiError)
  }
}
```

**Transformation Functions**:
```typescript
// Functor map - preserves error, transforms success
export const map = <T, U>(fn: (value: T) => U) => 
  (result: Result<T>): Result<U> => E.map(fn)(result)

// Monadic flatMap - enables chaining
export const flatMap = <T, U>(fn: (value: T) => Result<U>) => 
  (result: Result<T>): Result<U> => E.chain(fn)(result)

// Alias for functional composition
export const chain = flatMap
```

**Elimination Functions**:
```typescript
// Unsafe extraction - throws on error
export const unwrap = <T>(result: Result<T>): T => {
  if (isRight(result)) {
    return result.right
  }
  throw result.left
}

// Safe extraction with default
export const unwrapOr = <T>(defaultValue: T) => 
  (result: Result<T>): T => E.getOrElse(() => defaultValue)(result)

// Pattern matching
export const match = <T, U>(
  onSuccess: (value: T) => U,
  onError: (error: QiError) => U
) => (result: Result<T>): U => E.fold(onError, onSuccess)(result)

// Error recovery
export const orElse = <T>(alternative: (error: QiError) => Result<T>) => 
  (result: Result<T>): Result<T> => E.orElse(alternative)(result)
```

**Utility Functions**:
```typescript
// Type guards
export const isSuccess = <T>(result: Result<T>): result is Right<T> => isRight(result)
export const isFailure = <T>(result: Result<T>): result is Left<QiError> => isLeft(result)

// Extract error from failure
export const getError = <T>(result: Result<T>): QiError | null => 
  isLeft(result) ? result.left : null

// Extract value from success
export const getValue = <T>(result: Result<T>): T | null => 
  isRight(result) ? result.right : null
```

## TypeScript Performance Optimizations

**V8 Engine Optimizations**:
```typescript
// Use monomorphic object shapes for V8 optimization
interface OptimizedResult<T> {
  readonly _tag: 'Left' | 'Right'
  readonly left?: QiError
  readonly right?: T
}

// Avoid delete operations that deoptimize objects
const optimizedWithContext = (error: QiError, context: Record<string, unknown>): QiError => {
  return QiError.create(
    error.code,
    error.message,
    error.category,
    { ...error.context, ...context }, // Object spread instead of delete
    error.cause
  )
}

// Use object pooling for frequently created Results
class ResultPool {
  private static successPool: Array<Right<any>> = []
  private static failurePool: Array<Left<QiError>> = []

  static acquireSuccess<T>(value: T): Result<T> {
    // Implementation for object pooling if needed for high-performance scenarios
    return right(value)
  }

  static acquireFailure<T>(error: QiError): Result<T> {
    // Implementation for object pooling if needed for high-performance scenarios
    return left(error)
  }
}
```

**Memory Management Patterns**:
```typescript
// Minimize closure allocations in hot paths
const createMapFunction = <T, U>(transform: (x: T) => U) => {
  // Pre-bind the transform function to avoid closure allocation in each call
  return (result: Result<T>): Result<U> => {
    return isRight(result) ? right(transform(result.right)) : result as Left<QiError>
  }
}

// Use WeakMap for memory-efficient caching where appropriate
const errorCache = new WeakMap<object, QiError>()

const getCachedError = (source: object, factory: () => QiError): QiError => {
  let error = errorCache.get(source)
  if (!error) {
    error = factory()
    errorCache.set(source, error)
  }
  return error
}
```

## Integration Patterns with fp-ts

**Functional Composition Chains**:
```typescript
import { pipe } from 'fp-ts/function'

// Compose validation pipeline
const validateUser = (userData: unknown): Result<User> => 
  pipe(
    success(userData),
    flatMap(validateRequired),
    flatMap(validateEmail),
    flatMap(validateAge),
    map(createUser)
  )

// Error handling with context preservation
const withErrorContext = (context: Record<string, unknown>) => 
  <T>(result: Result<T>): Result<T> => 
    pipe(
      result,
      orElse(error => failure(error.withContext(context)))
    )
```

**Async Result Patterns**:
```typescript
import * as TE from 'fp-ts/TaskEither'

// TaskEither for async Result operations
type AsyncResult<T> = TE.TaskEither<QiError, T>

const asyncSuccess = <T>(value: T): AsyncResult<T> => TE.right(value)
const asyncFailure = <T>(error: QiError): AsyncResult<T> => TE.left(error)

// Convert Promise<Result<T>> to TaskEither
const fromPromiseResult = <T>(promiseResult: Promise<Result<T>>): AsyncResult<T> => 
  TE.fromTask(() => promiseResult.then(result => 
    isRight(result) ? result.right : Promise.reject(result.left)
  ))
```

## Testing Patterns

**Property-Based Testing with fast-check**:
```typescript
import * as fc from 'fast-check'

// Generators for property testing
const arbitraryErrorCategory = fc.constantFrom(...Object.values(ErrorCategory))
const arbitraryQiError = fc.record({
  code: fc.string({ minLength: 1 }),
  message: fc.string(),
  category: arbitraryErrorCategory,
  context: fc.option(fc.dictionary(fc.string(), fc.anything()), { nil: undefined })
})

// Monad law tests
describe('Result Monad Laws', () => {
  it('satisfies left identity law', () => {
    fc.assert(fc.property(
      fc.integer(),
      fc.func(fc.anything()),
      (a, f) => {
        const result1 = pipe(success(a), flatMap(f))
        const result2 = f(a)
        expect(result1).toEqual(result2)
      }
    ))
  })
})
```

**Performance Benchmarks**:
```typescript
import { performance } from 'perf_hooks'

const benchmarkResultCreation = (iterations: number = 1000): number => {
  const start = performance.now()
  
  for (let i = 0; i < iterations; i++) {
    success(i)
  }
  
  const end = performance.now()
  return (end - start) / iterations // microseconds per operation
}

const benchmarkErrorCreation = (iterations: number = 1000): number => {
  const start = performance.now()
  
  for (let i = 0; i < iterations; i++) {
    QiError.validation('TEST_ERROR', 'Test message')
  }
  
  const end = performance.now()
  return (end - start) / iterations
}
```

## Success Criteria for TypeScript Implementation

**Type Safety**:
- [ ] Strict TypeScript compilation with no errors
- [ ] Proper type inference for all Result operations
- [ ] Runtime type checking where appropriate
- [ ] No use of `any` types

**Mathematical Properties**:
- [ ] All monad laws verified with property tests
- [ ] All functor laws verified with property tests
- [ ] Immutability enforced and tested
- [ ] Error propagation works correctly

**Performance (Interpreted Tier - 100× baseline)**:
- [ ] Result creation: < 100μs per operation
- [ ] Error creation: < 100μs per operation
- [ ] Map operations: < 50μs per operation
- [ ] FlatMap operations: < 100μs per operation
- [ ] Context merging: < 10μs per operation

**Integration**:
- [ ] Clean fp-ts integration
- [ ] No version conflicts with fp-ts
- [ ] Proper tree-shaking support
- [ ] Ready for consumption by core components