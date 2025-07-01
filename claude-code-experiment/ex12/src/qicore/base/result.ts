/**
 * QiCore v4.0 - Base Result Component
 * 
 * Mathematical insight: Error handling is Either monad composition
 * Functional Result<T> using fp-ts Either for guaranteed monad law compliance
 */

import { type Either, left, right, isLeft, isRight } from "fp-ts/Either";
import type { QiError } from "./error.js";
import { ErrorCategory } from "./error.js";

/**
 * Result<T> type alias for Either<QiError, T>
 * Ensures monad laws are automatically satisfied by fp-ts
 */
export type Result<T> = Either<QiError, T>;

/**
 * Creates a successful result
 * Monad: return/pure operation
 */
export const success = <T>(value: T): Result<T> => right(value);

/**
 * Creates a failed result
 * Monad: failure injection
 */
export const failure = <T>(error: QiError): Result<T> => left(error);

/**
 * Functor: map operation preserving structure
 * Transforms successful values while preserving errors
 */
export const map = <T, U>(f: (value: T) => U) => 
  (result: Result<T>): Result<U> => 
    isRight(result) ? success(f(result.right)) : result;

/**
 * Monad: flatMap/bind operation for composition
 * Enables chaining computations that may fail
 */
export const flatMap = <T, U>(f: (value: T) => Result<U>) =>
  (result: Result<T>): Result<U> =>
    isRight(result) ? f(result.right) : result;

/**
 * Catamorphism: fold operation for result processing
 * Provides a way to handle both success and failure cases
 */
export const fold = <T, U>(
  onError: (error: QiError) => U,
  onSuccess: (value: T) => U
) => (result: Result<T>): U =>
  isLeft(result) ? onError(result.left) : onSuccess(result.right);

/**
 * Unsafe unwrap - throws on failure
 * Use only when certain of success
 */
export const unwrap = <T>(result: Result<T>): T => {
  if (isLeft(result)) {
    throw new Error(`Unwrapped failed Result: ${result.left.code} - ${result.left.message}`);
  }
  return result.right;
};

/**
 * Safe unwrap with default value
 * Returns default on failure
 */
export const unwrapOr = <T>(defaultValue: T) => (result: Result<T>): T =>
  isRight(result) ? result.right : defaultValue;

/**
 * Alternative operation - try another result on failure
 * Monad alternative pattern
 */
export const orElse = <T>(
  alternative: (error: QiError) => Result<T>
) => (result: Result<T>): Result<T> =>
  isLeft(result) ? alternative(result.left) : result;

/**
 * Predicate checking for success
 */
export const isSuccess = <T>(result: Result<T>): result is Extract<Result<T>, { _tag: "Right" }> =>
  isRight(result);

/**
 * Predicate checking for failure
 */
export const isFailure = <T>(result: Result<T>): result is Extract<Result<T>, { _tag: "Left" }> =>
  isLeft(result);

/**
 * Extract error from failed result
 */
export const getError = <T>(result: Result<T>): QiError | undefined =>
  isLeft(result) ? result.left : undefined;

/**
 * Extract value from successful result
 */
export const getValue = <T>(result: Result<T>): T | undefined =>
  isRight(result) ? result.right : undefined;

/**
 * Safe try-catch wrapper
 * Converts synchronous operations to Result
 */
export const fromTryCatch = <T>(operation: () => T): Result<T> => {
  try {
    return success(operation());
  } catch (error) {
    const qiError = error instanceof Error
      ? { 
          code: "OPERATION_FAILED",
          message: error.message,
          category: ErrorCategory.SYSTEM,
          context: new Map(),
          timestamp: Date.now()
        }
      : {
          code: "UNKNOWN_ERROR",
          message: String(error),
          category: ErrorCategory.SYSTEM,
          context: new Map(),
          timestamp: Date.now()
        };
    return failure(qiError);
  }
};

/**
 * Async try-catch wrapper
 * Converts asynchronous operations to Result
 */
export const fromAsyncTryCatch = async <T>(operation: () => Promise<T>): Promise<Result<T>> => {
  try {
    const result = await operation();
    return success(result);
  } catch (error) {
    const qiError = error instanceof Error
      ? { 
          code: "ASYNC_OPERATION_FAILED",
          message: error.message,
          category: ErrorCategory.SYSTEM,
          context: new Map(),
          timestamp: Date.now()
        }
      : {
          code: "UNKNOWN_ASYNC_ERROR",
          message: String(error),
          category: ErrorCategory.SYSTEM,
          context: new Map(),
          timestamp: Date.now()
        };
    return failure(qiError);
  }
};

/**
 * Sequence array of Results into Result of array
 * Fails if any Result fails
 */
export const sequence = <T>(results: Result<T>[]): Result<T[]> => {
  const values: T[] = [];
  
  for (const result of results) {
    if (isLeft(result)) {
      return result;
    }
    values.push(result.right);
  }
  
  return success(values);
};

/**
 * Map over array with Result-returning function
 * Combines map and sequence
 */
export const traverse = <T, U>(f: (value: T) => Result<U>) => 
  (values: T[]): Result<U[]> =>
    sequence(values.map(f));

/**
 * Apply function in Result to value in Result
 * Applicative functor pattern
 */
export const apply = <T, U>(
  f: Result<(value: T) => U>
) => (result: Result<T>): Result<U> =>
  isRight(f) && isRight(result) 
    ? success(f.right(result.right))
    : isLeft(f) ? f : result;

/**
 * Lift binary function to work with Results
 */
export const lift2 = <T, U, V>(
  f: (a: T, b: U) => V
) => (ra: Result<T>, rb: Result<U>): Result<V> =>
  apply(map(f)(ra))(rb);

/**
 * Lift ternary function to work with Results
 */
export const lift3 = <T, U, V, W>(
  f: (a: T, b: U, c: V) => W
) => (ra: Result<T>, rb: Result<U>, rc: Result<V>): Result<W> =>
  apply(apply(map(f)(ra))(rb))(rc);

/**
 * Filter successful values with predicate
 * Converts to failure if predicate fails
 */
export const filter = <T>(
  predicate: (value: T) => boolean,
  onFilterFailure: (value: T) => QiError
) => (result: Result<T>): Result<T> =>
  flatMap((value: T) => 
    predicate(value) ? success(value) : failure(onFilterFailure(value))
  )(result);

/**
 * Tap operation - perform side effect without changing Result
 * Useful for logging or debugging
 */
export const tap = <T>(
  sideEffect: (value: T) => void
) => (result: Result<T>): Result<T> => {
  if (isRight(result)) {
    sideEffect(result.right);
  }
  return result;
};

/**
 * Tap error operation - perform side effect on error
 */
export const tapError = <T>(
  sideEffect: (error: QiError) => void
) => (result: Result<T>): Result<T> => {
  if (isLeft(result)) {
    sideEffect(result.left);
  }
  return result;
};

/**
 * Bimap - map over both success and failure
 */
export const bimap = <T, U>(
  mapError: (error: QiError) => QiError,
  mapSuccess: (value: T) => U
) => (result: Result<T>): Result<U> =>
  isLeft(result) 
    ? failure(mapError(result.left))
    : success(mapSuccess(result.right));

/**
 * Map over error only
 */
export const mapError = <T>(
  f: (error: QiError) => QiError
) => (result: Result<T>): Result<T> =>
  isLeft(result) ? failure(f(result.left)) : result;