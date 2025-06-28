/**
 * QiCore v4.0 - Result<T> Monad Implementation
 *
 * Mathematical Contract-Based TypeScript Library
 * Component 1: Result<T> - Monad for error handling (8 operations)
 */

import type { QiError } from "./error.js";

/**
 * Result<T> represents a computation that can either succeed with a value of type T
 * or fail with a QiError. This implements the Result monad pattern for functional
 * error handling without exceptions.
 */
export abstract class Result<T> {
  /**
   * Operation 1: Create successful result
   */
  static success<T>(value: T): Result<T> {
    return new Success(value);
  }

  /**
   * Operation 2: Create failed result
   */
  static failure<T>(error: QiError): Result<T> {
    return new Failure(error);
  }

  /**
   * Operation 3: Map function over success value (Functor law)
   */
  abstract map<U>(fn: (value: T) => U): Result<U>;

  /**
   * Operation 4: FlatMap (bind) - Monadic bind operation
   */
  abstract flatMap<U>(fn: (value: T) => Result<U>): Result<U>;

  /**
   * Operation 5: Map error value
   */
  abstract mapError(fn: (error: QiError) => QiError): Result<T>;

  /**
   * Operation 6: Recover from error with fallback
   */
  abstract recover(fn: (error: QiError) => T): Result<T>;

  /**
   * Operation 7: Unwrap with default value
   */
  abstract unwrapOr(defaultValue: T): T;

  /**
   * Operation 8: Check if result is successful
   */
  abstract isSuccess(): boolean;

  /**
   * Check if result is a failure
   */
  isFailure(): boolean {
    return !this.isSuccess();
  }

  /**
   * Unwrap value (throws if failure)
   */
  abstract unwrap(): T;

  /**
   * Get error (throws if success)
   */
  abstract error(): QiError;

  /**
   * Apply function if success, otherwise return this
   */
  andThen<U>(fn: (value: T) => Result<U>): Result<U> {
    return this.flatMap(fn);
  }

  /**
   * Chain results together
   */
  and<U>(other: Result<U>): Result<U> {
    return this.flatMap(() => other);
  }

  /**
   * Return this if success, otherwise return other
   */
  or(other: Result<T>): Result<T> {
    return this.isSuccess() ? this : other;
  }

  /**
   * Match pattern for handling both success and failure cases
   */
  match<U>(cases: {
    success: (value: T) => U;
    failure: (error: QiError) => U;
  }): U {
    if (this.isSuccess()) {
      return cases.success(this.unwrap());
    }
    return cases.failure(this.error());
  }
}

/**
 * Success case of Result<T>
 */
class Success<T> extends Result<T> {
  constructor(private readonly value: T) {
    super();
  }

  map<U>(fn: (value: T) => U): Result<U> {
    try {
      return Result.success(fn(this.value));
    } catch (error) {
      // If mapping function throws, convert to failure
      return Result.failure({
        category: "RuntimeError",
        message: `Mapping function failed: ${error}`,
        context: { originalValue: this.value, error: String(error) },
        timestamp: Date.now(),
      } as QiError);
    }
  }

  flatMap<U>(fn: (value: T) => Result<U>): Result<U> {
    try {
      return fn(this.value);
    } catch (error) {
      return Result.failure({
        category: "RuntimeError",
        message: `FlatMap function failed: ${error}`,
        context: { originalValue: this.value, error: String(error) },
        timestamp: Date.now(),
      } as QiError);
    }
  }

  mapError(_fn: (error: QiError) => QiError): Result<T> {
    return this; // No error to map
  }

  recover(_fn: (error: QiError) => T): Result<T> {
    return this; // Already successful
  }

  unwrapOr(_defaultValue: T): T {
    return this.value;
  }

  isSuccess(): boolean {
    return true;
  }

  unwrap(): T {
    return this.value;
  }

  error(): never {
    throw new Error("Called error() on Success result");
  }
}

/**
 * Failure case of Result<T>
 */
class Failure<T> extends Result<T> {
  constructor(private readonly errorValue: QiError) {
    super();
  }

  map<U>(_fn: (value: T) => U): Result<U> {
    return new Failure<U>(this.errorValue);
  }

  flatMap<U>(_fn: (value: T) => Result<U>): Result<U> {
    return new Failure<U>(this.errorValue);
  }

  mapError(fn: (error: QiError) => QiError): Result<T> {
    try {
      return Result.failure(fn(this.errorValue));
    } catch (error) {
      // If error mapping function throws, create new error
      return Result.failure({
        category: "RuntimeError",
        message: `Error mapping function failed: ${error}`,
        context: { originalError: this.errorValue, error: String(error) },
        timestamp: Date.now(),
        cause: this.errorValue,
      } as QiError);
    }
  }

  recover(fn: (error: QiError) => T): Result<T> {
    try {
      return Result.success(fn(this.errorValue));
    } catch (error) {
      return Result.failure({
        category: "RuntimeError",
        message: `Recovery function failed: ${error}`,
        context: { originalError: this.errorValue, error: String(error) },
        timestamp: Date.now(),
        cause: this.errorValue,
      } as QiError);
    }
  }

  unwrapOr(defaultValue: T): T {
    return defaultValue;
  }

  isSuccess(): boolean {
    return false;
  }

  unwrap(): never {
    throw new Error(`Cannot unwrap failed Result: ${this.errorValue.message}`);
  }

  error(): QiError {
    return this.errorValue;
  }
}

/**
 * Utility functions for working with Results
 */
export namespace Result {
  /**
   * Sequence a list of Results into a Result of a list
   */
  export function sequence<T>(results: Result<T>[]): Result<T[]> {
    const values: T[] = [];
    for (const result of results) {
      if (result.isFailure()) {
        return result as Result<T[]>;
      }
      values.push(result.unwrap());
    }
    return Result.success(values);
  }

  /**
   * Traverse a list with a function that returns Results
   */
  export function traverse<T, U>(items: T[], fn: (item: T) => Result<U>): Result<U[]> {
    return sequence(items.map(fn));
  }

  /**
   * Convert all Results to values, collecting any errors
   */
  export function partition<T>(results: Result<T>[]): {
    successes: T[];
    failures: QiError[];
  } {
    const successes: T[] = [];
    const failures: QiError[] = [];

    for (const result of results) {
      if (result.isSuccess()) {
        successes.push(result.unwrap());
      } else {
        failures.push(result.error());
      }
    }

    return { successes, failures };
  }

  /**
   * Wrap a function that might throw into a Result
   */
  export function tryCatch<T>(fn: () => T): Result<T> {
    try {
      return Result.success(fn());
    } catch (error) {
      return Result.failure({
        category: "RuntimeError",
        message: `Function threw: ${error}`,
        context: { error: String(error) },
        timestamp: Date.now(),
      } as QiError);
    }
  }

  /**
   * Wrap an async function that might throw into a Result
   */
  export async function tryCatchAsync<T>(fn: () => Promise<T>): Promise<Result<T>> {
    try {
      const value = await fn();
      return Result.success(value);
    } catch (error) {
      return Result.failure({
        category: "RuntimeError",
        message: `Async function threw: ${error}`,
        context: { error: String(error) },
        timestamp: Date.now(),
      } as QiError);
    }
  }
}
