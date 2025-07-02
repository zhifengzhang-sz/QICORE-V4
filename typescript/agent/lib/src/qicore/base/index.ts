/**
 * QiCore v4.0 Base Components
 *
 * Clean fp-ts-based implementation following QiCore v4 TypeScript template.
 * Exports foundational types and operations with proven mathematical properties.
 */

// ============================================================================
// Result Type and Operations (fp-ts Either-based)
// ============================================================================

export type { Result as ResultType, Either, Left, Right } from "./result.js";

// Core factory functions
export {
  success,
  failure,
  fromTryCatch,
  fromAsyncTryCatch,
  fromMaybe,
  fromPredicate,
} from "./result.js";

// Functor operations
export {
  map,
  mapError,
  bimap,
} from "./result.js";

// Monad operations
export {
  flatMap,
  chain,
  chainFirst,
} from "./result.js";

// Applicative operations
export {
  ap,
  liftA2,
} from "./result.js";

// Alternative operations
export {
  alt,
  orElse,
} from "./result.js";

// Extraction operations
export {
  unwrap,
  unwrapOr,
  unwrapOrElse,
} from "./result.js";

// Pattern matching
export {
  match,
  fold,
} from "./result.js";

// Collection operations
export {
  sequence,
  traverse,
} from "./result.js";

// Query operations
export {
  isSuccess,
  isFailure,
  getData,
  getError,
} from "./result.js";

// Complete API and alternative names
export {
  QiResult,
  ResultOps,
  ResultImpl,
} from "./result.js";

// ============================================================================
// Error Types and Operations
// ============================================================================

export type {
  QiError,
  ErrorCategory,
  ErrorSeverity,
  ErrorData,
  RetryStrategy,
} from "./error.js";

export {
  createQiError,
  fromException,
  fromString,
  withContext,
  withCause,
  withSeverity,
  chain as chainError,
  aggregate,
  getRetryStrategy,
  isRetryable,
  isQiError,
  CommonErrors,
} from "./error.js";

// ============================================================================
// Complete Base API Object
// ============================================================================

// Import functions for API object construction
import {
  chain as resultChain,
  failure as resultFailure,
  flatMap as resultFlatMap,
  fromAsyncTryCatch as resultFromAsyncTryCatch,
  fromMaybe as resultFromMaybe,
  fromPredicate as resultFromPredicate,
  fromTryCatch as resultFromTryCatch,
  isFailure as resultIsFailure,
  isSuccess as resultIsSuccess,
  map as resultMap,
  match as resultMatch,
  sequence as resultSequence,
  success as resultSuccess,
} from "./result.js";

import {
  CommonErrors as errorCommonErrors,
  createQiError as errorCreate,
  fromException as errorFromException,
  fromString as errorFromString,
  isQiError as errorIsQiError,
  isRetryable as errorIsRetryable,
  withCause as errorWithCause,
  withContext as errorWithContext,
} from "./error.js";

/**
 * Complete QiCore Base API
 *
 * Provides both individual function exports and organized API objects
 * for different usage patterns.
 */
export const QiBase = {
  Result: {
    // Core construction
    success: resultSuccess,
    failure: resultFailure,
    fromTryCatch: resultFromTryCatch,
    fromAsyncTryCatch: resultFromAsyncTryCatch,
    fromMaybe: resultFromMaybe,
    fromPredicate: resultFromPredicate,

    // Core operations
    map: resultMap,
    flatMap: resultFlatMap,
    chain: resultChain,
    match: resultMatch,
    sequence: resultSequence,

    // Query operations
    isSuccess: resultIsSuccess,
    isFailure: resultIsFailure,
  },
  Error: {
    // Construction
    create: errorCreate,
    fromException: errorFromException,
    fromString: errorFromString,

    // Utilities
    withContext: errorWithContext,
    withCause: errorWithCause,
    CommonErrors: errorCommonErrors,
    isRetryable: errorIsRetryable,
    isQiError: errorIsQiError,
  },
} as const;
