/**
 * QiCore v4.0 - Structured Error with Context Chaining
 * 
 * Mathematical Foundation: Error chaining preserves monad composition
 * Implementation: Custom structured error with immutable context accumulation
 * Performance Target: ~25μs for error creation, ~15μs for context operations
 */

export type ErrorCategory = 
  | "VALIDATION" 
  | "CONFIGURATION" 
  | "NETWORK" 
  | "CACHE" 
  | "SYSTEM" 
  | "UNKNOWN";

/**
 * Structured error interface with immutable context chaining
 * Preserves monad laws while adding structured context accumulation
 */
export interface QiError {
  readonly code: string;
  readonly message: string;
  readonly category: ErrorCategory;
  readonly context: Map<string, unknown>;
  readonly cause?: QiError;
  readonly timestamp: number;
}

/**
 * Create a new QiError with specified properties
 */
export const createQiError = (
  code: string,
  message: string,
  category: ErrorCategory = "UNKNOWN",
  context?: Map<string, unknown>,
  cause?: QiError
): QiError => ({
  code,
  message,
  category,
  context: context ?? new Map(),
  cause,
  timestamp: Date.now()
});

/**
 * Add context to an existing error (immutable operation)
 * Performance: ~15μs for context operations
 */
export const withContext = (
  error: QiError,
  key: string,
  value: unknown
): QiError => ({
  ...error,
  context: new Map([...error.context, [key, value]])
});

/**
 * Add multiple context entries to an error (immutable operation)
 */
export const withContextMap = (
  error: QiError,
  contextMap: Map<string, unknown>
): QiError => ({
  ...error,
  context: new Map([...error.context, ...contextMap])
});

/**
 * Chain errors by setting cause (immutable operation)
 */
export const withCause = (
  error: QiError,
  cause: QiError
): QiError => ({
  ...error,
  cause
});

/**
 * Convert error to a readable string representation
 */
export const errorToString = (error: QiError): string => {
  const contextStr = error.context.size > 0 
    ? ` (${Array.from(error.context.entries()).map(([k, v]) => `${k}=${v}`).join(', ')})`
    : '';
  
  const causeStr = error.cause 
    ? `\nCaused by: ${errorToString(error.cause)}`
    : '';
  
  return `[${error.category}:${error.code}] ${error.message}${contextStr}${causeStr}`;
};

/**
 * Get the root cause of an error chain
 */
export const getRootCause = (error: QiError): QiError => {
  let current = error;
  while (current.cause) {
    current = current.cause;
  }
  return current;
};

/**
 * Check if error matches a specific code
 */
export const isErrorCode = (error: QiError, code: string): boolean => 
  error.code === code;

/**
 * Check if error matches a specific category
 */
export const isErrorCategory = (error: QiError, category: ErrorCategory): boolean =>
  error.category === category;

/**
 * Serialize error to JSON (for logging/transmission)
 */
export const serializeError = (error: QiError): Record<string, unknown> => ({
  code: error.code,
  message: error.message,
  category: error.category,
  context: Object.fromEntries(error.context),
  cause: error.cause ? serializeError(error.cause) : undefined,
  timestamp: error.timestamp
});

/**
 * Common error constructors for different categories
 */
export const ValidationError = (code: string, message: string, context?: Map<string, unknown>) =>
  createQiError(code, message, "VALIDATION", context);

export const ConfigurationError = (code: string, message: string, context?: Map<string, unknown>) =>
  createQiError(code, message, "CONFIGURATION", context);

export const NetworkError = (code: string, message: string, context?: Map<string, unknown>) =>
  createQiError(code, message, "NETWORK", context);

export const CacheError = (code: string, message: string, context?: Map<string, unknown>) =>
  createQiError(code, message, "CACHE", context);

export const SystemError = (code: string, message: string, context?: Map<string, unknown>) =>
  createQiError(code, message, "SYSTEM", context); 