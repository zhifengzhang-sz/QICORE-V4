/**
 * QiCore v4.0 - Base Error Component
 * 
 * Mathematical insight: Error is product type with context accumulation
 * Structured error representation with immutable context chaining
 */

export enum ErrorCategory {
  VALIDATION = "VALIDATION",
  NETWORK = "NETWORK",
  FILESYSTEM = "FILESYSTEM",
  CONFIGURATION = "CONFIGURATION",
  CACHE = "CACHE",
  TIMEOUT = "TIMEOUT",
  PERMISSION = "PERMISSION",
  SYSTEM = "SYSTEM"
}

export interface QiError {
  readonly code: string;
  readonly message: string;
  readonly category: ErrorCategory;
  readonly context: Map<string, unknown>;
  readonly cause?: QiError;
  readonly timestamp: number;
}

/**
 * Creates a structured QiError with optional context
 */
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

/**
 * Context chaining (monoid-like operation)
 * Adds additional context to existing error immutably
 */
export const withContext = (
  error: QiError,
  additionalContext: Record<string, unknown>
): QiError => ({
  ...error,
  context: new Map([...error.context, ...Object.entries(additionalContext)])
});

/**
 * Cause chaining for error composition
 * Links errors in a causal chain
 */
export const withCause = (error: QiError, cause: QiError): QiError => ({
  ...error,
  cause
});

/**
 * Converts error to structured data representation
 */
export const toStructuredData = (error: QiError): Record<string, unknown> => ({
  code: error.code,
  message: error.message,
  category: error.category,
  context: Object.fromEntries(error.context),
  cause: error.cause ? toStructuredData(error.cause) : undefined,
  timestamp: error.timestamp
});

/**
 * Serializes error to human-readable string
 */
export const toString = (error: QiError): string => {
  const contextStr = error.context.size > 0 
    ? ` (${Array.from(error.context.entries()).map(([k, v]) => `${k}=${v}`).join(', ')})`
    : '';
  
  const causeStr = error.cause ? `\nCaused by: ${toString(error.cause)}` : '';
  
  return `[${error.category}] ${error.code}: ${error.message}${contextStr}${causeStr}`;
};

/**
 * Gets the error category
 */
export const getCategory = (error: QiError): ErrorCategory => error.category;

/**
 * Checks if error has a specific category
 */
export const hasCategory = (category: ErrorCategory) => (error: QiError): boolean =>
  error.category === category;

/**
 * Gets all context keys
 */
export const getContextKeys = (error: QiError): string[] =>
  Array.from(error.context.keys());

/**
 * Gets context value by key
 */
export const getContextValue = <T = unknown>(key: string) => (error: QiError): T | undefined =>
  error.context.get(key) as T | undefined;

/**
 * Checks if error has context key
 */
export const hasContextKey = (key: string) => (error: QiError): boolean =>
  error.context.has(key);

/**
 * Traverses error cause chain
 */
export const getAllCauses = (error: QiError): QiError[] => {
  const causes: QiError[] = [];
  let current = error.cause;
  
  while (current) {
    causes.push(current);
    current = current.cause;
  }
  
  return causes;
};

/**
 * Finds error in cause chain by code
 */
export const findByCode = (code: string) => (error: QiError): QiError | undefined => {
  if (error.code === code) return error;
  
  for (const cause of getAllCauses(error)) {
    if (cause.code === code) return cause;
  }
  
  return undefined;
};

/**
 * Finds error in cause chain by category
 */
export const findByCategory = (category: ErrorCategory) => (error: QiError): QiError | undefined => {
  if (error.category === category) return error;
  
  for (const cause of getAllCauses(error)) {
    if (cause.category === category) return cause;
  }
  
  return undefined;
};