/**
 * QiCore v4.0 TypeScript Implementation
 *
 * Clean fp-ts-based implementation following QiCore v4 TypeScript template.
 * Complete API for building robust applications with proven mathematical foundations.
 *
 * Architecture:
 * - Base: Result<T> (fp-ts Either), QiError system
 * - Core: Configuration, Cache, Logger, HTTP Client
 * - Application: Web, AI, Document, CLI, Database, MCP
 */

// ============================================================================
// Base Components (fp-ts Either-based Result + QiError)
// ============================================================================

export {
  // Result operations (fp-ts Either-based)
  success,
  failure,
  fromTryCatch,
  fromAsyncTryCatch,
  fromMaybe,
  fromPredicate,
  map,
  flatMap,
  chain,
  match,
  sequence,
  isSuccess,
  isFailure,
  // Error operations
  createQiError,
  fromException,
  withContext,
  withCause,
  CommonErrors,
  isRetryable,
  isQiError,
  // Complete APIs
  QiResult,
  QiBase,
} from "./base/index.js";

export type {
  ResultType as Result,
  Either,
  Left,
  Right,
  QiError,
  ErrorCategory,
  ErrorSeverity,
} from "./base/index.js";

export {
  Result,
  QiError,
} from "./base/index.js";

// ============================================================================
// Core Components (Configuration, Logger, Cache, HTTP)
// ============================================================================

export {
  // Configuration operations
  empty as emptyConfig,
  merge,
  mergeAll,
  fromObject,
  fromString as fromConfigString,
  fromFile,
  fromEnvironment,
  fromDotenv,
  fromFileSecure,
  watchConfig,
  validate as validateConfig,
  QiConfig,
  Configuration,
} from "./core/config.js";

export type {
  ConfigData,
  ConfigValue,
  ConfigSource,
  ConfigMetadata,
  ConfigFormat,
  ConfigOptions,
} from "./core/config.js";

export {
  // Logger operations
  LogLevel,
  create as createLogger,
  createDefault as createDefaultLogger,
  createSilent as createSilentLogger,
  createTest as createTestLogger,
  parseLogLevel,
  logLevelToString,
  validateConfig as validateLoggerConfig,
  Logger,
  StructuredLogger,
} from "./core/logger.js";

export type {
  Logger as LoggerInterface,
  LoggerConfig,
  LogOutput,
  LogOutputFunction,
  LogFormatter,
  StructuredContext,
} from "./core/logger.js";

export {
  // Cache operations
  createCache,
  createMemoryCache,
  createRedisCache,
  QiCache,
  Cache,
} from "./core/cache.js";

export type {
  CacheInterface,
  CacheConfig,
  CacheStats,
} from "./core/cache.js";

export {
  // Performance monitoring
  measure,
  measureAsync,
  getStats as getPerformanceStats,
  getOperations as getPerformanceOperations,
  generateReport as generatePerformanceReport,
  benchmark,
  createMonitor,
  getGlobalMonitor,
  QiPerformance,
  PERFORMANCE_REQUIREMENTS,
} from "./core/performance.js";

export type {
  PerformanceMeasurement,
  PerformanceStats,
} from "./core/performance.js";
