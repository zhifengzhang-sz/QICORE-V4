/**
 * QiCore v4.0 Core Component - Complete Infrastructure Services
 *
 * Provides essential services for configuration, logging, caching, and performance monitoring
 * All services use QiCore Base for consistent error handling
 */

// Configuration system (pure functions with file watching)
export * from "./config.js";

// Logger system (effect interface with Winston integration)
export * from "./logger.js";

// Cache system (memory and Redis support)
export * from "./cache.js";

// Performance monitoring and benchmarking
export * from "./performance.js";

import * as Cache from "./cache.js";
// Export organized API for convenient access
import * as Configuration from "./config.js";
import * as Logger from "./logger.js";
import * as Performance from "./performance.js";

export const QiCore = {
  Configuration,
  Logger,
  Cache,
  Performance,
} as const;
