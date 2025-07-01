/**
 * QiCore v4.0 - Structured Logging with Winston
 * 
 * Mathematical Foundation: Simple effect interface (NOT free monad)
 * Implementation: Winston-based structured logging with level optimization
 * Performance Target: ~0.1-0.5μs for level checks, ~5-10μs per log message
 */

import winston from "winston";
import type { Result } from "../base/result.js";
import { success, failure } from "../base/result.js";
import { SystemError } from "../base/error.js";

/**
 * Log levels in order of severity (lowest to highest)
 */
export type LogLevel = "debug" | "info" | "warn" | "error";

/**
 * Log context for structured logging
 */
export interface LogContext {
  readonly [key: string]: unknown;
}

/**
 * Logger interface with simple effect semantics
 * Performance-optimized with level checking
 */
export interface Logger {
  readonly log: (level: LogLevel, message: string, context?: LogContext) => void;
  readonly debug: (message: string, context?: LogContext) => void;
  readonly info: (message: string, context?: LogContext) => void;
  readonly warn: (message: string, context?: LogContext) => void;
  readonly error: (message: string, context?: LogContext) => void;
  readonly isLevelEnabled: (level: LogLevel) => boolean;
}

/**
 * Logger configuration options
 */
export interface LoggerConfig {
  readonly level: LogLevel;
  readonly format?: "json" | "simple";
  readonly enableConsole?: boolean;
  readonly enableFile?: boolean;
  readonly filePath?: string;
  readonly maxFiles?: number;
  readonly maxSize?: string;
}

/**
 * Default logger configuration
 */
const defaultConfig: LoggerConfig = {
  level: "info",
  format: "json",
  enableConsole: true,
  enableFile: false
};

/**
 * Create logger with default configuration
 * Performance: Level checking optimized for <1μs
 */
export const createDefault = (): Result<Logger> => {
  try {
    const winstonLogger = winston.createLogger({
      level: defaultConfig.level,
      format: winston.format.combine(
        winston.format.timestamp(),
        winston.format.errors({ stack: true }),
        winston.format.json()
      ),
      transports: [
        new winston.transports.Console({
          format: winston.format.combine(
            winston.format.colorize(),
            winston.format.simple()
          )
        })
      ],
      silent: false
    });

    return success(createLoggerInterface(winstonLogger));
  } catch (error) {
    return failure(SystemError(
      "LOGGER_INIT_ERROR",
      `Failed to create default logger: ${error}`,
      new Map([["error", error]])
    ));
  }
};

/**
 * Create logger with custom configuration
 */
export const createWithConfig = (config: Partial<LoggerConfig>): Result<Logger> => {
  try {
    const fullConfig = { ...defaultConfig, ...config };
    const transports: winston.transport[] = [];

    // Console transport
    if (fullConfig.enableConsole) {
      transports.push(new winston.transports.Console({
        format: fullConfig.format === "simple" 
          ? winston.format.combine(
              winston.format.colorize(),
              winston.format.simple()
            )
          : winston.format.combine(
              winston.format.timestamp(),
              winston.format.json()
            )
      }));
    }

    // File transport
    if (fullConfig.enableFile && fullConfig.filePath) {
      transports.push(new winston.transports.File({
        filename: fullConfig.filePath,
        maxFiles: fullConfig.maxFiles ?? 5,
        maxsize: fullConfig.maxSize ? parseFileSize(fullConfig.maxSize) : 10485760, // 10MB
        format: winston.format.combine(
          winston.format.timestamp(),
          winston.format.json()
        )
      }));
    }

    const winstonLogger = winston.createLogger({
      level: fullConfig.level,
      format: winston.format.combine(
        winston.format.timestamp(),
        winston.format.errors({ stack: true }),
        winston.format.json()
      ),
      transports,
      silent: false
    });

    return success(createLoggerInterface(winstonLogger));
  } catch (error) {
    return failure(SystemError(
      "LOGGER_CONFIG_ERROR",
      `Failed to create logger with config: ${error}`,
      new Map([["config", config], ["error", error]])
    ));
  }
};

/**
 * Create the logger interface from Winston logger
 * Optimized for performance with level checking
 */
const createLoggerInterface = (winstonLogger: winston.Logger): Logger => ({
  log: (level: LogLevel, message: string, context?: LogContext) => {
    // Fast level check - performance critical path
    if (winstonLogger.isLevelEnabled(level)) {
      winstonLogger.log(level, message, context);
    }
  },

  debug: (message: string, context?: LogContext) => {
    if (winstonLogger.isLevelEnabled("debug")) {
      winstonLogger.debug(message, context);
    }
  },

  info: (message: string, context?: LogContext) => {
    if (winstonLogger.isLevelEnabled("info")) {
      winstonLogger.info(message, context);
    }
  },

  warn: (message: string, context?: LogContext) => {
    if (winstonLogger.isLevelEnabled("warn")) {
      winstonLogger.warn(message, context);
    }
  },

  error: (message: string, context?: LogContext) => {
    if (winstonLogger.isLevelEnabled("error")) {
      winstonLogger.error(message, context);
    }
  },

  isLevelEnabled: (level: LogLevel): boolean => 
    winstonLogger.isLevelEnabled(level)
});

/**
 * Parse file size string to bytes
 */
const parseFileSize = (sizeStr: string): number => {
  const units: Record<string, number> = {
    B: 1,
    KB: 1024,
    MB: 1024 * 1024,
    GB: 1024 * 1024 * 1024
  };

  const match = sizeStr.match(/^(\d+)\s*(B|KB|MB|GB)$/i);
  if (!match) {
    throw new Error(`Invalid file size format: ${sizeStr}`);
  }

  const [, size, unit] = match;
  return parseInt(size, 10) * units[unit.toUpperCase()];
};

/**
 * Create logger for specific component/module
 * Adds component context to all log messages
 */
export const createForComponent = (
  componentName: string,
  config?: Partial<LoggerConfig>
): Result<Logger> => {
  const loggerResult = config ? createWithConfig(config) : createDefault();
  
  if (loggerResult._tag === "Left") {
    return loggerResult;
  }

  const baseLogger = loggerResult.right;
  
  // Wrap logger to add component context
  const componentLogger: Logger = {
    log: (level: LogLevel, message: string, context?: LogContext) => {
      baseLogger.log(level, message, { 
        component: componentName, 
        ...context 
      });
    },

    debug: (message: string, context?: LogContext) => {
      baseLogger.debug(message, { 
        component: componentName, 
        ...context 
      });
    },

    info: (message: string, context?: LogContext) => {
      baseLogger.info(message, { 
        component: componentName, 
        ...context 
      });
    },

    warn: (message: string, context?: LogContext) => {
      baseLogger.warn(message, { 
        component: componentName, 
        ...context 
      });
    },

    error: (message: string, context?: LogContext) => {
      baseLogger.error(message, { 
        component: componentName, 
        ...context 
      });
    },

    isLevelEnabled: baseLogger.isLevelEnabled
  };

  return success(componentLogger);
};

/**
 * Utility to log function execution with timing
 */
export const loggedExecution = <T>(
  logger: Logger,
  operationName: string,
  operation: () => T
): T => {
  const start = performance.now();
  logger.debug(`Starting ${operationName}`);
  
  try {
    const result = operation();
    const duration = (performance.now() - start) * 1000; // microseconds
    
    logger.info(`Completed ${operationName}`, { 
      duration_us: Math.round(duration),
      status: "success"
    });
    
    return result;
  } catch (error) {
    const duration = (performance.now() - start) * 1000;
    
    logger.error(`Failed ${operationName}`, {
      duration_us: Math.round(duration),
      status: "error",
      error: error instanceof Error ? error.message : String(error)
    });
    
    throw error;
  }
};

/**
 * Utility to log async function execution with timing
 */
export const loggedAsyncExecution = async <T>(
  logger: Logger,
  operationName: string,
  operation: () => Promise<T>
): Promise<T> => {
  const start = performance.now();
  logger.debug(`Starting async ${operationName}`);
  
  try {
    const result = await operation();
    const duration = (performance.now() - start) * 1000; // microseconds
    
    logger.info(`Completed async ${operationName}`, { 
      duration_us: Math.round(duration),
      status: "success"
    });
    
    return result;
  } catch (error) {
    const duration = (performance.now() - start) * 1000;
    
    logger.error(`Failed async ${operationName}`, {
      duration_us: Math.round(duration),
      status: "error",
      error: error instanceof Error ? error.message : String(error)
    });
    
    throw error;
  }
};

/**
 * Create a null logger (for testing or disabled logging)
 */
export const createNull = (): Logger => ({
  log: () => {},
  debug: () => {},
  info: () => {},
  warn: () => {},
  error: () => {},
  isLevelEnabled: () => false
}); 