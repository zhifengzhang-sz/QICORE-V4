/**
 * QiCore v4.0 - Core Logger Component
 * 
 * Mathematical insight: Logging is simple effect interface
 * Winston-based structured logging with optimized level checking
 */

import winston from "winston";
import { createQiError, ErrorCategory } from "../base/error.js";
import { success, failure, type Result } from "../base/result.js";

/**
 * Log levels enum matching Winston levels
 */
export enum LogLevel {
  DEBUG = "debug",
  INFO = "info",
  WARN = "warn",
  ERROR = "error",
  FATAL = "fatal"
}

/**
 * Logger interface - simple effect abstraction
 */
export interface Logger {
  readonly log: (level: LogLevel, message: string, context?: Record<string, unknown>) => void;
  readonly isLevelEnabled: (level: LogLevel) => boolean;
  readonly debug: (message: string, context?: Record<string, unknown>) => void;
  readonly info: (message: string, context?: Record<string, unknown>) => void;
  readonly warn: (message: string, context?: Record<string, unknown>) => void;
  readonly error: (message: string, context?: Record<string, unknown>) => void;
  readonly fatal: (message: string, context?: Record<string, unknown>) => void;
  readonly setLevel: (level: LogLevel) => void;
  readonly getLevel: () => LogLevel;
}

/**
 * Logger configuration options
 */
export interface LoggerConfig {
  readonly level?: LogLevel;
  readonly format?: winston.Logform.Format;
  readonly transports?: winston.transport[];
  readonly silent?: boolean;
  readonly exitOnError?: boolean;
}

/**
 * Default logger configuration
 */
const defaultConfig: LoggerConfig = {
  level: LogLevel.INFO,
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.errors({ stack: true }),
    winston.format.json()
  ),
  transports: [new winston.transports.Console()],
  silent: false,
  exitOnError: false
};

/**
 * Creates a logger with default configuration
 */
export const createDefault = (): Result<Logger> => {
  return createWithConfig(defaultConfig);
};

/**
 * Creates a logger with custom configuration
 */
export const createWithConfig = (config: LoggerConfig): Result<Logger> => {
  try {
    const winstonLogger = winston.createLogger({
      level: config.level || defaultConfig.level,
      format: config.format || defaultConfig.format,
      transports: config.transports || defaultConfig.transports,
      silent: config.silent || defaultConfig.silent,
      exitOnError: config.exitOnError || defaultConfig.exitOnError
    });

    // Pre-compute level priorities for fast level checking
    const levelPriorities = {
      [LogLevel.DEBUG]: 0,
      [LogLevel.INFO]: 1,
      [LogLevel.WARN]: 2,
      [LogLevel.ERROR]: 3,
      [LogLevel.FATAL]: 4
    };

    let currentLevel = config.level || LogLevel.INFO;
    let currentLevelPriority = levelPriorities[currentLevel];

    const logger: Logger = {
      log: (level: LogLevel, message: string, context?: Record<string, unknown>) => {
        if (levelPriorities[level] >= currentLevelPriority) {
          winstonLogger.log(level, message, context);
        }
      },

      // Optimized level checking - critical for performance (< 1μs requirement)
      isLevelEnabled: (level: LogLevel): boolean => {
        return levelPriorities[level] >= currentLevelPriority;
      },

      // Convenience methods with level checks
      debug: (message: string, context?: Record<string, unknown>) => {
        if (levelPriorities[LogLevel.DEBUG] >= currentLevelPriority) {
          winstonLogger.debug(message, context);
        }
      },

      info: (message: string, context?: Record<string, unknown>) => {
        if (levelPriorities[LogLevel.INFO] >= currentLevelPriority) {
          winstonLogger.info(message, context);
        }
      },

      warn: (message: string, context?: Record<string, unknown>) => {
        if (levelPriorities[LogLevel.WARN] >= currentLevelPriority) {
          winstonLogger.warn(message, context);
        }
      },

      error: (message: string, context?: Record<string, unknown>) => {
        if (levelPriorities[LogLevel.ERROR] >= currentLevelPriority) {
          winstonLogger.error(message, context);
        }
      },

      fatal: (message: string, context?: Record<string, unknown>) => {
        if (levelPriorities[LogLevel.FATAL] >= currentLevelPriority) {
          winstonLogger.error(message, { ...context, level: 'fatal' });
        }
      },

      setLevel: (level: LogLevel) => {
        currentLevel = level;
        currentLevelPriority = levelPriorities[level];
        winstonLogger.level = level;
      },

      getLevel: () => currentLevel
    };

    return success(logger);
  } catch (error) {
    return failure(createQiError(
      "LOGGER_INIT_ERROR",
      `Failed to initialize logger: ${error}`,
      ErrorCategory.SYSTEM,
      { config }
    ));
  }
};

/**
 * Creates a file-based logger
 */
export const createFileLogger = (
  filename: string,
  config?: Partial<LoggerConfig>
): Result<Logger> => {
  try {
    const fileTransport = new winston.transports.File({ filename });
    
    const loggerConfig: LoggerConfig = {
      ...defaultConfig,
      ...config,
      transports: [fileTransport, ...(config?.transports || [])]
    };

    return createWithConfig(loggerConfig);
  } catch (error) {
    return failure(createQiError(
      "FILE_LOGGER_INIT_ERROR",
      `Failed to create file logger: ${error}`,
      ErrorCategory.FILESYSTEM,
      { filename, config }
    ));
  }
};

/**
 * Creates a console logger with colored output
 */
export const createConsoleLogger = (
  config?: Partial<LoggerConfig>
): Result<Logger> => {
  try {
    const consoleTransport = new winston.transports.Console({
      format: winston.format.combine(
        winston.format.colorize(),
        winston.format.timestamp(),
        winston.format.printf(({ timestamp, level, message, ...meta }) => {
          const metaStr = Object.keys(meta).length ? JSON.stringify(meta) : '';
          return `${timestamp} [${level}]: ${message} ${metaStr}`;
        })
      )
    });

    const loggerConfig: LoggerConfig = {
      ...defaultConfig,
      ...config,
      transports: [consoleTransport]
    };

    return createWithConfig(loggerConfig);
  } catch (error) {
    return failure(createQiError(
      "CONSOLE_LOGGER_INIT_ERROR",
      `Failed to create console logger: ${error}`,
      ErrorCategory.SYSTEM,
      { config }
    ));
  }
};

/**
 * Creates a silent logger (for testing)
 */
export const createSilentLogger = (): Result<Logger> => {
  return createWithConfig({
    ...defaultConfig,
    silent: true,
    transports: []
  });
};

/**
 * Creates a JSON structured logger
 */
export const createStructuredLogger = (
  config?: Partial<LoggerConfig>
): Result<Logger> => {
  try {
    const structuredFormat = winston.format.combine(
      winston.format.timestamp(),
      winston.format.errors({ stack: true }),
      winston.format.json(),
      winston.format.printf((info) => {
        return JSON.stringify({
          timestamp: info.timestamp,
          level: info.level,
          message: info.message,
          ...info
        });
      })
    );

    const loggerConfig: LoggerConfig = {
      ...defaultConfig,
      ...config,
      format: structuredFormat
    };

    return createWithConfig(loggerConfig);
  } catch (error) {
    return failure(createQiError(
      "STRUCTURED_LOGGER_INIT_ERROR",
      `Failed to create structured logger: ${error}`,
      ErrorCategory.SYSTEM,
      { config }
    ));
  }
};

/**
 * Logger utilities
 */

/**
 * Gets numeric priority for log level
 */
export const getLevelPriority = (level: LogLevel): number => {
  const priorities = {
    [LogLevel.DEBUG]: 0,
    [LogLevel.INFO]: 1,
    [LogLevel.WARN]: 2,
    [LogLevel.ERROR]: 3,
    [LogLevel.FATAL]: 4
  };
  return priorities[level];
};

/**
 * Compares log levels
 */
export const isLevelAtLeast = (level: LogLevel) => (targetLevel: LogLevel): boolean => {
  return getLevelPriority(level) >= getLevelPriority(targetLevel);
};

/**
 * Creates logger with performance monitoring
 */
export const createMonitoredLogger = (
  baseLogger: Logger,
  onLog?: (level: LogLevel, message: string, duration: number) => void
): Logger => {
  return {
    ...baseLogger,
    log: (level: LogLevel, message: string, context?: Record<string, unknown>) => {
      const start = performance.now();
      baseLogger.log(level, message, context);
      const duration = (performance.now() - start) * 1000; // microseconds
      onLog?.(level, message, duration);
    }
  };
};

/**
 * Creates logger with context enrichment
 */
export const createContextualLogger = (
  baseLogger: Logger,
  globalContext: Record<string, unknown>
): Logger => {
  return {
    ...baseLogger,
    log: (level: LogLevel, message: string, context?: Record<string, unknown>) => {
      const enrichedContext = { ...globalContext, ...context };
      baseLogger.log(level, message, enrichedContext);
    },
    debug: (message: string, context?: Record<string, unknown>) => {
      const enrichedContext = { ...globalContext, ...context };
      baseLogger.debug(message, enrichedContext);
    },
    info: (message: string, context?: Record<string, unknown>) => {
      const enrichedContext = { ...globalContext, ...context };
      baseLogger.info(message, enrichedContext);
    },
    warn: (message: string, context?: Record<string, unknown>) => {
      const enrichedContext = { ...globalContext, ...context };
      baseLogger.warn(message, enrichedContext);
    },
    error: (message: string, context?: Record<string, unknown>) => {
      const enrichedContext = { ...globalContext, ...context };
      baseLogger.error(message, enrichedContext);
    },
    fatal: (message: string, context?: Record<string, unknown>) => {
      const enrichedContext = { ...globalContext, ...context };
      baseLogger.fatal(message, enrichedContext);
    }
  };
};

/**
 * Creates logger with rate limiting
 */
export const createRateLimitedLogger = (
  baseLogger: Logger,
  maxLogsPerSecond = 100
): Logger => {
  const logCounts = new Map<string, { count: number; resetTime: number }>();
  
  const checkRateLimit = (key: string): boolean => {
    const now = Date.now();
    const entry = logCounts.get(key);
    
    if (!entry || now > entry.resetTime) {
      logCounts.set(key, { count: 1, resetTime: now + 1000 });
      return true;
    }
    
    if (entry.count < maxLogsPerSecond) {
      entry.count++;
      return true;
    }
    
    return false;
  };

  return {
    ...baseLogger,
    log: (level: LogLevel, message: string, context?: Record<string, unknown>) => {
      const key = `${level}:${message}`;
      if (checkRateLimit(key)) {
        baseLogger.log(level, message, context);
      }
    },
    debug: (message: string, context?: Record<string, unknown>) => {
      if (checkRateLimit(`debug:${message}`)) {
        baseLogger.debug(message, context);
      }
    },
    info: (message: string, context?: Record<string, unknown>) => {
      if (checkRateLimit(`info:${message}`)) {
        baseLogger.info(message, context);
      }
    },
    warn: (message: string, context?: Record<string, unknown>) => {
      if (checkRateLimit(`warn:${message}`)) {
        baseLogger.warn(message, context);
      }
    },
    error: (message: string, context?: Record<string, unknown>) => {
      if (checkRateLimit(`error:${message}`)) {
        baseLogger.error(message, context);
      }
    },
    fatal: (message: string, context?: Record<string, unknown>) => {
      if (checkRateLimit(`fatal:${message}`)) {
        baseLogger.fatal(message, context);
      }
    }
  };
};

/**
 * Validates log level string
 */
export const validateLogLevel = (level: string): Result<LogLevel> => {
  const validLevels = Object.values(LogLevel);
  if (validLevels.includes(level as LogLevel)) {
    return success(level as LogLevel);
  }
  
  return failure(createQiError(
    "INVALID_LOG_LEVEL",
    `Invalid log level: ${level}. Valid levels: ${validLevels.join(', ')}`,
    ErrorCategory.VALIDATION,
    { level, validLevels }
  ));
};

/**
 * Creates logger from configuration object
 */
export const fromConfig = (config: Record<string, unknown>): Result<Logger> => {
  try {
    const level = config.level ? validateLogLevel(config.level as string) : success(LogLevel.INFO);
    if (level._tag === "Left") return level;

    const loggerConfig: LoggerConfig = {
      level: level.right,
      silent: Boolean(config.silent),
      exitOnError: Boolean(config.exitOnError)
    };

    return createWithConfig(loggerConfig);
  } catch (error) {
    return failure(createQiError(
      "LOGGER_CONFIG_ERROR",
      `Failed to create logger from config: ${error}`,
      ErrorCategory.CONFIGURATION,
      { config }
    ));
  }
};