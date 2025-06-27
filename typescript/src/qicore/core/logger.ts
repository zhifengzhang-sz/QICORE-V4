/**
 * QiCore v4.0 - Logger Component
 * 
 * Mathematical Contract-Based TypeScript Library
 * Component 4: StructuredLogger - High-performance logging with context (7 operations)
 */

import pino, { type Logger as PinoLogger, type LoggerOptions } from "pino";
import { Result } from "../base/result.js";
import { QiError } from "../base/error.js";

/**
 * Log level enumeration
 */
export enum LogLevel {
  TRACE = "trace",
  DEBUG = "debug",
  INFO = "info",
  WARN = "warn",
  ERROR = "error",
  FATAL = "fatal",
}

/**
 * Log entry interface
 */
export interface LogEntry {
  level: LogLevel;
  message: string;
  context?: Record<string, unknown>;
  timestamp: number;
  component?: string;
  error?: QiError;
}

/**
 * Logger configuration
 */
export interface LoggerConfig {
  level: LogLevel;
  component: string;
  prettyPrint?: boolean;
  destination?: string;
  hooks?: {
    logMethod?: (args: unknown[], method: string) => void;
  };
}

/**
 * StructuredLogger provides high-performance structured logging with context preservation
 * and mathematical effects tracking
 */
export class StructuredLogger {
  private logger: PinoLogger;
  private component: string;
  private context: Record<string, unknown> = {};

  constructor(config: LoggerConfig) {
    this.component = config.component;
    
    const pinoConfig: LoggerOptions = {
      level: config.level,
      timestamp: () => `,"timestamp":${Date.now()}`,
      base: {
        component: this.component,
      },
    };

    if (config.hooks) {
      pinoConfig.hooks = config.hooks as any;
    }

    if (config.prettyPrint) {
      pinoConfig.transport = {
        target: "pino-pretty",
        options: {
          colorize: true,
          translateTime: true,
          ignore: "pid,hostname",
        },
      };
    }

    this.logger = pino(pinoConfig);
  }

  /**
   * Operation 1: Log trace message
   */
  trace(message: string, context?: Record<string, unknown>): Result<void> {
    return this.log(LogLevel.TRACE, message, context);
  }

  /**
   * Operation 2: Log debug message
   */
  debug(message: string, context?: Record<string, unknown>): Result<void> {
    return this.log(LogLevel.DEBUG, message, context);
  }

  /**
   * Operation 3: Log info message
   */
  info(message: string, context?: Record<string, unknown>): Result<void> {
    return this.log(LogLevel.INFO, message, context);
  }

  /**
   * Operation 4: Log warning message
   */
  warn(message: string, context?: Record<string, unknown>): Result<void> {
    return this.log(LogLevel.WARN, message, context);
  }

  /**
   * Operation 5: Log error message
   */
  error(message: string, error?: QiError, context?: Record<string, unknown>): Result<void> {
    const fullContext = {
      ...this.context,
      ...context,
      ...(error && {
        error: {
          category: error.category,
          message: error.message,
          context: error.context,
          timestamp: error.timestamp,
        },
      }),
    };

    return this.log(LogLevel.ERROR, message, fullContext);
  }

  /**
   * Operation 6: Log fatal message
   */
  fatal(message: string, error?: QiError, context?: Record<string, unknown>): Result<void> {
    const fullContext = {
      ...this.context,
      ...context,
      ...(error && {
        error: {
          category: error.category,
          message: error.message,
          context: error.context,
          timestamp: error.timestamp,
        },
      }),
    };

    return this.log(LogLevel.FATAL, message, fullContext);
  }

  /**
   * Operation 7: Add persistent context
   */
  withContext(context: Record<string, unknown>): StructuredLogger {
    const newLogger = new StructuredLogger({
      level: this.getLevel(),
      component: this.component,
    });
    newLogger.logger = this.logger.child(context);
    newLogger.context = { ...this.context, ...context };
    return newLogger;
  }

  /**
   * Core logging implementation
   */
  private log(level: LogLevel, message: string, context?: Record<string, unknown>): Result<void> {
    try {
      const logContext = {
        ...this.context,
        ...context,
        timestamp: Date.now(),
        component: this.component,
      };

      this.logger[level](logContext, message);
      return Result.success(undefined);
    } catch (error) {
      return Result.failure(
        QiError.integrationError(
          `Logging failed: ${error}`,
          "logger",
          "log",
        ),
      );
    }
  }

  /**
   * Get current log level
   */
  getLevel(): LogLevel {
    return this.logger.level as LogLevel;
  }

  /**
   * Set log level
   */
  setLevel(level: LogLevel): Result<void> {
    try {
      this.logger.level = level;
      return Result.success(undefined);
    } catch (error) {
      return Result.failure(
        QiError.configurationError(
          `Setting log level failed: ${error}`,
          "level",
          "LogLevel",
        ),
      );
    }
  }

  /**
   * Check if level is enabled
   */
  isLevelEnabled(level: LogLevel): boolean {
    return this.logger.isLevelEnabled(level);
  }

  /**
   * Get component name
   */
  getComponent(): string {
    return this.component;
  }

  /**
   * Get current context
   */
  getContext(): Record<string, unknown> {
    return { ...this.context };
  }

  /**
   * Flush logs (useful for testing)
   */
  async flush(): Promise<Result<void>> {
    try {
      await new Promise<void>((resolve) => {
        this.logger.flush((error) => {
          if (error) {
            console.error("Log flush error:", error);
          }
          resolve();
        });
      });
      return Result.success(undefined);
    } catch (error) {
      return Result.failure(
        QiError.integrationError(
          `Log flush failed: ${error}`,
          "logger",
          "flush",
        ),
      );
    }
  }
}

/**
 * Global logger management
 */
export class LoggerManager {
  private static loggers = new Map<string, StructuredLogger>();
  private static defaultConfig: Partial<LoggerConfig> = {
    level: LogLevel.INFO,
    prettyPrint: true,
  };

  /**
   * Get or create logger for component
   */
  static getLogger(component: string, config?: Partial<LoggerConfig>): StructuredLogger {
    if (!this.loggers.has(component)) {
      const fullConfig: LoggerConfig = {
        level: LogLevel.INFO,
        prettyPrint: true,
        ...this.defaultConfig,
        ...config,
        component,
      };
      this.loggers.set(component, new StructuredLogger(fullConfig));
    }
    return this.loggers.get(component)!;
  }

  /**
   * Configure default logger settings
   */
  static configure(config: Partial<LoggerConfig>): void {
    this.defaultConfig = { ...this.defaultConfig, ...config };
  }

  /**
   * Remove logger for component
   */
  static removeLogger(component: string): boolean {
    return this.loggers.delete(component);
  }

  /**
   * Clear all loggers
   */
  static clear(): void {
    this.loggers.clear();
  }

  /**
   * Get all logger components
   */
  static getComponents(): string[] {
    return Array.from(this.loggers.keys());
  }
}

/**
 * Performance monitoring decorator
 */
export function LogPerformance(component: string) {
  return function <T extends (...args: any[]) => any>(
    target: any,
    propertyKey: string,
    descriptor: TypedPropertyDescriptor<T>,
  ) {
    const originalMethod = descriptor.value;
    if (!originalMethod) return;

    descriptor.value = function (this: any, ...args: any[]) {
      const logger = LoggerManager.getLogger(component);
      const startTime = Date.now();
      
      logger.debug(`Starting ${propertyKey}`, {
        method: propertyKey,
        args: args.length,
      });

      try {
        const result = originalMethod.apply(this, args);
        
        // Handle both sync and async results
        if (result && typeof result.then === "function") {
          return result
            .then((value: any) => {
              const duration = Date.now() - startTime;
              logger.info(`Completed ${propertyKey}`, {
                method: propertyKey,
                duration,
                success: true,
              });
              return value;
            })
            .catch((error: any) => {
              const duration = Date.now() - startTime;
              logger.error(`Failed ${propertyKey}`, error, {
                method: propertyKey,
                duration,
                success: false,
              });
              throw error;
            });
        } else {
          const duration = Date.now() - startTime;
          logger.info(`Completed ${propertyKey}`, {
            method: propertyKey,
            duration,
            success: true,
          });
          return result;
        }
      } catch (error) {
        const duration = Date.now() - startTime;
        logger.error(`Failed ${propertyKey}`, error as QiError, {
          method: propertyKey,
          duration,
          success: false,
        });
        throw error;
      }
    } as T;
  };
}

/**
 * Utility functions for logging
 */
/**
 * Configure global logging
 */
export function configureLogging(config: {
    level?: LogLevel;
    prettyPrint?: boolean;
    format?: "json" | "pretty";
  }): Result<void> {
    try {
      LoggerManager.configure({
        level: config.level || LogLevel.INFO,
        prettyPrint: config.prettyPrint ?? (config.format === "pretty"),
      });
      return Result.success(undefined);
    } catch (error) {
      return Result.failure(
        QiError.configurationError(
          `Logger configuration failed: ${error}`,
          "logging",
          "LoggerConfig",
        ),
      );
    }
  }

/**
 * Create logger with context
 */
export function createLogger(
    component: string,
    context?: Record<string, unknown>,
  ): StructuredLogger {
    const logger = LoggerManager.getLogger(component);
    return context ? logger.withContext(context) : logger;
  }

/**
 * Log operation with timing
 */
export async function logOperation<T>(
    logger: StructuredLogger,
    operation: string,
    fn: () => Promise<T>,
  ): Promise<T> {
    const startTime = Date.now();
    
    logger.debug(`Starting operation: ${operation}`);
    
    try {
      const result = await fn();
      const duration = Date.now() - startTime;
      
      logger.info(`Operation completed: ${operation}`, {
        operation,
        duration,
        success: true,
      });
      
      return result;
    } catch (error) {
      const duration = Date.now() - startTime;
      
      logger.error(`Operation failed: ${operation}`, error as QiError, {
        operation,
        duration,
        success: false,
      });
      
      throw error;
    }
  }