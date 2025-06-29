import winston from "winston";
import { z } from "zod";
import { createQiError } from "../base/error";
import { type Result, failure, success } from "../base/result";

// ============================================================================
// LOG LEVEL SYSTEM (Winston Integration)
// ============================================================================

/**
 * Log Levels mapped to Winston levels
 * Mathematical Structure: TRACE < DEBUG < INFO < WARN < ERROR < FATAL
 */
export const LogLevel = {
  TRACE: "verbose" as const,
  DEBUG: "debug" as const,
  INFO: "info" as const,
  WARN: "warn" as const,
  ERROR: "error" as const,
  FATAL: "error" as const, // Winston doesn't have fatal, maps to error
} as const;

// eslint-disable-next-line no-redeclare
export type LogLevel = (typeof LogLevel)[keyof typeof LogLevel];

/**
 * Zod schema for log level validation
 */
const LogLevelSchema = z.enum(["verbose", "debug", "info", "warn", "error"]);

/**
 * Parse log level with Zod validation
 * @pure
 */
export const parseLogLevel = (level: string): Result<LogLevel> => {
  const normalized = level.toLowerCase();
  const result = LogLevelSchema.safeParse(normalized);

  if (result.success) {
    return success(result.data);
  }

  return failure(
    createQiError("INVALID_LOG_LEVEL", `Invalid log level: ${level}`, "VALIDATION", {
      level,
      validLevels: LogLevelSchema.options,
      zodError: result.error.message,
    })
  );
};

/**
 * Convert LogLevel to string for display
 * @pure
 */
export const logLevelToString = (level: LogLevel): string => {
  switch (level) {
    case LogLevel.TRACE:
      return "TRACE";
    case LogLevel.DEBUG:
      return "DEBUG";
    case LogLevel.INFO:
      return "INFO";
    case LogLevel.WARN:
      return "WARN";
    case LogLevel.ERROR:
      return "ERROR";
    case LogLevel.FATAL:
      return "FATAL";
    default:
      return "UNKNOWN";
  }
};

// ============================================================================
// LOGGER CONFIGURATION
// ============================================================================

export interface LoggerConfig {
  readonly level: LogLevel;
  readonly enableColors: boolean;
  readonly enableTimestamp: boolean;
  readonly timestampFormat: "iso" | "relative" | "none";
  readonly output: LogOutput;
  readonly prefix?: string;
  readonly formatter?: LogFormatter;
  readonly outputFile?: string; // For Winston file transport
  readonly enableConsole?: boolean; // Control console transport
  // Advanced features
  readonly logEndpoint?: string; // HTTP transport endpoint
  readonly logHost?: string; // HTTP transport host
  readonly logPort?: number; // HTTP transport port
  readonly logPath?: string; // HTTP transport path
  readonly enableStructuredContext?: boolean; // Structured context support
}

export type LogOutput = "console" | "silent" | "winston" | LogOutputFunction;
export type LogOutputFunction = (message: string) => void;
export type LogFormatter = (
  level: LogLevel,
  message: string,
  context?: Record<string, unknown>,
  timestamp?: Date
) => string;

/**
 * Structured Context for Enhanced Logging
 */
export interface StructuredContext {
  readonly traceId?: string;
  readonly userId?: string;
  readonly requestId?: string;
  readonly operation?: string;
  readonly [key: string]: unknown;
}

export const createDefaultLoggerConfig = (): LoggerConfig => ({
  level: LogLevel.INFO,
  enableColors: true,
  enableTimestamp: true,
  timestampFormat: "iso",
  output: "winston",
  enableConsole: true,
});

export const createSilentLoggerConfig = (): LoggerConfig => ({
  level: LogLevel.FATAL,
  enableColors: false,
  enableTimestamp: false,
  timestampFormat: "none",
  output: "silent",
  enableConsole: false,
});

export const createTestLoggerConfig = (): LoggerConfig => ({
  level: LogLevel.TRACE,
  enableColors: false,
  enableTimestamp: false,
  timestampFormat: "none",
  output: "silent",
  enableConsole: false,
});

// ============================================================================
// WINSTON LOGGER CREATION
// ============================================================================

/**
 * Create Winston logger instance with proper configuration
 */
const createWinstonLogger = (config: LoggerConfig): winston.Logger => {
  const transports: winston.transport[] = [];

  // Console transport
  if (config.enableConsole !== false && config.output !== "silent") {
    transports.push(
      new winston.transports.Console({
        level: config.level,
        format: winston.format.combine(
          winston.format.timestamp(),
          winston.format.errors({ stack: true }),
          winston.format.colorize({ all: config.enableColors }),
          winston.format.printf(({ timestamp, level, message, ...meta }) => {
            const prefix =
              config.prefix !== null && config.prefix !== undefined && config.prefix !== ""
                ? `[${config.prefix}] `
                : "";
            const metaString = Object.keys(meta).length > 0 ? ` ${JSON.stringify(meta)}` : "";
            return `${timestamp} ${level}: ${prefix}${message}${metaString}`;
          })
        ),
      })
    );
  }

  // File transport if specified
  if (config.outputFile !== null && config.outputFile !== undefined && config.outputFile !== "") {
    transports.push(
      new winston.transports.File({
        filename: config.outputFile,
        level: config.level,
        format: winston.format.combine(
          winston.format.timestamp(),
          winston.format.errors({ stack: true }),
          winston.format.json()
        ),
      })
    );
  }

  // HTTP transport for centralized logging
  if (config.logEndpoint || (config.logHost && config.logPort)) {
    const httpTransport = new winston.transports.Http({
      host: config.logHost || "localhost",
      port: config.logPort || 80,
      path: config.logPath || "/logs",
      level: config.level,
      format: winston.format.combine(
        winston.format.timestamp(),
        winston.format.errors({ stack: true }),
        winston.format.json()
      ),
    });
    transports.push(httpTransport);
  }

  return winston.createLogger({
    level: config.level,
    transports,
    exitOnError: false,
    silent: config.output === "silent",
  });
};

// ============================================================================
// LOGGER INTERFACE
// ============================================================================

export interface Logger {
  // Level-based logging methods
  readonly trace: (message: string, context?: Record<string, unknown>) => void;
  readonly debug: (message: string, context?: Record<string, unknown>) => void;
  readonly info: (message: string, context?: Record<string, unknown>) => void;
  readonly warn: (message: string, context?: Record<string, unknown>) => void;
  readonly error: (message: string, context?: Record<string, unknown>) => void;
  readonly fatal: (message: string, context?: Record<string, unknown>) => void;

  // Generic logging method
  readonly log: (level: LogLevel, message: string, context?: Record<string, unknown>) => void;

  // Level checking
  readonly isLevelEnabled: (level: LogLevel) => boolean;

  // Configuration access
  readonly getConfig: () => LoggerConfig;
  readonly getLevel: () => LogLevel;
}

// ============================================================================
// LOGGER IMPLEMENTATION
// ============================================================================

class LoggerImpl implements Logger {
  private readonly winstonLogger: winston.Logger;

  constructor(private readonly config: LoggerConfig) {
    this.winstonLogger = createWinstonLogger(config);
  }

  trace(message: string, context?: Record<string, unknown>): void {
    this.log(LogLevel.TRACE, message, context);
  }

  debug(message: string, context?: Record<string, unknown>): void {
    this.log(LogLevel.DEBUG, message, context);
  }

  info(message: string, context?: Record<string, unknown>): void {
    this.log(LogLevel.INFO, message, context);
  }

  warn(message: string, context?: Record<string, unknown>): void {
    this.log(LogLevel.WARN, message, context);
  }

  error(message: string, context?: Record<string, unknown>): void {
    this.log(LogLevel.ERROR, message, context);
  }

  fatal(message: string, context?: Record<string, unknown>): void {
    this.log(LogLevel.FATAL, message, context);
  }

  log(level: LogLevel, message: string, context?: Record<string, unknown>): void {
    if (!this.isLevelEnabled(level)) {
      return;
    }

    if (this.config.output === "winston") {
      // Use Winston for logging
      const logData = context ? { message, ...context } : message;
      this.winstonLogger.log(level, logData);
    } else {
      // Fallback to custom formatting for other outputs
      const timestamp = new Date();
      const formattedMessage = this.config.formatter
        ? this.config.formatter(level, message, context, timestamp)
        : this.formatMessage(level, message, context, timestamp);

      this.writeOutput(formattedMessage);
    }
  }

  isLevelEnabled(level: LogLevel): boolean {
    const levels = ["verbose", "debug", "info", "warn", "error"];
    const currentLevelIndex = levels.indexOf(this.config.level);
    const messageLevelIndex = levels.indexOf(level);
    return messageLevelIndex >= currentLevelIndex;
  }

  getConfig(): LoggerConfig {
    return this.config;
  }

  getLevel(): LogLevel {
    return this.config.level;
  }

  private formatMessage(
    level: LogLevel,
    message: string,
    context?: Record<string, unknown>,
    timestamp?: Date
  ): string {
    const parts: string[] = [];

    // Timestamp
    if (this.config.enableTimestamp && timestamp) {
      const timeStr = this.formatTimestamp(timestamp);
      parts.push(timeStr);
    }

    // Level
    const levelStr = this.formatLogLevel(level);
    parts.push(levelStr);

    // Prefix
    if (
      this.config.prefix !== null &&
      this.config.prefix !== undefined &&
      this.config.prefix !== ""
    ) {
      parts.push(`[${this.config.prefix}]`);
    }

    // Message
    parts.push(message);

    // Context
    if (context && Object.keys(context).length > 0) {
      const contextStr = Object.entries(context)
        .map(([key, value]) => `${key}=${this.formatValue(value)}`)
        .join(" ");
      parts.push(`{${contextStr}}`);
    }

    return parts.join(" ");
  }

  private formatTimestamp(timestamp: Date): string {
    switch (this.config.timestampFormat) {
      case "iso":
        return timestamp.toISOString();
      case "relative":
        return `+${Date.now() - timestamp.getTime()}ms`;
      case "none":
        return "";
      default:
        return "";
    }
  }

  private formatLogLevel(level: LogLevel): string {
    const levelStr = logLevelToString(level);

    if (!this.config.enableColors) {
      return `[${levelStr}]`;
    }

    // ANSI color codes mapped to Winston levels
    const colors: Record<LogLevel, string> = {
      verbose: "\x1b[90m", // gray (TRACE)
      debug: "\x1b[36m", // cyan (DEBUG)
      info: "\x1b[32m", // green (INFO)
      warn: "\x1b[33m", // yellow (WARN)
      error: "\x1b[31m", // red (ERROR/FATAL)
    };
    const reset = "\x1b[0m";

    const color = colors[level] || "";
    return `${color}[${levelStr}]${reset}`;
  }

  private formatValue(value: unknown): string {
    if (value === null) {
      return "null";
    }
    if (value === undefined) {
      return "undefined";
    }
    if (typeof value === "string") {
      return `"${value}"`;
    }
    if (typeof value === "number" || typeof value === "boolean") {
      return String(value);
    }
    try {
      return JSON.stringify(value);
    } catch {
      return String(value);
    }
  }

  private writeOutput(message: string): void {
    switch (this.config.output) {
      case "console":
        // Use console methods based on level for proper browser dev tools integration
        if (typeof console !== "undefined") {
          // eslint-disable-next-line no-console
          console.log(message);
        }
        break;
      case "silent":
        // Do nothing
        break;
      case "winston":
        // Winston handled in main log method
        break;
      default:
        if (typeof this.config.output === "function") {
          this.config.output(message);
        }
        break;
    }
  }
}

// ============================================================================
// FACTORY FUNCTIONS
// ============================================================================

/**
 * Create a logger instance with the given configuration
 * @pure - Factory function
 * Performance: < 100μs (TypeScript interpreted tier)
 */
export const create = (config: LoggerConfig): Result<Logger> => {
  try {
    const logger = new LoggerImpl(config);
    return success(logger);
  } catch (error) {
    return failure(
      createQiError("LOGGER_CREATION_FAILED", `Failed to create logger: ${error}`, "SYSTEM", {
        error: String(error),
      })
    );
  }
};

/**
 * Create logger with default configuration
 * @pure
 */
export const createDefault = (): Result<Logger> => create(createDefaultLoggerConfig());

/**
 * Create silent logger (for testing)
 * @pure
 */
export const createSilent = (): Result<Logger> => create(createSilentLoggerConfig());

/**
 * Create test logger with full tracing
 * @pure
 */
export const createTest = (): Result<Logger> => create(createTestLoggerConfig());

// ============================================================================
// VALIDATION HELPERS
// ============================================================================

/**
 * Zod schema for logger configuration validation
 */
const LoggerConfigSchema = z.object({
  level: LogLevelSchema,
  enableColors: z.boolean(),
  enableTimestamp: z.boolean(),
  timestampFormat: z.enum(["iso", "relative", "none"]),
  output: z.union([z.literal("console"), z.literal("silent"), z.literal("winston"), z.function()]),
  prefix: z.string().optional(),
  formatter: z.function().optional(),
  outputFile: z.string().optional(),
  enableConsole: z.boolean().optional(),
});

/**
 * Validate logger configuration using Zod
 * @pure
 */
export const validateConfig = (config: LoggerConfig): Result<LoggerConfig> => {
  const result = LoggerConfigSchema.safeParse(config);

  if (result.success) {
    return success(result.data as LoggerConfig);
  }

  return failure(
    createQiError("INVALID_LOGGER_CONFIG", "Logger configuration validation failed", "VALIDATION", {
      config,
      zodError: result.error.message,
      issues: result.error.issues,
    })
  );
};

// ============================================================================
// CONVENIENCE EXPORTS
// ============================================================================

// eslint-disable-next-line no-redeclare
export const Logger = {
  // Factory functions
  create,
  createDefault,
  createSilent,
  createTest,

  // Configuration helpers
  createDefaultLoggerConfig,
  createSilentLoggerConfig,
  createTestLoggerConfig,
  validateConfig,

  // Utilities
  parseLogLevel,
  logLevelToString,
} as const;
