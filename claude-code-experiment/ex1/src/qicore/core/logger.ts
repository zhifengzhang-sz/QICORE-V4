import winston from "winston";
import { ErrorCategory, type QiError, createQiError } from "../base/error.js";
import { type Result, failure, success } from "../base/result.js";

export enum LogLevel {
	DEBUG = "debug",
	INFO = "info",
	WARN = "warn",
	ERROR = "error",
	FATAL = "fatal",
}

export interface LogConfig {
	level: LogLevel;
	format?: "json" | "plain";
	destination?: "console" | "file";
	filePath?: string;
	maxFiles?: number;
	maxSize?: string;
}

export interface Logger {
	debug(message: string, context?: Record<string, unknown>): void;
	info(message: string, context?: Record<string, unknown>): void;
	warn(message: string, context?: Record<string, unknown>): void;
	error(
		message: string,
		error?: Error | QiError,
		context?: Record<string, unknown>,
	): void;
	fatal(
		message: string,
		error?: Error | QiError,
		context?: Record<string, unknown>,
	): void;
	isLevelEnabled(level: LogLevel): boolean;
}

class WinstonLogger implements Logger {
	private winston: winston.Logger;
	private currentLevel: LogLevel;

	constructor(winston: winston.Logger, level: LogLevel) {
		this.winston = winston;
		this.currentLevel = level;
	}

	debug(message: string, context?: Record<string, unknown>): void {
		if (this.isLevelEnabled(LogLevel.DEBUG)) {
			this.winston.debug(message, context);
		}
	}

	info(message: string, context?: Record<string, unknown>): void {
		if (this.isLevelEnabled(LogLevel.INFO)) {
			this.winston.info(message, context);
		}
	}

	warn(message: string, context?: Record<string, unknown>): void {
		if (this.isLevelEnabled(LogLevel.WARN)) {
			this.winston.warn(message, context);
		}
	}

	error(
		message: string,
		error?: Error | QiError,
		context?: Record<string, unknown>,
	): void {
		if (this.isLevelEnabled(LogLevel.ERROR)) {
			const logContext = { ...context };

			if (error) {
				if ("toStructuredData" in error) {
					// QiError
					logContext.error = error.toStructuredData();
				} else {
					// Standard Error
					logContext.error = {
						name: error.name,
						message: error.message,
						stack: error.stack,
					};
				}
			}

			this.winston.error(message, logContext);
		}
	}

	fatal(
		message: string,
		error?: Error | QiError,
		context?: Record<string, unknown>,
	): void {
		// Winston doesn't have fatal level, use error with fatal metadata
		if (this.isLevelEnabled(LogLevel.ERROR)) {
			const logContext = { ...context, level: "fatal" };

			if (error) {
				if ("toStructuredData" in error) {
					// QiError
					logContext.error = error.toStructuredData();
				} else {
					// Standard Error
					logContext.error = {
						name: error.name,
						message: error.message,
						stack: error.stack,
					};
				}
			}

			this.winston.error(message, logContext);
		}
	}

	isLevelEnabled(level: LogLevel): boolean {
		const levels = {
			[LogLevel.DEBUG]: 0,
			[LogLevel.INFO]: 1,
			[LogLevel.WARN]: 2,
			[LogLevel.ERROR]: 3,
			[LogLevel.FATAL]: 3, // Same as error for Winston
		};

		return levels[level] >= levels[this.currentLevel];
	}
}

export const create = (config: LogConfig): Result<Logger> => {
	try {
		const transports: winston.transport[] = [];

		// Configure format
		const logFormat =
			config.format === "json"
				? winston.format.combine(
						winston.format.timestamp(),
						winston.format.errors({ stack: true }),
						winston.format.json(),
					)
				: winston.format.combine(
						winston.format.timestamp({ format: "YYYY-MM-DD HH:mm:ss" }),
						winston.format.errors({ stack: true }),
						winston.format.printf(({ timestamp, level, message, ...rest }) => {
							const context =
								Object.keys(rest).length > 0 ? ` ${JSON.stringify(rest)}` : "";
							return `${timestamp} [${level.toUpperCase()}] ${message}${context}`;
						}),
					);

		// Configure destination
		if (config.destination === "console" || !config.destination) {
			transports.push(
				new winston.transports.Console({
					format: logFormat,
				}),
			);
		}

		if (config.destination === "file" && config.filePath) {
			transports.push(
				new winston.transports.File({
					filename: config.filePath,
					format: logFormat,
					maxFiles: config.maxFiles || 5,
					maxsize: parseMaxSize(config.maxSize || "10MB"),
				}),
			);
		}

		const winstonLogger = winston.createLogger({
			level: mapLogLevel(config.level),
			transports,
			exitOnError: false,
		});

		return success(new WinstonLogger(winstonLogger, config.level));
	} catch (error) {
		return failure(
			createQiError(
				"LOGGER_CREATION_ERROR",
				`Failed to create logger: ${error}`,
				ErrorCategory.CONFIGURATION,
				{ config, originalError: error },
			),
		);
	}
};

export const createDefault = (): Result<Logger> => {
	return create({
		level: LogLevel.INFO,
		format: "plain",
		destination: "console",
	});
};

// Helper functions
function mapLogLevel(level: LogLevel): string {
	switch (level) {
		case LogLevel.DEBUG:
			return "debug";
		case LogLevel.INFO:
			return "info";
		case LogLevel.WARN:
			return "warn";
		case LogLevel.ERROR:
			return "error";
		case LogLevel.FATAL:
			return "error"; // Winston doesn't have fatal
		default:
			return "info";
	}
}

function parseMaxSize(size: string): number {
	const match = size.match(/^(\d+)(MB|KB|GB)?$/i);
	if (!match) {
		return 10 * 1024 * 1024; // Default 10MB
	}

	const value = Number.parseInt(match[1], 10);
	const unit = (match[2] || "MB").toUpperCase();

	switch (unit) {
		case "KB":
			return value * 1024;
		case "MB":
			return value * 1024 * 1024;
		case "GB":
			return value * 1024 * 1024 * 1024;
		default:
			return value;
	}
}
