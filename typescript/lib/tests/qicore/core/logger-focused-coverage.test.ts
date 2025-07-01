/**
 * Focused Logger Coverage Tests for QiCore v4.0 Core
 *
 * Carefully targets specific uncovered lines in logger.ts
 * Focuses on improving coverage from 49.84% significantly
 */

import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { getData, isFailure, isSuccess } from "../../../src/qicore/base/result.js";
import {
  type LogLevel,
  type LogOutput,
  type Logger,
  type LoggerConfig,
  create,
  createDefault,
  createDefaultLoggerConfig,
  createSilent,
  createSilentLoggerConfig,
  createTest,
  createTestLoggerConfig,
  logLevelToString,
  parseLogLevel,
  validateConfig,
} from "../../../src/qicore/core/logger.js";

describe("Logger Coverage Enhancement - Targeted Tests", () => {
  describe("Log Level Operations", () => {
    it("should parse valid log levels correctly", () => {
      const validLevels = ["verbose", "debug", "info", "warn", "error"];

      for (const level of validLevels) {
        const result = parseLogLevel(level);
        expect(isSuccess(result)).toBe(true);
        if (isSuccess(result)) {
          expect(getData(result)).toBe(level);
        }
      }
    });

    it("should parse log levels case-insensitively", () => {
      const testCases = [
        ["VERBOSE", "verbose"],
        ["Debug", "debug"],
        ["INFO", "info"],
        ["Warn", "warn"],
        ["ERROR", "error"],
      ];

      for (const [input, expected] of testCases) {
        const result = parseLogLevel(input);
        expect(isSuccess(result)).toBe(true);
        if (isSuccess(result)) {
          expect(getData(result)).toBe(expected);
        }
      }
    });

    it("should reject invalid log levels", () => {
      const invalidLevels = ["invalid", "trace", "fatal", "critical", ""];

      for (const level of invalidLevels) {
        const result = parseLogLevel(level);
        expect(isFailure(result)).toBe(true);
        if (isFailure(result)) {
          expect(result.left.code).toBe("INVALID_LOG_LEVEL");
          expect(result.left.message).toContain(level);
          expect(result.left.category).toBe("VALIDATION");
        }
      }
    });

    it("should convert log levels to strings correctly", () => {
      const levelMappings: Array<[LogLevel, string]> = [
        ["verbose", "TRACE"],
        ["debug", "DEBUG"],
        ["info", "INFO"],
        ["warn", "WARN"],
        ["error", "ERROR"],
      ];

      for (const [level, expected] of levelMappings) {
        const result = logLevelToString(level);
        expect(result).toBe(expected);
      }
    });
  });

  describe("Logger Configuration", () => {
    it("should create default logger config", () => {
      const config = createDefaultLoggerConfig();

      expect(config.level).toBe("info");
      expect(config.output).toBe("winston");
      expect(config.enableColors).toBe(true);
      expect(config.enableTimestamp).toBe(true);
      expect(config.timestampFormat).toBe("iso");
      expect(config.enableConsole).toBe(true);
    });

    it("should create silent logger config", () => {
      const config = createSilentLoggerConfig();

      expect(config.level).toBe("error");
      expect(config.output).toBe("silent");
      expect(config.enableColors).toBe(false);
      expect(config.enableTimestamp).toBe(false);
      expect(config.timestampFormat).toBe("none");
      expect(config.enableConsole).toBe(false);
    });

    it("should create test logger config", () => {
      const config = createTestLoggerConfig();

      expect(config.level).toBe("verbose");
      expect(config.output).toBe("silent");
      expect(config.enableColors).toBe(false);
      expect(config.enableTimestamp).toBe(false);
      expect(config.timestampFormat).toBe("none");
      expect(config.enableConsole).toBe(false);
    });

    it("should validate valid logger configuration", () => {
      const validConfig: LoggerConfig = {
        level: "debug",
        output: "winston",
        enableColors: false,
        enableTimestamp: true,
        timestampFormat: "iso",
      };

      const result = validateConfig(validConfig);
      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        expect(getData(result)).toEqual(validConfig);
      }
    });

    it("should reject invalid logger configuration", () => {
      const invalidConfig = {
        level: "invalid_level",
        output: "console",
        format: "json",
        timestamp: true,
        colorize: false,
        metadata: true,
      } as LoggerConfig;

      const result = validateConfig(invalidConfig);
      expect(isFailure(result)).toBe(true);
      if (isFailure(result)) {
        expect(result.left.code).toBe("INVALID_LOGGER_CONFIG");
        expect(result.left.category).toBe("VALIDATION");
      }
    });
  });

  describe("Logger Creation", () => {
    it("should create default logger successfully", () => {
      const result = createDefault();
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const logger = getData(result)!;
        expect(logger).toBeDefined();
        expect(typeof logger.log).toBe("function");
        expect(typeof logger.debug).toBe("function");
        expect(typeof logger.info).toBe("function");
        expect(typeof logger.warn).toBe("function");
        expect(typeof logger.error).toBe("function");
      }
    });

    it("should create silent logger successfully", () => {
      const result = createSilent();
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const logger = getData(result)!;
        expect(logger).toBeDefined();
        expect(typeof logger.log).toBe("function");
      }
    });

    it("should create test logger successfully", () => {
      const result = createTest();
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const logger = getData(result)!;
        expect(logger).toBeDefined();
        expect(typeof logger.log).toBe("function");
      }
    });

    it("should create logger with custom configuration", () => {
      const customConfig: LoggerConfig = {
        level: "warn",
        output: "silent",
        format: "json",
        timestamp: true,
        colorize: false,
        metadata: true,
      };

      const result = create(customConfig);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const logger = getData(result)!;
        expect(logger).toBeDefined();
      }
    });

    it("should create logger with function output", () => {
      const mockOutput = vi.fn();

      const customConfig: LoggerConfig = {
        level: "info",
        output: mockOutput,
        format: "json",
        timestamp: false,
        colorize: false,
        metadata: false,
      };

      const result = create(customConfig);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const logger = getData(result)!;

        // Test that the logger actually calls our mock function
        logger.info("test message");
        expect(mockOutput).toHaveBeenCalled();
      }
    });
  });

  describe("Logger Operations", () => {
    let logger: Logger;

    beforeEach(() => {
      const result = createTest(); // Use test logger to avoid console output
      if (isSuccess(result)) {
        logger = getData(result)!;
      }
    });

    it("should log messages at different levels", () => {
      // These should not throw errors
      expect(() => logger.debug("debug message")).not.toThrow();
      expect(() => logger.info("info message")).not.toThrow();
      expect(() => logger.warn("warn message")).not.toThrow();
      expect(() => logger.error("error message")).not.toThrow();
    });

    it("should log with metadata", () => {
      const metadata = { userId: 123, action: "test" };

      expect(() => logger.log("info", "message with metadata", metadata)).not.toThrow();
      expect(() => logger.info("info with metadata", metadata)).not.toThrow();
      expect(() => logger.error("error with metadata", metadata)).not.toThrow();
    });

    it("should handle complex metadata objects", () => {
      const complexMetadata = {
        user: { id: 123, name: "test" },
        request: { method: "GET", path: "/api/test" },
        performance: { duration: 50.5, memory: 1024 },
        nested: {
          deep: {
            value: "deeply nested",
          },
        },
      };

      expect(() => logger.info("complex metadata", complexMetadata)).not.toThrow();
    });

    it("should handle empty and null metadata", () => {
      expect(() => logger.info("no metadata")).not.toThrow();
      expect(() => logger.info("empty metadata", {})).not.toThrow();
      expect(() => logger.info("null metadata", null)).not.toThrow();
      expect(() => logger.info("undefined metadata", undefined)).not.toThrow();
    });
  });

  describe("Different Output Types", () => {
    let consoleSpy: ReturnType<typeof vi.spyOn>;

    beforeEach(() => {
      consoleSpy = vi.spyOn(console, "log").mockImplementation(() => {});
    });

    afterEach(() => {
      consoleSpy.mockRestore();
    });

    it("should handle console output", () => {
      const consoleConfig: LoggerConfig = {
        level: "info",
        output: "console",
        format: "pretty",
        timestamp: false,
        colorize: false,
        metadata: false,
      };

      const result = create(consoleConfig);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const logger = getData(result)!;
        logger.info("test console message");

        // Console should have been called
        expect(consoleSpy).toHaveBeenCalled();
      }
    });

    it("should handle silent output", () => {
      const silentConfig: LoggerConfig = {
        level: "info",
        output: "silent",
        format: "json",
        timestamp: false,
        colorize: false,
        metadata: false,
      };

      const result = create(silentConfig);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const logger = getData(result)!;
        logger.info("test silent message");

        // Console should NOT have been called for silent output
        expect(consoleSpy).not.toHaveBeenCalled();
      }
    });

    it("should handle winston output", () => {
      const winstonConfig: LoggerConfig = {
        level: "error", // Use error level to reduce winston noise in tests
        output: "winston",
        format: "json",
        timestamp: true,
        colorize: false,
        metadata: true,
      };

      const result = create(winstonConfig);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const logger = getData(result)!;

        // Should not throw with winston output
        expect(() => logger.error("test winston message")).not.toThrow();
      }
    });
  });

  describe("Format Variations", () => {
    it("should handle JSON format", () => {
      const jsonConfig: LoggerConfig = {
        level: "info",
        output: "silent",
        format: "json",
        timestamp: true,
        colorize: false,
        metadata: true,
      };

      const result = create(jsonConfig);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const logger = getData(result)!;
        expect(() => logger.info("json format test", { key: "value" })).not.toThrow();
      }
    });

    it("should handle pretty format", () => {
      const prettyConfig: LoggerConfig = {
        level: "info",
        output: "silent",
        format: "pretty",
        timestamp: true,
        colorize: true,
        metadata: true,
      };

      const result = create(prettyConfig);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const logger = getData(result)!;
        expect(() => logger.info("pretty format test", { key: "value" })).not.toThrow();
      }
    });
  });

  describe("Edge Cases and Error Conditions", () => {
    it("should handle very long log messages", () => {
      const result = createTest();
      if (isSuccess(result)) {
        const logger = getData(result)!;
        const longMessage = "x".repeat(10000);

        expect(() => logger.info(longMessage)).not.toThrow();
      }
    });

    it("should handle special characters in messages", () => {
      const result = createTest();
      if (isSuccess(result)) {
        const logger = getData(result)!;
        const specialMessage = "Special chars: \n\t\r\"'\\{}[]<>&";

        expect(() => logger.info(specialMessage)).not.toThrow();
      }
    });

    it("should handle circular references in metadata", () => {
      const result = createTest();
      if (isSuccess(result)) {
        const logger = getData(result)!;

        const circular: any = { name: "test" };
        circular.self = circular;

        // Should not throw even with circular reference
        expect(() => logger.info("circular metadata", circular)).not.toThrow();
      }
    });

    it("should handle undefined and null messages", () => {
      const result = createTest();
      if (isSuccess(result)) {
        const logger = getData(result)!;

        expect(() => logger.info(undefined as any)).not.toThrow();
        expect(() => logger.info(null as any)).not.toThrow();
        expect(() => logger.info("")).not.toThrow();
      }
    });
  });
});
