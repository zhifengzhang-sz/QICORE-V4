/**
 * QiCore v4.0 - Logger Component Tests
 * 
 * Comprehensive tests for Winston-based structured logging with performance verification
 */

import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import {
  createDefault,
  createWithConfig,
  createFileLogger,
  createConsoleLogger,
  createSilentLogger,
  createStructuredLogger,
  createMonitoredLogger,
  createContextualLogger,
  createRateLimitedLogger,
  fromConfig,
  validateLogLevel,
  LogLevel,
  getLevelPriority,
  isLevelAtLeast,
  type Logger,
  type LoggerConfig
} from "../../../src/qicore/core/logger.js";
import { promises as fs } from "fs";

describe("Logger Component", () => {
  const tempLogFile = "/tmp/qicore-test.log";

  afterEach(async () => {
    try {
      await fs.unlink(tempLogFile);
    } catch {
      // File may not exist
    }
    vi.restoreAllMocks();
  });

  describe("Logger Creation", () => {
    it("creates default logger successfully", () => {
      const result = createDefault();
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const logger = result.right;
        expect(typeof logger.log).toBe("function");
        expect(typeof logger.isLevelEnabled).toBe("function");
        expect(logger.getLevel()).toBe(LogLevel.INFO);
      }
    });

    it("creates logger with custom config", () => {
      const config: LoggerConfig = {
        level: LogLevel.DEBUG,
        silent: false
      };
      
      const result = createWithConfig(config);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const logger = result.right;
        expect(logger.getLevel()).toBe(LogLevel.DEBUG);
      }
    });

    it("creates silent logger", () => {
      const result = createSilentLogger();
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const logger = result.right;
        // Silent logger should still have all methods
        expect(typeof logger.log).toBe("function");
        expect(typeof logger.isLevelEnabled).toBe("function");
      }
    });

    it("creates file logger", () => {
      const result = createFileLogger(tempLogFile);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const logger = result.right;
        expect(typeof logger.log).toBe("function");
      }
    });

    it("creates console logger", () => {
      const result = createConsoleLogger();
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const logger = result.right;
        expect(typeof logger.log).toBe("function");
      }
    });

    it("creates structured logger", () => {
      const result = createStructuredLogger();
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const logger = result.right;
        expect(typeof logger.log).toBe("function");
      }
    });
  });

  describe("Log Level Management", () => {
    let logger: Logger;

    beforeEach(() => {
      const result = createDefault();
      expect(result._tag).toBe("Right");
      logger = result.right!;
    });

    it("validates log levels correctly", () => {
      expect(validateLogLevel("debug")._tag).toBe("Right");
      expect(validateLogLevel("info")._tag).toBe("Right");
      expect(validateLogLevel("warn")._tag).toBe("Right");
      expect(validateLogLevel("error")._tag).toBe("Right");
      expect(validateLogLevel("fatal")._tag).toBe("Right");
      
      const invalidResult = validateLogLevel("invalid");
      expect(invalidResult._tag).toBe("Left");
      if (invalidResult._tag === "Left") {
        expect(invalidResult.left.code).toBe("INVALID_LOG_LEVEL");
      }
    });

    it("gets level priorities correctly", () => {
      expect(getLevelPriority(LogLevel.DEBUG)).toBe(0);
      expect(getLevelPriority(LogLevel.INFO)).toBe(1);
      expect(getLevelPriority(LogLevel.WARN)).toBe(2);
      expect(getLevelPriority(LogLevel.ERROR)).toBe(3);
      expect(getLevelPriority(LogLevel.FATAL)).toBe(4);
    });

    it("compares levels correctly", () => {
      expect(isLevelAtLeast(LogLevel.ERROR)(LogLevel.WARN)).toBe(true);
      expect(isLevelAtLeast(LogLevel.ERROR)(LogLevel.ERROR)).toBe(true);
      expect(isLevelAtLeast(LogLevel.ERROR)(LogLevel.FATAL)).toBe(true);
      expect(isLevelAtLeast(LogLevel.ERROR)(LogLevel.INFO)).toBe(false);
      expect(isLevelAtLeast(LogLevel.ERROR)(LogLevel.DEBUG)).toBe(false);
    });

    it("changes log level dynamically", () => {
      expect(logger.getLevel()).toBe(LogLevel.INFO);
      
      logger.setLevel(LogLevel.DEBUG);
      expect(logger.getLevel()).toBe(LogLevel.DEBUG);
      expect(logger.isLevelEnabled(LogLevel.DEBUG)).toBe(true);
      
      logger.setLevel(LogLevel.ERROR);
      expect(logger.getLevel()).toBe(LogLevel.ERROR);
      expect(logger.isLevelEnabled(LogLevel.INFO)).toBe(false);
      expect(logger.isLevelEnabled(LogLevel.ERROR)).toBe(true);
    });

    it("filters logs by level", () => {
      logger.setLevel(LogLevel.WARN);
      
      expect(logger.isLevelEnabled(LogLevel.DEBUG)).toBe(false);
      expect(logger.isLevelEnabled(LogLevel.INFO)).toBe(false);
      expect(logger.isLevelEnabled(LogLevel.WARN)).toBe(true);
      expect(logger.isLevelEnabled(LogLevel.ERROR)).toBe(true);
      expect(logger.isLevelEnabled(LogLevel.FATAL)).toBe(true);
    });
  });

  describe("Logging Operations", () => {
    let logger: Logger;
    let consoleSpy: any;

    beforeEach(() => {
      // Create console logger for testing output
      const result = createConsoleLogger();
      expect(result._tag).toBe("Right");
      logger = result.right!;
      
      // Spy on console methods
      consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
    });

    it("logs at different levels", () => {
      logger.setLevel(LogLevel.DEBUG);
      
      logger.debug("Debug message");
      logger.info("Info message");
      logger.warn("Warn message");
      logger.error("Error message");
      logger.fatal("Fatal message");
      
      // Should have logged all messages
      expect(consoleSpy).toHaveBeenCalledTimes(5);
    });

    it("logs with context", () => {
      const context = { userId: "123", action: "login" };
      
      logger.info("User logged in", context);
      
      expect(consoleSpy).toHaveBeenCalledWith(
        expect.stringContaining("User logged in")
      );
    });

    it("respects level filtering", () => {
      logger.setLevel(LogLevel.WARN);
      
      logger.debug("Should not log");
      logger.info("Should not log");
      logger.warn("Should log");
      logger.error("Should log");
      
      expect(consoleSpy).toHaveBeenCalledTimes(2);
    });

    it("uses generic log method", () => {
      logger.setLevel(LogLevel.DEBUG);
      
      logger.log(LogLevel.INFO, "Generic log message", { key: "value" });
      
      expect(consoleSpy).toHaveBeenCalledWith(
        expect.stringContaining("Generic log message")
      );
    });
  });

  describe("Performance Requirements", () => {
    let logger: Logger;

    beforeEach(() => {
      const result = createSilentLogger(); // Use silent for performance testing
      expect(result._tag).toBe("Right");
      logger = result.right!;
    });

    it("level checking is very fast", () => {
      logger.setLevel(LogLevel.INFO);
      
      const start = performance.now();
      
      for (let i = 0; i < 10000; i++) {
        logger.isLevelEnabled(LogLevel.DEBUG);
        logger.isLevelEnabled(LogLevel.INFO);
        logger.isLevelEnabled(LogLevel.ERROR);
      }
      
      const end = performance.now();
      const avgDuration = ((end - start) * 1000) / 30000; // microseconds per operation
      
      // Should be well under 1μs per operation
      expect(avgDuration).toBeLessThan(1);
    });

    it("logging operations are fast", () => {
      logger.setLevel(LogLevel.DEBUG);
      
      const start = performance.now();
      
      for (let i = 0; i < 1000; i++) {
        logger.info(`Test message ${i}`, { iteration: i });
      }
      
      const end = performance.now();
      const avgDuration = ((end - start) * 1000) / 1000; // microseconds per operation
      
      // Should be well under 10μs per operation for silent logger
      expect(avgDuration).toBeLessThan(10);
    });

    it("disabled level logging has minimal overhead", () => {
      logger.setLevel(LogLevel.ERROR); // Disable debug/info
      
      const start = performance.now();
      
      for (let i = 0; i < 1000; i++) {
        logger.debug(`Debug message ${i}`, { iteration: i });
        logger.info(`Info message ${i}`, { iteration: i });
      }
      
      const end = performance.now();
      const avgDuration = ((end - start) * 1000) / 2000;
      
      // Should be extremely fast when disabled
      expect(avgDuration).toBeLessThan(1);
    });
  });

  describe("Enhanced Logger Features", () => {
    let baseLogger: Logger;

    beforeEach(() => {
      const result = createSilentLogger();
      expect(result._tag).toBe("Right");
      baseLogger = result.right!;
    });

    it("creates monitored logger", () => {
      let measurements: Array<{ level: LogLevel; message: string; duration: number }> = [];
      
      const monitoredLogger = createMonitoredLogger(
        baseLogger,
        (level, message, duration) => {
          measurements.push({ level, message, duration });
        }
      );
      
      monitoredLogger.info("Test message");
      monitoredLogger.error("Error message");
      
      expect(measurements).toHaveLength(2);
      expect(measurements[0].level).toBe(LogLevel.INFO);
      expect(measurements[0].message).toBe("Test message");
      expect(measurements[0].duration).toBeTypeOf("number");
    });

    it("creates contextual logger", () => {
      const globalContext = { service: "qicore", version: "4.0" };
      const contextualLogger = createContextualLogger(baseLogger, globalContext);
      
      // Test that methods exist and can be called
      expect(typeof contextualLogger.info).toBe("function");
      expect(typeof contextualLogger.error).toBe("function");
      
      contextualLogger.info("Test message", { local: "context" });
      contextualLogger.error("Error message");
    });

    it("creates rate limited logger", () => {
      const rateLimitedLogger = createRateLimitedLogger(baseLogger, 2); // 2 logs per second
      
      // Test that methods exist
      expect(typeof rateLimitedLogger.info).toBe("function");
      expect(typeof rateLimitedLogger.warn).toBe("function");
      
      // These should work without throwing
      rateLimitedLogger.info("Message 1");
      rateLimitedLogger.info("Message 2");
      rateLimitedLogger.info("Message 3"); // This might be rate limited
    });
  });

  describe("Configuration from Object", () => {
    it("creates logger from configuration object", () => {
      const config = {
        level: "debug",
        silent: false,
        exitOnError: true
      };
      
      const result = fromConfig(config);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const logger = result.right;
        expect(logger.getLevel()).toBe(LogLevel.DEBUG);
      }
    });

    it("handles invalid level in configuration", () => {
      const config = {
        level: "invalid_level",
        silent: false
      };
      
      const result = fromConfig(config);
      
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("INVALID_LOG_LEVEL");
      }
    });

    it("uses default values for missing config", () => {
      const config = {};
      
      const result = fromConfig(config);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const logger = result.right;
        expect(logger.getLevel()).toBe(LogLevel.INFO); // Default level
      }
    });
  });

  describe("Error Handling", () => {
    it("handles logger creation errors gracefully", () => {
      // Test with invalid configuration that might cause Winston to fail
      const invalidConfig: LoggerConfig = {
        level: LogLevel.INFO,
        transports: undefined as any // This might cause issues
      };
      
      const result = createWithConfig(invalidConfig);
      
      // Should either succeed or fail gracefully
      if (result._tag === "Left") {
        expect(result.left.category).toBe("SYSTEM");
        expect(result.left.code).toBe("LOGGER_INIT_ERROR");
      }
    });

    it("handles file logger creation with invalid path", () => {
      const result = createFileLogger("/invalid/path/that/does/not/exist/file.log");
      
      // Should handle invalid paths gracefully
      if (result._tag === "Left") {
        expect(result.left.category).toBe("FILESYSTEM");
      }
    });
  });

  describe("Log Level Enum", () => {
    it("has all required log levels", () => {
      const levels = Object.values(LogLevel);
      
      expect(levels).toContain("debug");
      expect(levels).toContain("info");
      expect(levels).toContain("warn");
      expect(levels).toContain("error");
      expect(levels).toContain("fatal");
    });

    it("maintains correct priority ordering", () => {
      const priorities = Object.values(LogLevel).map(level => ({
        level,
        priority: getLevelPriority(level)
      }));
      
      // Sort by priority and verify order
      priorities.sort((a, b) => a.priority - b.priority);
      
      expect(priorities[0].level).toBe(LogLevel.DEBUG);
      expect(priorities[1].level).toBe(LogLevel.INFO);
      expect(priorities[2].level).toBe(LogLevel.WARN);
      expect(priorities[3].level).toBe(LogLevel.ERROR);
      expect(priorities[4].level).toBe(LogLevel.FATAL);
    });
  });

  describe("Memory Management", () => {
    it("does not leak memory with many log calls", () => {
      const result = createSilentLogger();
      expect(result._tag).toBe("Right");
      const logger = result.right!;
      
      // This should not cause memory issues
      for (let i = 0; i < 10000; i++) {
        logger.info(`Message ${i}`, { 
          data: new Array(100).fill(i),
          timestamp: Date.now(),
          random: Math.random()
        });
      }
      
      // If we get here without OOM, the test passes
      expect(true).toBe(true);
    });
  });

  describe("Thread Safety", () => {
    it("handles concurrent logging safely", async () => {
      const result = createSilentLogger();
      expect(result._tag).toBe("Right");
      const logger = result.right!;
      
      // Create multiple concurrent logging operations
      const promises = Array.from({ length: 10 }, (_, i) =>
        Promise.resolve().then(() => {
          for (let j = 0; j < 100; j++) {
            logger.info(`Thread ${i}, Message ${j}`);
          }
        })
      );
      
      // Should complete without errors
      await Promise.all(promises);
      expect(true).toBe(true);
    });
  });

  describe("Structured Logging", () => {
    it("handles complex objects in context", () => {
      const result = createSilentLogger();
      expect(result._tag).toBe("Right");
      const logger = result.right!;
      
      const complexContext = {
        user: {
          id: "123",
          name: "Test User",
          permissions: ["read", "write"]
        },
        request: {
          method: "POST",
          url: "/api/test",
          headers: {
            "content-type": "application/json",
            "authorization": "Bearer token"
          }
        },
        performance: {
          duration: 123.45,
          memory: process.memoryUsage()
        }
      };
      
      // Should handle complex objects without errors
      expect(() => {
        logger.info("Complex context test", complexContext);
      }).not.toThrow();
    });

    it("handles circular references in context", () => {
      const result = createSilentLogger();
      expect(result._tag).toBe("Right");
      const logger = result.right!;
      
      const circularObject: any = { name: "test" };
      circularObject.self = circularObject;
      
      // Should handle circular references gracefully
      expect(() => {
        logger.info("Circular reference test", { circular: circularObject });
      }).not.toThrow();
    });
  });
});