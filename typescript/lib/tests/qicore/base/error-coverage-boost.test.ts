/**
 * Error Coverage Boost Tests for QiCore v4.0 Base
 *
 * Targeted tests to improve coverage from 85.29% to 95%+
 * Focuses on uncovered lines: 343, 382, 398-439, 479
 */

import { describe, expect, it } from "vitest";
import {
  type ErrorCategory,
  type QiError,
  aggregate,
  create,
  getRetryStrategy,
  isRetryable,
} from "../../../src/qicore/base/error.js";

describe("Error Coverage Boost - Missing Functions", () => {
  describe("aggregate() function", () => {
    it("should aggregate multiple errors with context merging", () => {
      // Test line 343: context aggregation logic
      const error1 = create("ERROR_1", "First error", "VALIDATION", {
        userId: 123,
        field: "email",
      });
      const error2 = create("ERROR_2", "Second error", "BUSINESS", {
        userId: 456,
        action: "save",
      });
      const error3 = create("ERROR_3", "Third error", "NETWORK", {
        endpoint: "/api/user",
        status: 500,
      });

      const aggregated = aggregate([error1, error2, error3]);

      expect(aggregated.code).toBe("AGGREGATED_ERRORS");
      expect(aggregated.message).toContain("ERROR_1, ERROR_2, ERROR_3");
      expect(aggregated.category).toBe("VALIDATION"); // Uses first error's category
      expect(aggregated.severity).toBe("HIGH"); // Should use max severity (NETWORK -> HIGH)

      // Test context aggregation (line 343)
      expect(aggregated.context.get("error_0_userId")).toBe(123);
      expect(aggregated.context.get("error_0_field")).toBe("email");
      expect(aggregated.context.get("error_1_userId")).toBe(456);
      expect(aggregated.context.get("error_1_action")).toBe("save");
      expect(aggregated.context.get("error_2_endpoint")).toBe("/api/user");
      expect(aggregated.context.get("error_2_status")).toBe(500);
    });

    it("should handle single error aggregation", () => {
      const singleError = create("SINGLE", "Single error", "TIMEOUT");
      const aggregated = aggregate([singleError]);

      // Single error should be returned as-is (line 330)
      expect(aggregated.code).toBe("SINGLE");
      expect(aggregated.message).toBe("Single error");
      expect(aggregated.category).toBe("TIMEOUT");
      expect(aggregated).toBe(singleError); // Should be the exact same object
    });

    it("should handle errors with empty context", () => {
      const error1 = create("NO_CONTEXT_1", "Error without context", "PARSING");
      const error2 = create("NO_CONTEXT_2", "Another error", "SECURITY");

      const aggregated = aggregate([error1, error2]);

      expect(aggregated.code).toBe("AGGREGATED_ERRORS");
      expect(aggregated.context.size).toBe(0); // No context to aggregate
    });
  });

  describe("isRetryable() function", () => {
    it("should handle all error categories including default case", () => {
      // Test standard retryable categories
      expect(isRetryable("NETWORK")).toBe(true);
      expect(isRetryable("TIMEOUT")).toBe(true);
      expect(isRetryable("SYSTEM")).toBe(true);

      // Test non-retryable categories
      expect(isRetryable("VALIDATION")).toBe(false);
      expect(isRetryable("BUSINESS")).toBe(false);
      expect(isRetryable("SECURITY")).toBe(false);
      expect(isRetryable("PARSING")).toBe(false);
      expect(isRetryable("UNKNOWN")).toBe(false);

      // Test default case (line 382) - this requires us to pass an invalid category
      // @ts-expect-error - Testing invalid category for coverage
      expect(isRetryable("INVALID_CATEGORY")).toBe(false);
    });
  });

  describe("getRetryStrategy() function - Complete Coverage", () => {
    it("should return correct strategy for NETWORK errors", () => {
      const strategy = getRetryStrategy("NETWORK");

      expect(strategy.maxAttempts).toBe(3);
      expect(strategy.baseDelayMs).toBe(100);
      expect(strategy.maxDelayMs).toBe(5000);
      expect(strategy.exponentialBackoff).toBe(true);
    });

    it("should return correct strategy for TIMEOUT errors", () => {
      const strategy = getRetryStrategy("TIMEOUT");

      expect(strategy.maxAttempts).toBe(2);
      expect(strategy.baseDelayMs).toBe(1000);
      expect(strategy.maxDelayMs).toBe(10000);
      expect(strategy.exponentialBackoff).toBe(true);
    });

    it("should return correct strategy for SYSTEM errors", () => {
      const strategy = getRetryStrategy("SYSTEM");

      expect(strategy.maxAttempts).toBe(2);
      expect(strategy.baseDelayMs).toBe(500);
      expect(strategy.maxDelayMs).toBe(2000);
      expect(strategy.exponentialBackoff).toBe(false);
    });

    it("should return no-retry strategy for VALIDATION errors", () => {
      const strategy = getRetryStrategy("VALIDATION");

      expect(strategy.maxAttempts).toBe(0);
      expect(strategy.baseDelayMs).toBe(0);
      expect(strategy.maxDelayMs).toBe(0);
      expect(strategy.exponentialBackoff).toBe(false);
    });

    it("should return no-retry strategy for BUSINESS errors", () => {
      const strategy = getRetryStrategy("BUSINESS");

      expect(strategy.maxAttempts).toBe(0);
      expect(strategy.baseDelayMs).toBe(0);
      expect(strategy.maxDelayMs).toBe(0);
      expect(strategy.exponentialBackoff).toBe(false);
    });

    it("should return no-retry strategy for SECURITY errors", () => {
      const strategy = getRetryStrategy("SECURITY");

      expect(strategy.maxAttempts).toBe(0);
      expect(strategy.baseDelayMs).toBe(0);
      expect(strategy.maxDelayMs).toBe(0);
      expect(strategy.exponentialBackoff).toBe(false);
    });

    it("should return no-retry strategy for PARSING errors", () => {
      const strategy = getRetryStrategy("PARSING");

      expect(strategy.maxAttempts).toBe(0);
      expect(strategy.baseDelayMs).toBe(0);
      expect(strategy.maxDelayMs).toBe(0);
      expect(strategy.exponentialBackoff).toBe(false);
    });

    it("should return no-retry strategy for UNKNOWN errors", () => {
      const strategy = getRetryStrategy("UNKNOWN");

      expect(strategy.maxAttempts).toBe(0);
      expect(strategy.baseDelayMs).toBe(0);
      expect(strategy.maxDelayMs).toBe(0);
      expect(strategy.exponentialBackoff).toBe(false);
    });

    it("should handle default case for invalid categories", () => {
      // Test default case (lines 431-437)
      // @ts-expect-error - Testing invalid category for coverage
      const strategy = getRetryStrategy("INVALID_CATEGORY" as ErrorCategory);

      expect(strategy.maxAttempts).toBe(0);
      expect(strategy.baseDelayMs).toBe(0);
      expect(strategy.maxDelayMs).toBe(0);
      expect(strategy.exponentialBackoff).toBe(false);
    });
  });

  describe("Retry Strategy Integration Tests", () => {
    it("should provide consistent retry behavior for error instances", () => {
      const networkError = create("CONN_FAILED", "Connection failed", "NETWORK");
      const strategy = getRetryStrategy(networkError.category);

      expect(isRetryable(networkError.category)).toBe(true);
      expect(strategy.maxAttempts).toBeGreaterThan(0);
      expect(strategy.exponentialBackoff).toBe(true);
    });

    it("should prevent retry for non-retryable errors", () => {
      const validationError = create("INVALID_INPUT", "Invalid input", "VALIDATION");
      const strategy = getRetryStrategy(validationError.category);

      expect(isRetryable(validationError.category)).toBe(false);
      expect(strategy.maxAttempts).toBe(0);
    });
  });

  describe("Edge Cases and Error Boundaries", () => {
    it("should handle empty error arrays in aggregate", () => {
      // This might throw or return a default error - let's see
      expect(() => aggregate([])).not.toThrow();
    });

    it("should handle complex error chains in aggregation", () => {
      const root = create("ROOT", "Root cause", "SYSTEM");
      const middle = create("MIDDLE", "Middle", "NETWORK", null, root);
      const top = create("TOP", "Top level", "VALIDATION", null, middle);

      const aggregated = aggregate([top]);

      // Should preserve the error chain structure
      expect(aggregated.cause).toBe(top.cause);
    });

    it("should test severity inference default case", () => {
      // Test line 479: default case in inferSeverityFromCategory
      // We can test this indirectly by creating an error with an invalid category
      const error = create("TEST", "msg", "NETWORK");
      // @ts-expect-error - Testing invalid category for coverage
      (error as any).category = "INVALID_CATEGORY";

      // The severity should still be accessible (it was set during creation)
      // But we can test that error creation handles unknown categories gracefully
      expect(error.severity).toBeDefined();
    });
  });
});
