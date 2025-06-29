/**
 * Basic Error Tests for QiCore v4.0 Base - Coverage Boost
 *
 * Tests core error functionality to boost coverage from 31% to 80%+
 */

import {
  CommonErrors,
  type ErrorCategory,
  type ErrorSeverity,
  type QiError,
  aggregate,
  chain,
  create,
  createQiError,
  fromException,
  fromString,
  isQiError,
  isRetryable,
  withCause,
  withContext,
  withSeverity,
} from "@qicore/base/error";
import { beforeEach, describe, expect, it } from "vitest";

describe("Error Factory Functions", () => {
  describe("create()", () => {
    it("should create basic error", () => {
      const error = create("TEST_CODE", "Test message", "VALIDATION");

      expect(error.code).toBe("TEST_CODE");
      expect(error.message).toBe("Test message");
      expect(error.category).toBe("VALIDATION");
      expect(error.severity).toBe("LOW"); // VALIDATION maps to LOW
      expect(error.context.size).toBe(0);
      expect(error.cause).toBeNull();
    });

    it("should create error with context", () => {
      const context = { userId: 123 };
      const error = create("TEST", "message", "NETWORK", context);

      expect(error.context.get("userId")).toBe(123);
      expect(error.severity).toBe("HIGH"); // NETWORK maps to HIGH
    });

    it("should create error with cause", () => {
      const cause = create("CAUSE", "Cause message", "SYSTEM");
      const error = create("WRAPPER", "Wrapper", "BUSINESS", null, cause);

      expect(error.cause).toBe(cause);
      expect(error.severity).toBe("MEDIUM"); // BUSINESS maps to MEDIUM
    });

    it("should override severity when provided", () => {
      const error = create("TEST", "message", "VALIDATION", null, null, "CRITICAL");

      expect(error.severity).toBe("CRITICAL");
    });
  });

  describe("fromException()", () => {
    it("should convert Error to QiError", () => {
      const jsError = new Error("JS error");
      jsError.name = "TypeError";
      const qiError = fromException(jsError);

      expect(qiError.code).toBe("TypeError");
      expect(qiError.message).toBe("JS error");
      expect(qiError.category).toBe("UNKNOWN");
      expect(qiError.severity).toBe("HIGH");
    });

    it("should use custom category", () => {
      const jsError = new Error("Network failed");
      const qiError = fromException(jsError, "NETWORK");

      expect(qiError.category).toBe("NETWORK");
    });

    it("should handle empty name and message", () => {
      const jsError = new Error();
      jsError.name = "";
      const qiError = fromException(jsError);

      expect(qiError.code).toBe("");
      expect(qiError.message).toBe("");
    });
  });

  describe("fromString()", () => {
    it("should create error from string", () => {
      const error = fromString("Simple error");

      expect(error.code).toBe("STRING_ERROR");
      expect(error.message).toBe("Simple error");
      expect(error.category).toBe("UNKNOWN");
    });

    it("should use custom category", () => {
      const error = fromString("Validation failed", "VALIDATION");

      expect(error.category).toBe("VALIDATION");
    });
  });
});

describe("Error Instance Methods", () => {
  let testError: QiError;

  beforeEach(() => {
    testError = create("TEST", "Test message", "VALIDATION", { key: "value" });
  });

  describe("toString()", () => {
    it("should format error string", () => {
      const result = testError.toString();

      expect(result).toContain("[VALIDATION:LOW]");
      expect(result).toContain("TEST");
      expect(result).toContain("Test message");
      expect(result).toContain("key=value");
    });

    it("should format error with cause", () => {
      const cause = create("CAUSE", "Cause message", "NETWORK");
      const errorWithCause = withCause(testError, cause);
      const result = errorWithCause.toString();

      expect(result).toContain("(caused by: CAUSE)");
    });
  });

  describe("toStructuredData()", () => {
    it("should convert to serializable data", () => {
      const data = testError.toStructuredData();

      expect(data.code).toBe("TEST");
      expect(data.message).toBe("Test message");
      expect(data.category).toBe("VALIDATION");
      expect(data.context).toEqual({ key: "value" });
      expect(data.severity).toBe("LOW");
    });
  });

  describe("getCategory() and getSeverity()", () => {
    it("should return category and severity", () => {
      expect(testError.getCategory()).toBe("VALIDATION");
      expect(testError.getSeverity()).toBe("LOW");
    });
  });

  describe("getRootCause()", () => {
    it("should return self when no cause", () => {
      expect(testError.getRootCause()).toBe(testError);
    });

    it("should return root cause", () => {
      const root = create("ROOT", "Root", "SYSTEM");
      const middle = withCause(create("MIDDLE", "Middle", "NETWORK"), root);
      const top = withCause(create("TOP", "Top", "SECURITY"), middle);

      expect(top.getRootCause()).toBe(root);
    });
  });

  describe("getErrorChain()", () => {
    it("should return chain from root to current", () => {
      const root = create("ROOT", "Root", "SYSTEM");
      const middle = withCause(create("MIDDLE", "Middle", "NETWORK"), root);
      const top = withCause(create("TOP", "Top", "SECURITY"), middle);

      const chain = top.getErrorChain();
      expect(chain).toHaveLength(3);
      expect(chain[0]).toBe(root);
      expect(chain[1]).toBe(middle);
      expect(chain[2]).toBe(top);
    });
  });
});

describe("Immutable Update Operations", () => {
  let baseError: QiError;

  beforeEach(() => {
    baseError = create("BASE", "Base message", "VALIDATION", { initial: "value" });
  });

  describe("withContext()", () => {
    it("should add new context", () => {
      const updated = withContext(baseError, { newKey: "newValue" });

      expect(updated.context.get("initial")).toBe("value");
      expect(updated.context.get("newKey")).toBe("newValue");
      expect(baseError.context.size).toBe(1); // Original unchanged
    });

    it("should override existing context", () => {
      const updated = withContext(baseError, { initial: "newValue" });

      expect(updated.context.get("initial")).toBe("newValue");
      expect(baseError.context.get("initial")).toBe("value");
    });
  });

  describe("withCause()", () => {
    it("should set cause", () => {
      const cause = create("CAUSE", "Cause", "NETWORK");
      const updated = withCause(baseError, cause);

      expect(updated.cause).toBe(cause);
      expect(baseError.cause).toBeNull();
    });
  });

  describe("withSeverity()", () => {
    it("should update severity", () => {
      const updated = withSeverity(baseError, "CRITICAL");

      expect(updated.severity).toBe("CRITICAL");
      expect(baseError.severity).toBe("LOW");
    });
  });

  describe("chain()", () => {
    it("should be alias for withCause", () => {
      const cause = create("CAUSE", "Cause", "NETWORK");
      const chained = chain(baseError, cause);
      const withCauseResult = withCause(baseError, cause);

      expect(chained.cause).toBe(withCauseResult.cause);
    });
  });
});

describe("Error Aggregation", () => {
  describe("aggregate()", () => {
    it("should combine multiple errors", () => {
      const errors = [
        create("ERROR1", "First", "VALIDATION"),
        create("ERROR2", "Second", "NETWORK"),
        create("ERROR3", "Third", "SYSTEM"),
      ];

      const aggregated = aggregate(errors);

      expect(aggregated.code).toBe("AGGREGATED_ERRORS");
      expect(aggregated.message).toBe("Multiple errors occurred: ERROR1, ERROR2, ERROR3");
      expect(aggregated.category).toBe("VALIDATION"); // First error's category
      expect(aggregated.severity).toBe("HIGH"); // Highest severity (SYSTEM->HIGH)
    });

    it("should handle single error", () => {
      const errors = [create("SINGLE", "Single", "VALIDATION")];
      const aggregated = aggregate(errors);

      expect(aggregated).toBe(errors[0]); // Should return the single error
    });

    it("should handle empty array", () => {
      const aggregated = aggregate([]);

      expect(aggregated.code).toBe("NO_ERRORS");
      expect(aggregated.message).toBe("Empty error list provided");
      expect(aggregated.category).toBe("VALIDATION");
    });
  });

  describe("isRetryable()", () => {
    it("should identify retryable categories", () => {
      expect(isRetryable("NETWORK")).toBe(true);
      expect(isRetryable("TIMEOUT")).toBe(true);
      expect(isRetryable("SYSTEM")).toBe(true);
    });

    it("should identify non-retryable categories", () => {
      expect(isRetryable("VALIDATION")).toBe(false);
      expect(isRetryable("BUSINESS")).toBe(false);
      expect(isRetryable("SECURITY")).toBe(false);
      expect(isRetryable("PARSING")).toBe(false);
      expect(isRetryable("UNKNOWN")).toBe(false);
    });
  });
});

describe("Common Errors", () => {
  it("should provide validation error", () => {
    const error = CommonErrors.validation("Field required", { field: "email" });

    expect(error.code).toBe("VALIDATION_ERROR");
    expect(error.category).toBe("VALIDATION");
    expect(error.severity).toBe("LOW");
  });

  it("should provide network error", () => {
    const error = CommonErrors.network("Connection failed", { host: "api.com" });

    expect(error.code).toBe("NETWORK_ERROR");
    expect(error.category).toBe("NETWORK");
    expect(error.severity).toBe("HIGH");
  });

  it("should provide timeout error", () => {
    const error = CommonErrors.timeout("Request timeout", 5000, { url: "/api" });

    expect(error.code).toBe("TIMEOUT_ERROR");
    expect(error.category).toBe("TIMEOUT");
    expect(error.severity).toBe("MEDIUM");
    expect(error.context.get("timeoutMs")).toBe(5000);
  });

  it("should provide not found error", () => {
    const error = CommonErrors.notFound("user", { id: 123 });

    expect(error.code).toBe("NOT_FOUND");
    expect(error.category).toBe("BUSINESS");
    expect(error.message).toBe("Resource not found: user");
  });

  it("should provide unauthorized error", () => {
    const error = CommonErrors.unauthorized("Access denied", { userId: 123 });

    expect(error.code).toBe("UNAUTHORIZED");
    expect(error.category).toBe("SECURITY");
    expect(error.severity).toBe("CRITICAL");
  });

  it("should provide forbidden error", () => {
    const error = CommonErrors.forbidden("Forbidden", { action: "delete" });

    expect(error.code).toBe("FORBIDDEN");
    expect(error.category).toBe("SECURITY");
  });

  it("should provide conflict error", () => {
    const error = CommonErrors.conflict("Resource conflict", { resource: "user" });

    expect(error.code).toBe("CONFLICT");
    expect(error.category).toBe("BUSINESS");
  });

  it("should provide rate limit error", () => {
    const error = CommonErrors.rateLimit("Rate limit exceeded", 60000, { ip: "1.2.3.4" });

    expect(error.code).toBe("RATE_LIMIT_EXCEEDED");
    expect(error.category).toBe("NETWORK");
    expect(error.context.get("retryAfterMs")).toBe(60000);
  });

  it("should provide unknown error", () => {
    const error = CommonErrors.unknown("Something went wrong", { trace: "..." });

    expect(error.code).toBe("UNKNOWN_ERROR");
    expect(error.category).toBe("UNKNOWN");
    expect(error.severity).toBe("CRITICAL");
  });
});

describe("Type Guards", () => {
  describe("isQiError()", () => {
    it("should identify QiError instances", () => {
      const qiError = create("TEST", "message", "VALIDATION");
      expect(isQiError(qiError)).toBe(true);
    });

    it("should reject JavaScript Errors", () => {
      const jsError = new Error("JS error");
      expect(isQiError(jsError)).toBe(false);
    });

    it("should reject null and primitives", () => {
      expect(isQiError(null)).toBe(false);
      expect(isQiError(undefined)).toBe(false);
      expect(isQiError("string")).toBe(false);
      expect(isQiError(123)).toBe(false);
      expect(isQiError({})).toBe(false);
    });
  });
});

describe("Edge Cases and Performance", () => {
  it("should handle large contexts", () => {
    const largeContext: Record<string, unknown> = {};
    for (let i = 0; i < 1000; i++) {
      largeContext[`key${i}`] = `value${i}`;
    }

    const error = create("LARGE", "Large context", "VALIDATION", largeContext);
    expect(error.context.size).toBe(1000);
  });

  it("should handle deep cause chains", () => {
    let error = create("ROOT", "Root", "NETWORK");

    for (let i = 0; i < 50; i++) {
      error = withCause(create(`LEVEL_${i}`, `Level ${i}`, "SYSTEM"), error);
    }

    const rootCause = error.getRootCause();
    expect(rootCause.code).toBe("ROOT");

    const chain = error.getErrorChain();
    expect(chain).toHaveLength(51);
  });

  it("should handle special characters", () => {
    const specialCode = "ERROR_🚀_Unicode_世界";
    const specialMessage = 'Message with quotes "and" newlines\n!';

    const error = create(specialCode, specialMessage, "VALIDATION");

    expect(error.code).toBe(specialCode);
    expect(error.message).toBe(specialMessage);
    expect(() => error.toString()).not.toThrow();
  });

  it("should handle circular references in context", () => {
    const obj: Record<string, unknown> = { name: "test" };
    obj.self = obj;

    const error = create("CIRCULAR", "message", "VALIDATION", { circular: obj });

    expect(error.context.get("circular")).toBe(obj);
    expect(() => error.toString()).not.toThrow();
  });

  it("should maintain timestamp precision", () => {
    const before = Date.now();
    const error = create("TIMING", "message", "VALIDATION");
    const after = Date.now();

    expect(error.timestamp).toBeGreaterThanOrEqual(before);
    expect(error.timestamp).toBeLessThanOrEqual(after);
  });

  it("should handle all categories and severities", () => {
    const categories: ErrorCategory[] = [
      "VALIDATION",
      "NETWORK",
      "SYSTEM",
      "BUSINESS",
      "SECURITY",
      "PARSING",
      "TIMEOUT",
      "UNKNOWN",
    ];

    const severities: ErrorSeverity[] = ["LOW", "MEDIUM", "HIGH", "CRITICAL"];

    for (const category of categories) {
      for (const severity of severities) {
        const error = create("TEST", "message", category, {}, null, severity);
        expect(error.category).toBe(category);
        expect(error.severity).toBe(severity);
      }
    }
  });
});
