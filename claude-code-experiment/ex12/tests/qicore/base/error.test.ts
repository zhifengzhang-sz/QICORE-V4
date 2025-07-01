/**
 * QiCore v4.0 - Error Component Tests
 * 
 * Comprehensive tests for QiError with context chaining and immutability
 */

import { describe, it, expect, beforeEach } from "vitest";
import {
  createQiError,
  withContext,
  withCause,
  toString,
  toStructuredData,
  getCategory,
  hasCategory,
  getContextKeys,
  getContextValue,
  hasContextKey,
  getAllCauses,
  findByCode,
  findByCategory,
  ErrorCategory
} from "../../../src/qicore/base/error.js";

describe("QiError Component", () => {
  describe("Error Creation", () => {
    it("creates basic error correctly", () => {
      const error = createQiError(
        "TEST_ERROR",
        "Test error message",
        ErrorCategory.VALIDATION
      );

      expect(error.code).toBe("TEST_ERROR");
      expect(error.message).toBe("Test error message");
      expect(error.category).toBe(ErrorCategory.VALIDATION);
      expect(error.context.size).toBe(0);
      expect(error.cause).toBeUndefined();
      expect(error.timestamp).toBeTypeOf("number");
    });

    it("creates error with context", () => {
      const context = { userId: "123", action: "login" };
      const error = createQiError(
        "LOGIN_FAILED",
        "Invalid credentials",
        ErrorCategory.VALIDATION,
        context
      );

      expect(error.context.get("userId")).toBe("123");
      expect(error.context.get("action")).toBe("login");
      expect(error.context.size).toBe(2);
    });

    it("creates error with empty context when undefined", () => {
      const error = createQiError(
        "TEST_ERROR",
        "Test message",
        ErrorCategory.SYSTEM
      );

      expect(error.context.size).toBe(0);
    });
  });

  describe("Context Operations", () => {
    let baseError: ReturnType<typeof createQiError>;

    beforeEach(() => {
      baseError = createQiError(
        "BASE_ERROR",
        "Base error",
        ErrorCategory.SYSTEM,
        { original: "value" }
      );
    });

    it("adds context immutably", () => {
      const enriched = withContext(baseError, { additional: "context" });

      // Original error unchanged
      expect(baseError.context.has("additional")).toBe(false);
      expect(baseError.context.get("original")).toBe("value");

      // New error has both contexts
      expect(enriched.context.has("additional")).toBe(true);
      expect(enriched.context.has("original")).toBe(true);
      expect(enriched.context.get("additional")).toBe("context");
      expect(enriched.context.get("original")).toBe("value");
    });

    it("overwrites context keys with same name", () => {
      const enriched = withContext(baseError, { original: "new_value" });

      expect(enriched.context.get("original")).toBe("new_value");
      expect(enriched.context.size).toBe(1);
    });

    it("preserves all other error properties", () => {
      const enriched = withContext(baseError, { new: "context" });

      expect(enriched.code).toBe(baseError.code);
      expect(enriched.message).toBe(baseError.message);
      expect(enriched.category).toBe(baseError.category);
      expect(enriched.timestamp).toBe(baseError.timestamp);
      expect(enriched.cause).toBe(baseError.cause);
    });
  });

  describe("Cause Chaining", () => {
    it("chains errors with cause", () => {
      const rootCause = createQiError(
        "ROOT_CAUSE",
        "Original error",
        ErrorCategory.NETWORK
      );

      const wrapper = withCause(
        createQiError("WRAPPER", "Wrapper error", ErrorCategory.SYSTEM),
        rootCause
      );

      expect(wrapper.cause).toBe(rootCause);
      expect(wrapper.code).toBe("WRAPPER");
    });

    it("creates deep cause chains", () => {
      const level1 = createQiError("L1", "Level 1", ErrorCategory.FILESYSTEM);
      const level2 = withCause(
        createQiError("L2", "Level 2", ErrorCategory.CACHE),
        level1
      );
      const level3 = withCause(
        createQiError("L3", "Level 3", ErrorCategory.SYSTEM),
        level2
      );

      expect(level3.cause).toBe(level2);
      expect(level3.cause?.cause).toBe(level1);
    });

    it("preserves immutability in cause chaining", () => {
      const original = createQiError("ORIG", "Original", ErrorCategory.VALIDATION);
      const cause = createQiError("CAUSE", "Cause", ErrorCategory.NETWORK);
      
      const chained = withCause(original, cause);

      expect(original.cause).toBeUndefined();
      expect(chained.cause).toBe(cause);
    });
  });

  describe("Error Serialization", () => {
    it("converts to structured data", () => {
      const error = createQiError(
        "STRUCT_TEST",
        "Structured test",
        ErrorCategory.CONFIGURATION,
        { key: "value", number: 42 }
      );

      const structured = toStructuredData(error);

      expect(structured.code).toBe("STRUCT_TEST");
      expect(structured.message).toBe("Structured test");
      expect(structured.category).toBe(ErrorCategory.CONFIGURATION);
      expect(structured.context).toEqual({ key: "value", number: 42 });
      expect(structured.cause).toBeUndefined();
      expect(structured.timestamp).toBe(error.timestamp);
    });

    it("converts to structured data with cause", () => {
      const cause = createQiError("CAUSE", "Cause error", ErrorCategory.NETWORK);
      const error = withCause(
        createQiError("MAIN", "Main error", ErrorCategory.SYSTEM),
        cause
      );

      const structured = toStructuredData(error);

      expect(structured.cause).toBeDefined();
      expect(structured.cause).toEqual({
        code: "CAUSE",
        message: "Cause error",
        category: ErrorCategory.NETWORK,
        context: {},
        cause: undefined,
        timestamp: cause.timestamp
      });
    });

    it("converts to human-readable string", () => {
      const error = createQiError(
        "STRING_TEST",
        "String test error",
        ErrorCategory.VALIDATION,
        { field: "email", value: "invalid" }
      );

      const str = toString(error);

      expect(str).toContain("[VALIDATION]");
      expect(str).toContain("STRING_TEST");
      expect(str).toContain("String test error");
      expect(str).toContain("field=email");
      expect(str).toContain("value=invalid");
    });

    it("converts to string with cause chain", () => {
      const rootCause = createQiError("ROOT", "Root error", ErrorCategory.NETWORK);
      const wrapper = withCause(
        createQiError("WRAPPER", "Wrapper error", ErrorCategory.SYSTEM),
        rootCause
      );

      const str = toString(wrapper);

      expect(str).toContain("WRAPPER: Wrapper error");
      expect(str).toContain("Caused by:");
      expect(str).toContain("ROOT: Root error");
    });
  });

  describe("Error Utilities", () => {
    let error: ReturnType<typeof createQiError>;

    beforeEach(() => {
      error = createQiError(
        "UTIL_TEST",
        "Utility test",
        ErrorCategory.CACHE,
        { key1: "value1", key2: 42, key3: true }
      );
    });

    it("gets error category", () => {
      expect(getCategory(error)).toBe(ErrorCategory.CACHE);
    });

    it("checks category membership", () => {
      expect(hasCategory(ErrorCategory.CACHE)(error)).toBe(true);
      expect(hasCategory(ErrorCategory.NETWORK)(error)).toBe(false);
    });

    it("gets context keys", () => {
      const keys = getContextKeys(error);
      expect(keys.sort()).toEqual(["key1", "key2", "key3"]);
    });

    it("gets context values", () => {
      expect(getContextValue("key1")(error)).toBe("value1");
      expect(getContextValue("key2")(error)).toBe(42);
      expect(getContextValue("key3")(error)).toBe(true);
      expect(getContextValue("nonexistent")(error)).toBeUndefined();
    });

    it("checks context key existence", () => {
      expect(hasContextKey("key1")(error)).toBe(true);
      expect(hasContextKey("nonexistent")(error)).toBe(false);
    });
  });

  describe("Cause Chain Traversal", () => {
    let errorChain: ReturnType<typeof withCause>;

    beforeEach(() => {
      const level1 = createQiError("LEVEL_1", "Level 1", ErrorCategory.FILESYSTEM);
      const level2 = withCause(
        createQiError("LEVEL_2", "Level 2", ErrorCategory.CACHE),
        level1
      );
      const level3 = withCause(
        createQiError("LEVEL_3", "Level 3", ErrorCategory.SYSTEM),
        level2
      );
      errorChain = level3;
    });

    it("gets all causes in chain", () => {
      const causes = getAllCauses(errorChain);
      
      expect(causes).toHaveLength(2);
      expect(causes[0].code).toBe("LEVEL_2");
      expect(causes[1].code).toBe("LEVEL_1");
    });

    it("finds error by code in chain", () => {
      const found = findByCode("LEVEL_2")(errorChain);
      expect(found?.code).toBe("LEVEL_2");

      const notFound = findByCode("NONEXISTENT")(errorChain);
      expect(notFound).toBeUndefined();

      const foundRoot = findByCode("LEVEL_3")(errorChain);
      expect(foundRoot?.code).toBe("LEVEL_3");
    });

    it("finds error by category in chain", () => {
      const found = findByCategory(ErrorCategory.FILESYSTEM)(errorChain);
      expect(found?.code).toBe("LEVEL_1");

      const notFound = findByCategory(ErrorCategory.NETWORK)(errorChain);
      expect(notFound).toBeUndefined();

      const foundRoot = findByCategory(ErrorCategory.SYSTEM)(errorChain);
      expect(foundRoot?.code).toBe("LEVEL_3");
    });

    it("handles empty cause chain", () => {
      const singleError = createQiError("SINGLE", "Single", ErrorCategory.VALIDATION);
      
      expect(getAllCauses(singleError)).toHaveLength(0);
      expect(findByCode("SINGLE")(singleError)?.code).toBe("SINGLE");
      expect(findByCode("OTHER")(singleError)).toBeUndefined();
    });
  });

  describe("Error Categories", () => {
    it("has all required categories", () => {
      const categories = Object.values(ErrorCategory);
      
      expect(categories).toContain("VALIDATION");
      expect(categories).toContain("NETWORK");
      expect(categories).toContain("FILESYSTEM");
      expect(categories).toContain("CONFIGURATION");
      expect(categories).toContain("CACHE");
      expect(categories).toContain("TIMEOUT");
      expect(categories).toContain("PERMISSION");
      expect(categories).toContain("SYSTEM");
    });

    it("creates errors with all categories", () => {
      for (const category of Object.values(ErrorCategory)) {
        const error = createQiError("TEST", "Test", category);
        expect(error.category).toBe(category);
        expect(hasCategory(category)(error)).toBe(true);
      }
    });
  });

  describe("Performance Requirements", () => {
    it("creates errors quickly", () => {
      const start = performance.now();
      
      for (let i = 0; i < 100; i++) {
        createQiError(`ERROR_${i}`, `Error ${i}`, ErrorCategory.SYSTEM);
      }
      
      const end = performance.now();
      const avgDuration = ((end - start) * 1000) / 100; // microseconds per operation
      
      // Should be well under 25μs per operation
      expect(avgDuration).toBeLessThan(25);
    });

    it("adds context quickly", () => {
      const baseError = createQiError("BASE", "Base", ErrorCategory.SYSTEM);
      const start = performance.now();
      
      for (let i = 0; i < 100; i++) {
        withContext(baseError, { iteration: i });
      }
      
      const end = performance.now();
      const avgDuration = ((end - start) * 1000) / 100;
      
      // Should be well under 15μs per operation
      expect(avgDuration).toBeLessThan(15);
    });
  });

  describe("Immutability Properties", () => {
    it("maintains immutability with context operations", () => {
      const original = createQiError(
        "IMMUTABLE_TEST",
        "Original",
        ErrorCategory.VALIDATION,
        { original: "data" }
      );

      const modified = withContext(original, { new: "data" });

      // Original unchanged
      expect(original.context.has("new")).toBe(false);
      expect(original.context.get("original")).toBe("data");

      // Modified has both
      expect(modified.context.has("new")).toBe(true);
      expect(modified.context.has("original")).toBe(true);

      // Different objects
      expect(original).not.toBe(modified);
      expect(original.context).not.toBe(modified.context);
    });

    it("maintains immutability with cause operations", () => {
      const original = createQiError("ORIG", "Original", ErrorCategory.SYSTEM);
      const cause = createQiError("CAUSE", "Cause", ErrorCategory.NETWORK);

      const chained = withCause(original, cause);

      expect(original.cause).toBeUndefined();
      expect(chained.cause).toBe(cause);
      expect(original).not.toBe(chained);
    });
  });
});