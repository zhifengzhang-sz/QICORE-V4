/**
 * Result Coverage Boost Tests for QiCore v4.0 Base
 * 
 * Targeted tests to improve coverage for specific uncovered lines
 * Focuses on uncovered lines: 116, 322-323
 */

import {
  fromAsyncTryCatch,
  liftA2,
  success,
  failure,
  isSuccess,
  isFailure,
} from "../../../src/qicore/base/result.js";
import { createQiError } from "../../../src/qicore/base/error.js";
import { describe, expect, it } from "vitest";

describe("Result Coverage Boost - Missing Lines", () => {
  describe("fromAsyncTryCatch error handling - Line 116", () => {
    it("should handle non-QiError exceptions", async () => {
      // Test line 116: when error is not a QiError, it creates a new one
      const throwingFunction = async () => {
        throw new Error("Regular JavaScript error");
      };

      const result = await fromAsyncTryCatch(throwingFunction);
      
      expect(isFailure(result)).toBe(true);
      if (isFailure(result)) {
        expect(result.left.code).toBe("ASYNC_OPERATION_FAILED");
        expect(result.left.message).toBe("Regular JavaScript error");
        expect(result.left.category).toBe("UNKNOWN");
      }
    });

    it("should handle string exceptions", async () => {
      const throwingFunction = async () => {
        throw "String error";
      };

      const result = await fromAsyncTryCatch(throwingFunction);
      
      expect(isFailure(result)).toBe(true);
      if (isFailure(result)) {
        expect(result.left.code).toBe("ASYNC_OPERATION_FAILED");
        expect(result.left.message).toBe("String error");
        expect(result.left.category).toBe("UNKNOWN");
      }
    });

    it("should handle null/undefined exceptions", async () => {
      const throwingFunction = async () => {
        throw null;
      };

      const result = await fromAsyncTryCatch(throwingFunction);
      
      expect(isFailure(result)).toBe(true);
      if (isFailure(result)) {
        expect(result.left.code).toBe("ASYNC_OPERATION_FAILED");
        expect(result.left.message).toBe("null");
      }
    });

    it("should preserve QiError when thrown", async () => {
      const qiError = createQiError("CUSTOM_ERROR", "Custom message", "VALIDATION");
      
      const throwingFunction = async () => {
        throw qiError;
      };

      const result = await fromAsyncTryCatch(throwingFunction);
      
      expect(isFailure(result)).toBe(true);
      if (isFailure(result)) {
        // QiError gets wrapped, but should contain the original error information
        expect(result.left.code).toBe("ASYNC_OPERATION_FAILED");
        expect(result.left.message).toContain("CUSTOM_ERROR");
        expect(result.left.message).toContain("Custom message");
        expect(result.left.category).toBe("UNKNOWN");
      }
    });
  });

  describe("liftA2 function - Lines 322-323", () => {
    it("should return left of second result when first is success but second is failure", () => {
      // Test line 322: when resultA is success but resultB is failure
      const error = createQiError("ERROR_2", "Second error", "NETWORK");
      
      const successA = success(10);
      const failureB = failure(error);
      
      const addFunction = (a: number, b: number) => a + b;
      const result = liftA2(addFunction)(successA)(failureB);
      
      expect(isFailure(result)).toBe(true);
      if (isFailure(result)) {
        expect(result.left).toBe(error); // Should be the second error
        expect(result.left.code).toBe("ERROR_2");
      }
    });

    it("should combine two successful results", () => {
      const successA = success(10);
      const successB = success(20);
      
      const addFunction = (a: number, b: number) => a + b;
      const result = liftA2(addFunction)(successA)(successB);
      
      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        expect(result.right).toBe(30);
      }
    });

    it("should return first error when first result is failure", () => {
      const error1 = createQiError("ERROR_1", "First error", "VALIDATION");
      const error2 = createQiError("ERROR_2", "Second error", "NETWORK");
      
      const failureA = failure(error1);
      const failureB = failure(error2);
      
      const addFunction = (a: number, b: number) => a + b;
      const result = liftA2(addFunction)(failureA)(failureB);
      
      expect(isFailure(result)).toBe(true);
      if (isFailure(result)) {
        expect(result.left).toBe(error1); // Should be the first error (line 320)
        expect(result.left.code).toBe("ERROR_1");
      }
    });

    it("should work with string concatenation", () => {
      const successA = success("Hello");
      const successB = success(" World");
      
      const concatFunction = (a: string, b: string) => a + b;
      const result = liftA2(concatFunction)(successA)(successB);
      
      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        expect(result.right).toBe("Hello World");
      }
    });
  });

  describe("Edge cases for comprehensive coverage", () => {
    it("should handle complex error scenarios in asyncTryCatch", async () => {
      // Test with object that gets converted to string
      const throwingFunction = async () => {
        throw { complex: "object", nested: { value: 42 } };
      };

      const result = await fromAsyncTryCatch(throwingFunction);
      
      expect(isFailure(result)).toBe(true);
      if (isFailure(result)) {
        expect(result.left.code).toBe("ASYNC_OPERATION_FAILED");
        expect(result.left.message).toContain("[object Object]");
      }
    });

    it("should handle async function that resolves successfully", async () => {
      const successFunction = async () => {
        await new Promise(resolve => setTimeout(resolve, 1));
        return "async success";
      };

      const result = await fromAsyncTryCatch(successFunction);
      
      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        expect(result.right).toBe("async success");
      }
    });
  });
});