/**
 * Basic Result Tests for QiCore v4.0 Base - Coverage Boost
 *
 * Tests core Result functionality to boost coverage from 47% to 80%+
 */

import { type QiError, create as createError } from "@qicore/base/error";
import {
  QiResult,
  type Result,
  alt,
  ap,
  bimap,
  chain,
  chainFirst,
  failure,
  flatMap,
  fold,
  fromAsyncTryCatch,
  fromMaybe,
  fromPredicate,
  fromTryCatch,
  getData,
  getError,
  isFailure,
  isSuccess,
  liftA2,
  map,
  mapError,
  match,
  orElse,
  sequence,
  success,
  traverse,
  unwrap,
  unwrapOr,
  unwrapOrElse,
} from "@qicore/base/result";
import { describe, expect, it } from "vitest";

describe("Result Construction", () => {
  describe("success()", () => {
    it("should create successful result", () => {
      const result = success(42);

      expect(isSuccess(result)).toBe(true);
      expect(isFailure(result)).toBe(false);
      expect(getData(result)).toBe(42);
      expect(getError(result)).toBeNull();
    });

    it("should handle null and undefined", () => {
      const nullResult = success(null);
      const undefinedResult = success(undefined);

      expect(isSuccess(nullResult)).toBe(true);
      expect(isSuccess(undefinedResult)).toBe(true);
      expect(getData(nullResult)).toBeNull();
      expect(getData(undefinedResult)).toBeUndefined();
    });
  });

  describe("failure()", () => {
    it("should create failed result", () => {
      const error = createError("TEST", "Test error", "VALIDATION");
      const result = failure<number>(error);

      expect(isFailure(result)).toBe(true);
      expect(isSuccess(result)).toBe(false);
      expect(getData(result)).toBeNull();
      expect(getError(result)).toBe(error);
    });
  });
});

describe("Conversion Functions", () => {
  describe("fromTryCatch()", () => {
    it("should convert successful operation", () => {
      const result = fromTryCatch(() => 42);

      expect(isSuccess(result)).toBe(true);
      expect(getData(result)).toBe(42);
    });

    it("should convert throwing operation", () => {
      const result = fromTryCatch(() => {
        throw new Error("Operation failed");
      });

      expect(isFailure(result)).toBe(true);
      const error = getError(result);
      expect(error?.code).toBe("OPERATION_FAILED");
      expect(error?.message).toBe("Operation failed");
    });

    it("should handle non-Error throws", () => {
      const stringResult = fromTryCatch(() => {
        throw "String error";
      });

      const numberResult = fromTryCatch(() => {
        throw 123;
      });

      expect(isFailure(stringResult)).toBe(true);
      expect(isFailure(numberResult)).toBe(true);
    });
  });

  describe("fromAsyncTryCatch()", () => {
    it("should convert successful async operation", async () => {
      const result = await fromAsyncTryCatch(async () => {
        await new Promise((resolve) => setTimeout(resolve, 1));
        return 42;
      });

      expect(isSuccess(result)).toBe(true);
      expect(getData(result)).toBe(42);
    });

    it("should convert rejected async operation", async () => {
      const result = await fromAsyncTryCatch(async () => {
        throw new Error("Async failed");
      });

      expect(isFailure(result)).toBe(true);
      expect(getError(result)?.message).toBe("Async failed");
    });
  });

  describe("fromMaybe()", () => {
    it("should convert non-null to success", () => {
      const error = createError("NULL", "Null value", "VALIDATION");
      const result = fromMaybe(error, 42);

      expect(isSuccess(result)).toBe(true);
      expect(getData(result)).toBe(42);
    });

    it("should convert null to failure", () => {
      const error = createError("NULL", "Null value", "VALIDATION");
      const result = fromMaybe(error, null);

      expect(isFailure(result)).toBe(true);
      expect(getError(result)).toBe(error);
    });

    it("should convert undefined to failure", () => {
      const error = createError("UNDEFINED", "Undefined value", "VALIDATION");
      const result = fromMaybe(error, undefined);

      expect(isFailure(result)).toBe(true);
      expect(getError(result)).toBe(error);
    });

    it("should handle falsy but valid values", () => {
      const error = createError("NULL", "Null value", "VALIDATION");

      expect(isSuccess(fromMaybe(error, false))).toBe(true);
      expect(isSuccess(fromMaybe(error, 0))).toBe(true);
      expect(isSuccess(fromMaybe(error, ""))).toBe(true);
    });
  });

  describe("fromPredicate()", () => {
    const isPositive = (n: number) => n > 0;
    const error = createError("NOT_POSITIVE", "Must be positive", "VALIDATION");

    it("should convert passing predicate to success", () => {
      const result = fromPredicate(isPositive, error, 42);

      expect(isSuccess(result)).toBe(true);
      expect(getData(result)).toBe(42);
    });

    it("should convert failing predicate to failure", () => {
      const result = fromPredicate(isPositive, error, -5);

      expect(isFailure(result)).toBe(true);
      expect(getError(result)).toBe(error);
    });
  });
});

describe("Functor Operations", () => {
  describe("map()", () => {
    it("should transform successful result", () => {
      const result = success(5);
      const mapped = map((x: number) => x * 2)(result);

      expect(isSuccess(mapped)).toBe(true);
      expect(getData(mapped)).toBe(10);
    });

    it("should not transform failed result", () => {
      const error = createError("TEST", "Test", "VALIDATION");
      const result = failure<number>(error);
      const mapped = map((x: number) => x * 2)(result);

      expect(isFailure(mapped)).toBe(true);
      expect(getError(mapped)).toBe(error);
    });

    it("should handle type transformations", () => {
      const result = success(42);
      const mapped = map((x: number) => x.toString())(result);

      expect(getData(mapped)).toBe("42");
    });
  });

  describe("mapError()", () => {
    it("should transform error in failed result", () => {
      const originalError = createError("ORIGINAL", "Original", "VALIDATION");
      const result = failure<number>(originalError);

      const mapped = mapError((err: QiError) =>
        createError("TRANSFORMED", `Transformed: ${err.message}`, "SYSTEM")
      )(result);

      expect(isFailure(mapped)).toBe(true);
      const newError = getError(mapped);
      expect(newError?.code).toBe("TRANSFORMED");
      expect(newError?.message).toBe("Transformed: Original");
    });

    it("should not transform successful result", () => {
      const result = success(42);
      const mapped = mapError((_: QiError) =>
        createError("SHOULD_NOT_USE", "Should not use", "SYSTEM")
      )(result);

      expect(isSuccess(mapped)).toBe(true);
      expect(getData(mapped)).toBe(42);
    });
  });

  describe("bimap()", () => {
    it("should transform error in failed result", () => {
      const error = createError("TEST", "Test", "VALIDATION");
      const result = failure<number>(error);

      const bimapped = bimap(
        (err: QiError) => createError("MAPPED", `Mapped: ${err.message}`, "SYSTEM"),
        (x: number) => x * 2
      )(result);

      expect(isFailure(bimapped)).toBe(true);
      expect(getError(bimapped)?.code).toBe("MAPPED");
    });

    it("should transform value in successful result", () => {
      const result = success(21);

      const bimapped = bimap(
        (_: QiError) => createError("UNUSED", "Unused", "SYSTEM"),
        (x: number) => x * 2
      )(result);

      expect(isSuccess(bimapped)).toBe(true);
      expect(getData(bimapped)).toBe(42);
    });
  });
});

describe("Monad Operations", () => {
  describe("flatMap() and chain()", () => {
    it("should chain successful operations", () => {
      const result = success(5);
      const chained = flatMap((x: number) => success(x * 2))(result);

      expect(isSuccess(chained)).toBe(true);
      expect(getData(chained)).toBe(10);
    });

    it("should short-circuit on first failure", () => {
      const error = createError("FIRST", "First error", "VALIDATION");
      const result = failure<number>(error);
      const chained = flatMap((x: number) => success(x * 2))(result);

      expect(isFailure(chained)).toBe(true);
      expect(getError(chained)).toBe(error);
    });

    it("should propagate second failure", () => {
      const secondError = createError("SECOND", "Second error", "NETWORK");
      const result = success(5);
      const chained = flatMap((_: number) => failure<number>(secondError))(result);

      expect(isFailure(chained)).toBe(true);
      expect(getError(chained)).toBe(secondError);
    });

    it("should verify chain is alias for flatMap", () => {
      const result = success(5);
      const flatMapped = flatMap((x: number) => success(x * 2))(result);
      const chained = chain((x: number) => success(x * 2))(result);

      expect(getData(flatMapped)).toBe(getData(chained));
    });
  });

  describe("chainFirst()", () => {
    it("should execute operation but keep original value", () => {
      const result = success(42);
      let sideEffect = 0;

      const chained = chainFirst((x: number) => {
        sideEffect = x;
        return success("ignored");
      })(result);

      expect(isSuccess(chained)).toBe(true);
      expect(getData(chained)).toBe(42);
      expect(sideEffect).toBe(42);
    });

    it("should fail if chained operation fails", () => {
      const error = createError("CHAIN_ERROR", "Chain error", "VALIDATION");
      const result = success(42);

      const chained = chainFirst((_: number) => failure<string>(error))(result);

      expect(isFailure(chained)).toBe(true);
      expect(getError(chained)).toBe(error);
    });
  });
});

describe("Applicative Operations", () => {
  describe("ap()", () => {
    it("should apply function to value when both successful", () => {
      const funcResult = success((x: number) => x * 2);
      const valueResult = success(21);

      const applied = ap(valueResult)(funcResult);

      expect(isSuccess(applied)).toBe(true);
      expect(getData(applied)).toBe(42);
    });

    it("should fail if function result is failure", () => {
      const error = createError("FUNC_ERROR", "Function error", "VALIDATION");
      const funcResult = failure<(x: number) => number>(error);
      const valueResult = success(21);

      const applied = ap(valueResult)(funcResult);

      expect(isFailure(applied)).toBe(true);
      expect(getError(applied)).toBe(error);
    });

    it("should fail if value result is failure", () => {
      const error = createError("VALUE_ERROR", "Value error", "VALIDATION");
      const funcResult = success((x: number) => x * 2);
      const valueResult = failure<number>(error);

      const applied = ap(valueResult)(funcResult);

      expect(isFailure(applied)).toBe(true);
      expect(getError(applied)).toBe(error);
    });
  });

  describe("liftA2()", () => {
    it("should lift binary function over two successful results", () => {
      const add = (a: number, b: number) => a + b;
      const result1 = success(5);
      const result2 = success(3);

      const lifted = liftA2(add)(result1)(result2);

      expect(isSuccess(lifted)).toBe(true);
      expect(getData(lifted)).toBe(8);
    });

    it("should fail if first result is failure", () => {
      const error = createError("FIRST_ERROR", "First error", "VALIDATION");
      const add = (a: number, b: number) => a + b;
      const result1 = failure<number>(error);
      const result2 = success(3);

      const lifted = liftA2(add)(result1)(result2);

      expect(isFailure(lifted)).toBe(true);
      expect(getError(lifted)).toBe(error);
    });
  });
});

describe("Alternative Operations", () => {
  describe("alt()", () => {
    it("should return first result if successful", () => {
      const result1 = success(42);
      const result2 = success(24);

      const alternative = alt(result2)(result1);

      expect(isSuccess(alternative)).toBe(true);
      expect(getData(alternative)).toBe(42);
    });

    it("should return second result if first fails", () => {
      const error = createError("FIRST_ERROR", "First error", "VALIDATION");
      const result1 = failure<number>(error);
      const result2 = success(24);

      const alternative = alt(result2)(result1);

      expect(isSuccess(alternative)).toBe(true);
      expect(getData(alternative)).toBe(24);
    });
  });

  describe("orElse()", () => {
    it("should return original if successful", () => {
      const result = success(42);
      const fallback = () => success(24);

      const orElseResult = orElse(fallback)(result);

      expect(isSuccess(orElseResult)).toBe(true);
      expect(getData(orElseResult)).toBe(42);
    });

    it("should execute fallback if original fails", () => {
      const error = createError("ORIGINAL", "Original error", "VALIDATION");
      const result = failure<number>(error);
      const fallback = (err: QiError) => success(err.message.length);

      const orElseResult = orElse(fallback)(result);

      expect(isSuccess(orElseResult)).toBe(true);
      expect(getData(orElseResult)).toBe("Original error".length);
    });
  });
});

describe("Pattern Matching and Unwrapping", () => {
  describe("match() and fold()", () => {
    it("should execute success handler for successful result", () => {
      const result = success(42);

      const matched = match(
        (value: number) => `Value: ${value}`,
        (error: QiError) => `Error: ${error.message}`
      )(result);

      expect(matched).toBe("Value: 42");
    });

    it("should execute error handler for failed result", () => {
      const error = createError("TEST_ERROR", "Test error", "VALIDATION");
      const result = failure<number>(error);

      const matched = match(
        (value: number) => `Value: ${value}`,
        (err: QiError) => `Error: ${err.message}`
      )(result);

      expect(matched).toBe("Error: Test error");
    });

    it("should verify fold is alias for match", () => {
      const result = success(42);
      const errorHandler = (error: QiError) => `Error: ${error.message}`;
      const successHandler = (value: number) => `Value: ${value}`;

      const matched = match(successHandler, errorHandler)(result);
      const folded = fold(errorHandler, successHandler)(result);

      expect(matched).toBe(folded);
    });
  });

  describe("unwrap()", () => {
    it("should return value for successful result", () => {
      const result = success(42);
      const value = unwrap(result);

      expect(value).toBe(42);
    });

    it("should throw for failed result", () => {
      const error = createError("TEST_ERROR", "Test error", "VALIDATION");
      const result = failure<number>(error);

      expect(() => unwrap(result)).toThrow();
    });
  });

  describe("unwrapOr()", () => {
    it("should return value for successful result", () => {
      const result = success(42);
      const value = unwrapOr(0)(result);

      expect(value).toBe(42);
    });

    it("should return default for failed result", () => {
      const error = createError("TEST_ERROR", "Test error", "VALIDATION");
      const result = failure<number>(error);
      const value = unwrapOr(99)(result);

      expect(value).toBe(99);
    });
  });

  describe("unwrapOrElse()", () => {
    it("should return value for successful result", () => {
      const result = success(42);
      const value = unwrapOrElse((error: QiError) => error.message.length)(result);

      expect(value).toBe(42);
    });

    it("should execute function for failed result", () => {
      const error = createError("TEST_ERROR", "Test error", "VALIDATION");
      const result = failure<number>(error);
      const value = unwrapOrElse((err: QiError) => err.message.length)(result);

      expect(value).toBe("Test error".length);
    });
  });
});

describe("Sequence and Traverse Operations", () => {
  describe("sequence()", () => {
    it("should convert array of successful results to successful array", () => {
      const results = [success(1), success(2), success(3)];
      const sequenced = sequence(results);

      expect(isSuccess(sequenced)).toBe(true);
      expect(getData(sequenced)).toEqual([1, 2, 3]);
    });

    it("should fail if any result is failure", () => {
      const error = createError("MIDDLE_ERROR", "Middle error", "VALIDATION");
      const results = [success(1), failure<number>(error), success(3)];
      const sequenced = sequence(results);

      expect(isFailure(sequenced)).toBe(true);
      expect(getError(sequenced)).toBe(error);
    });

    it("should handle empty array", () => {
      const results: Result<number>[] = [];
      const sequenced = sequence(results);

      expect(isSuccess(sequenced)).toBe(true);
      expect(getData(sequenced)).toEqual([]);
    });
  });

  describe("traverse()", () => {
    it("should map and sequence in one operation", () => {
      const numbers = [1, 2, 3];
      const safeDouble = (n: number) => success(n * 2);

      const traversed = traverse(safeDouble)(numbers);

      expect(isSuccess(traversed)).toBe(true);
      expect(getData(traversed)).toEqual([2, 4, 6]);
    });

    it("should fail if any mapping fails", () => {
      const error = createError("NEGATIVE_ERROR", "Negative not allowed", "VALIDATION");
      const numbers = [1, -2, 3];
      const safePositive = (n: number) => (n > 0 ? success(n) : failure<number>(error));

      const traversed = traverse(safePositive)(numbers);

      expect(isFailure(traversed)).toBe(true);
      expect(getError(traversed)).toBe(error);
    });

    it("should handle empty array", () => {
      const numbers: number[] = [];
      const safeDouble = (n: number) => success(n * 2);

      const traversed = traverse(safeDouble)(numbers);

      expect(isSuccess(traversed)).toBe(true);
      expect(getData(traversed)).toEqual([]);
    });
  });
});

describe("QiResult API Object", () => {
  it("should provide all operations as methods", () => {
    expect(typeof QiResult.success).toBe("function");
    expect(typeof QiResult.failure).toBe("function");
    expect(typeof QiResult.fromTryCatch).toBe("function");
    expect(typeof QiResult.fromAsyncTryCatch).toBe("function");
    expect(typeof QiResult.map).toBe("function");
    expect(typeof QiResult.flatMap).toBe("function");
    expect(typeof QiResult.match).toBe("function");
    expect(typeof QiResult.sequence).toBe("function");
    expect(typeof QiResult.isSuccess).toBe("function");
    expect(typeof QiResult.isFailure).toBe("function");
  });
});

describe("Performance and Edge Cases", () => {
  it("should handle large arrays in sequence", () => {
    const largeArray = Array.from({ length: 1000 }, (_, i) => success(i));
    const sequenced = sequence(largeArray);

    expect(isSuccess(sequenced)).toBe(true);
    expect(getData(sequenced)?.length).toBe(1000);
    expect(getData(sequenced)?.[999]).toBe(999);
  });

  it("should handle deeply nested operations", () => {
    let result: Result<number> = success(0);

    // Chain 100 operations
    for (let i = 0; i < 100; i++) {
      result = flatMap((x: number) => success(x + 1))(result);
    }

    expect(isSuccess(result)).toBe(true);
    expect(getData(result)).toBe(100);
  });

  it("should handle complex object transformations", () => {
    const complexObject = {
      id: 123,
      name: "test",
      metadata: { tags: ["tag1", "tag2"] },
      items: [
        { id: 1, value: "first" },
        { id: 2, value: "second" },
      ],
    };

    const result = map((obj: typeof complexObject) => ({
      ...obj,
      metadata: {
        ...obj.metadata,
        tags: [...obj.metadata.tags, "processed"],
      },
      processed: true,
    }))(success(complexObject));

    expect(isSuccess(result)).toBe(true);
    const data = getData(result);
    expect(data?.metadata.tags).toContain("processed");
  });
});
