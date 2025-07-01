/**
 * QiCore v4.0 - Result Component Tests
 * 
 * Comprehensive tests for Result<T> monad including mathematical law verification
 */

import { describe, it, expect } from "vitest";
import {
  success,
  failure,
  map,
  flatMap,
  fold,
  unwrap,
  unwrapOr,
  orElse,
  isSuccess,
  isFailure,
  getError,
  getValue,
  fromTryCatch,
  fromAsyncTryCatch,
  sequence,
  traverse,
  apply,
  lift2,
  lift3,
  filter,
  tap,
  tapError,
  bimap,
  mapError,
  type Result
} from "../../../src/qicore/base/result.js";
import { createQiError, ErrorCategory } from "../../../src/qicore/base/error.js";

describe("Result<T> Component", () => {
  const testError = createQiError("TEST_ERROR", "Test error", ErrorCategory.VALIDATION);
  const testError2 = createQiError("TEST_ERROR_2", "Second test error", ErrorCategory.NETWORK);

  describe("Basic Operations", () => {
    it("creates successful results", () => {
      const result = success(42);
      
      expect(result._tag).toBe("Right");
      expect(result.right).toBe(42);
      expect(isSuccess(result)).toBe(true);
      expect(isFailure(result)).toBe(false);
    });

    it("creates failed results", () => {
      const result = failure<number>(testError);
      
      expect(result._tag).toBe("Left");
      expect(result.left).toBe(testError);
      expect(isSuccess(result)).toBe(false);
      expect(isFailure(result)).toBe(true);
    });

    it("extracts values safely", () => {
      const successResult = success(42);
      const failureResult = failure<number>(testError);

      expect(getValue(successResult)).toBe(42);
      expect(getValue(failureResult)).toBeUndefined();
      
      expect(getError(successResult)).toBeUndefined();
      expect(getError(failureResult)).toBe(testError);
    });
  });

  describe("Functor Laws", () => {
    it("satisfies identity law: map(id) = id", () => {
      const identity = <T>(x: T): T => x;
      
      const successResult = success(42);
      const failureResult = failure<number>(testError);

      expect(map(identity)(successResult)).toEqual(successResult);
      expect(map(identity)(failureResult)).toEqual(failureResult);
    });

    it("satisfies composition law: map(g ∘ f) = map(g) ∘ map(f)", () => {
      const f = (x: number): string => x.toString();
      const g = (x: string): number => x.length;
      const composed = (x: number): number => g(f(x));

      const result = success(123);
      
      const leftSide = map(composed)(result);
      const rightSide = map(g)(map(f)(result));

      expect(leftSide).toEqual(rightSide);
    });

    it("preserves structure for failures", () => {
      const f = (x: number): string => x.toString();
      const failureResult = failure<number>(testError);

      const mapped = map(f)(failureResult);
      
      expect(mapped._tag).toBe("Left");
      expect(mapped.left).toBe(testError);
    });
  });

  describe("Monad Laws", () => {
    const f = (x: number): Result<string> => success(x.toString());
    const g = (x: string): Result<number> => success(x.length);

    it("satisfies left identity: return(a) >>= f ≡ f(a)", () => {
      const value = 42;
      
      const leftSide = flatMap(f)(success(value));
      const rightSide = f(value);

      expect(leftSide).toEqual(rightSide);
    });

    it("satisfies right identity: m >>= return ≡ m", () => {
      const m = success(42);
      
      const leftSide = flatMap(success)(m);
      const rightSide = m;

      expect(leftSide).toEqual(rightSide);
    });

    it("satisfies right identity for failures", () => {
      const m = failure<number>(testError);
      
      const leftSide = flatMap(success)(m);
      const rightSide = m;

      expect(leftSide).toEqual(rightSide);
    });

    it("satisfies associativity: (m >>= f) >>= g ≡ m >>= (\\x -> f(x) >>= g)", () => {
      const m = success(42);
      
      const leftSide = flatMap(g)(flatMap(f)(m));
      const rightSide = flatMap((x: number) => flatMap(g)(f(x)))(m);

      expect(leftSide).toEqual(rightSide);
    });

    it("short-circuits on failure", () => {
      const failureResult = failure<number>(testError);
      
      const result = flatMap(f)(failureResult);
      
      expect(result._tag).toBe("Left");
      expect(result.left).toBe(testError);
    });
  });

  describe("Fold Operations", () => {
    it("folds successful results", () => {
      const result = success(42);
      
      const folded = fold(
        (error) => `Error: ${error.code}`,
        (value) => `Success: ${value}`
      )(result);

      expect(folded).toBe("Success: 42");
    });

    it("folds failed results", () => {
      const result = failure<number>(testError);
      
      const folded = fold(
        (error) => `Error: ${error.code}`,
        (value) => `Success: ${value}`
      )(result);

      expect(folded).toBe("Error: TEST_ERROR");
    });
  });

  describe("Unwrap Operations", () => {
    it("unwraps successful results", () => {
      const result = success(42);
      expect(unwrap(result)).toBe(42);
    });

    it("throws on unwrapping failures", () => {
      const result = failure<number>(testError);
      expect(() => unwrap(result)).toThrow("Unwrapped failed Result");
    });

    it("unwraps with default value", () => {
      const successResult = success(42);
      const failureResult = failure<number>(testError);

      expect(unwrapOr(0)(successResult)).toBe(42);
      expect(unwrapOr(0)(failureResult)).toBe(0);
    });
  });

  describe("Alternative Operations", () => {
    it("uses alternative on failure", () => {
      const result = failure<number>(testError);
      const alternative = (error: any) => success(99);

      const recovered = orElse(alternative)(result);
      
      expect(recovered._tag).toBe("Right");
      expect(recovered.right).toBe(99);
    });

    it("ignores alternative on success", () => {
      const result = success(42);
      const alternative = (error: any) => success(99);

      const recovered = orElse(alternative)(result);
      
      expect(recovered).toEqual(result);
    });
  });

  describe("Try-Catch Wrappers", () => {
    it("wraps successful operations", () => {
      const result = fromTryCatch(() => 42);
      
      expect(result._tag).toBe("Right");
      expect(result.right).toBe(42);
    });

    it("wraps throwing operations", () => {
      const result = fromTryCatch(() => {
        throw new Error("Test error");
      });
      
      expect(result._tag).toBe("Left");
      expect(result.left.code).toBe("OPERATION_FAILED");
      expect(result.left.message).toBe("Test error");
    });

    it("wraps async successful operations", async () => {
      const result = await fromAsyncTryCatch(async () => 42);
      
      expect(result._tag).toBe("Right");
      expect(result.right).toBe(42);
    });

    it("wraps async throwing operations", async () => {
      const result = await fromAsyncTryCatch(async () => {
        throw new Error("Async test error");
      });
      
      expect(result._tag).toBe("Left");
      expect(result.left.code).toBe("ASYNC_OPERATION_FAILED");
      expect(result.left.message).toBe("Async test error");
    });
  });

  describe("Sequence Operations", () => {
    it("sequences successful results", () => {
      const results = [success(1), success(2), success(3)];
      const sequenced = sequence(results);
      
      expect(sequenced._tag).toBe("Right");
      expect(sequenced.right).toEqual([1, 2, 3]);
    });

    it("fails on first failure", () => {
      const results = [success(1), failure<number>(testError), success(3)];
      const sequenced = sequence(results);
      
      expect(sequenced._tag).toBe("Left");
      expect(sequenced.left).toBe(testError);
    });

    it("handles empty array", () => {
      const results: Result<number>[] = [];
      const sequenced = sequence(results);
      
      expect(sequenced._tag).toBe("Right");
      expect(sequenced.right).toEqual([]);
    });
  });

  describe("Traverse Operations", () => {
    it("traverses with successful function", () => {
      const values = [1, 2, 3];
      const f = (x: number): Result<string> => success(x.toString());
      
      const result = traverse(f)(values);
      
      expect(result._tag).toBe("Right");
      expect(result.right).toEqual(["1", "2", "3"]);
    });

    it("fails on first function failure", () => {
      const values = [1, 2, 3];
      const f = (x: number): Result<string> => 
        x === 2 ? failure(testError) : success(x.toString());
      
      const result = traverse(f)(values);
      
      expect(result._tag).toBe("Left");
      expect(result.left).toBe(testError);
    });
  });

  describe("Applicative Operations", () => {
    it("applies function in Result to value in Result", () => {
      const f = success((x: number) => x * 2);
      const value = success(21);
      
      const result = apply(f)(value);
      
      expect(result._tag).toBe("Right");
      expect(result.right).toBe(42);
    });

    it("fails if function Result fails", () => {
      const f = failure<(x: number) => number>(testError);
      const value = success(21);
      
      const result = apply(f)(value);
      
      expect(result._tag).toBe("Left");
      expect(result.left).toBe(testError);
    });

    it("fails if value Result fails", () => {
      const f = success((x: number) => x * 2);
      const value = failure<number>(testError);
      
      const result = apply(f)(value);
      
      expect(result._tag).toBe("Left");
      expect(result.left).toBe(testError);
    });

    it("lifts binary functions", () => {
      const add = (a: number, b: number): number => a + b;
      
      const result = lift2(add)(success(2), success(3));
      
      expect(result._tag).toBe("Right");
      expect(result.right).toBe(5);
    });

    it("lifts ternary functions", () => {
      const add3 = (a: number, b: number, c: number): number => a + b + c;
      
      const result = lift3(add3)(success(1), success(2), success(3));
      
      expect(result._tag).toBe("Right");
      expect(result.right).toBe(6);
    });
  });

  describe("Filter Operations", () => {
    it("filters successful values", () => {
      const result = success(42);
      const predicate = (x: number) => x > 0;
      const onFailure = (x: number) => createQiError("NEGATIVE", "Negative number", ErrorCategory.VALIDATION);
      
      const filtered = filter(predicate, onFailure)(result);
      
      expect(filtered._tag).toBe("Right");
      expect(filtered.right).toBe(42);
    });

    it("converts to failure when predicate fails", () => {
      const result = success(-5);
      const predicate = (x: number) => x > 0;
      const onFailure = (x: number) => createQiError("NEGATIVE", "Negative number", ErrorCategory.VALIDATION);
      
      const filtered = filter(predicate, onFailure)(result);
      
      expect(filtered._tag).toBe("Left");
      expect(filtered.left.code).toBe("NEGATIVE");
    });

    it("preserves failures", () => {
      const result = failure<number>(testError);
      const predicate = (x: number) => x > 0;
      const onFailure = (x: number) => createQiError("NEGATIVE", "Negative number", ErrorCategory.VALIDATION);
      
      const filtered = filter(predicate, onFailure)(result);
      
      expect(filtered._tag).toBe("Left");
      expect(filtered.left).toBe(testError);
    });
  });

  describe("Side Effect Operations", () => {
    it("performs side effects on success", () => {
      let sideEffectCalled = false;
      const result = success(42);
      
      const tapped = tap((value) => {
        sideEffectCalled = true;
        expect(value).toBe(42);
      })(result);
      
      expect(sideEffectCalled).toBe(true);
      expect(tapped).toEqual(result);
    });

    it("skips side effects on failure", () => {
      let sideEffectCalled = false;
      const result = failure<number>(testError);
      
      const tapped = tap((value) => {
        sideEffectCalled = true;
      })(result);
      
      expect(sideEffectCalled).toBe(false);
      expect(tapped).toEqual(result);
    });

    it("performs side effects on errors", () => {
      let sideEffectCalled = false;
      const result = failure<number>(testError);
      
      const tapped = tapError((error) => {
        sideEffectCalled = true;
        expect(error).toBe(testError);
      })(result);
      
      expect(sideEffectCalled).toBe(true);
      expect(tapped).toEqual(result);
    });

    it("skips error side effects on success", () => {
      let sideEffectCalled = false;
      const result = success(42);
      
      const tapped = tapError((error) => {
        sideEffectCalled = true;
      })(result);
      
      expect(sideEffectCalled).toBe(false);
      expect(tapped).toEqual(result);
    });
  });

  describe("Bimap Operations", () => {
    it("maps successful values", () => {
      const result = success(42);
      const mapError = (error: any) => createQiError("MAPPED", "Mapped error", ErrorCategory.SYSTEM);
      const mapSuccess = (value: number) => value.toString();
      
      const mapped = bimap(mapError, mapSuccess)(result);
      
      expect(mapped._tag).toBe("Right");
      expect(mapped.right).toBe("42");
    });

    it("maps error values", () => {
      const result = failure<number>(testError);
      const mapError = (error: any) => createQiError("MAPPED", "Mapped error", ErrorCategory.SYSTEM);
      const mapSuccess = (value: number) => value.toString();
      
      const mapped = bimap(mapError, mapSuccess)(result);
      
      expect(mapped._tag).toBe("Left");
      expect(mapped.left.code).toBe("MAPPED");
    });

    it("maps only errors", () => {
      const successResult = success(42);
      const failureResult = failure<number>(testError);
      const mapFn = (error: any) => createQiError("MAPPED", "Mapped error", ErrorCategory.SYSTEM);
      
      const mappedSuccess = mapError(mapFn)(successResult);
      const mappedFailure = mapError(mapFn)(failureResult);
      
      expect(mappedSuccess).toEqual(successResult);
      expect(mappedFailure._tag).toBe("Left");
      expect(mappedFailure.left.code).toBe("MAPPED");
    });
  });

  describe("Performance Requirements", () => {
    it("creates results quickly", () => {
      const start = performance.now();
      
      for (let i = 0; i < 1000; i++) {
        success(i);
        failure(testError);
      }
      
      const end = performance.now();
      const avgDuration = ((end - start) * 1000) / 2000; // microseconds per operation
      
      // Should be well under 5μs per operation
      expect(avgDuration).toBeLessThan(5);
    });

    it("maps results quickly", () => {
      const results = Array.from({ length: 1000 }, (_, i) => success(i));
      const mapFn = (x: number) => x * 2;
      
      const start = performance.now();
      
      results.forEach(result => map(mapFn)(result));
      
      const end = performance.now();
      const avgDuration = ((end - start) * 1000) / 1000;
      
      // Should be well under 20μs per operation
      expect(avgDuration).toBeLessThan(20);
    });

    it("flatMaps results quickly", () => {
      const results = Array.from({ length: 1000 }, (_, i) => success(i));
      const flatMapFn = (x: number) => success(x.toString());
      
      const start = performance.now();
      
      results.forEach(result => flatMap(flatMapFn)(result));
      
      const end = performance.now();
      const avgDuration = ((end - start) * 1000) / 1000;
      
      // Should be well under 40μs per operation
      expect(avgDuration).toBeLessThan(40);
    });
  });

  describe("Type Safety", () => {
    it("maintains type safety through transformations", () => {
      const numberResult: Result<number> = success(42);
      const stringResult: Result<string> = map((x: number) => x.toString())(numberResult);
      const lengthResult: Result<number> = map((x: string) => x.length)(stringResult);
      
      expect(lengthResult._tag).toBe("Right");
      expect(lengthResult.right).toBe(2);
    });

    it("maintains type safety through flatMap", () => {
      const numberResult: Result<number> = success(42);
      const maybeString: Result<string> = flatMap((x: number) => 
        x > 0 ? success(x.toString()) : failure(testError)
      )(numberResult);
      
      expect(maybeString._tag).toBe("Right");
      expect(maybeString.right).toBe("42");
    });
  });

  describe("Error Propagation", () => {
    it("propagates first error in chain", () => {
      const result = failure<number>(testError);
      
      const chained = map((x: string) => x.length)(
        flatMap((x: number) => success(x.toString()))(
          map((x: number) => x * 2)(result)
        )
      );
      
      expect(chained._tag).toBe("Left");
      expect(chained.left).toBe(testError);
    });

    it("stops execution on first error", () => {
      let executed = false;
      const result = failure<number>(testError);
      
      const chained = flatMap((x: number) => {
        executed = true;
        return success(x * 2);
      })(result);
      
      expect(executed).toBe(false);
      expect(chained._tag).toBe("Left");
    });
  });
});