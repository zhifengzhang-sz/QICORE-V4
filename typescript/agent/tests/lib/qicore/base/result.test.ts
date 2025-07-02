import { describe, expect, it } from "vitest";
import {
	alt,
	ap,
	bimap,
	chainFirst,
	createQiError,
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
	type ResultType,
	sequence,
	success,
	traverse,
	unwrap,
	unwrapOr,
	unwrapOrElse,
} from "../../../../lib/src/qicore/base/index.js";

describe("Result Type", () => {
	describe("Construction", () => {
		it("should create success result", () => {
			const result = success(42);
			expect(isSuccess(result)).toBe(true);
			expect(isFailure(result)).toBe(false);
			expect(getData(result)).toBe(42);
		});

		it("should create failure result", () => {
			const error = createQiError("TEST_ERROR", "Test error message", "VALIDATION");
			const result = failure<number>(error);
			expect(isSuccess(result)).toBe(false);
			expect(isFailure(result)).toBe(true);
			expect(getError(result)).toBe(error);
		});
	});

	describe("Functor Laws", () => {
		it("should satisfy identity law: map(id) = id", () => {
			const identity = <T>(x: T): T => x;
			const result = success(42);
			const mapped = map(identity)(result);
			expect(mapped).toEqual(result);
		});

		it("should satisfy composition law: map(g ∘ f) = map(g) ∘ map(f)", () => {
			const f = (x: number) => x * 2;
			const g = (x: number) => x + 1;
			const compose = (x: number) => g(f(x));

			const result = success(21);
			const composed = map(compose)(result);
			const sequential = map(g)(map(f)(result));

			expect(composed).toEqual(sequential);
		});

		it("should preserve failures in map", () => {
			const error = createQiError("TEST_ERROR", "Test error", "VALIDATION");
			const result = failure<number>(error);
			const mapped = map((x: number) => x * 2)(result);

			expect(isFailure(mapped)).toBe(true);
			expect(getError(mapped)).toBe(error);
		});
	});

	describe("Monad Laws", () => {
		it("should satisfy left identity: flatMap(unit(a), f) = f(a)", () => {
			const a = 42;
			const f = (x: number) => success(x * 2);

			const leftSide = flatMap(f)(success(a));
			const rightSide = f(a);

			expect(leftSide).toEqual(rightSide);
		});

		it("should satisfy right identity: flatMap(m, unit) = m", () => {
			const m = success(42);
			const unit = success;

			const result = flatMap(unit)(m);

			expect(result).toEqual(m);
		});

		it("should satisfy associativity: flatMap(flatMap(m, f), g) = flatMap(m, x => flatMap(f(x), g))", () => {
			const m = success(21);
			const f = (x: number) => success(x * 2);
			const g = (x: number) => success(x + 1);

			const leftSide = flatMap(g)(flatMap(f)(m));
			const rightSide = flatMap((x: number) => flatMap(g)(f(x)))(m);

			expect(leftSide).toEqual(rightSide);
		});

		it("should short-circuit on failure in flatMap", () => {
			const error = createQiError("TEST_ERROR", "Test error", "VALIDATION");
			const result = failure<number>(error);
			const mapped = flatMap((x: number) => success(x * 2))(result);

			expect(isFailure(mapped)).toBe(true);
			expect(getError(mapped)).toBe(error);
		});
	});

	describe("Pattern Matching", () => {
		it("should handle success in match", () => {
			const result = success(42);
			const matched = match(
				(value: number) => `Success: ${value}`,
				(error) => `Error: ${error.message}`
			)(result);

			expect(matched).toBe("Success: 42");
		});

		it("should handle failure in match", () => {
			const error = createQiError("TEST_ERROR", "Test error message", "VALIDATION");
			const result = failure<number>(error);
			const matched = match(
				(value: number) => `Success: ${value}`,
				(error) => `Error: ${error.message}`
			)(result);

			expect(matched).toBe("Error: Test error message");
		});
	});

	describe("fromTryCatch", () => {
		it("should catch exceptions and convert to failure", () => {
			const throwingFunction = () => {
				throw new Error("Something went wrong");
			};

			const result = fromTryCatch(throwingFunction);

			expect(isFailure(result)).toBe(true);
			const error = getError(result);
			expect(error?.message).toContain("Something went wrong");
		});

		it("should return success for non-throwing functions", () => {
			const safeFunction = () => 42;

			const result = fromTryCatch(safeFunction);

			expect(isSuccess(result)).toBe(true);
			expect(getData(result)).toBe(42);
		});
	});

	describe("fromAsyncTryCatch", () => {
		it("should catch async exceptions and convert to failure", async () => {
			const throwingAsyncFunction = async () => {
				throw new Error("Async operation failed");
			};

			const result = await fromAsyncTryCatch(throwingAsyncFunction);

			expect(isFailure(result)).toBe(true);
			const error = getError(result);
			expect(error?.message).toContain("Async operation failed");
			expect(error?.code).toBe("ASYNC_OPERATION_FAILED");
		});

		it("should return success for non-throwing async functions", async () => {
			const safeAsyncFunction = async () => 42;

			const result = await fromAsyncTryCatch(safeAsyncFunction);

			expect(isSuccess(result)).toBe(true);
			expect(getData(result)).toBe(42);
		});
	});

	describe("fromMaybe", () => {
		it("should convert non-null values to success", () => {
			const defaultError = createQiError("NULL_VALUE", "Value was null", "VALIDATION");
			const result = fromMaybe(defaultError, 42);

			expect(isSuccess(result)).toBe(true);
			expect(getData(result)).toBe(42);
		});

		it("should convert null to failure", () => {
			const defaultError = createQiError("NULL_VALUE", "Value was null", "VALIDATION");
			const result = fromMaybe(defaultError, null);

			expect(isFailure(result)).toBe(true);
			expect(getError(result)).toBe(defaultError);
		});

		it("should convert undefined to failure", () => {
			const defaultError = createQiError("UNDEFINED_VALUE", "Value was undefined", "VALIDATION");
			const result = fromMaybe(defaultError, undefined);

			expect(isFailure(result)).toBe(true);
			expect(getError(result)).toBe(defaultError);
		});
	});

	describe("fromPredicate", () => {
		it("should return success when predicate is true", () => {
			const isPositive = (x: number) => x > 0;
			const error = createQiError("NOT_POSITIVE", "Number must be positive", "VALIDATION");
			const result = fromPredicate(isPositive, error, 42);

			expect(isSuccess(result)).toBe(true);
			expect(getData(result)).toBe(42);
		});

		it("should return failure when predicate is false", () => {
			const isPositive = (x: number) => x > 0;
			const error = createQiError("NOT_POSITIVE", "Number must be positive", "VALIDATION");
			const result = fromPredicate(isPositive, error, -5);

			expect(isFailure(result)).toBe(true);
			expect(getError(result)).toBe(error);
		});
	});

	describe("mapError", () => {
		it("should transform error while preserving success", () => {
			const originalError = createQiError("ORIGINAL", "Original error", "VALIDATION");
			const transformError = (err: import("../../../../lib/src/qicore/base/index.js").QiError) =>
				createQiError("TRANSFORMED", `Transformed: ${err.message}`, err.category);

			const failureResult = failure<number>(originalError);
			const mapped = mapError(transformError)(failureResult);

			expect(isFailure(mapped)).toBe(true);
			const error = getError(mapped);
			expect(error?.code).toBe("TRANSFORMED");
			expect(error?.message).toBe("Transformed: Original error");
		});

		it("should preserve success values unchanged", () => {
			const transformError = (err: import("../../../../lib/src/qicore/base/index.js").QiError) =>
				createQiError("TRANSFORMED", `Transformed: ${err.message}`, err.category);

			const successResult = success(42);
			const mapped = mapError(transformError)(successResult);

			expect(isSuccess(mapped)).toBe(true);
			expect(getData(mapped)).toBe(42);
		});
	});

	describe("orElse", () => {
		it("should return original success result", () => {
			const successResult = success(42);
			const fallback = () => success(100);
			const result = orElse(fallback)(successResult);

			expect(isSuccess(result)).toBe(true);
			expect(getData(result)).toBe(42);
		});

		it("should return fallback for failure result", () => {
			const error = createQiError("ORIGINAL_ERROR", "Original error", "VALIDATION");
			const failureResult = failure<number>(error);
			const fallback = () => success(100);
			const result = orElse(fallback)(failureResult);

			expect(isSuccess(result)).toBe(true);
			expect(getData(result)).toBe(100);
		});

		it("should chain failures if fallback also fails", () => {
			const originalError = createQiError("ORIGINAL_ERROR", "Original error", "VALIDATION");
			const fallbackError = createQiError("FALLBACK_ERROR", "Fallback error", "SYSTEM");
			const failureResult = failure<number>(originalError);
			const fallback = () => failure<number>(fallbackError);
			const result = orElse(fallback)(failureResult);

			expect(isFailure(result)).toBe(true);
			expect(getError(result)).toBe(fallbackError);
		});
	});

	describe("unwrap operations", () => {
		it("unwrap should extract value from success", () => {
			const result = success(42);
			const value = unwrap(result);
			expect(value).toBe(42);
		});

		it("unwrap should throw on failure", () => {
			const error = createQiError("TEST_ERROR", "Test error", "VALIDATION");
			const result = failure<number>(error);
			expect(() => unwrap(result)).toThrow("Test error");
		});

		it("unwrapOr should return value from success", () => {
			const result = success(42);
			const value = unwrapOr(100)(result);
			expect(value).toBe(42);
		});

		it("unwrapOr should return default from failure", () => {
			const error = createQiError("TEST_ERROR", "Test error", "VALIDATION");
			const result = failure<number>(error);
			const value = unwrapOr(100)(result);
			expect(value).toBe(100);
		});

		it("unwrapOrElse should return value from success", () => {
			const result = success(42);
			const value = unwrapOrElse(() => 100)(result);
			expect(value).toBe(42);
		});

		it("unwrapOrElse should call function on failure", () => {
			const error = createQiError("TEST_ERROR", "Test error", "VALIDATION");
			const result = failure<number>(error);
			const fallback = (err: import("../../../../lib/src/qicore/base/index.js").QiError) => err.message.length;
			const value = unwrapOrElse(fallback)(result);
			expect(value).toBe("Test error".length);
		});
	});

	describe("sequence", () => {
		it("should convert array of successes to success of array", () => {
			const results = [success(1), success(2), success(3)];
			const sequenced = sequence(results);

			expect(isSuccess(sequenced)).toBe(true);
			expect(getData(sequenced)).toEqual([1, 2, 3]);
		});

		it("should return first failure when any result fails", () => {
			const error = createQiError("SECOND_ERROR", "Second error", "VALIDATION");
			const results = [success(1), failure<number>(error), success(3)];
			const sequenced = sequence(results);

			expect(isFailure(sequenced)).toBe(true);
			expect(getError(sequenced)).toBe(error);
		});

		it("should handle empty array", () => {
			const results: ResultType<number>[] = [];
			const sequenced = sequence(results);

			expect(isSuccess(sequenced)).toBe(true);
			expect(getData(sequenced)).toEqual([]);
		});
	});

	describe("Advanced Operations", () => {
		describe("bimap", () => {
			it("should transform both success and error cases", () => {
				const transformError = (err: import("../../../../lib/src/qicore/base/index.js").QiError) =>
					createQiError("TRANSFORMED", `Enhanced: ${err.message}`, err.category);
				const transformValue = (x: number) => x.toString();

				const successResult = success(42);
				const failureResult = failure<number>(createQiError("ORIGINAL", "Original error", "VALIDATION"));

				const mappedSuccess = bimap(transformError, transformValue)(successResult);
				const mappedFailure = bimap(transformError, transformValue)(failureResult);

				expect(isSuccess(mappedSuccess)).toBe(true);
				expect(getData(mappedSuccess)).toBe("42");

				expect(isFailure(mappedFailure)).toBe(true);
				const error = getError(mappedFailure);
				expect(error?.code).toBe("TRANSFORMED");
				expect(error?.message).toBe("Enhanced: Original error");
			});
		});

		describe("chainFirst", () => {
			it("should execute operation but return original value on success", () => {
				let sideEffectExecuted = false;
				const sideEffect = (x: number) => {
					sideEffectExecuted = true;
					return success(undefined);
				};

				const result = chainFirst(sideEffect)(success(42));

				expect(isSuccess(result)).toBe(true);
				expect(getData(result)).toBe(42);
				expect(sideEffectExecuted).toBe(true);
			});

			it("should fail if side effect fails", () => {
				const sideEffectError = createQiError("SIDE_EFFECT_FAILED", "Side effect failed", "SYSTEM");
				const sideEffect = () => failure<void>(sideEffectError);

				const result = chainFirst(sideEffect)(success(42));

				expect(isFailure(result)).toBe(true);
				expect(getError(result)).toBe(sideEffectError);
			});
		});

		describe("ap (applicative)", () => {
			it("should apply function to value when both are successful", () => {
				const addOne = (x: number) => x + 1;
				const wrappedFunction = success(addOne);
				const value = success(42);

				const result = ap(value)(wrappedFunction);

				expect(isSuccess(result)).toBe(true);
				expect(getData(result)).toBe(43);
			});

			it("should fail if function is a failure", () => {
				const error = createQiError("FUNCTION_ERROR", "Function failed", "VALIDATION");
				const wrappedFunction = failure<(x: number) => number>(error);
				const value = success(42);

				const result = ap(value)(wrappedFunction);

				expect(isFailure(result)).toBe(true);
				expect(getError(result)).toBe(error);
			});
		});

		describe("liftA2", () => {
			it("should lift binary function over two successful Results", () => {
				const add = (a: number, b: number) => a + b;
				const resultA = success(10);
				const resultB = success(20);

				const result = liftA2(add)(resultA)(resultB);

				expect(isSuccess(result)).toBe(true);
				expect(getData(result)).toBe(30);
			});

			it("should return first error when first Result fails", () => {
				const add = (a: number, b: number) => a + b;
				const errorA = createQiError("ERROR_A", "First error", "VALIDATION");
				const errorB = createQiError("ERROR_B", "Second error", "VALIDATION");
				const resultA = failure<number>(errorA);
				const resultB = failure<number>(errorB);

				const result = liftA2(add)(resultA)(resultB);

				expect(isFailure(result)).toBe(true);
				expect(getError(result)).toBe(errorA);
			});
		});

		describe("alt", () => {
			it("should return original result if successful", () => {
				const original = success(42);
				const alternative = success(100);

				const result = alt(alternative)(original);

				expect(isSuccess(result)).toBe(true);
				expect(getData(result)).toBe(42);
			});

			it("should return alternative if original fails", () => {
				const error = createQiError("ORIGINAL_ERROR", "Original failed", "VALIDATION");
				const original = failure<number>(error);
				const alternative = success(100);

				const result = alt(alternative)(original);

				expect(isSuccess(result)).toBe(true);
				expect(getData(result)).toBe(100);
			});
		});

		describe("fold", () => {
			it("should handle success case with second function", () => {
				const result = success(42);
				const folded = fold(
					(error) => `Error: ${error.message}`,
					(value) => `Success: ${value}`
				)(result);

				expect(folded).toBe("Success: 42");
			});

			it("should handle failure case with first function", () => {
				const error = createQiError("TEST_ERROR", "Test error message", "VALIDATION");
				const result = failure<number>(error);
				const folded = fold(
					(error) => `Error: ${error.message}`,
					(value) => `Success: ${value}`
				)(result);

				expect(folded).toBe("Error: Test error message");
			});
		});

		describe("traverse", () => {
			it("should transform and sequence array successfully", () => {
				const transform = (x: number) => x > 0 ? success(x * 2) : failure<number>(createQiError("NEGATIVE", "Negative number", "VALIDATION"));
				const numbers = [1, 2, 3];

				const result = traverse(transform)(numbers);

				expect(isSuccess(result)).toBe(true);
				expect(getData(result)).toEqual([2, 4, 6]);
			});

			it("should fail on first invalid item", () => {
				const error = createQiError("NEGATIVE", "Negative number", "VALIDATION");
				const transform = (x: number) => x > 0 ? success(x * 2) : failure<number>(error);
				const numbers = [1, -2, 3];

				const result = traverse(transform)(numbers);

				expect(isFailure(result)).toBe(true);
				expect(getError(result)).toBe(error);
			});
		});
	});

	describe("Type Safety", () => {
		it("should maintain type safety through transformations", () => {
			const result: ResultType<number> = success(42);
			const stringResult: ResultType<string> = map((x: number) => x.toString())(result);
			const booleanResult: ResultType<boolean> = flatMap((x: string) => success(x.length > 0))(
				stringResult
			);

			expect(isSuccess(booleanResult)).toBe(true);
			expect(getData(booleanResult)).toBe(true);
		});
	});
});
