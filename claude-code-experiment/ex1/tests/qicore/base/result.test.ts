import { describe, expect, it } from "vitest";
import {
	ErrorCategory,
	createQiError,
} from "../../../src/qicore/base/error.js";
import {
	all,
	failure,
	flatMap,
	fromAsyncTryCatch,
	fromTryCatch,
	isFailure,
	isSuccess,
	map,
	match,
	orElse,
	success,
	tap,
	tapError,
	traverse,
	unwrap,
	unwrapOr,
} from "../../../src/qicore/base/result.js";

describe("Result", () => {
	const testError = createQiError(
		"TEST_ERROR",
		"Test error message",
		ErrorCategory.VALIDATION,
	);

	describe("constructors", () => {
		it("should create success result", () => {
			const result = success(42);
			expect(isSuccess(result)).toBe(true);
			expect(isFailure(result)).toBe(false);
		});

		it("should create failure result", () => {
			const result = failure(testError);
			expect(isSuccess(result)).toBe(false);
			expect(isFailure(result)).toBe(true);
		});
	});

	describe("map", () => {
		it("should transform success value", () => {
			const result = success(42);
			const mapped = map((x: number) => x * 2)(result);

			expect(isSuccess(mapped)).toBe(true);
			if (mapped._tag === "Right") {
				expect(mapped.right).toBe(84);
			}
		});

		it("should pass through failure unchanged", () => {
			const result = failure<number>(testError);
			const mapped = map((x: number) => x * 2)(result);

			expect(isFailure(mapped)).toBe(true);
			if (mapped._tag === "Left") {
				expect(mapped.left).toBe(testError);
			}
		});

		it("should chain multiple maps", () => {
			const result = success(10);
			const chained = map((x: number) => x * 2)(
				map((x: number) => x + 1)(result),
			);

			expect(isSuccess(chained)).toBe(true);
			if (chained._tag === "Right") {
				expect(chained.right).toBe(22); // (10 + 1) * 2
			}
		});
	});

	describe("flatMap", () => {
		it("should chain successful operations", () => {
			const result = success(42);
			const chained = flatMap((x: number) => success(x.toString()))(result);

			expect(isSuccess(chained)).toBe(true);
			if (chained._tag === "Right") {
				expect(chained.right).toBe("42");
			}
		});

		it("should short-circuit on first failure", () => {
			const result = success(42);
			const chained = flatMap((x: number) => failure<string>(testError))(
				result,
			);

			expect(isFailure(chained)).toBe(true);
			if (chained._tag === "Left") {
				expect(chained.left).toBe(testError);
			}
		});

		it("should pass through existing failure", () => {
			const result = failure<number>(testError);
			const chained = flatMap((x: number) => success(x.toString()))(result);

			expect(isFailure(chained)).toBe(true);
			if (chained._tag === "Left") {
				expect(chained.left).toBe(testError);
			}
		});
	});

	describe("unwrap", () => {
		it("should return value from success", () => {
			const result = success(42);
			expect(unwrap(result)).toBe(42);
		});

		it("should throw error from failure", () => {
			const result = failure<number>(testError);
			expect(() => unwrap(result)).toThrow();
		});
	});

	describe("unwrapOr", () => {
		it("should return value from success", () => {
			const result = success(42);
			expect(unwrapOr(0)(result)).toBe(42);
		});

		it("should return default value from failure", () => {
			const result = failure<number>(testError);
			expect(unwrapOr(0)(result)).toBe(0);
		});
	});

	describe("match", () => {
		it("should call success handler for success", () => {
			const result = success(42);
			const matched = match(
				(x: number) => `Success: ${x}`,
				(error) => `Error: ${error.message}`,
			)(result);

			expect(matched).toBe("Success: 42");
		});

		it("should call error handler for failure", () => {
			const result = failure<number>(testError);
			const matched = match(
				(x: number) => `Success: ${x}`,
				(error) => `Error: ${error.message}`,
			)(result);

			expect(matched).toBe("Error: Test error message");
		});
	});

	describe("orElse", () => {
		it("should return original success", () => {
			const result = success(42);
			const recovered = orElse((error) => success(0))(result);

			expect(isSuccess(recovered)).toBe(true);
			if (recovered._tag === "Right") {
				expect(recovered.right).toBe(42);
			}
		});

		it("should call recovery function for failure", () => {
			const result = failure<number>(testError);
			const recovered = orElse((error) => success(0))(result);

			expect(isSuccess(recovered)).toBe(true);
			if (recovered._tag === "Right") {
				expect(recovered.right).toBe(0);
			}
		});

		it("should allow recovery function to fail", () => {
			const result = failure<number>(testError);
			const newError = createQiError(
				"RECOVERY_ERROR",
				"Recovery failed",
				ErrorCategory.UNKNOWN,
			);
			const recovered = orElse((error) => failure<number>(newError))(result);

			expect(isFailure(recovered)).toBe(true);
			if (recovered._tag === "Left") {
				expect(recovered.left).toBe(newError);
			}
		});
	});

	describe("fromTryCatch", () => {
		it("should create success for successful function", () => {
			const result = fromTryCatch(
				() => 42,
				(error) =>
					createQiError("CATCH_ERROR", "Caught error", ErrorCategory.UNKNOWN),
			);

			expect(isSuccess(result)).toBe(true);
			if (result._tag === "Right") {
				expect(result.right).toBe(42);
			}
		});

		it("should create failure for throwing function", () => {
			const result = fromTryCatch(
				() => {
					throw new Error("Test error");
				},
				(error) =>
					createQiError("CATCH_ERROR", "Caught error", ErrorCategory.UNKNOWN),
			);

			expect(isFailure(result)).toBe(true);
			if (result._tag === "Left") {
				expect(result.left.code).toBe("CATCH_ERROR");
			}
		});
	});

	describe("fromAsyncTryCatch", () => {
		it("should create success for successful async function", async () => {
			const result = await fromAsyncTryCatch(
				async () => 42,
				(error) =>
					createQiError("ASYNC_ERROR", "Async error", ErrorCategory.UNKNOWN),
			);

			expect(isSuccess(result)).toBe(true);
			if (result._tag === "Right") {
				expect(result.right).toBe(42);
			}
		});

		it("should create failure for rejecting async function", async () => {
			const result = await fromAsyncTryCatch(
				async () => {
					throw new Error("Async error");
				},
				(error) =>
					createQiError("ASYNC_ERROR", "Async error", ErrorCategory.UNKNOWN),
			);

			expect(isFailure(result)).toBe(true);
			if (result._tag === "Left") {
				expect(result.left.code).toBe("ASYNC_ERROR");
			}
		});
	});

	describe("all", () => {
		it("should combine all successes", () => {
			const results = [success(1), success(2), success(3)];
			const combined = all(results);

			expect(isSuccess(combined)).toBe(true);
			if (combined._tag === "Right") {
				expect(combined.right).toEqual([1, 2, 3]);
			}
		});

		it("should fail on first failure", () => {
			const results = [success(1), failure<number>(testError), success(3)];
			const combined = all(results);

			expect(isFailure(combined)).toBe(true);
			if (combined._tag === "Left") {
				expect(combined.left).toBe(testError);
			}
		});

		it("should handle empty array", () => {
			const results: (typeof success<number>)[] = [];
			const combined = all(results);

			expect(isSuccess(combined)).toBe(true);
			if (combined._tag === "Right") {
				expect(combined.right).toEqual([]);
			}
		});
	});

	describe("traverse", () => {
		it("should apply function to all values", () => {
			const values = [1, 2, 3];
			const fn = (x: number) => success(x * 2);
			const result = traverse(fn)(values);

			expect(isSuccess(result)).toBe(true);
			if (result._tag === "Right") {
				expect(result.right).toEqual([2, 4, 6]);
			}
		});

		it("should fail on first function failure", () => {
			const values = [1, 2, 3];
			const fn = (x: number) =>
				x === 2 ? failure<number>(testError) : success(x * 2);
			const result = traverse(fn)(values);

			expect(isFailure(result)).toBe(true);
			if (result._tag === "Left") {
				expect(result.left).toBe(testError);
			}
		});
	});

	describe("tap", () => {
		it("should run side effect on success without changing result", () => {
			let sideEffect = 0;
			const result = success(42);
			const tapped = tap((x: number) => {
				sideEffect = x;
			})(result);

			expect(sideEffect).toBe(42);
			expect(tapped).toBe(result);
		});

		it("should not run side effect on failure", () => {
			let sideEffect = 0;
			const result = failure<number>(testError);
			const tapped = tap((x: number) => {
				sideEffect = x;
			})(result);

			expect(sideEffect).toBe(0);
			expect(tapped).toBe(result);
		});
	});

	describe("tapError", () => {
		it("should run side effect on failure without changing result", () => {
			let sideEffect = "";
			const result = failure<number>(testError);
			const tapped = tapError((error) => {
				sideEffect = error.code;
			})(result);

			expect(sideEffect).toBe("TEST_ERROR");
			expect(tapped).toBe(result);
		});

		it("should not run side effect on success", () => {
			let sideEffect = "";
			const result = success(42);
			const tapped = tapError((error) => {
				sideEffect = error.code;
			})(result);

			expect(sideEffect).toBe("");
			expect(tapped).toBe(result);
		});
	});

	describe("monad laws", () => {
		// Left identity: flatMap(f)(success(a)) === f(a)
		it("should satisfy left identity law", () => {
			const a = 42;
			const f = (x: number) => success(x.toString());

			const left = flatMap(f)(success(a));
			const right = f(a);

			expect(left).toEqual(right);
		});

		// Right identity: flatMap(success)(m) === m
		it("should satisfy right identity law", () => {
			const m = success(42);

			const left = flatMap(success)(m);
			const right = m;

			expect(left).toEqual(right);
		});

		// Associativity: flatMap(f)(flatMap(g)(m)) === flatMap(x => flatMap(f)(g(x)))(m)
		it("should satisfy associativity law", () => {
			const m = success(42);
			const g = (x: number) => success(x + 1);
			const f = (x: number) => success(x * 2);

			const left = flatMap(f)(flatMap(g)(m));
			const right = flatMap((x: number) => flatMap(f)(g(x)))(m);

			expect(left).toEqual(right);
		});
	});

	describe("functor laws", () => {
		// Identity: map(id) === id
		it("should satisfy functor identity law", () => {
			const result = success(42);
			const identity = <T>(x: T) => x;

			const mapped = map(identity)(result);

			expect(mapped).toEqual(result);
		});

		// Composition: map(f ∘ g) === map(f) ∘ map(g)
		it("should satisfy functor composition law", () => {
			const result = success(42);
			const f = (x: number) => x * 2;
			const g = (x: number) => x + 1;
			const compose =
				(f: (x: number) => number, g: (x: number) => number) => (x: number) =>
					f(g(x));

			const left = map(compose(f, g))(result);
			const right = map(f)(map(g)(result));

			expect(left).toEqual(right);
		});
	});
});
