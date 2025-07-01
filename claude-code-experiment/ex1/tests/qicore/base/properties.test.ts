import { describe, expect, it } from "vitest";
import {
	ErrorCategory,
	createQiError,
} from "../../../src/qicore/base/error.js";
import {
	all,
	failure,
	flatMap,
	isFailure,
	isSuccess,
	map,
	success,
	unwrapOr,
} from "../../../src/qicore/base/result.js";
import { empty, fromObject, merge } from "../../../src/qicore/core/config.js";

describe("Mathematical Property Tests", () => {
	// Property test utilities
	const generateNumber = () => Math.floor(Math.random() * 1000);
	const generateString = () => Math.random().toString(36).substring(2, 15);
	const generateError = () =>
		createQiError(
			`ERROR_${generateNumber()}`,
			`Message ${generateNumber()}`,
			ErrorCategory.VALIDATION,
		);

	const generateResult = <T>(value: T) =>
		Math.random() > 0.5 ? success(value) : failure<T>(generateError());

	describe("Result Monad Laws", () => {
		it("should satisfy left identity law: flatMap(f)(success(a)) === f(a)", () => {
			// Property: for any value a and function f,
			// flatMap(f)(success(a)) should equal f(a)

			for (let i = 0; i < 100; i++) {
				const a = generateNumber();
				const f = (x: number) => success(x.toString());

				const left = flatMap(f)(success(a));
				const right = f(a);

				expect(left).toEqual(right);
			}
		});

		it("should satisfy right identity law: flatMap(success)(m) === m", () => {
			// Property: for any Result m,
			// flatMap(success)(m) should equal m

			for (let i = 0; i < 100; i++) {
				const m = generateResult(generateNumber());

				const left = flatMap(success)(m);
				const right = m;

				expect(left).toEqual(right);
			}
		});

		it("should satisfy associativity law", () => {
			// Property: flatMap(f)(flatMap(g)(m)) === flatMap(x => flatMap(f)(g(x)))(m)

			for (let i = 0; i < 50; i++) {
				const m = generateResult(generateNumber());
				const g = (x: number) => success(x + 1);
				const f = (x: number) => success(x * 2);

				const left = flatMap(f)(flatMap(g)(m));
				const right = flatMap((x: number) => flatMap(f)(g(x)))(m);

				expect(left).toEqual(right);
			}
		});

		it("should preserve error short-circuiting", () => {
			// Property: operations on failed Results should remain failed

			for (let i = 0; i < 50; i++) {
				const error = generateError();
				const failedResult = failure<number>(error);

				const mapped = map((x: number) => x * 2)(failedResult);
				const chained = flatMap((x: number) => success(x + 1))(failedResult);

				expect(isFailure(mapped)).toBe(true);
				expect(isFailure(chained)).toBe(true);

				if (mapped._tag === "Left" && chained._tag === "Left") {
					expect(mapped.left).toBe(error);
					expect(chained.left).toBe(error);
				}
			}
		});
	});

	describe("Result Functor Laws", () => {
		it("should satisfy identity law: map(id)(result) === result", () => {
			// Property: mapping the identity function should not change the result

			const identity = <T>(x: T) => x;

			for (let i = 0; i < 100; i++) {
				const result = generateResult(generateNumber());
				const mapped = map(identity)(result);

				expect(mapped).toEqual(result);
			}
		});

		it("should satisfy composition law: map(f ∘ g) === map(f) ∘ map(g)", () => {
			// Property: mapping the composition of two functions should equal
			// the composition of mapping each function

			for (let i = 0; i < 50; i++) {
				const result = generateResult(generateNumber());
				const f = (x: number) => x * 2;
				const g = (x: number) => x + 1;
				const compose =
					(f: (x: number) => number, g: (x: number) => number) => (x: number) =>
						f(g(x));

				const left = map(compose(f, g))(result);
				const right = map(f)(map(g)(result));

				expect(left).toEqual(right);
			}
		});
	});

	describe("Configuration Monoid Laws", () => {
		it("should satisfy left identity law: merge(empty, config) === config", () => {
			// Property: merging empty config with any config should return the config unchanged

			for (let i = 0; i < 50; i++) {
				const obj = {
					name: generateString(),
					port: generateNumber(),
					nested: {
						value: generateString(),
					},
				};
				const configResult = fromObject(obj);

				expect(isSuccess(configResult)).toBe(true);
				if (configResult._tag === "Right") {
					const config = configResult.right;
					const merged = merge(empty, config);

					// Should have same keys and values
					expect(merged.data.size).toBe(config.data.size);
					for (const [key, value] of config.data) {
						expect(merged.data.get(key)).toEqual(value);
					}
				}
			}
		});

		it("should satisfy right identity law: merge(config, empty) === config", () => {
			// Property: merging any config with empty should return the config unchanged

			for (let i = 0; i < 50; i++) {
				const obj = {
					name: generateString(),
					port: generateNumber(),
					enabled: Math.random() > 0.5,
				};
				const configResult = fromObject(obj);

				expect(isSuccess(configResult)).toBe(true);
				if (configResult._tag === "Right") {
					const config = configResult.right;
					const merged = merge(config, empty);

					expect(merged.data.size).toBe(config.data.size);
					for (const [key, value] of config.data) {
						expect(merged.data.get(key)).toEqual(value);
					}
				}
			}
		});

		it("should satisfy associativity law: merge(a, merge(b, c)) === merge(merge(a, b), c)", () => {
			// Property: order of merging operations should not matter

			for (let i = 0; i < 20; i++) {
				const objA = { a: generateNumber(), shared: "first" };
				const objB = { b: generateNumber(), shared: "second" };
				const objC = { c: generateNumber(), shared: "third" };

				const configA = fromObject(objA);
				const configB = fromObject(objB);
				const configC = fromObject(objC);

				if (
					configA._tag === "Right" &&
					configB._tag === "Right" &&
					configC._tag === "Right"
				) {
					const left = merge(
						configA.right,
						merge(configB.right, configC.right),
					);
					const right = merge(
						merge(configA.right, configB.right),
						configC.right,
					);

					// Should have same final values (though right-bias means rightmost wins)
					expect(left.data.get("a")).toBe(right.data.get("a"));
					expect(left.data.get("b")).toBe(right.data.get("b"));
					expect(left.data.get("c")).toBe(right.data.get("c"));
					expect(left.data.get("shared")).toBe(right.data.get("shared")); // Should be "third"
					expect(left.data.get("shared")).toBe("third");
				}
			}
		});

		it("should preserve right-bias consistently", () => {
			// Property: in case of key conflicts, rightmost value should always win

			for (let i = 0; i < 50; i++) {
				const value1 = generateString();
				const value2 = generateString();
				const value3 = generateString();

				const config1 = fromObject({ key: value1 });
				const config2 = fromObject({ key: value2 });
				const config3 = fromObject({ key: value3 });

				if (
					config1._tag === "Right" &&
					config2._tag === "Right" &&
					config3._tag === "Right"
				) {
					const merged12 = merge(config1.right, config2.right);
					const merged123 = merge(merged12, config3.right);

					expect(merged12.data.get("key")).toBe(value2); // Right bias
					expect(merged123.data.get("key")).toBe(value3); // Rightmost wins
				}
			}
		});
	});

	describe("Result Composition Properties", () => {
		it("should compose map operations efficiently", () => {
			// Property: chaining multiple maps should be equivalent to composing functions first

			for (let i = 0; i < 50; i++) {
				const result = success(generateNumber());
				const f1 = (x: number) => x + 1;
				const f2 = (x: number) => x * 2;
				const f3 = (x: number) => x - 5;

				const chained = map(f3)(map(f2)(map(f1)(result)));
				const composed = map((x: number) => f3(f2(f1(x))))(result);

				expect(chained).toEqual(composed);
			}
		});

		it("should maintain type safety through transformations", () => {
			// Property: type transformations should preserve success/failure status

			for (let i = 0; i < 50; i++) {
				const numberResult = generateResult(generateNumber());

				const stringResult = map((x: number) => x.toString())(numberResult);
				const boolResult = map((s: string) => s.length > 5)(stringResult);

				expect(isSuccess(numberResult)).toBe(isSuccess(stringResult));
				expect(isSuccess(stringResult)).toBe(isSuccess(boolResult));
				expect(isFailure(numberResult)).toBe(isFailure(stringResult));
				expect(isFailure(stringResult)).toBe(isFailure(boolResult));
			}
		});
	});

	describe("Error Propagation Properties", () => {
		it("should propagate errors through operation chains", () => {
			// Property: once an error occurs, it should propagate through all subsequent operations

			for (let i = 0; i < 50; i++) {
				const error = generateError();
				const initialResult = failure<number>(error);

				const operations = [
					map((x: number) => x + 1),
					flatMap((x: number) => success(x.toString())),
					map((s: string) => s.length),
					flatMap((n: number) => success(n > 0)),
				];

				let current: unknown = initialResult;
				for (const op of operations) {
					current = op(current);
					expect(isFailure(current)).toBe(true);
					if (current._tag === "Left") {
						expect(current.left).toBe(error);
					}
				}
			}
		});

		it("should handle mixed success and failure sequences", () => {
			// Property: operations should properly handle sequences with both successes and failures

			for (let i = 0; i < 30; i++) {
				const successValue = generateNumber();
				const error = generateError();

				const sequence = [
					success(successValue),
					flatMap((x: number) =>
						x > 500 ? success(x) : failure<number>(error),
					),
					map((x: number) => x * 2),
				];

				let result = sequence[0];
				for (let j = 1; j < sequence.length; j++) {
					result = sequence[j](result as Result<number>);
				}

				if (successValue > 500) {
					expect(isSuccess(result)).toBe(true);
					if (result._tag === "Right") {
						expect(result.right).toBe(successValue * 2);
					}
				} else {
					expect(isFailure(result)).toBe(true);
					if (result._tag === "Left") {
						expect(result.left).toBe(error);
					}
				}
			}
		});
	});

	describe("Collection Operation Properties", () => {
		it("should handle all() with consistent success/failure semantics", () => {
			// Property: all() should succeed only if all inputs succeed

			for (let i = 0; i < 30; i++) {
				const size = Math.floor(Math.random() * 10) + 1;
				const results = [];
				let hasFailure = false;

				for (let j = 0; j < size; j++) {
					const shouldFail = Math.random() < 0.3; // 30% chance of failure
					if (shouldFail) {
						results.push(failure<number>(generateError()));
						hasFailure = true;
					} else {
						results.push(success(generateNumber()));
					}
				}

				const combined = all(results);

				if (hasFailure) {
					expect(isFailure(combined)).toBe(true);
				} else {
					expect(isSuccess(combined)).toBe(true);
					if (combined._tag === "Right") {
						expect(combined.right.length).toBe(size);
					}
				}
			}
		});
	});

	describe("Performance-Aware Properties", () => {
		it("should maintain performance characteristics under composition", () => {
			// Property: heavily composed operations should still meet performance targets

			const iterations = 100;
			const start = performance.now();

			for (let i = 0; i < iterations; i++) {
				const result = success(i);
				const composed = map((x: number) => x + 1)(
					map((x: number) => x * 2)(
						flatMap((x: number) => success(x - 5))(
							map((x: number) => x / 2)(result),
						),
					),
				);

				expect(isSuccess(composed)).toBe(true);
			}

			const end = performance.now();
			const avgTime = ((end - start) * 1000) / iterations; // microseconds

			// Should meet TypeScript tier requirement (< 100μs per operation)
			expect(avgTime).toBeLessThan(100);
		});
	});
});
