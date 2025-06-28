/**
 * QiCore v4.0 - Result Monad Tests
 */

import { describe, expect, test } from "vitest";
import { QiError, Result } from "../../src/qicore/base/index.js";

describe("Result Monad", () => {
  test("should create success result", () => {
    const result = Result.success(42);
    expect(result.isSuccess()).toBe(true);
    expect(result.isFailure()).toBe(false);
    expect(result.unwrap()).toBe(42);
  });

  test("should create failure result", () => {
    const error = QiError.validationError("Test error", "field", "value");
    const result = Result.failure<number>(error);

    expect(result.isSuccess()).toBe(false);
    expect(result.isFailure()).toBe(true);
    expect(result.error()).toEqual(error);
  });

  test("should implement functor laws", () => {
    const result = Result.success(5);
    const f = (x: number) => x * 2;
    const g = (x: number) => x + 3;

    // Identity law: map(id) = id
    const identity = result.map((x) => x);
    expect(identity.unwrap()).toBe(result.unwrap());

    // Composition law: map(g ∘ f) = map(g) ∘ map(f)
    const composed = result.map((x) => g(f(x)));
    const chained = result.map(f).map(g);
    expect(composed.unwrap()).toBe(chained.unwrap());
  });

  test("should implement monad laws", () => {
    const value = 5;
    const f = (x: number) => Result.success(x * 2);
    const g = (x: number) => Result.success(x + 3);

    // Left identity: flatMap(f)(success(a)) = f(a)
    const leftIdentity1 = Result.success(value).flatMap(f);
    const leftIdentity2 = f(value);
    expect(leftIdentity1.unwrap()).toBe(leftIdentity2.unwrap());

    // Right identity: flatMap(success)(m) = m
    const m = Result.success(value);
    const rightIdentity = m.flatMap((x) => Result.success(x));
    expect(rightIdentity.unwrap()).toBe(m.unwrap());

    // Associativity: flatMap(g)(flatMap(f)(m)) = flatMap(x => flatMap(g)(f(x)))(m)
    const assoc1 = m.flatMap(f).flatMap(g);
    const assoc2 = m.flatMap((x) => f(x).flatMap(g));
    expect(assoc1.unwrap()).toBe(assoc2.unwrap());
  });

  test("should handle error mapping", () => {
    const error = QiError.validationError("Original error", "field", "value");
    const result = Result.failure<number>(error);

    const mapped = result.mapError((err) =>
      QiError.configurationError("Mapped error", "config", "string")
    );

    expect(mapped.isFailure()).toBe(true);
    expect(mapped.error().category).toBe("ConfigurationError");
  });

  test("should recover from errors", () => {
    const error = QiError.validationError("Error", "field", "value");
    const result = Result.failure<number>(error);

    const recovered = result.recover(() => 42);

    expect(recovered.isSuccess()).toBe(true);
    expect(recovered.unwrap()).toBe(42);
  });

  test("should unwrap with default", () => {
    const success = Result.success(42);
    const failure = Result.failure<number>(QiError.validationError("Error", "field", "value"));

    expect(success.unwrapOr(0)).toBe(42);
    expect(failure.unwrapOr(0)).toBe(0);
  });

  test("should implement match pattern", () => {
    const success = Result.success(42);
    const failure = Result.failure<number>(QiError.validationError("Error", "field", "value"));

    const successResult = success.match({
      success: (value) => `Success: ${value}`,
      failure: (error) => `Error: ${error.message}`,
    });

    const failureResult = failure.match({
      success: (value) => `Success: ${value}`,
      failure: (error) => `Error: ${error.message}`,
    });

    expect(successResult).toBe("Success: 42");
    expect(failureResult).toBe("Error: Error");
  });

  test("should sequence results", () => {
    const results = [Result.success(1), Result.success(2), Result.success(3)];

    const sequenced = Result.sequence(results);
    expect(sequenced.isSuccess()).toBe(true);
    expect(sequenced.unwrap()).toEqual([1, 2, 3]);
  });

  test("should fail sequence on first error", () => {
    const results = [
      Result.success(1),
      Result.failure<number>(QiError.validationError("Error", "field", "value")),
      Result.success(3),
    ];

    const sequenced = Result.sequence(results);
    expect(sequenced.isFailure()).toBe(true);
  });

  test("should partition results", () => {
    const results = [
      Result.success(1),
      Result.failure<number>(QiError.validationError("Error1", "field", "value")),
      Result.success(2),
      Result.failure<number>(QiError.validationError("Error2", "field", "value")),
    ];

    const { successes, failures } = Result.partition(results);

    expect(successes).toEqual([1, 2]);
    expect(failures).toHaveLength(2);
    expect(failures[0].message).toBe("Error1");
    expect(failures[1].message).toBe("Error2");
  });

  test("should wrap functions with tryCatch", () => {
    const safeDiv = (a: number, b: number) => {
      if (b === 0) throw new Error("Division by zero");
      return a / b;
    };

    const success = Result.tryCatch(() => safeDiv(10, 2));
    const failure = Result.tryCatch(() => safeDiv(10, 0));

    expect(success.isSuccess()).toBe(true);
    expect(success.unwrap()).toBe(5);

    expect(failure.isFailure()).toBe(true);
    expect(failure.error().category).toBe("RuntimeError");
  });
});
