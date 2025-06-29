/**
 * Property-Based Tests for Mathematical Contract Verification
 *
 * Tests the mathematical laws that QiCore components must satisfy:
 * - Monoid Laws for Configuration
 * - Monad Laws for Result<T>
 * - Functor Laws for Result<T>
 */

import { type Result, createQiError, failure, flatMap, map, success } from "@qicore/base/index";
import { type ConfigData, empty as emptyConfig, fromObject, merge } from "@qicore/core/config";
import fc from "fast-check";
import { describe, expect, it } from "vitest";

// ============================================================================
// Configuration Monoid Laws
// ============================================================================

describe("Configuration Monoid Laws", () => {
  // Arbitrary generator for ConfigData
  const configArbitrary = fc
    .record({
      key1: fc.string(),
      key2: fc.integer(),
      key3: fc.boolean(),
    })
    .map((obj) => {
      const result = fromObject(obj);
      if (result._tag === "Right") {
        return result.right;
      }
      throw new Error("Failed to create config");
    });

  it("should satisfy left identity: empty ⊕ a = a", () => {
    fc.assert(
      fc.property(configArbitrary, (config) => {
        const result = merge(emptyConfig(), config);
        expect(configsEqual(result, config)).toBe(true);
      }),
      { numRuns: 100 }
    );
  });

  it("should satisfy right identity: a ⊕ empty = a", () => {
    fc.assert(
      fc.property(configArbitrary, (config) => {
        const result = merge(config, emptyConfig());
        expect(configsEqual(result, config)).toBe(true);
      }),
      { numRuns: 100 }
    );
  });

  it("should satisfy associativity: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)", () => {
    fc.assert(
      fc.property(configArbitrary, configArbitrary, configArbitrary, (a, b, c) => {
        const left = merge(merge(a, b), c);
        const right = merge(a, merge(b, c));
        expect(configsEqual(left, right)).toBe(true);
      }),
      { numRuns: 100 }
    );
  });

  it("should be right-biased: b overwrites a", () => {
    fc.assert(
      fc.property(
        fc.string(),
        fc.string().filter((s) => s !== ""),
        fc.string().filter((s) => s !== ""),
        (key, value1, value2) => {
          const config1Result = fromObject({ [key]: value1 });
          const config2Result = fromObject({ [key]: value2 });

          if (config1Result._tag === "Right" && config2Result._tag === "Right") {
            const merged = merge(config1Result.right, config2Result.right);
            const valueResult = merged.get(key);

            if (valueResult._tag === "Right") {
              expect(valueResult.right).toBe(value2);
            }
          }
        }
      ),
      { numRuns: 100 }
    );
  });
});

// ============================================================================
// Result Monad Laws
// ============================================================================

describe("Result Monad Laws", () => {
  // Arbitrary generators
  const numberArbitrary = fc.integer();
  const _stringArbitrary = fc.string();
  const errorArbitrary = fc.constant(createQiError("TEST_ERROR", "Test error", "VALIDATION"));

  // Helper functions for testing
  const addOne = (x: number): Result<number> => success(x + 1);
  const toStr = (x: number): Result<string> => success(x.toString());

  it("should satisfy left identity: success(a).flatMap(f) ≡ f(a)", () => {
    fc.assert(
      fc.property(numberArbitrary, (a) => {
        const left = flatMap(addOne)(success(a));
        const right = addOne(a);
        expect(resultsEqual(left, right)).toBe(true);
      }),
      { numRuns: 100 }
    );
  });

  it("should satisfy right identity: m.flatMap(success) ≡ m", () => {
    fc.assert(
      fc.property(fc.oneof(numberArbitrary.map(success), errorArbitrary.map(failure)), (m) => {
        const result = flatMap(success)(m);
        expect(resultsEqual(result, m)).toBe(true);
      }),
      { numRuns: 100 }
    );
  });

  it("should satisfy associativity: m.flatMap(f).flatMap(g) ≡ m.flatMap(x => f(x).flatMap(g))", () => {
    fc.assert(
      fc.property(fc.oneof(numberArbitrary.map(success), errorArbitrary.map(failure)), (m) => {
        const left = flatMap(toStr)(flatMap(addOne)(m));
        const right = flatMap((x: number) => flatMap(toStr)(addOne(x)))(m);
        expect(resultsEqual(left, right)).toBe(true);
      }),
      { numRuns: 100 }
    );
  });
});

// ============================================================================
// Result Functor Laws
// ============================================================================

describe("Result Functor Laws", () => {
  const numberArbitrary = fc.integer();
  const errorArbitrary = fc.constant(createQiError("TEST_ERROR", "Test error", "VALIDATION"));

  // Helper functions
  const addOne = (x: number): number => x + 1;
  const multiplyTwo = (x: number): number => x * 2;
  const identity = <T>(x: T): T => x;

  it("should satisfy identity: map(identity) ≡ identity", () => {
    fc.assert(
      fc.property(fc.oneof(numberArbitrary.map(success), errorArbitrary.map(failure)), (result) => {
        const mapped = map(identity)(result);
        expect(resultsEqual(mapped, result)).toBe(true);
      }),
      { numRuns: 100 }
    );
  });

  it("should satisfy composition: map(f).map(g) ≡ map(g ∘ f)", () => {
    fc.assert(
      fc.property(fc.oneof(numberArbitrary.map(success), errorArbitrary.map(failure)), (result) => {
        const left = map(multiplyTwo)(map(addOne)(result));
        const right = map((x: number) => multiplyTwo(addOne(x)))(result);
        expect(resultsEqual(left, right)).toBe(true);
      }),
      { numRuns: 100 }
    );
  });
});

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Check if two ConfigData objects are equal
 */
function configsEqual(a: ConfigData, b: ConfigData): boolean {
  if (a.size() !== b.size()) return false;

  const aKeys = a.keys().sort();
  const bKeys = b.keys().sort();

  if (aKeys.length !== bKeys.length) return false;

  for (let i = 0; i < aKeys.length; i++) {
    if (aKeys[i] !== bKeys[i]) return false;

    const aValue = a.get(aKeys[i]);
    const bValue = b.get(bKeys[i]);

    if (aValue._tag !== bValue._tag) return false;

    if (aValue._tag === "Right" && bValue._tag === "Right") {
      if (aValue.right !== bValue.right) return false;
    }
  }

  return true;
}

/**
 * Check if two Result objects are equal
 */
function resultsEqual<T>(a: Result<T>, b: Result<T>): boolean {
  if (a._tag !== b._tag) return false;

  if (a._tag === "Right" && b._tag === "Right") {
    return deepEqual(a.right, b.right);
  }

  if (a._tag === "Left" && b._tag === "Left") {
    return a.left.code === b.left.code && a.left.message === b.left.message;
  }

  return false;
}

/**
 * Deep equality check for arbitrary values
 */
function deepEqual(a: unknown, b: unknown): boolean {
  if (a === b) return true;

  if (typeof a !== typeof b) return false;

  if (typeof a === "object" && a !== null && b !== null) {
    const aKeys = Object.keys(a as Record<string, unknown>).sort();
    const bKeys = Object.keys(b as Record<string, unknown>).sort();

    if (aKeys.length !== bKeys.length) return false;

    for (let i = 0; i < aKeys.length; i++) {
      if (aKeys[i] !== bKeys[i]) return false;

      const aValue = (a as Record<string, unknown>)[aKeys[i]];
      const bValue = (b as Record<string, unknown>)[bKeys[i]];

      if (!deepEqual(aValue, bValue)) return false;
    }

    return true;
  }

  return false;
}
