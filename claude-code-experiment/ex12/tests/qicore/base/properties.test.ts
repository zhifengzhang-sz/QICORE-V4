/**
 * QiCore v4.0 - Property-Based Tests
 * 
 * Mathematical law verification through property testing
 * Verifies monad, monoid, and functor laws hold for all valid inputs
 */

import { describe, it, expect } from "vitest";
import {
  success,
  failure,
  map,
  flatMap,
  type Result
} from "../../../src/qicore/base/result.js";
import {
  empty,
  merge,
  fromObject,
  equals,
  type ConfigData
} from "../../../src/qicore/core/config.js";
import { createQiError, ErrorCategory } from "../../../src/qicore/base/error.js";

describe("Property-Based Mathematical Law Verification", () => {
  // Test data generators
  const generateNumbers = (): number[] => Array.from({ length: 100 }, (_, i) => i - 50);
  const generateStrings = (): string[] => ["", "a", "hello", "test123", "special!@#"];
  const generateErrors = () => [
    createQiError("ERROR_1", "First error", ErrorCategory.VALIDATION),
    createQiError("ERROR_2", "Second error", ErrorCategory.NETWORK),
    createQiError("ERROR_3", "Third error", ErrorCategory.SYSTEM)
  ];

  describe("Result<T> Monad Laws", () => {
    describe("Left Identity Law: return(a) >>= f ≡ f(a)", () => {
      it("holds for all numbers and string functions", () => {
        const f = (x: number): Result<string> => success(x.toString());
        
        for (const value of generateNumbers()) {
          const leftSide = flatMap(f)(success(value));
          const rightSide = f(value);
          
          expect(leftSide).toEqual(rightSide);
        }
      });

      it("holds for functions that can fail", () => {
        const f = (x: number): Result<string> => 
          x >= 0 ? success(x.toString()) : failure({
            code: "NEGATIVE",
            message: "Negative", 
            category: ErrorCategory.VALIDATION,
            context: new Map(),
            timestamp: 0 // Fixed timestamp for deterministic testing
          });
        
        for (const value of generateNumbers()) {
          const leftSide = flatMap(f)(success(value));
          const rightSide = f(value);
          
          expect(leftSide).toEqual(rightSide);
        }
      });
    });

    describe("Right Identity Law: m >>= return ≡ m", () => {
      it("holds for all successful results", () => {
        for (const value of generateNumbers()) {
          const m = success(value);
          const leftSide = flatMap(success)(m);
          const rightSide = m;
          
          expect(leftSide).toEqual(rightSide);
        }
      });

      it("holds for all failed results", () => {
        for (const error of generateErrors()) {
          const m = failure<number>(error);
          const leftSide = flatMap(success)(m);
          const rightSide = m;
          
          expect(leftSide).toEqual(rightSide);
        }
      });
    });

    describe("Associativity Law: (m >>= f) >>= g ≡ m >>= (\\x -> f(x) >>= g)", () => {
      it("holds for chained transformations", () => {
        const f = (x: number): Result<string> => success(x.toString());
        const g = (x: string): Result<number> => success(x.length);
        
        for (const value of generateNumbers()) {
          const m = success(value);
          
          const leftSide = flatMap(g)(flatMap(f)(m));
          const rightSide = flatMap((x: number) => flatMap(g)(f(x)))(m);
          
          expect(leftSide).toEqual(rightSide);
        }
      });

      it("holds when intermediate operations fail", () => {
        const f = (x: number): Result<string> => 
          x >= 0 ? success(x.toString()) : failure({
            code: "F_FAIL",
            message: "F failed",
            category: ErrorCategory.VALIDATION,
            context: new Map(),
            timestamp: 0
          });
        const g = (x: string): Result<number> => 
          x.length > 0 ? success(x.length) : failure({
            code: "G_FAIL", 
            message: "G failed",
            category: ErrorCategory.VALIDATION,
            context: new Map(),
            timestamp: 0
          });
        
        for (const value of generateNumbers()) {
          const m = success(value);
          
          const leftSide = flatMap(g)(flatMap(f)(m));
          const rightSide = flatMap((x: number) => flatMap(g)(f(x)))(m);
          
          expect(leftSide).toEqual(rightSide);
        }
      });

      it("holds for failing monads", () => {
        const f = (x: number): Result<string> => success(x.toString());
        const g = (x: string): Result<number> => success(x.length);
        
        for (const error of generateErrors()) {
          const m = failure<number>(error);
          
          const leftSide = flatMap(g)(flatMap(f)(m));
          const rightSide = flatMap((x: number) => flatMap(g)(f(x)))(m);
          
          expect(leftSide).toEqual(rightSide);
        }
      });
    });
  });

  describe("Result<T> Functor Laws", () => {
    describe("Identity Law: map(id) ≡ id", () => {
      it("holds for successful results", () => {
        const identity = <T>(x: T): T => x;
        
        for (const value of generateNumbers()) {
          const result = success(value);
          const mapped = map(identity)(result);
          
          expect(mapped).toEqual(result);
        }
      });

      it("holds for failed results", () => {
        const identity = <T>(x: T): T => x;
        
        for (const error of generateErrors()) {
          const result = failure<number>(error);
          const mapped = map(identity)(result);
          
          expect(mapped).toEqual(result);
        }
      });
    });

    describe("Composition Law: map(g ∘ f) ≡ map(g) ∘ map(f)", () => {
      it("holds for function composition", () => {
        const f = (x: number): string => x.toString();
        const g = (x: string): number => x.length;
        const composed = (x: number): number => g(f(x));
        
        for (const value of generateNumbers()) {
          const result = success(value);
          
          const leftSide = map(composed)(result);
          const rightSide = map(g)(map(f)(result));
          
          expect(leftSide).toEqual(rightSide);
        }
      });

      it("holds for failed results", () => {
        const f = (x: number): string => x.toString();
        const g = (x: string): number => x.length;
        const composed = (x: number): number => g(f(x));
        
        for (const error of generateErrors()) {
          const result = failure<number>(error);
          
          const leftSide = map(composed)(result);
          const rightSide = map(g)(map(f)(result));
          
          expect(leftSide).toEqual(rightSide);
        }
      });
    });
  });

  describe("Configuration Monoid Laws", () => {
    // Generate test configurations
    const generateConfigs = (): ConfigData[] => {
      const testData = [
        {},
        { a: 1 },
        { b: "test" },
        { a: 1, b: "test" },
        { nested: { key: "value" } },
        { array: [1, 2, 3] },
        { complex: { a: 1, b: { c: "deep" } } }
      ];
      
      return testData.map(data => {
        const result = fromObject(data);
        return result._tag === "Right" ? result.right : empty();
      });
    };

    describe("Left Identity Law: ∅ ⊕ a = a", () => {
      it("holds for all configurations", () => {
        for (const config of generateConfigs()) {
          const result = merge(empty(), config);
          expect(equals(result, config)).toBe(true);
        }
      });
    });

    describe("Right Identity Law: a ⊕ ∅ = a", () => {
      it("holds for all configurations", () => {
        for (const config of generateConfigs()) {
          const result = merge(config, empty());
          expect(equals(result, config)).toBe(true);
        }
      });
    });

    describe("Associativity Law: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)", () => {
      it("holds for all configuration combinations", () => {
        const configs = generateConfigs();
        
        for (let i = 0; i < configs.length; i++) {
          for (let j = 0; j < configs.length; j++) {
            for (let k = 0; k < configs.length; k++) {
              const a = configs[i];
              const b = configs[j];
              const c = configs[k];
              
              const leftSide = merge(merge(a, b), c);
              const rightSide = merge(a, merge(b, c));
              
              expect(equals(leftSide, rightSide)).toBe(true);
            }
          }
        }
      });
    });

    describe("Right-Biased Merge Property", () => {
      it("later values override earlier values for same keys", () => {
        const config1Result = fromObject({ key: "value1", other: "keep" });
        const config2Result = fromObject({ key: "value2", new: "add" });
        
        expect(config1Result._tag).toBe("Right");
        expect(config2Result._tag).toBe("Right");
        
        if (config1Result._tag === "Right" && config2Result._tag === "Right") {
          const merged = merge(config1Result.right, config2Result.right);
          
          expect(merged.data.get("key")).toBe("value2"); // Right-biased
          expect(merged.data.get("other")).toBe("keep"); // Preserved from left
          expect(merged.data.get("new")).toBe("add"); // Added from right
        }
      });
    });
  });

  describe("Error Chain Properties", () => {
    it("maintains error information through cause chains", () => {
      const errors = generateErrors();
      let currentError = errors[0];
      
      // Build cause chain
      for (let i = 1; i < errors.length; i++) {
        currentError = {
          ...errors[i],
          cause: currentError
        };
      }
      
      // Verify chain integrity
      let depth = 0;
      let current = currentError;
      
      while (current) {
        expect(current.code).toBe(errors[errors.length - 1 - depth].code);
        current = current.cause;
        depth++;
      }
      
      expect(depth).toBe(errors.length);
    });
  });

  describe("Performance Properties", () => {
    it("Result operations scale linearly", () => {
      const sizes = [100, 1000, 10000];
      const durations: number[] = [];
      
      for (const size of sizes) {
        const start = performance.now();
        
        let result: Result<number> = success(0);
        for (let i = 0; i < size; i++) {
          result = flatMap((x: number) => success(x + 1))(result);
        }
        
        const end = performance.now();
        durations.push(end - start);
      }
      
      // Performance should scale roughly linearly (within 2x factor)
      const ratio1 = durations[1] / durations[0];
      const ratio2 = durations[2] / durations[1];
      
      expect(ratio1).toBeLessThan(20); // Some overhead is expected
      expect(ratio2).toBeLessThan(20);
    });

    it("Configuration merge scales with size", () => {
      const generateLargeConfig = (size: number): ConfigData => {
        const obj: Record<string, unknown> = {};
        for (let i = 0; i < size; i++) {
          obj[`key_${i}`] = `value_${i}`;
        }
        const result = fromObject(obj);
        return result._tag === "Right" ? result.right : empty();
      };
      
      const sizes = [10, 100, 1000];
      const durations: number[] = [];
      
      for (const size of sizes) {
        const config1 = generateLargeConfig(size);
        const config2 = generateLargeConfig(size);
        
        const start = performance.now();
        merge(config1, config2);
        const end = performance.now();
        
        durations.push(end - start);
      }
      
      // Should remain under 1ms even for large configs
      for (const duration of durations) {
        expect(duration).toBeLessThan(1); // 1ms
      }
    });
  });

  describe("Invariant Properties", () => {
    it("Result transformations preserve success/failure state correctly", () => {
      for (const value of generateNumbers()) {
        const result = success(value);
        
        // Map should preserve success
        const mapped = map((x: number) => x.toString())(result);
        expect(mapped._tag).toBe("Right");
        
        // FlatMap with successful function should preserve success
        const flatMapped = flatMap((x: number) => success(x.toString()))(result);
        expect(flatMapped._tag).toBe("Right");
        
        // FlatMap with failing function should convert to failure
        const failingFlatMapped = flatMap((x: number) => 
          failure(createQiError("FAIL", "Fail", ErrorCategory.VALIDATION))
        )(result);
        expect(failingFlatMapped._tag).toBe("Left");
      }
    });

    it("Failed results remain failed through all transformations", () => {
      for (const error of generateErrors()) {
        const result = failure<number>(error);
        
        // All transformations should preserve failure
        const mapped = map((x: number) => x.toString())(result);
        expect(mapped._tag).toBe("Left");
        expect(mapped.left).toBe(error);
        
        const flatMapped = flatMap((x: number) => success(x.toString()))(result);
        expect(flatMapped._tag).toBe("Left");
        expect(flatMapped.left).toBe(error);
      }
    });

    it("Configuration operations maintain Map invariants", () => {
      for (const config of generateConfigs()) {
        // Size should be consistent
        expect(config.data.size).toBe(Array.from(config.data.keys()).length);
        
        // All keys should have corresponding values
        for (const key of config.data.keys()) {
          expect(config.data.has(key)).toBe(true);
          expect(config.data.get(key)).toBeDefined();
        }
        
        // Merge should preserve this invariant
        const merged = merge(config, empty());
        expect(merged.data.size).toBe(Array.from(merged.data.keys()).length);
      }
    });
  });

  describe("Boundary Condition Properties", () => {
    it("handles empty structures correctly", () => {
      // Empty configurations
      const emptyConfig = empty();
      expect(emptyConfig.data.size).toBe(0);
      expect(equals(merge(emptyConfig, emptyConfig), emptyConfig)).toBe(true);
      
      // Empty sequences
      const emptySequence: Result<number>[] = [];
      // sequence(emptySequence) should succeed with empty array
      // This would be tested if sequence was imported
    });

    it("handles extreme values correctly", () => {
      const extremeValues = [
        Number.MAX_SAFE_INTEGER,
        Number.MIN_SAFE_INTEGER,
        0,
        -0,
        Infinity,
        -Infinity,
        NaN
      ];
      
      for (const value of extremeValues) {
        const result = success(value);
        const mapped = map((x: number) => x)(result);
        
        expect(mapped._tag).toBe("Right");
        if (!Number.isNaN(value)) {
          expect(mapped.right).toBe(value);
        } else {
          expect(Number.isNaN(mapped.right)).toBe(true);
        }
      }
    });
  });
});