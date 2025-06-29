/**
 * Performance Benchmarks for QiCore v4.0
 *
 * Verifies that all operations meet the TypeScript interpreted tier requirements:
 * - Result operations: < 100μs
 * - Configuration merge: < 1ms
 * - Logger level check: < 1μs
 * - Cache operations: < 50μs
 */

import { createMemoryCache } from "../../../src/qicore/core/cache.js";
import { empty as emptyConfig, fromObject, merge } from "../../../src/qicore/core/config.js";
import { createDefault as createDefaultLogger } from "../../../src/qicore/core/logger.js";
import {
  PERFORMANCE_REQUIREMENTS,
  benchmark,
} from "../../../src/qicore/core/performance.js";
import {
  createQiError,
  failure,
  flatMap,
  map,
  measure,
  success,
} from "../../../src/qicore/index.js";
import { describe, expect, it } from "vitest";

describe("Performance Benchmarks - TypeScript Tier Compliance", () => {
  it("Result operations should be < 100μs", async () => {
    const stats = await benchmark(
      "result_operations",
      () => {
        const result = success(42);
        const mapped = map((x: number) => x * 2)(result);
        const chained = flatMap((x: number) => success(x + 1))(mapped);
        return chained;
      },
      1000
    );

    expect(stats._tag).toBe("Right");
    if (stats._tag === "Right") {
      expect(stats.right.mean).toBeLessThan(PERFORMANCE_REQUIREMENTS.RESULT_OPERATIONS);
      expect(stats.right.p95).toBeLessThan(PERFORMANCE_REQUIREMENTS.RESULT_OPERATIONS * 2);
      expect(stats.right.tierCompliant).toBe(true);
    }
  });

  it("Configuration merge should be < 1ms", async () => {
    const config1Result = fromObject({ a: 1, b: 2, c: 3 });
    const config2Result = fromObject({ d: 4, e: 5, f: 6 });

    expect(config1Result._tag).toBe("Right");
    expect(config2Result._tag).toBe("Right");

    if (config1Result._tag === "Right" && config2Result._tag === "Right") {
      const stats = await benchmark(
        "config_merge",
        () => {
          merge(config1Result.right, config2Result.right);
        },
        1000
      );

      expect(stats._tag).toBe("Right");
      if (stats._tag === "Right") {
        expect(stats.right.mean).toBeLessThan(PERFORMANCE_REQUIREMENTS.CONFIG_MERGE);
        expect(stats.right.tierCompliant).toBe(true);
      }
    }
  });

  it("Logger level check should be < 1μs", async () => {
    const loggerResult = createDefaultLogger();
    expect(loggerResult._tag).toBe("Right");

    if (loggerResult._tag === "Right") {
      const logger = loggerResult.right;

      const stats = await benchmark(
        "logger_level_check",
        () => {
          logger.isLevelEnabled("info");
        },
        10000
      ); // More iterations for micro-operations

      expect(stats._tag).toBe("Right");
      if (stats._tag === "Right") {
        expect(stats.right.mean).toBeLessThan(PERFORMANCE_REQUIREMENTS.LOGGER_LEVEL_CHECK);
        expect(stats.right.tierCompliant).toBe(true);
      }
    }
  });

  it("Cache operations should be < 50μs", async () => {
    const cacheResult = createMemoryCache();
    expect(cacheResult._tag).toBe("Right");

    if (cacheResult._tag === "Right") {
      const cache = cacheResult.right;

      const stats = await benchmark(
        "cache_operations",
        async () => {
          await cache.set("test-key", "test-value");
          await cache.get("test-key");
          await cache.has("test-key");
        },
        1000
      );

      expect(stats._tag).toBe("Right");
      if (stats._tag === "Right") {
        expect(stats.right.mean).toBeLessThan(PERFORMANCE_REQUIREMENTS.CACHE_OPERATIONS);
        expect(stats.right.tierCompliant).toBe(true);
      }
    }
  });

  it("Error creation should be efficient", async () => {
    const stats = await benchmark(
      "error_creation",
      () => {
        createQiError("TEST_ERROR", "Test error message", "VALIDATION", {
          context: "benchmark",
          iteration: Math.random(),
        });
      },
      1000
    );

    expect(stats._tag).toBe("Right");
    if (stats._tag === "Right") {
      // Error creation should be very fast
      expect(stats.right.mean).toBeLessThan(50); // 50μs
    }
  });

  it("Measurement overhead should be minimal", async () => {
    let counter = 0;

    const stats = await benchmark(
      "measurement_overhead",
      () => {
        measure("test_operation", () => {
          counter++;
        });
      },
      1000
    );

    expect(stats._tag).toBe("Right");
    if (stats._tag === "Right") {
      // Measurement overhead should be minimal
      expect(stats.right.mean).toBeLessThan(10); // 10μs overhead
    }

    expect(counter).toBe(1000);
  });

  it("Complex operations should remain efficient", async () => {
    const stats = await benchmark(
      "complex_operations",
      () => {
        // Chain multiple operations
        const error = createQiError("COMPLEX_TEST", "Complex operation test", "BUSINESS");
        const result1 = success(1);
        const result2 = map((x: number) => x * 2)(result1);
        const result3 = flatMap((x: number) => (x > 0 ? success(x) : failure(error)))(result2);

        return result3;
      },
      1000
    );

    expect(stats._tag).toBe("Right");
    if (stats._tag === "Right") {
      // Complex operations should still be fast
      expect(stats.right.mean).toBeLessThan(200); // 200μs for complex operations
    }
  });

  it("should verify all operations meet tier requirements", () => {
    // This test documents the performance requirements
    const requirements = PERFORMANCE_REQUIREMENTS;

    expect(requirements.RESULT_OPERATIONS).toBe(100);
    expect(requirements.CONFIG_MERGE).toBe(1000);
    expect(requirements.LOGGER_LEVEL_CHECK).toBe(1);
    expect(requirements.CACHE_OPERATIONS).toBe(50);
    expect(requirements.HTTP_OVERHEAD).toBe(5000);
    expect(requirements.DB_OVERHEAD).toBe(1000);
  });
});
