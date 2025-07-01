/**
 * QiCore v4.0 - Performance Benchmarks
 * 
 * Comprehensive performance testing and tier compliance verification
 */

import { describe, it, expect, beforeEach, afterEach } from "vitest";
import {
  measure,
  measureAsync,
  benchmark,
  recordMeasurement,
  getStats,
  getOperations,
  clearMeasurements,
  generateReport,
  createMonitor,
  verifyTierCompliance,
  runComprehensiveBenchmark,
  withPerformanceMonitoring,
  memoizeWithPerformance,
  retryWithPerformance,
  PERFORMANCE_REQUIREMENTS,
  type PerformanceMeasurement,
  type PerformanceStats
} from "../../../src/qicore/core/performance.js";
import {
  success,
  failure,
  map,
  flatMap
} from "../../../src/qicore/base/result.js";
import {
  empty,
  merge,
  fromObject
} from "../../../src/qicore/core/config.js";
import { createQiError, ErrorCategory } from "../../../src/qicore/base/error.js";
import { createDefault as createLogger } from "../../../src/qicore/core/logger.js";
import { createMemoryCache } from "../../../src/qicore/core/cache.js";

describe("Performance Benchmarks", () => {
  beforeEach(() => {
    clearMeasurements();
  });

  afterEach(() => {
    clearMeasurements();
  });

  describe("Basic Measurement Operations", () => {
    it("measures synchronous operations", () => {
      const result = measure("test_sync_operation", () => {
        // Simulate work
        let sum = 0;
        for (let i = 0; i < 1000; i++) {
          sum += i;
        }
        return sum;
      });

      expect(result).toBe(499500); // Sum of 0 to 999

      const statsResult = getStats("test_sync_operation");
      expect(statsResult._tag).toBe("Right");
      if (statsResult._tag === "Right") {
        expect(statsResult.right.sampleCount).toBe(1);
        expect(statsResult.right.mean).toBeGreaterThan(0);
      }
    });

    it("measures asynchronous operations", async () => {
      const result = await measureAsync("test_async_operation", async () => {
        await new Promise(resolve => setTimeout(resolve, 10)); // 10ms delay
        return "async result";
      });

      expect(result).toBe("async result");

      const statsResult = getStats("test_async_operation");
      expect(statsResult._tag).toBe("Right");
      if (statsResult._tag === "Right") {
        expect(statsResult.right.sampleCount).toBe(1);
        expect(statsResult.right.mean).toBeGreaterThan(5000); // Should be around 10ms = 10,000μs
      }
    });

    it("records manual measurements", () => {
      const measurement: PerformanceMeasurement = {
        operation: "manual_test",
        duration: 1500, // 1.5ms in microseconds
        timestamp: Date.now()
      };

      recordMeasurement(measurement);

      const statsResult = getStats("manual_test");
      expect(statsResult._tag).toBe("Right");
      if (statsResult._tag === "Right") {
        expect(statsResult.right.mean).toBe(1500);
        expect(statsResult.right.sampleCount).toBe(1);
      }
    });
  });

  describe("Statistical Analysis", () => {
    it("calculates statistics correctly", async () => {
      const durations = [100, 200, 300, 400, 500]; // μs
      
      for (const duration of durations) {
        recordMeasurement({
          operation: "stats_test",
          duration,
          timestamp: Date.now()
        });
      }

      const statsResult = getStats("stats_test");
      expect(statsResult._tag).toBe("Right");
      
      if (statsResult._tag === "Right") {
        const stats = statsResult.right;
        expect(stats.sampleCount).toBe(5);
        expect(stats.mean).toBe(300); // Average
        expect(stats.median).toBe(300); // Middle value
        expect(stats.min).toBe(100);
        expect(stats.max).toBe(500);
        expect(stats.p95).toBeGreaterThanOrEqual(400); // 95th percentile
      }
    });

    it("handles large sample sets", async () => {
      const sampleSize = 10000;
      
      for (let i = 0; i < sampleSize; i++) {
        recordMeasurement({
          operation: "large_sample_test",
          duration: Math.random() * 1000, // 0-1000μs
          timestamp: Date.now()
        });
      }

      const statsResult = getStats("large_sample_test");
      expect(statsResult._tag).toBe("Right");
      
      if (statsResult._tag === "Right") {
        const stats = statsResult.right;
        expect(stats.sampleCount).toBe(sampleSize);
        expect(stats.mean).toBeGreaterThan(0);
        expect(stats.mean).toBeLessThan(1000);
        expect(stats.stdDev).toBeGreaterThan(0);
      }
    });
  });

  describe("Benchmark Operations", () => {
    it("benchmarks simple operations", async () => {
      const benchmarkResult = await benchmark(
        "simple_operation",
        () => {
          return Math.sqrt(42);
        },
        1000 // 1000 iterations
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        const stats = benchmarkResult.right;
        expect(stats.sampleCount).toBe(1000);
        expect(stats.mean).toBeGreaterThan(0);
        expect(stats.mean).toBeLessThan(100); // Should be very fast
      }
    });

    it("benchmarks async operations", async () => {
      const benchmarkResult = await benchmark(
        "async_operation",
        async () => {
          await Promise.resolve(42);
        },
        100 // Fewer iterations for async
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        const stats = benchmarkResult.right;
        expect(stats.sampleCount).toBe(100);
        expect(stats.mean).toBeGreaterThan(0);
      }
    });

    it("handles benchmark errors gracefully", async () => {
      const benchmarkResult = await benchmark(
        "failing_operation",
        () => {
          throw new Error("Benchmark test error");
        },
        10
      );

      expect(benchmarkResult._tag).toBe("Left");
      if (benchmarkResult._tag === "Left") {
        expect(benchmarkResult.left.code).toBe("BENCHMARK_ERROR");
      }
    });
  });

  describe("Result<T> Performance Verification", () => {
    it("meets Result operation requirements", async () => {
      const benchmarkResult = await benchmark(
        "result_operations",
        () => {
          const result = success(42);
          const mapped = map((x: number) => x * 2)(result);
          const flatMapped = flatMap((x: number) => success(x + 1))(mapped);
          return flatMapped;
        },
        1000
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        const stats = benchmarkResult.right;
        expect(stats.mean).toBeLessThan(PERFORMANCE_REQUIREMENTS.RESULT_OPERATIONS); // < 100μs
        expect(stats.tierCompliant).toBe(true);
      }
    });

    it("measures Result success creation", async () => {
      const benchmarkResult = await benchmark(
        "result_success",
        () => success(Math.random()),
        10000
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        expect(benchmarkResult.right.mean).toBeLessThan(10); // Should be ~5μs
      }
    });

    it("measures Result failure creation", async () => {
      const error = createQiError("TEST", "Test error", ErrorCategory.VALIDATION);
      
      const benchmarkResult = await benchmark(
        "result_failure",
        () => failure(error),
        10000
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        expect(benchmarkResult.right.mean).toBeLessThan(10); // Should be ~5μs
      }
    });

    it("measures Result map operations", async () => {
      const result = success(42);
      const mapFn = (x: number) => x.toString();
      
      const benchmarkResult = await benchmark(
        "result_map",
        () => map(mapFn)(result),
        10000
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        expect(benchmarkResult.right.mean).toBeLessThan(30); // Should be ~20μs
      }
    });

    it("measures Result flatMap operations", async () => {
      const result = success(42);
      const flatMapFn = (x: number) => success(x.toString());
      
      const benchmarkResult = await benchmark(
        "result_flatmap",
        () => flatMap(flatMapFn)(result),
        10000
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        expect(benchmarkResult.right.mean).toBeLessThan(50); // Should be ~40μs
      }
    });
  });

  describe("Configuration Performance Verification", () => {
    it("meets Config merge requirements", async () => {
      const config1 = fromObject({ a: 1, b: 2, c: 3 }).right!;
      const config2 = fromObject({ d: 4, e: 5, f: 6 }).right!;
      
      const benchmarkResult = await benchmark(
        "config_merge",
        () => merge(config1, config2),
        1000
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        const stats = benchmarkResult.right;
        expect(stats.mean).toBeLessThan(PERFORMANCE_REQUIREMENTS.CONFIG_MERGE); // < 1ms
        expect(stats.tierCompliant).toBe(true);
      }
    });

    it("measures large config merge performance", async () => {
      const largeObj1 = Object.fromEntries(
        Array.from({ length: 100 }, (_, i) => [`key${i}`, `value${i}`])
      );
      const largeObj2 = Object.fromEntries(
        Array.from({ length: 100 }, (_, i) => [`key${i + 50}`, `value${i + 50}_new`])
      );
      
      const config1 = fromObject(largeObj1).right!;
      const config2 = fromObject(largeObj2).right!;
      
      const benchmarkResult = await benchmark(
        "large_config_merge",
        () => merge(config1, config2),
        100
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        // Should still be under 1ms even for large configs
        expect(benchmarkResult.right.mean).toBeLessThan(1000);
      }
    });
  });

  describe("Logger Performance Verification", () => {
    it("meets Logger level check requirements", async () => {
      const loggerResult = createLogger();
      expect(loggerResult._tag).toBe("Right");
      const logger = loggerResult.right!;
      
      const benchmarkResult = await benchmark(
        "logger_level_check",
        () => logger.isLevelEnabled("info" as any),
        10000
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        const stats = benchmarkResult.right;
        expect(stats.mean).toBeLessThan(PERFORMANCE_REQUIREMENTS.LOGGER_LEVEL_CHECK); // < 1μs
        expect(stats.tierCompliant).toBe(true);
      }
    });
  });

  describe("Cache Performance Verification", () => {
    it("meets Cache operation requirements", async () => {
      const cacheResult = createMemoryCache<string, string>();
      expect(cacheResult._tag).toBe("Right");
      const cache = cacheResult.right!;
      
      // Pre-populate cache
      await cache.set("test_key", "test_value");
      
      const getBenchmark = await benchmark(
        "cache_get",
        async () => await cache.get("test_key"),
        1000
      );

      const setBenchmark = await benchmark(
        "cache_set",
        async () => await cache.set("new_key", "new_value"),
        1000
      );

      expect(getBenchmark._tag).toBe("Right");
      expect(setBenchmark._tag).toBe("Right");
      
      if (getBenchmark._tag === "Right" && setBenchmark._tag === "Right") {
        expect(getBenchmark.right.mean).toBeLessThan(PERFORMANCE_REQUIREMENTS.CACHE_OPERATIONS); // < 50μs
        expect(setBenchmark.right.mean).toBeLessThan(PERFORMANCE_REQUIREMENTS.CACHE_OPERATIONS); // < 50μs
      }
    });
  });

  describe("Performance Monitoring Infrastructure", () => {
    it("meets measurement overhead requirements", async () => {
      const simpleOperation = () => 42;
      
      const benchmarkResult = await benchmark(
        "performance_measurement",
        () => measure("test_op", simpleOperation),
        1000
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        const stats = benchmarkResult.right;
        expect(stats.mean).toBeLessThan(PERFORMANCE_REQUIREMENTS.PERFORMANCE_MEASUREMENT); // < 10μs
      }
    });

    it("creates performance monitor with custom settings", () => {
      const monitor = createMonitor(5000);
      expect(monitor).toBeDefined();
      
      // Test basic operations
      monitor.recordMeasurement({
        operation: "custom_monitor_test",
        duration: 100,
        timestamp: Date.now()
      });
      
      const statsResult = monitor.getStats("custom_monitor_test");
      expect(statsResult._tag).toBe("Right");
    });
  });

  describe("Tier Compliance Verification", () => {
    it("verifies overall tier compliance", async () => {
      // Run some operations to generate data
      measure("compliant_op", () => 42); // Fast operation
      
      recordMeasurement({
        operation: "result_operations",
        duration: 50, // Well under 100μs limit
        timestamp: Date.now()
      });
      
      recordMeasurement({
        operation: "config_merge",
        duration: 500, // Well under 1ms limit
        timestamp: Date.now()
      });
      
      const complianceResult = verifyTierCompliance();
      expect(complianceResult._tag).toBe("Right");
      if (complianceResult._tag === "Right") {
        expect(complianceResult.right).toBe(true);
      }
    });

    it("detects non-compliance", async () => {
      // Record operations that exceed limits
      recordMeasurement({
        operation: "result_operations",
        duration: 200, // Exceeds 100μs limit
        timestamp: Date.now()
      });
      
      const complianceResult = verifyTierCompliance();
      expect(complianceResult._tag).toBe("Right");
      if (complianceResult._tag === "Right") {
        expect(complianceResult.right).toBe(false);
      }
    });
  });

  describe("Performance Report Generation", () => {
    it("generates comprehensive performance report", async () => {
      // Generate some test data
      recordMeasurement({ operation: "fast_op", duration: 10, timestamp: Date.now() });
      recordMeasurement({ operation: "fast_op", duration: 15, timestamp: Date.now() });
      recordMeasurement({ operation: "slow_op", duration: 2000, timestamp: Date.now() });
      
      const reportResult = generateReport();
      expect(reportResult._tag).toBe("Right");
      
      if (reportResult._tag === "Right") {
        const report = reportResult.right;
        expect(report).toContain("QiCore v4.0 Performance Report");
        expect(report).toContain("fast_op");
        expect(report).toContain("slow_op");
        expect(report).toContain("Overall Compliance");
        expect(report).toContain("TypeScript Tier Requirements");
      }
    });

    it("handles empty performance data", () => {
      clearMeasurements();
      
      const reportResult = generateReport();
      expect(reportResult._tag).toBe("Right");
      
      if (reportResult._tag === "Right") {
        expect(reportResult.right).toContain("No performance data available");
      }
    });
  });

  describe("Advanced Performance Features", () => {
    it("wraps functions with performance monitoring", () => {
      const originalFn = (x: number, y: number) => x + y;
      const monitoredFn = withPerformanceMonitoring("addition", originalFn);
      
      const result = monitoredFn(2, 3);
      expect(result).toBe(5);
      
      const statsResult = getStats("addition");
      expect(statsResult._tag).toBe("Right");
    });

    it("creates memoized functions with performance tracking", () => {
      let callCount = 0;
      const expensiveFunction = (x: number) => {
        callCount++;
        return x * x;
      };
      
      const memoizedFn = memoizeWithPerformance(expensiveFunction, "memoized_square");
      
      expect(memoizedFn(5)).toBe(25);
      expect(memoizedFn(5)).toBe(25); // Cached call
      expect(callCount).toBe(1); // Only called once
      
      const computeStats = getStats("memoized_square.compute");
      const cacheStats = getStats("memoized_square.cache_hit");
      
      expect(computeStats._tag).toBe("Right");
      expect(cacheStats._tag).toBe("Right");
    });

    it("implements retry with performance tracking", async () => {
      let attempts = 0;
      const unreliableOperation = async () => {
        attempts++;
        if (attempts < 3) {
          throw new Error("Simulated failure");
        }
        return "success";
      };
      
      const result = await retryWithPerformance(
        unreliableOperation,
        "retry_test",
        3,
        10 // 10ms delay
      );
      
      expect(result).toBe("success");
      expect(attempts).toBe(3);
      
      const attempt1Stats = getStats("retry_test.attempt_1");
      const attempt2Stats = getStats("retry_test.attempt_2");
      const attempt3Stats = getStats("retry_test.attempt_3");
      
      expect(attempt1Stats._tag).toBe("Right");
      expect(attempt2Stats._tag).toBe("Right");
      expect(attempt3Stats._tag).toBe("Right");
    });
  });

  describe("Comprehensive Benchmark Suite", () => {
    it("runs comprehensive benchmark for all operations", async () => {
      const result = await runComprehensiveBenchmark();
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const benchmarks = result.right;
        expect(benchmarks.size).toBeGreaterThan(0);
        
        for (const [operation, stats] of benchmarks) {
          expect(stats.sampleCount).toBeGreaterThan(0);
          expect(stats.mean).toBeGreaterThan(0);
          expect(typeof stats.tierCompliant).toBe("boolean");
        }
      }
    });
  });

  describe("Edge Cases and Error Handling", () => {
    it("handles operations with no measurements", () => {
      const statsResult = getStats("nonexistent_operation");
      
      expect(statsResult._tag).toBe("Left");
      if (statsResult._tag === "Left") {
        expect(statsResult.left.code).toBe("NO_PERFORMANCE_DATA");
      }
    });

    it("handles very fast operations", async () => {
      const benchmarkResult = await benchmark(
        "very_fast_operation",
        () => 1, // Trivial operation
        10000
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        expect(benchmarkResult.right.mean).toBeGreaterThanOrEqual(0);
        expect(benchmarkResult.right.min).toBeGreaterThanOrEqual(0);
      }
    });

    it("handles operations with high variance", async () => {
      const benchmarkResult = await benchmark(
        "variable_operation",
        () => {
          // Simulate variable work
          const work = Math.random() * 1000;
          let sum = 0;
          for (let i = 0; i < work; i++) {
            sum += i;
          }
          return sum;
        },
        100
      );

      expect(benchmarkResult._tag).toBe("Right");
      if (benchmarkResult._tag === "Right") {
        expect(benchmarkResult.right.stdDev).toBeGreaterThan(0);
        expect(benchmarkResult.right.max).toBeGreaterThan(benchmarkResult.right.min);
      }
    });
  });

  describe("Memory and Resource Usage", () => {
    it("handles large numbers of measurements efficiently", () => {
      const monitor = createMonitor(100000); // Large capacity
      
      // Record many measurements
      for (let i = 0; i < 50000; i++) {
        monitor.recordMeasurement({
          operation: "bulk_test",
          duration: Math.random() * 1000,
          timestamp: Date.now()
        });
      }
      
      const statsResult = monitor.getStats("bulk_test");
      expect(statsResult._tag).toBe("Right");
      
      if (statsResult._tag === "Right") {
        expect(statsResult.right.sampleCount).toBe(50000);
      }
    });

    it("respects sample limits to prevent memory leaks", () => {
      const monitor = createMonitor(1000); // Limited capacity
      
      // Record more measurements than capacity
      for (let i = 0; i < 2000; i++) {
        monitor.recordMeasurement({
          operation: "limited_test",
          duration: i,
          timestamp: Date.now()
        });
      }
      
      const statsResult = monitor.getStats("limited_test");
      expect(statsResult._tag).toBe("Right");
      
      if (statsResult._tag === "Right") {
        // Should be limited to max capacity
        expect(statsResult.right.sampleCount).toBeLessThanOrEqual(1000);
      }
    });
  });
});