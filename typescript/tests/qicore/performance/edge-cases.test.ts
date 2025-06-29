/**
 * Performance Edge Cases and Monitoring Tests
 * 
 * Tests performance monitoring edge cases, error handling, and 
 * benchmarking utilities to achieve comprehensive coverage.
 */

import {
  PERFORMANCE_REQUIREMENTS,
  QiPerformance,
  benchmark,
  createMonitor,
  generateReport,
  getGlobalMonitor,
  getOperations,
  getStats,
  measure,
  measureAsync,
  type PerformanceMeasurement,
  type PerformanceStats,
} from "../../../src/qicore/core/performance.js";
import { createQiError, failure, success } from "../../../src/qicore/index.js";
import { describe, expect, it, beforeEach } from "vitest";

describe("Performance Monitoring Edge Cases", () => {
  beforeEach(() => {
    // Clear global monitor state between tests
    getGlobalMonitor().clear();
  });

  it("should handle empty performance data gracefully", () => {
    const monitor = createMonitor();
    const result = monitor.getStats("nonexistent-operation");
    
    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.code).toBe("NO_PERFORMANCE_DATA");
      expect(result.left.message).toContain("No performance data for operation: nonexistent-operation");
    }
  });

  it("should limit samples to prevent memory leaks", () => {
    const monitor = createMonitor(5); // Small limit for testing
    
    // Add more samples than the limit
    for (let i = 0; i < 10; i++) {
      monitor.measure(`test-operation-${i}`, () => Math.random());
    }
    
    // Should have at most 10 operations (since each operation gets its own key)
    expect(monitor.getOperations()).toHaveLength(10);
    
    // Test same operation with multiple samples
    for (let i = 0; i < 10; i++) {
      monitor.measure("same-operation", () => Math.random());
    }
    
    const result = monitor.getStats("same-operation");
    expect(result._tag).toBe("Right");
    if (result._tag === "Right") {
      // Should be limited to 5 samples
      expect(result.right.sampleCount).toBe(5);
    }
  });

  it("should handle manual measurement recording", () => {
    const monitor = createMonitor();
    const measurement: PerformanceMeasurement = {
      operation: "manual-test",
      duration: 150.5,
      timestamp: Date.now(),
      metadata: { custom: "data" },
    };
    
    monitor.recordMeasurement(measurement);
    
    const result = monitor.getStats("manual-test");
    expect(result._tag).toBe("Right");
    if (result._tag === "Right") {
      expect(result.right.sampleCount).toBe(1);
      expect(result.right.mean).toBe(150.5);
      expect(result.right.min).toBe(150.5);
      expect(result.right.max).toBe(150.5);
    }
  });

  it("should calculate correct percentiles with odd sample count", () => {
    const monitor = createMonitor();
    const durations = [10, 20, 30, 40, 50]; // 5 samples
    
    durations.forEach((duration, i) => {
      monitor.recordMeasurement({
        operation: "percentile-test",
        duration,
        timestamp: Date.now(),
      });
    });
    
    const result = monitor.getStats("percentile-test");
    expect(result._tag).toBe("Right");
    if (result._tag === "Right") {
      expect(result.right.median).toBe(30); // Middle value
      expect(result.right.p95).toBe(50);    // 95th percentile
      expect(result.right.p99).toBe(50);    // 99th percentile
      expect(result.right.mean).toBe(30);   // (10+20+30+40+50)/5
    }
  });

  it("should calculate correct percentiles with even sample count", () => {
    const monitor = createMonitor();
    const durations = [10, 20, 30, 40]; // 4 samples
    
    durations.forEach((duration) => {
      monitor.recordMeasurement({
        operation: "even-percentile-test",
        duration,
        timestamp: Date.now(),
      });
    });
    
    const result = monitor.getStats("even-percentile-test");
    expect(result._tag).toBe("Right");
    if (result._tag === "Right") {
      expect(result.right.median).toBe(30); // Math.floor(4/2) = 2, so index 2 = 30
      expect(result.right.mean).toBe(25);   // (10+20+30+40)/4
    }
  });

  it("should detect tier compliance correctly", () => {
    const monitor = createMonitor();
    
    // Test compliant operation
    monitor.recordMeasurement({
      operation: "result_fast_operation",
      duration: 50, // Less than 100μs requirement
      timestamp: Date.now(),
    });
    
    // Test non-compliant operation
    monitor.recordMeasurement({
      operation: "result_slow_operation",
      duration: 200, // More than 100μs requirement
      timestamp: Date.now(),
    });
    
    const fastResult = monitor.getStats("result_fast_operation");
    const slowResult = monitor.getStats("result_slow_operation");
    
    expect(fastResult._tag).toBe("Right");
    expect(slowResult._tag).toBe("Right");
    
    if (fastResult._tag === "Right" && slowResult._tag === "Right") {
      expect(fastResult.right.tierCompliant).toBe(true);
      expect(slowResult.right.tierCompliant).toBe(false);
    }
  });

  it("should handle unknown operation types gracefully", () => {
    const monitor = createMonitor();
    
    monitor.recordMeasurement({
      operation: "unknown_mysterious_operation",
      duration: 1000, // Any duration
      timestamp: Date.now(),
    });
    
    const result = monitor.getStats("unknown_mysterious_operation");
    expect(result._tag).toBe("Right");
    if (result._tag === "Right") {
      // Unknown operations should still be compliant (no specific requirement)
      expect(result.right.tierCompliant).toBe(true);
    }
  });

  it("should handle async measurement errors", async () => {
    const monitor = createMonitor();
    
    const result = await benchmark(
      "async_error_test",
      async () => {
        throw new Error("Intentional async error");
      },
      10
    );
    
    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.code).toBe("BENCHMARK_ERROR");
      expect(result.left.message).toContain("Benchmark failed for async_error_test");
    }
  });

  it("should handle sync measurement errors", async () => {
    const result = await benchmark(
      "sync_error_test",
      () => {
        throw new Error("Intentional sync error");
      },
      10
    );
    
    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.code).toBe("BENCHMARK_ERROR");
      expect(result.left.message).toContain("Benchmark failed for sync_error_test");
    }
  });

  it("should measure async operations correctly", async () => {
    let executionCount = 0;
    
    const result = await measureAsync(
      "async_test_operation",
      async () => {
        executionCount++;
        await new Promise(resolve => setTimeout(resolve, 1)); // 1ms delay
        return "async result";
      }
    );
    
    expect(result).toBe("async result");
    expect(executionCount).toBe(1);
    
    const statsResult = getStats("async_test_operation");
    expect(statsResult._tag).toBe("Right");
    if (statsResult._tag === "Right") {
      expect(statsResult.right.sampleCount).toBe(1);
      expect(statsResult.right.mean).toBeGreaterThan(0);
    }
  });

  it("should generate meaningful reports", () => {
    // Add some test data
    const monitor = getGlobalMonitor();
    monitor.recordMeasurement({
      operation: "result_operations",
      duration: 50,
      timestamp: Date.now(),
    });
    monitor.recordMeasurement({
      operation: "cache_operations",
      duration: 200, // Non-compliant
      timestamp: Date.now(),
    });
    
    const reportResult = generateReport();
    expect(reportResult._tag).toBe("Right");
    
    if (reportResult._tag === "Right") {
      const report = reportResult.right;
      expect(report).toContain("QiCore Performance Report");
      expect(report).toContain("result_operations");
      expect(report).toContain("cache_operations");
      expect(report).toContain("❌ FAIL"); // Should fail due to cache operation
      expect(report).toContain("Performance Requirements");
    }
  });

  it("should generate empty report when no data", () => {
    const monitor = getGlobalMonitor();
    monitor.clear();
    
    const reportResult = generateReport();
    expect(reportResult._tag).toBe("Left");
    
    if (reportResult._tag === "Left") {
      expect(reportResult.left.code).toBe("NO_PERFORMANCE_DATA");
      expect(reportResult.left.message).toBe("No performance data available");
    }
  });

  it("should handle metadata in measurements", () => {
    const metadata = { userId: "test-123", version: "4.0", env: "test" };
    
    const result = measure(
      "metadata_test",
      () => "test result",
      metadata
    );
    
    expect(result).toBe("test result");
    
    const operations = getOperations();
    expect(operations).toContain("metadata_test");
  });

  it("should handle very large sample sets efficiently", () => {
    const monitor = createMonitor(10000); // Large sample size
    
    // Add many samples
    for (let i = 0; i < 5000; i++) {
      monitor.recordMeasurement({
        operation: "large_sample_test",
        duration: Math.random() * 100 + 50, // 50-150μs range
        timestamp: Date.now(),
      });
    }
    
    const start = performance.now();
    const result = monitor.getStats("large_sample_test");
    const end = performance.now();
    
    // Stats calculation should be fast even with large samples
    expect(end - start).toBeLessThan(10); // Less than 10ms
    
    expect(result._tag).toBe("Right");
    if (result._tag === "Right") {
      expect(result.right.sampleCount).toBe(5000);
      expect(result.right.mean).toBeGreaterThan(50);
      expect(result.right.mean).toBeLessThan(150);
    }
  });

  it("should validate QiPerformance API completeness", () => {
    // Verify all expected API methods exist
    expect(typeof QiPerformance.createMonitor).toBe("function");
    expect(typeof QiPerformance.getGlobalMonitor).toBe("function");
    expect(typeof QiPerformance.measure).toBe("function");
    expect(typeof QiPerformance.measureAsync).toBe("function");
    expect(typeof QiPerformance.getStats).toBe("function");
    expect(typeof QiPerformance.getOperations).toBe("function");
    expect(typeof QiPerformance.generateReport).toBe("function");
    expect(typeof QiPerformance.benchmark).toBe("function");
    expect(typeof QiPerformance.REQUIREMENTS).toBe("object");
    
    // Verify requirements are correct
    expect(QiPerformance.REQUIREMENTS.RESULT_OPERATIONS).toBe(100);
    expect(QiPerformance.REQUIREMENTS.CONFIG_MERGE).toBe(1000);
    expect(QiPerformance.REQUIREMENTS.LOGGER_LEVEL_CHECK).toBe(1);
    expect(QiPerformance.REQUIREMENTS.CACHE_OPERATIONS).toBe(50);
    expect(QiPerformance.REQUIREMENTS.HTTP_OVERHEAD).toBe(5000);
    expect(QiPerformance.REQUIREMENTS.DB_OVERHEAD).toBe(1000);
  });

  it("should handle operation requirement detection edge cases", () => {
    const monitor = createMonitor();
    
    // Test all requirement detection patterns
    const testCases = [
      { op: "custom_result_test", requirement: PERFORMANCE_REQUIREMENTS.RESULT_OPERATIONS },
      { op: "success_operation", requirement: PERFORMANCE_REQUIREMENTS.RESULT_OPERATIONS },
      { op: "failure_check", requirement: PERFORMANCE_REQUIREMENTS.RESULT_OPERATIONS },
      { op: "config_update", requirement: PERFORMANCE_REQUIREMENTS.CONFIG_MERGE },
      { op: "merge_configs", requirement: PERFORMANCE_REQUIREMENTS.CONFIG_MERGE },
      { op: "log_debug", requirement: PERFORMANCE_REQUIREMENTS.LOGGER_LEVEL_CHECK },
      { op: "level_check", requirement: PERFORMANCE_REQUIREMENTS.LOGGER_LEVEL_CHECK },
      { op: "cache_get", requirement: PERFORMANCE_REQUIREMENTS.CACHE_OPERATIONS },
      { op: "http_request", requirement: PERFORMANCE_REQUIREMENTS.HTTP_OVERHEAD },
      { op: "db_query", requirement: PERFORMANCE_REQUIREMENTS.DB_OVERHEAD },
      { op: "database_insert", requirement: PERFORMANCE_REQUIREMENTS.DB_OVERHEAD },
      { op: "unknown_operation", requirement: null }, // Should be compliant
    ];
    
    testCases.forEach(({ op, requirement }) => {
      const duration = requirement ? requirement + 50 : 100; // Exceed requirement
      
      monitor.recordMeasurement({
        operation: op,
        duration,
        timestamp: Date.now(),
      });
      
      const result = monitor.getStats(op);
      expect(result._tag).toBe("Right");
      
      if (result._tag === "Right") {
        if (requirement === null) {
          expect(result.right.tierCompliant).toBe(true);
        } else {
          expect(result.right.tierCompliant).toBe(false); // Should exceed requirement
        }
      }
    });
  });
});