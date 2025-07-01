/**
 * QiCore v4.0 Performance Benchmarking and Monitoring
 *
 * Mathematical Foundation:
 * - Performance Tier Compliance: TypeScript (interpreted) = 100× baseline
 * - Statistical Analysis: Mean, median, percentiles for operation timing
 * - Contract Verification: Automated verification of performance requirements
 *
 * Production Features:
 * - Real-time performance monitoring
 * - Automated tier compliance checking
 * - Performance regression detection
 */

import { createQiError } from "../base/error.js";
import { type Result, failure, success } from "../base/result.js";

// ============================================================================
// Performance Measurement Types
// ============================================================================

/**
 * Performance Measurement Result
 */
export interface PerformanceMeasurement {
  readonly operation: string;
  readonly duration: number; // microseconds
  readonly timestamp: number;
  readonly metadata?: Record<string, unknown>;
}

/**
 * Performance Statistics
 */
export interface PerformanceStats {
  readonly operation: string;
  readonly sampleCount: number;
  readonly mean: number; // microseconds
  readonly median: number; // microseconds
  readonly p95: number; // 95th percentile
  readonly p99: number; // 99th percentile
  readonly min: number;
  readonly max: number;
  readonly tierCompliant: boolean; // Meets TypeScript tier requirements
}

/**
 * Performance Tier Requirements (TypeScript interpreted = 100× baseline)
 */
export const PERFORMANCE_REQUIREMENTS = {
  RESULT_OPERATIONS: 100, // μs
  CONFIG_MERGE: 1000, // μs (1ms)
  LOGGER_LEVEL_CHECK: 1, // μs
  CACHE_OPERATIONS: 50, // μs
  HTTP_OVERHEAD: 5000, // μs (5ms)
  DB_OVERHEAD: 1000, // μs (1ms)
} as const;

// ============================================================================
// Performance Monitor Class
// ============================================================================

class PerformanceMonitor {
  private measurements = new Map<string, PerformanceMeasurement[]>();
  private readonly maxSamples: number;

  constructor(maxSamples = 1000) {
    this.maxSamples = maxSamples;
  }

  /**
   * Measure execution time of a synchronous operation
   */
  measure<T>(operation: string, fn: () => T, metadata?: Record<string, unknown>): T {
    const start = performance.now();
    const result = fn();
    const end = performance.now();

    this.recordMeasurement({
      operation,
      duration: (end - start) * 1000, // Convert to microseconds
      timestamp: Date.now(),
      metadata,
    });

    return result;
  }

  /**
   * Measure execution time of an asynchronous operation
   */
  async measureAsync<T>(
    operation: string,
    fn: () => Promise<T>,
    metadata?: Record<string, unknown>
  ): Promise<T> {
    const start = performance.now();
    const result = await fn();
    const end = performance.now();

    this.recordMeasurement({
      operation,
      duration: (end - start) * 1000, // Convert to microseconds
      timestamp: Date.now(),
      metadata,
    });

    return result;
  }

  /**
   * Record a manual measurement
   */
  recordMeasurement(measurement: PerformanceMeasurement): void {
    const samples = this.measurements.get(measurement.operation) || [];
    samples.push(measurement);

    // Keep only the latest samples to prevent memory leaks
    if (samples.length > this.maxSamples) {
      samples.shift();
    }

    this.measurements.set(measurement.operation, samples);
  }

  /**
   * Get performance statistics for an operation
   */
  getStats(operation: string): Result<PerformanceStats> {
    const samples = this.measurements.get(operation);
    if (!samples || samples.length === 0) {
      return failure(
        createQiError(
          "NO_PERFORMANCE_DATA",
          `No performance data for operation: ${operation}`,
          "VALIDATION",
          {
            operation,
          }
        )
      );
    }

    const durations = samples.map((s) => s.duration).sort((a, b) => a - b);
    const mean = durations.reduce((sum, d) => sum + d, 0) / durations.length;
    const median = durations[Math.floor(durations.length / 2)];
    const p95 = durations[Math.floor(durations.length * 0.95)];
    const p99 = durations[Math.floor(durations.length * 0.99)];
    const min = durations[0];
    const max = durations[durations.length - 1];

    // Check tier compliance based on operation type
    const requirement = this.getRequirementForOperation(operation);
    const tierCompliant = requirement ? mean <= requirement : true;

    return success({
      operation,
      sampleCount: durations.length,
      mean,
      median,
      p95,
      p99,
      min,
      max,
      tierCompliant,
    });
  }

  /**
   * Get all available operation names
   */
  getOperations(): string[] {
    return Array.from(this.measurements.keys());
  }

  /**
   * Clear all measurements
   */
  clear(): void {
    this.measurements.clear();
  }

  /**
   * Get requirement for specific operation
   */
  private getRequirementForOperation(operation: string): number | null {
    const lowerOp = operation.toLowerCase();

    if (lowerOp.includes("result") || lowerOp.includes("success") || lowerOp.includes("failure")) {
      return PERFORMANCE_REQUIREMENTS.RESULT_OPERATIONS;
    }
    if (lowerOp.includes("config") || lowerOp.includes("merge")) {
      return PERFORMANCE_REQUIREMENTS.CONFIG_MERGE;
    }
    if (lowerOp.includes("log") || lowerOp.includes("level")) {
      return PERFORMANCE_REQUIREMENTS.LOGGER_LEVEL_CHECK;
    }
    if (lowerOp.includes("cache")) {
      return PERFORMANCE_REQUIREMENTS.CACHE_OPERATIONS;
    }
    if (lowerOp.includes("http")) {
      return PERFORMANCE_REQUIREMENTS.HTTP_OVERHEAD;
    }
    if (lowerOp.includes("db") || lowerOp.includes("database")) {
      return PERFORMANCE_REQUIREMENTS.DB_OVERHEAD;
    }

    return null;
  }
}

// ============================================================================
// Global Performance Monitor Instance
// ============================================================================

let globalMonitor: PerformanceMonitor | null = null;

/**
 * Get or create the global performance monitor
 */
export const getGlobalMonitor = (): PerformanceMonitor => {
  if (!globalMonitor) {
    globalMonitor = new PerformanceMonitor();
  }
  return globalMonitor;
};

/**
 * Create a new performance monitor instance
 */
export const createMonitor = (maxSamples = 1000): PerformanceMonitor => {
  return new PerformanceMonitor(maxSamples);
};

// ============================================================================
// Convenience Functions
// ============================================================================

/**
 * Measure a synchronous operation using the global monitor
 */
export const measure = <T>(
  operation: string,
  fn: () => T,
  metadata?: Record<string, unknown>
): T => {
  return getGlobalMonitor().measure(operation, fn, metadata);
};

/**
 * Measure an asynchronous operation using the global monitor
 */
export const measureAsync = async <T>(
  operation: string,
  fn: () => Promise<T>,
  metadata?: Record<string, unknown>
): Promise<T> => {
  return getGlobalMonitor().measureAsync(operation, fn, metadata);
};

/**
 * Get performance statistics for an operation
 */
export const getStats = (operation: string): Result<PerformanceStats> => {
  return getGlobalMonitor().getStats(operation);
};

/**
 * Get all available operations
 */
export const getOperations = (): string[] => {
  return getGlobalMonitor().getOperations();
};

/**
 * Generate a performance report for all operations
 */
export const generateReport = (): Result<string> => {
  const monitor = getGlobalMonitor();
  const operations = monitor.getOperations();

  if (operations.length === 0) {
    return failure(
      createQiError("NO_PERFORMANCE_DATA", "No performance data available", "VALIDATION")
    );
  }

  const lines: string[] = [];
  lines.push("# QiCore Performance Report");
  lines.push("");
  lines.push("## Performance Tier: TypeScript (Interpreted) = 100× Baseline");
  lines.push("");
  lines.push("| Operation | Samples | Mean (μs) | P95 (μs) | P99 (μs) | Compliant |");
  lines.push("|-----------|---------|-----------|----------|----------|-----------|");

  let allCompliant = true;

  for (const operation of operations.sort()) {
    const statsResult = monitor.getStats(operation);
    if (statsResult._tag === "Right") {
      const stats = statsResult.right;
      const compliance = stats.tierCompliant ? "✅" : "❌";

      if (!stats.tierCompliant) {
        allCompliant = false;
      }

      lines.push(
        `| ${operation} | ${stats.sampleCount} | ${stats.mean.toFixed(1)} | ${stats.p95.toFixed(1)} | ${stats.p99.toFixed(1)} | ${compliance} |`
      );
    }
  }

  lines.push("");
  lines.push(`## Overall Compliance: ${allCompliant ? "✅ PASS" : "❌ FAIL"}`);
  lines.push("");
  lines.push("### Performance Requirements:");
  lines.push(`- Result Operations: ≤ ${PERFORMANCE_REQUIREMENTS.RESULT_OPERATIONS}μs`);
  lines.push(`- Config Merge: ≤ ${PERFORMANCE_REQUIREMENTS.CONFIG_MERGE}μs`);
  lines.push(`- Logger Level Check: ≤ ${PERFORMANCE_REQUIREMENTS.LOGGER_LEVEL_CHECK}μs`);
  lines.push(`- Cache Operations: ≤ ${PERFORMANCE_REQUIREMENTS.CACHE_OPERATIONS}μs`);

  return success(lines.join("\n"));
};

// ============================================================================
// Performance Testing Utilities
// ============================================================================

/**
 * Run a performance benchmark for an operation
 */
export const benchmark = async (
  operation: string,
  fn: () => void | Promise<void>,
  iterations = 1000
): Promise<Result<PerformanceStats>> => {
  const monitor = createMonitor();

  try {
    for (let i = 0; i < iterations; i++) {
      if (fn.constructor.name === "AsyncFunction") {
        await monitor.measureAsync(operation, fn as () => Promise<void>);
      } else {
        monitor.measure(operation, fn as () => void);
      }
    }

    return monitor.getStats(operation);
  } catch (error) {
    return failure(
      createQiError("BENCHMARK_ERROR", `Benchmark failed for ${operation}: ${error}`, "SYSTEM", {
        operation,
        iterations,
        error: String(error),
      })
    );
  }
};

// ============================================================================
// Complete Performance API
// ============================================================================

/**
 * Performance API following QiCore v4 mathematical specification
 */
export const QiPerformance = {
  // Monitor creation
  createMonitor,
  getGlobalMonitor,

  // Measurement functions
  measure,
  measureAsync,

  // Statistics and reporting
  getStats,
  getOperations,
  generateReport,
  benchmark,

  // Requirements
  REQUIREMENTS: PERFORMANCE_REQUIREMENTS,
} as const;

// Export types
// Types already exported above
