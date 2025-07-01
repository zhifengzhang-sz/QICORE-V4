/**
 * QiCore v4.0 - Performance Monitoring & Benchmarking
 * 
 * Mathematical Foundation: Performance measurement is function composition with timing
 * Implementation: Native Performance API + custom statistics
 * Performance Target: ~5-10μs measurement overhead, ~1-5ms statistical calculation
 */

import type { Result } from "../base/result.js";
import { success, failure } from "../base/result.js";
import { SystemError } from "../base/error.js";

/**
 * Performance measurement data point
 */
export interface Measurement {
  readonly operation: string;
  readonly duration: number; // microseconds
  readonly timestamp: number;
  readonly metadata?: Record<string, unknown>;
}

/**
 * Statistical analysis of performance measurements
 */
export interface PerformanceStats {
  readonly operation: string;
  readonly count: number;
  readonly mean: number;
  readonly median: number;
  readonly min: number;
  readonly max: number;
  readonly p95: number; // 95th percentile
  readonly p99: number; // 99th percentile
  readonly stdDev: number;
  readonly totalDuration: number;
}

/**
 * TypeScript tier performance requirements
 * Based on interpreted language tier (100× baseline)
 */
export interface PerformanceTiers {
  readonly result_operations: number; // <100μs
  readonly config_merge: number; // <1ms  
  readonly logger_level_check: number; // <1μs
  readonly cache_memory_ops: number; // <50μs
  readonly measurement_overhead: number; // <10μs
}

/**
 * Default performance tier requirements (microseconds)
 */
const DEFAULT_TIERS: PerformanceTiers = {
  result_operations: 100,
  config_merge: 1000,
  logger_level_check: 1,
  cache_memory_ops: 50,
  measurement_overhead: 10
};

/**
 * Performance monitor with statistical analysis
 */
export class PerformanceMonitor {
  private readonly measurements: Map<string, Measurement[]>;
  private readonly maxSamples: number;
  private readonly enabledOperations: Set<string>;

  constructor(
    options: {
      maxSamples?: number;
      enabledOperations?: string[];
    } = {}
  ) {
    this.measurements = new Map();
    this.maxSamples = options.maxSamples ?? 10000;
    this.enabledOperations = new Set(options.enabledOperations ?? []);
  }

  /**
   * Measure synchronous operation execution
   * Performance: ~5-10μs measurement overhead
   */
  measure<T>(operationName: string, operation: () => T, metadata?: Record<string, unknown>): T {
    if (this.enabledOperations.size > 0 && !this.enabledOperations.has(operationName)) {
      return operation();
    }

    const start = performance.now();
    const result = operation();
    const end = performance.now();
    
    const duration = (end - start) * 1000; // Convert to microseconds
    this.recordMeasurement({
      operation: operationName,
      duration,
      timestamp: Date.now(),
      metadata
    });
    
    return result;
  }

  /**
   * Measure asynchronous operation execution
   * Performance: ~5-10μs measurement overhead (excluding async operation time)
   */
  async measureAsync<T>(
    operationName: string, 
    operation: () => Promise<T>,
    metadata?: Record<string, unknown>
  ): Promise<T> {
    if (this.enabledOperations.size > 0 && !this.enabledOperations.has(operationName)) {
      return operation();
    }

    const start = performance.now();
    const result = await operation();
    const end = performance.now();
    
    const duration = (end - start) * 1000; // Convert to microseconds
    this.recordMeasurement({
      operation: operationName,
      duration,
      timestamp: Date.now(),
      metadata
    });
    
    return result;
  }

  /**
   * Record a measurement manually
   */
  recordMeasurement(measurement: Measurement): void {
    const operations = this.measurements.get(measurement.operation) ?? [];
    operations.push(measurement);
    
    // Limit sample size to prevent memory issues
    if (operations.length > this.maxSamples) {
      operations.shift(); // Remove oldest measurement
    }
    
    this.measurements.set(measurement.operation, operations);
  }

  /**
   * Get statistical analysis for an operation
   * Performance: ~1-5ms for large sample sets
   */
  getStats(operationName: string): Result<PerformanceStats> {
    const measurements = this.measurements.get(operationName);
    
    if (!measurements || measurements.length === 0) {
      return failure(SystemError(
        "NO_MEASUREMENTS",
        `No measurements found for operation: ${operationName}`,
        new Map([["operation", operationName]])
      ));
    }
    
    const durations = measurements.map(m => m.duration).sort((a, b) => a - b);
    const count = durations.length;
    
    // Basic statistics
    const min = durations[0];
    const max = durations[count - 1];
    const sum = durations.reduce((acc, d) => acc + d, 0);
    const mean = sum / count;
    
    // Median
    const median = count % 2 === 0
      ? (durations[Math.floor(count / 2) - 1] + durations[Math.floor(count / 2)]) / 2
      : durations[Math.floor(count / 2)];
    
    // Percentiles
    const p95Index = Math.ceil(count * 0.95) - 1;
    const p99Index = Math.ceil(count * 0.99) - 1;
    const p95 = durations[Math.max(0, p95Index)];
    const p99 = durations[Math.max(0, p99Index)];
    
    // Standard deviation
    const variance = durations.reduce((acc, d) => acc + Math.pow(d - mean, 2), 0) / count;
    const stdDev = Math.sqrt(variance);
    
    return success({
      operation: operationName,
      count,
      mean,
      median,
      min,
      max,
      p95,
      p99,
      stdDev,
      totalDuration: sum
    });
  }

  /**
   * Get all operation names with measurements
   */
  getOperations(): string[] {
    return Array.from(this.measurements.keys());
  }

  /**
   * Clear measurements for specific operation
   */
  clearOperation(operationName: string): void {
    this.measurements.delete(operationName);
  }

  /**
   * Clear all measurements
   */
  clearAll(): void {
    this.measurements.clear();
  }

  /**
   * Get raw measurements for an operation
   */
  getMeasurements(operationName: string): Measurement[] {
    return this.measurements.get(operationName) ?? [];
  }

  /**
   * Check if operation meets performance tier requirements
   */
  checkTierCompliance(
    operationName: string, 
    tierRequirements: Partial<PerformanceTiers> = {}
  ): Result<{
    compliant: boolean;
    requirement: number;
    actual: number;
    operation: string;
  }> {
    const statsResult = this.getStats(operationName);
    if (statsResult._tag === "Left") return statsResult;
    
    const stats = statsResult.right;
    const tiers = { ...DEFAULT_TIERS, ...tierRequirements };
    
    // Map operation names to tier requirements
    const tierMapping: Record<string, keyof PerformanceTiers> = {
      "result_operations": "result_operations",
      "config_merge": "config_merge", 
      "logger_level_check": "logger_level_check",
      "cache_memory_ops": "cache_memory_ops",
      "measurement": "measurement_overhead"
    };
    
    const tierKey = tierMapping[operationName] ?? "measurement_overhead";
    const requirement = tiers[tierKey];
    const actual = stats.p95; // Use 95th percentile for compliance check
    
    return success({
      compliant: actual <= requirement,
      requirement,
      actual,
      operation: operationName
    });
  }

  /**
   * Generate performance report for all operations
   */
  generateReport(tierRequirements?: Partial<PerformanceTiers>): string {
    const operations = this.getOperations();
    if (operations.length === 0) {
      return "No performance measurements recorded.";
    }
    
    let report = "📊 Performance Report\n";
    report += "=" .repeat(50) + "\n\n";
    
    for (const operation of operations) {
      const statsResult = this.getStats(operation);
      if (statsResult._tag === "Left") continue;
      
      const stats = statsResult.right;
      const complianceResult = this.checkTierCompliance(operation, tierRequirements);
      
      report += `Operation: ${operation}\n`;
      report += `-`.repeat(30) + "\n";
      report += `Count: ${stats.count}\n`;
      report += `Mean: ${stats.mean.toFixed(2)}μs\n`;
      report += `Median: ${stats.median.toFixed(2)}μs\n`;
      report += `P95: ${stats.p95.toFixed(2)}μs\n`;
      report += `P99: ${stats.p99.toFixed(2)}μs\n`;
      report += `Min: ${stats.min.toFixed(2)}μs\n`;
      report += `Max: ${stats.max.toFixed(2)}μs\n`;
      report += `Std Dev: ${stats.stdDev.toFixed(2)}μs\n`;
      
      if (complianceResult._tag === "Right") {
        const compliance = complianceResult.right;
        const status = compliance.compliant ? "✅ PASS" : "❌ FAIL";
        report += `Tier Compliance: ${status} (${compliance.actual.toFixed(2)}μs ≤ ${compliance.requirement}μs)\n`;
      }
      
      report += "\n";
    }
    
    return report;
  }
}

/**
 * Global performance monitor instance
 */
let globalMonitor: PerformanceMonitor | undefined;

/**
 * Get or create global performance monitor
 */
export const getGlobalMonitor = (): PerformanceMonitor => {
  if (!globalMonitor) {
    globalMonitor = new PerformanceMonitor();
  }
  return globalMonitor;
};

/**
 * Set global performance monitor
 */
export const setGlobalMonitor = (monitor: PerformanceMonitor): void => {
  globalMonitor = monitor;
};

/**
 * Measure synchronous operation with global monitor
 */
export const measure = <T>(
  operationName: string, 
  operation: () => T,
  metadata?: Record<string, unknown>
): T => getGlobalMonitor().measure(operationName, operation, metadata);

/**
 * Measure asynchronous operation with global monitor
 */
export const measureAsync = <T>(
  operationName: string,
  operation: () => Promise<T>,
  metadata?: Record<string, unknown>
): Promise<T> => getGlobalMonitor().measureAsync(operationName, operation, metadata);

/**
 * Benchmark function execution multiple times
 */
export const benchmark = <T>(
  operationName: string,
  operation: () => T,
  iterations = 1000
): Result<PerformanceStats> => {
  const monitor = new PerformanceMonitor();
  
  try {
    // Warm up
    for (let i = 0; i < Math.min(10, iterations); i++) {
      operation();
    }
    
    // Benchmark
    for (let i = 0; i < iterations; i++) {
      monitor.measure(operationName, operation);
    }
    
    return monitor.getStats(operationName);
  } catch (error) {
    return failure(SystemError(
      "BENCHMARK_ERROR",
      `Benchmark failed: ${error}`,
      new Map([
        ["operation", operationName],
        ["iterations", iterations],
        ["error", error]
      ])
    ));
  }
};

/**
 * Async benchmark function execution multiple times
 */
export const benchmarkAsync = async <T>(
  operationName: string,
  operation: () => Promise<T>,
  iterations = 100
): Promise<Result<PerformanceStats>> => {
  const monitor = new PerformanceMonitor();
  
  try {
    // Warm up
    for (let i = 0; i < Math.min(3, iterations); i++) {
      await operation();
    }
    
    // Benchmark
    for (let i = 0; i < iterations; i++) {
      await monitor.measureAsync(operationName, operation);
    }
    
    return monitor.getStats(operationName);
  } catch (error) {
    return failure(SystemError(
      "ASYNC_BENCHMARK_ERROR",
      `Async benchmark failed: ${error}`,
      new Map([
        ["operation", operationName],
        ["iterations", iterations],
        ["error", error]
      ])
    ));
  }
};

/**
 * Create a performance-monitored version of a function
 */
export const monitored = <TArgs extends unknown[], TReturn>(
  operationName: string,
  fn: (...args: TArgs) => TReturn
) => (...args: TArgs): TReturn => measure(operationName, () => fn(...args));

/**
 * Create a performance-monitored version of an async function
 */
export const monitoredAsync = <TArgs extends unknown[], TReturn>(
  operationName: string,
  fn: (...args: TArgs) => Promise<TReturn>
) => (...args: TArgs): Promise<TReturn> => measureAsync(operationName, () => fn(...args)); 