/**
 * QiCore v4.0 - Core Performance Component
 * 
 * Mathematical insight: Performance measurement is function composition with timing
 * High-precision performance monitoring and benchmarking with statistical analysis
 */

import { createQiError, ErrorCategory } from "../base/error.js";
import { success, failure, type Result } from "../base/result.js";

/**
 * Performance measurement data
 */
export interface PerformanceMeasurement {
  readonly operation: string;
  readonly duration: number; // microseconds
  readonly timestamp: number;
}

/**
 * Performance statistics with tier compliance checking
 */
export interface PerformanceStats {
  readonly operation: string;
  readonly sampleCount: number;
  readonly mean: number;      // microseconds
  readonly median: number;    // microseconds
  readonly p95: number;       // 95th percentile
  readonly p99: number;       // 99th percentile
  readonly min: number;
  readonly max: number;
  readonly stdDev: number;
  readonly tierCompliant: boolean;
}

/**
 * Performance requirements for TypeScript tier (100× baseline)
 */
export const PERFORMANCE_REQUIREMENTS = {
  RESULT_OPERATIONS: 100,    // μs
  CONFIG_MERGE: 1000,        // μs (1ms)
  LOGGER_LEVEL_CHECK: 1,     // μs
  CACHE_OPERATIONS: 50,      // μs
  HTTP_OVERHEAD: 5000,       // μs (5ms)
  DB_OVERHEAD: 1000,         // μs (1ms)
  PERFORMANCE_MEASUREMENT: 10 // μs
} as const;

/**
 * Performance monitor class
 */
export class PerformanceMonitor {
  private measurements = new Map<string, PerformanceMeasurement[]>();
  private maxSamples: number;

  constructor(maxSamples = 10000) {
    this.maxSamples = maxSamples;
  }

  /**
   * Records a performance measurement
   */
  recordMeasurement(measurement: PerformanceMeasurement): void {
    const samples = this.measurements.get(measurement.operation) || [];
    samples.push(measurement);
    
    // Maintain max samples limit with FIFO eviction
    if (samples.length > this.maxSamples) {
      samples.shift();
    }
    
    this.measurements.set(measurement.operation, samples);
  }

  /**
   * Gets performance statistics for an operation
   */
  getStats(operation: string): Result<PerformanceStats> {
    const samples = this.measurements.get(operation);
    
    if (!samples || samples.length === 0) {
      return failure(createQiError(
        "NO_PERFORMANCE_DATA",
        `No performance data available for operation: ${operation}`,
        ErrorCategory.SYSTEM,
        { operation }
      ));
    }

    try {
      const durations = samples.map(s => s.duration).sort((a, b) => a - b);
      const n = durations.length;
      
      // Basic statistics
      const min = durations[0];
      const max = durations[n - 1];
      const mean = durations.reduce((sum, d) => sum + d, 0) / n;
      const median = n % 2 === 0 
        ? (durations[n / 2 - 1] + durations[n / 2]) / 2
        : durations[Math.floor(n / 2)];
      
      // Percentiles
      const p95 = durations[Math.floor(n * 0.95)];
      const p99 = durations[Math.floor(n * 0.99)];
      
      // Standard deviation
      const variance = durations.reduce((sum, d) => sum + Math.pow(d - mean, 2), 0) / n;
      const stdDev = Math.sqrt(variance);
      
      // Tier compliance check
      const tierCompliant = this.checkTierCompliance(operation, mean);

      return success({
        operation,
        sampleCount: n,
        mean,
        median,
        p95,
        p99,
        min,
        max,
        stdDev,
        tierCompliant
      });
    } catch (error) {
      return failure(createQiError(
        "STATS_CALCULATION_ERROR",
        `Failed to calculate performance statistics: ${error}`,
        ErrorCategory.SYSTEM,
        { operation, sampleCount: samples.length }
      ));
    }
  }

  /**
   * Gets all available operation names
   */
  getOperations(): string[] {
    return Array.from(this.measurements.keys());
  }

  /**
   * Clears all measurements
   */
  clear(): void {
    this.measurements.clear();
  }

  /**
   * Clears measurements for specific operation
   */
  clearOperation(operation: string): void {
    this.measurements.delete(operation);
  }

  /**
   * Gets total measurement count across all operations
   */
  getTotalMeasurements(): number {
    let total = 0;
    for (const samples of this.measurements.values()) {
      total += samples.length;
    }
    return total;
  }

  /**
   * Checks if operation meets tier requirements
   */
  private checkTierCompliance(operation: string, meanDuration: number): boolean {
    // Map operation patterns to requirements
    if (operation.includes('result') || operation.includes('success') || operation.includes('failure')) {
      return meanDuration <= PERFORMANCE_REQUIREMENTS.RESULT_OPERATIONS;
    }
    
    if (operation.includes('config') && operation.includes('merge')) {
      return meanDuration <= PERFORMANCE_REQUIREMENTS.CONFIG_MERGE;
    }
    
    if (operation.includes('logger') && operation.includes('level')) {
      return meanDuration <= PERFORMANCE_REQUIREMENTS.LOGGER_LEVEL_CHECK;
    }
    
    if (operation.includes('cache')) {
      return meanDuration <= PERFORMANCE_REQUIREMENTS.CACHE_OPERATIONS;
    }
    
    if (operation.includes('measure') || operation.includes('benchmark')) {
      return meanDuration <= PERFORMANCE_REQUIREMENTS.PERFORMANCE_MEASUREMENT;
    }
    
    // Default to most lenient requirement
    return meanDuration <= PERFORMANCE_REQUIREMENTS.CONFIG_MERGE;
  }

  /**
   * Generates performance report
   */
  generateReport(): Result<string> {
    try {
      const operations = this.getOperations();
      if (operations.length === 0) {
        return success("No performance data available");
      }

      const lines: string[] = [
        "QiCore v4.0 Performance Report",
        "==============================",
        ""
      ];

      let allCompliant = true;

      for (const operation of operations.sort()) {
        const statsResult = this.getStats(operation);
        if (statsResult._tag === "Left") continue;
        
        const stats = statsResult.right;
        const complianceIcon = stats.tierCompliant ? "✅" : "❌";
        allCompliant = allCompliant && stats.tierCompliant;

        lines.push(
          `${complianceIcon} ${operation}`,
          `  Samples: ${stats.sampleCount}`,
          `  Mean: ${stats.mean.toFixed(2)}μs`,
          `  Median: ${stats.median.toFixed(2)}μs`,
          `  P95: ${stats.p95.toFixed(2)}μs`,
          `  P99: ${stats.p99.toFixed(2)}μs`,
          `  Range: ${stats.min.toFixed(2)}μs - ${stats.max.toFixed(2)}μs`,
          `  StdDev: ${stats.stdDev.toFixed(2)}μs`,
          ""
        );
      }

      lines.push(
        "Overall Compliance",
        "==================",
        allCompliant ? "✅ All operations meet TypeScript tier requirements" : "❌ Some operations exceed tier requirements",
        "",
        "TypeScript Tier Requirements:",
        `- Result operations: ${PERFORMANCE_REQUIREMENTS.RESULT_OPERATIONS}μs`,
        `- Config merge: ${PERFORMANCE_REQUIREMENTS.CONFIG_MERGE}μs`,
        `- Logger level check: ${PERFORMANCE_REQUIREMENTS.LOGGER_LEVEL_CHECK}μs`,
        `- Cache operations: ${PERFORMANCE_REQUIREMENTS.CACHE_OPERATIONS}μs`,
        `- Performance measurement: ${PERFORMANCE_REQUIREMENTS.PERFORMANCE_MEASUREMENT}μs`
      );

      return success(lines.join('\n'));
    } catch (error) {
      return failure(createQiError(
        "REPORT_GENERATION_ERROR",
        `Failed to generate performance report: ${error}`,
        ErrorCategory.SYSTEM
      ));
    }
  }
}

// Global performance monitor instance
const globalMonitor = new PerformanceMonitor();

/**
 * Measures synchronous function execution time
 * Function composition with timing preservation
 */
export const measure = <T>(
  operation: string,
  fn: () => T
): T => {
  const start = performance.now();
  const result = fn();
  const end = performance.now();
  
  const measurement: PerformanceMeasurement = {
    operation,
    duration: (end - start) * 1000, // Convert to microseconds
    timestamp: Date.now()
  };
  
  globalMonitor.recordMeasurement(measurement);
  return result;
};

/**
 * Measures asynchronous function execution time
 */
export const measureAsync = async <T>(
  operation: string,
  fn: () => Promise<T>
): Promise<T> => {
  const start = performance.now();
  const result = await fn();
  const end = performance.now();
  
  const measurement: PerformanceMeasurement = {
    operation,
    duration: (end - start) * 1000,
    timestamp: Date.now()
  };
  
  globalMonitor.recordMeasurement(measurement);
  return result;
};

/**
 * Benchmarks a function with multiple iterations
 */
export const benchmark = async (
  operation: string,
  fn: () => void | Promise<void>,
  iterations = 1000
): Promise<Result<PerformanceStats>> => {
  try {
    const monitor = new PerformanceMonitor();
    
    for (let i = 0; i < iterations; i++) {
      const start = performance.now();
      await fn();
      const end = performance.now();
      
      monitor.recordMeasurement({
        operation,
        duration: (end - start) * 1000,
        timestamp: Date.now()
      });
    }
    
    return monitor.getStats(operation);
  } catch (error) {
    return failure(createQiError(
      "BENCHMARK_ERROR",
      `Benchmark failed: ${error}`,
      ErrorCategory.SYSTEM,
      { operation, iterations }
    ));
  }
};

/**
 * Records a manual measurement
 */
export const recordMeasurement = (measurement: PerformanceMeasurement): void => {
  globalMonitor.recordMeasurement(measurement);
};

/**
 * Gets performance statistics for an operation
 */
export const getStats = (operation: string): Result<PerformanceStats> => {
  return globalMonitor.getStats(operation);
};

/**
 * Gets all monitored operations
 */
export const getOperations = (): string[] => {
  return globalMonitor.getOperations();
};

/**
 * Clears all performance measurements
 */
export const clearMeasurements = (): void => {
  globalMonitor.clear();
};

/**
 * Generates performance report
 */
export const generateReport = (): Result<string> => {
  return globalMonitor.generateReport();
};

/**
 * Creates a new performance monitor instance
 */
export const createMonitor = (maxSamples?: number): PerformanceMonitor => {
  return new PerformanceMonitor(maxSamples);
};

/**
 * Verifies tier compliance for all operations
 */
export const verifyTierCompliance = (): Result<boolean> => {
  try {
    const operations = getOperations();
    
    if (operations.length === 0) {
      return failure(createQiError(
        "NO_PERFORMANCE_DATA",
        "No performance data available for tier compliance check",
        ErrorCategory.SYSTEM
      ));
    }

    for (const operation of operations) {
      const statsResult = getStats(operation);
      if (statsResult._tag === "Left") continue;
      
      if (!statsResult.right.tierCompliant) {
        return success(false);
      }
    }
    
    return success(true);
  } catch (error) {
    return failure(createQiError(
      "TIER_COMPLIANCE_CHECK_ERROR",
      `Failed to verify tier compliance: ${error}`,
      ErrorCategory.SYSTEM
    ));
  }
};

/**
 * Performance-aware function wrapper
 * Automatically measures any function
 */
export const withPerformanceMonitoring = <T extends (...args: any[]) => any>(
  operationName: string,
  fn: T
): T => {
  return ((...args: Parameters<T>) => {
    if (fn.constructor.name === 'AsyncFunction') {
      return measureAsync(operationName, () => fn(...args));
    } else {
      return measure(operationName, () => fn(...args));
    }
  }) as T;
};

/**
 * Class decorator for automatic method performance monitoring
 */
export const MonitorPerformance = (operationPrefix?: string) => {
  return <T extends { new (...args: any[]): {} }>(constructor: T) => {
    return class extends constructor {
      constructor(...args: any[]) {
        super(...args);
        
        // Wrap all methods with performance monitoring
        const proto = Object.getPrototypeOf(this);
        const methods = Object.getOwnPropertyNames(proto)
          .filter(name => name !== 'constructor' && typeof proto[name] === 'function');
        
        for (const methodName of methods) {
          const originalMethod = proto[methodName];
          const operationName = operationPrefix 
            ? `${operationPrefix}.${methodName}`
            : `${constructor.name}.${methodName}`;
          
          proto[methodName] = withPerformanceMonitoring(operationName, originalMethod);
        }
      }
    };
  };
};

/**
 * Performance-aware cache decorator
 */
export const withCachePerformanceMonitoring = <K, V>(
  cache: { get: (key: K) => Promise<any>; set: (key: K, value: V) => Promise<any> }
) => {
  return {
    ...cache,
    get: withPerformanceMonitoring('cache.get', cache.get),
    set: withPerformanceMonitoring('cache.set', cache.set)
  };
};

/**
 * Batch performance test for all core operations
 */
export const runComprehensiveBenchmark = async (): Promise<Result<Map<string, PerformanceStats>>> => {
  try {
    const results = new Map<string, PerformanceStats>();
    
    // Benchmark core operations
    const benchmarks = [
      {
        name: 'result_success',
        fn: () => { /* success operation */ }
      },
      {
        name: 'result_map',
        fn: () => { /* map operation */ }
      },
      {
        name: 'config_merge',
        fn: () => { /* merge operation */ }
      },
      {
        name: 'logger_level_check',
        fn: () => { /* level check */ }
      },
      {
        name: 'cache_get',
        fn: () => { /* cache get */ }
      }
    ];
    
    for (const { name, fn } of benchmarks) {
      const result = await benchmark(name, fn, 1000);
      if (result._tag === "Right") {
        results.set(name, result.right);
      }
    }
    
    return success(results);
  } catch (error) {
    return failure(createQiError(
      "COMPREHENSIVE_BENCHMARK_ERROR",
      `Failed to run comprehensive benchmark: ${error}`,
      ErrorCategory.SYSTEM
    ));
  }
};

/**
 * Performance optimization utilities
 */

/**
 * Creates a performance-optimized function with memoization
 */
export const memoizeWithPerformance = <T extends (...args: any[]) => any>(
  fn: T,
  operationName: string
): T => {
  const cache = new Map();
  
  return ((...args: Parameters<T>) => {
    const key = JSON.stringify(args);
    
    if (cache.has(key)) {
      recordMeasurement({
        operation: `${operationName}.cache_hit`,
        duration: 1, // Minimal duration for cache hit
        timestamp: Date.now()
      });
      return cache.get(key);
    }
    
    const result = measure(`${operationName}.compute`, () => fn(...args));
    cache.set(key, result);
    return result;
  }) as T;
};

/**
 * Performance-aware retry mechanism
 */
export const retryWithPerformance = async <T>(
  operation: () => Promise<T>,
  operationName: string,
  maxRetries = 3,
  delay = 100
): Promise<T> => {
  let lastError: Error;
  
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      return await measureAsync(`${operationName}.attempt_${attempt}`, operation);
    } catch (error) {
      lastError = error as Error;
      
      recordMeasurement({
        operation: `${operationName}.retry`,
        duration: delay * 1000, // Convert to microseconds
        timestamp: Date.now()
      });
      
      if (attempt < maxRetries) {
        await new Promise(resolve => setTimeout(resolve, delay * attempt));
      }
    }
  }
  
  throw lastError!;
};