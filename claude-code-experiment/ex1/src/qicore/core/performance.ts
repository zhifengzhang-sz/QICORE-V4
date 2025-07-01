import { ErrorCategory, createQiError } from "../base/error.js";
import { type Result, failure, success } from "../base/result.js";

export interface PerformanceMeasurement {
	duration: number; // microseconds
	startTime: number;
	endTime: number;
	operation: string;
}

export interface PerformanceStats {
	count: number;
	mean: number;
	median: number;
	p95: number;
	p99: number;
	min: number;
	max: number;
	standardDeviation: number;
}

export interface BenchmarkResult {
	operation: string;
	measurements: PerformanceMeasurement[];
	stats: PerformanceStats;
	tierCompliance: {
		target: number;
		actual: number;
		compliant: boolean;
		tier: "native" | "vm" | "interpreted" | "functional";
	};
}

// TypeScript tier requirements (interpreted tier)
const TIER_REQUIREMENTS = {
	native: 1, // < 1μs
	vm: 10, // < 10μs
	interpreted: 100, // < 100μs
	functional: 50, // < 50μs
};

class PerformanceTracker {
	private measurements: Map<string, PerformanceMeasurement[]> = new Map();
	private maxSamples = 1000; // Limit memory usage

	measure<T>(operation: string, fn: () => T): Result<T> {
		try {
			const start = performance.now();
			const result = fn();
			const end = performance.now();

			const measurement: PerformanceMeasurement = {
				duration: (end - start) * 1000, // Convert to microseconds
				startTime: start,
				endTime: end,
				operation,
			};

			this.recordMeasurement(operation, measurement);
			return success(result);
		} catch (error) {
			return failure(
				createQiError(
					"PERFORMANCE_MEASUREMENT_ERROR",
					`Performance measurement failed for operation ${operation}: ${error}`,
					ErrorCategory.UNKNOWN,
					{ operation, originalError: error },
				),
			);
		}
	}

	async measureAsync<T>(
		operation: string,
		fn: () => Promise<T>,
	): Promise<Result<T>> {
		try {
			const start = performance.now();
			const result = await fn();
			const end = performance.now();

			const measurement: PerformanceMeasurement = {
				duration: (end - start) * 1000, // Convert to microseconds
				startTime: start,
				endTime: end,
				operation,
			};

			this.recordMeasurement(operation, measurement);
			return success(result);
		} catch (error) {
			return failure(
				createQiError(
					"PERFORMANCE_MEASUREMENT_ERROR",
					`Async performance measurement failed for operation ${operation}: ${error}`,
					ErrorCategory.UNKNOWN,
					{ operation, originalError: error },
				),
			);
		}
	}

	getStats(operation: string): Result<PerformanceStats> {
		const measurements = this.measurements.get(operation);

		if (!measurements || measurements.length === 0) {
			return failure(
				createQiError(
					"NO_MEASUREMENTS",
					`No measurements found for operation: ${operation}`,
					ErrorCategory.VALIDATION,
					{ operation },
				),
			);
		}

		const durations = measurements.map((m) => m.duration).sort((a, b) => a - b);

		const stats: PerformanceStats = {
			count: durations.length,
			mean: this.calculateMean(durations),
			median: this.calculateMedian(durations),
			p95: this.calculatePercentile(durations, 95),
			p99: this.calculatePercentile(durations, 99),
			min: durations[0],
			max: durations[durations.length - 1],
			standardDeviation: this.calculateStandardDeviation(durations),
		};

		return success(stats);
	}

	benchmark(
		operation: string,
		fn: () => unknown,
		options: {
			iterations?: number;
			warmup?: number;
			tier?: keyof typeof TIER_REQUIREMENTS;
		} = {},
	): Result<BenchmarkResult> {
		const { iterations = 1000, warmup = 100, tier = "interpreted" } = options;

		try {
			// Clear previous measurements for this operation
			this.measurements.delete(operation);

			// Warmup runs
			for (let i = 0; i < warmup; i++) {
				fn();
			}

			// Actual benchmark runs
			const measurements: PerformanceMeasurement[] = [];

			for (let i = 0; i < iterations; i++) {
				const start = performance.now();
				fn();
				const end = performance.now();

				measurements.push({
					duration: (end - start) * 1000, // Convert to microseconds
					startTime: start,
					endTime: end,
					operation,
				});
			}

			// Store measurements
			this.measurements.set(operation, measurements);

			// Calculate statistics
			const statsResult = this.getStats(operation);
			if (statsResult._tag === "Left") {
				return statsResult;
			}

			const stats = statsResult.right;
			const targetTime = TIER_REQUIREMENTS[tier];

			const result: BenchmarkResult = {
				operation,
				measurements,
				stats,
				tierCompliance: {
					target: targetTime,
					actual: stats.mean,
					compliant: stats.mean <= targetTime,
					tier,
				},
			};

			return success(result);
		} catch (error) {
			return failure(
				createQiError(
					"BENCHMARK_ERROR",
					`Benchmark failed for operation ${operation}: ${error}`,
					ErrorCategory.UNKNOWN,
					{ operation, originalError: error },
				),
			);
		}
	}

	async benchmarkAsync(
		operation: string,
		fn: () => Promise<unknown>,
		options: {
			iterations?: number;
			warmup?: number;
			tier?: keyof typeof TIER_REQUIREMENTS;
		} = {},
	): Promise<Result<BenchmarkResult>> {
		const { iterations = 100, warmup = 10, tier = "interpreted" } = options;

		try {
			// Clear previous measurements for this operation
			this.measurements.delete(operation);

			// Warmup runs
			for (let i = 0; i < warmup; i++) {
				await fn();
			}

			// Actual benchmark runs
			const measurements: PerformanceMeasurement[] = [];

			for (let i = 0; i < iterations; i++) {
				const start = performance.now();
				await fn();
				const end = performance.now();

				measurements.push({
					duration: (end - start) * 1000, // Convert to microseconds
					startTime: start,
					endTime: end,
					operation,
				});
			}

			// Store measurements
			this.measurements.set(operation, measurements);

			// Calculate statistics
			const statsResult = this.getStats(operation);
			if (statsResult._tag === "Left") {
				return statsResult;
			}

			const stats = statsResult.right;
			const targetTime = TIER_REQUIREMENTS[tier];

			const result: BenchmarkResult = {
				operation,
				measurements,
				stats,
				tierCompliance: {
					target: targetTime,
					actual: stats.mean,
					compliant: stats.mean <= targetTime,
					tier,
				},
			};

			return success(result);
		} catch (error) {
			return failure(
				createQiError(
					"ASYNC_BENCHMARK_ERROR",
					`Async benchmark failed for operation ${operation}: ${error}`,
					ErrorCategory.UNKNOWN,
					{ operation, originalError: error },
				),
			);
		}
	}

	reset(operation?: string): void {
		if (operation) {
			this.measurements.delete(operation);
		} else {
			this.measurements.clear();
		}
	}

	getAllOperations(): string[] {
		return Array.from(this.measurements.keys());
	}

	private recordMeasurement(
		operation: string,
		measurement: PerformanceMeasurement,
	): void {
		if (!this.measurements.has(operation)) {
			this.measurements.set(operation, []);
		}

		const measurements = this.measurements.get(operation);
		if (!measurements) {
			throw new Error(`No measurements found for operation: ${operation}`);
		}
		measurements.push(measurement);

		// Limit memory usage by keeping only recent measurements
		if (measurements.length > this.maxSamples) {
			measurements.splice(0, measurements.length - this.maxSamples);
		}
	}

	private calculateMean(values: number[]): number {
		return values.reduce((sum, val) => sum + val, 0) / values.length;
	}

	private calculateMedian(sortedValues: number[]): number {
		const mid = Math.floor(sortedValues.length / 2);

		if (sortedValues.length % 2 === 0) {
			return (sortedValues[mid - 1] + sortedValues[mid]) / 2;
		}
		return sortedValues[mid];
	}

	private calculatePercentile(
		sortedValues: number[],
		percentile: number,
	): number {
		const index = Math.ceil((percentile / 100) * sortedValues.length) - 1;
		return sortedValues[Math.max(0, Math.min(index, sortedValues.length - 1))];
	}

	private calculateStandardDeviation(values: number[]): number {
		const mean = this.calculateMean(values);
		const squaredDiffs = values.map((value) => (value - mean) ** 2);
		const avgSquaredDiff = this.calculateMean(squaredDiffs);
		return Math.sqrt(avgSquaredDiff);
	}
}

// Global performance tracker instance
const globalTracker = new PerformanceTracker();

// Export functions that use the global tracker
export const measure = <T>(operation: string, fn: () => T): Result<T> => {
	return globalTracker.measure(operation, fn);
};

export const measureAsync = async <T>(
	operation: string,
	fn: () => Promise<T>,
): Promise<Result<T>> => {
	return globalTracker.measureAsync(operation, fn);
};

export const getStats = (operation: string): Result<PerformanceStats> => {
	return globalTracker.getStats(operation);
};

export const benchmark = (
	operation: string,
	fn: () => unknown,
	options?: {
		iterations?: number;
		warmup?: number;
		tier?: keyof typeof TIER_REQUIREMENTS;
	},
): Result<BenchmarkResult> => {
	return globalTracker.benchmark(operation, fn, options);
};

export const benchmarkAsync = async (
	operation: string,
	fn: () => Promise<unknown>,
	options?: {
		iterations?: number;
		warmup?: number;
		tier?: keyof typeof TIER_REQUIREMENTS;
	},
): Promise<Result<BenchmarkResult>> => {
	return globalTracker.benchmarkAsync(operation, fn, options);
};

export const reset = (operation?: string): void => {
	globalTracker.reset(operation);
};

export const getAllOperations = (): string[] => {
	return globalTracker.getAllOperations();
};

// Create a new isolated performance tracker
export const createTracker = (): PerformanceTracker => {
	return new PerformanceTracker();
};
