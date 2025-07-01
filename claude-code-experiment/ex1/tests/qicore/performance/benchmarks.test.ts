import { describe, expect, it } from "vitest";
import {
	ErrorCategory,
	createQiError,
} from "../../../src/qicore/base/error.js";
import {
	failure,
	flatMap,
	map,
	success,
} from "../../../src/qicore/base/result.js";
import { isFailure, isSuccess } from "../../../src/qicore/base/result.js";
import { createMemoryCache } from "../../../src/qicore/core/cache.js";
import { fromObject, merge } from "../../../src/qicore/core/config.js";
import {
	benchmark,
	benchmarkAsync,
	createTracker,
	getAllOperations,
	getStats,
	measure,
	measureAsync,
	reset,
} from "../../../src/qicore/core/performance.js";

describe("Performance Benchmarks", () => {
	describe("Result Performance", () => {
		it("should meet TypeScript tier requirements for Result operations", () => {
			const benchmarkResult = benchmark(
				"result_operations",
				() => {
					const result = success(42);
					const mapped = map((x: number) => x * 2)(result);
					const chained = flatMap((x: number) => success(x + 1))(mapped);
					return chained;
				},
				{ iterations: 1000, tier: "interpreted" },
			);

			expect(isSuccess(benchmarkResult)).toBe(true);
			if (benchmarkResult._tag === "Right") {
				const bench = benchmarkResult.right;
				expect(bench.tierCompliance.compliant).toBe(true);
				expect(bench.stats.mean).toBeLessThan(100); // < 100μs for interpreted tier
				expect(bench.measurements.length).toBe(1000);
			}
		});

		it("should benchmark Result creation and error handling", () => {
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
			);

			const benchmarkResult = benchmark(
				"result_error_handling",
				() => {
					const success1 = success(42);
					const failure1 = failure<number>(error);
					const mapped = map((x: number) => x * 2)(success1);
					const errorMapped = map((x: number) => x * 2)(failure1);
					return { mapped, errorMapped };
				},
				{ iterations: 1000, tier: "interpreted" },
			);

			expect(isSuccess(benchmarkResult)).toBe(true);
			if (benchmarkResult._tag === "Right") {
				const bench = benchmarkResult.right;
				expect(bench.tierCompliance.compliant).toBe(true);
				expect(bench.stats.mean).toBeLessThan(100);
			}
		});
	});

	describe("Configuration Performance", () => {
		it("should meet TypeScript tier requirements for config merge", () => {
			const config1 = fromObject({
				server: { host: "localhost", port: 8080 },
				database: { url: "postgres://localhost" },
			});
			const config2 = fromObject({
				server: { port: 3000, ssl: true },
				cache: { enabled: true },
			});

			if (config1._tag === "Right" && config2._tag === "Right") {
				const benchmarkResult = benchmark(
					"config_merge",
					() => {
						return merge(config1.right, config2.right);
					},
					{ iterations: 1000, tier: "interpreted" },
				);

				expect(isSuccess(benchmarkResult)).toBe(true);
				if (benchmarkResult._tag === "Right") {
					const bench = benchmarkResult.right;
					// Config merge has a higher target (1ms vs 100μs)
					expect(bench.stats.mean).toBeLessThan(1000); // < 1ms for config operations
				}
			}
		});

		it("should benchmark object parsing performance", () => {
			const largeObject = {
				app: {
					name: "test-app",
					version: "1.0.0",
					features: ["auth", "logging", "caching"],
					settings: {
						timeout: 30000,
						retries: 3,
						cache: {
							ttl: 3600,
							maxSize: 1000,
						},
					},
				},
				database: {
					host: "localhost",
					port: 5432,
					ssl: true,
					pool: {
						min: 1,
						max: 10,
					},
				},
			};

			const benchmarkResult = benchmark(
				"config_parsing",
				() => {
					return fromObject(largeObject);
				},
				{ iterations: 1000, tier: "interpreted" },
			);

			expect(isSuccess(benchmarkResult)).toBe(true);
			if (benchmarkResult._tag === "Right") {
				const bench = benchmarkResult.right;
				expect(bench.stats.mean).toBeLessThan(1000); // < 1ms for parsing
			}
		});
	});

	describe("Cache Performance", () => {
		it("should meet TypeScript tier requirements for cache operations", () => {
			const cacheResult = createMemoryCache<string, number>({ maxSize: 1000 });
			expect(isSuccess(cacheResult)).toBe(true);

			if (cacheResult._tag === "Right") {
				const cache = cacheResult.right;

				// Pre-populate cache for get operations
				for (let i = 0; i < 100; i++) {
					cache.set(`key${i}`, i);
				}

				const benchmarkResult = benchmark(
					"cache_operations",
					() => {
						const key = `key${Math.floor(Math.random() * 100)}`;
						const getResult = cache.get(key);
						const setResult = cache.set(`new_${Date.now()}`, Math.random());
						return { getResult, setResult };
					},
					{ iterations: 1000, tier: "interpreted" },
				);

				expect(isSuccess(benchmarkResult)).toBe(true);
				if (benchmarkResult._tag === "Right") {
					const bench = benchmarkResult.right;
					expect(bench.tierCompliance.compliant).toBe(true);
					expect(bench.stats.mean).toBeLessThan(50); // < 50μs for cache operations
				}
			}
		});

		it("should benchmark cache eviction performance", () => {
			const cacheResult = createMemoryCache<string, number>({ maxSize: 10 });
			expect(isSuccess(cacheResult)).toBe(true);

			if (cacheResult._tag === "Right") {
				const cache = cacheResult.right;

				const benchmarkResult = benchmark(
					"cache_eviction",
					() => {
						// This will trigger eviction after cache fills up
						const key = `key_${Date.now()}_${Math.random()}`;
						return cache.set(key, Math.random());
					},
					{ iterations: 100, tier: "interpreted" },
				);

				expect(isSuccess(benchmarkResult)).toBe(true);
				if (benchmarkResult._tag === "Right") {
					const bench = benchmarkResult.right;
					expect(bench.stats.mean).toBeLessThan(100); // Should still be fast even with eviction
				}
			}
		});
	});

	describe("Performance Measurement Overhead", () => {
		it("should have minimal measurement overhead", () => {
			const directBenchmark = benchmark(
				"direct_operation",
				() => {
					let sum = 0;
					for (let i = 0; i < 1000; i++) {
						sum += i;
					}
					return sum;
				},
				{ iterations: 100 },
			);

			const measuredBenchmark = benchmark(
				"measured_operation",
				() => {
					const result = measure("inner_operation", () => {
						let sum = 0;
						for (let i = 0; i < 1000; i++) {
							sum += i;
						}
						return sum;
					});
					return result;
				},
				{ iterations: 100 },
			);

			expect(isSuccess(directBenchmark)).toBe(true);
			expect(isSuccess(measuredBenchmark)).toBe(true);

			if (
				directBenchmark._tag === "Right" &&
				measuredBenchmark._tag === "Right"
			) {
				const directTime = directBenchmark.right.stats.mean;
				const measuredTime = measuredBenchmark.right.stats.mean;

				// Measurement overhead should be reasonable (less than 200% of operation time)
				const overhead = measuredTime - directTime;
				expect(overhead).toBeLessThan(directTime * 2.0);
			}
		});

		it("should measure async operations correctly", async () => {
			const benchmarkResult = await benchmarkAsync(
				"async_operation",
				async () => {
					await new Promise((resolve) => setTimeout(resolve, 1));
					return 42;
				},
				{ iterations: 10 },
			);

			expect(isSuccess(benchmarkResult)).toBe(true);
			if (benchmarkResult._tag === "Right") {
				const bench = benchmarkResult.right;
				// Should measure at least 1ms per operation (the setTimeout)
				expect(bench.stats.mean).toBeGreaterThan(1000); // > 1ms
				expect(bench.measurements.length).toBe(10);
			}
		});
	});

	describe("Statistical Analysis", () => {
		it("should calculate statistics correctly", () => {
			// Create predictable performance data
			reset("test_stats");

			const values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]; // microseconds

			for (const value of values) {
				measure("test_stats", () => {
					// Simulate work that takes a specific amount of time
					const start = performance.now();
					while ((performance.now() - start) * 1000 < value) {
						// Busy wait
					}
					return value;
				});
			}

			const statsResult = getStats("test_stats");
			expect(isSuccess(statsResult)).toBe(true);

			if (statsResult._tag === "Right") {
				const stats = statsResult.right;

				expect(stats.count).toBe(10);
				expect(stats.min).toBeGreaterThan(0);
				expect(stats.max).toBeGreaterThan(stats.min);
				expect(stats.mean).toBeGreaterThan(0);
				expect(stats.median).toBeGreaterThan(0);
				expect(stats.p95).toBeGreaterThanOrEqual(stats.median);
				expect(stats.p99).toBeGreaterThanOrEqual(stats.p95);
				expect(stats.standardDeviation).toBeGreaterThan(0);
			}
		});
	});

	describe("Tier Compliance Verification", () => {
		it("should verify all core components meet TypeScript tier requirements", () => {
			const testResults = [];

			// Test Result operations
			const resultBench = benchmark(
				"result_comprehensive",
				() => {
					const result = success(42);
					const mapped = map((x: number) => x * 2)(result);
					const chained = flatMap((x: number) => success(x.toString()))(mapped);
					return chained;
				},
				{ iterations: 1000, tier: "interpreted" },
			);

			if (resultBench._tag === "Right") {
				testResults.push({
					component: "Result",
					compliant: resultBench.right.tierCompliance.compliant,
					actual: resultBench.right.stats.mean,
					target: resultBench.right.tierCompliance.target,
				});
			}

			// Test Configuration operations
			const config1 = fromObject({ a: 1, b: { c: 2 } });
			const config2 = fromObject({ b: { d: 3 }, e: 4 });

			if (config1._tag === "Right" && config2._tag === "Right") {
				const configBench = benchmark(
					"config_comprehensive",
					() => {
						const merged = merge(config1.right, config2.right);
						return merged;
					},
					{ iterations: 1000, tier: "interpreted" },
				);

				if (configBench._tag === "Right") {
					testResults.push({
						component: "Configuration",
						compliant: configBench.right.stats.mean < 1000, // 1ms target for config
						actual: configBench.right.stats.mean,
						target: 1000,
					});
				}
			}

			// Test Cache operations
			const cacheResult = createMemoryCache<string, number>({ maxSize: 100 });
			if (cacheResult._tag === "Right") {
				const cache = cacheResult.right;

				const cacheBench = benchmark(
					"cache_comprehensive",
					() => {
						const key = `key${Math.floor(Math.random() * 50)}`;
						cache.set(key, Math.random());
						return cache.get(key);
					},
					{ iterations: 1000, tier: "interpreted" },
				);

				if (cacheBench._tag === "Right") {
					testResults.push({
						component: "Cache",
						compliant: cacheBench.right.tierCompliance.compliant,
						actual: cacheBench.right.stats.mean,
						target: cacheBench.right.tierCompliance.target,
					});
				}
			}

			// Verify all components meet their targets
			for (const result of testResults) {
				console.log(
					`${result.component}: ${result.actual.toFixed(2)}μs (target: ${result.target}μs) - ${result.compliant ? "✅ PASS" : "❌ FAIL"}`,
				);
				expect(result.compliant).toBe(true);
			}

			expect(testResults.length).toBeGreaterThan(0);
		});
	});

	describe("Memory Usage and Cleanup", () => {
		it("should not leak memory during repeated operations", () => {
			const tracker = createTracker();

			// Perform many operations and check that getAllOperations doesn't grow indefinitely
			for (let i = 0; i < 50; i++) {
				tracker.measure(`operation_${i}`, () => Math.random());
			}

			const operations = tracker.getAllOperations();
			expect(operations.length).toBe(50);

			// Reset should clear operations
			tracker.reset();
			expect(tracker.getAllOperations().length).toBe(0);
		});
	});
});
