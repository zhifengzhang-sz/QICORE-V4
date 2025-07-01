/**
 * @fileoverview Metrics Collector Unit Tests
 * @purpose Test performance monitoring and health status calculation
 */

import { describe, expect, it } from "vitest";

// Note: This tests the metrics collector pattern from our implementation
interface AgentMetrics {
  requestCount: number;
  successCount: number;
  failureCount: number;
  successRate: number;
  averageResponseTime: number;
  rateLimitHits: number;
  circuitBreakerTrips: number;
}

class MetricsCollector {
  private requestCount = 0;
  private successCount = 0;
  private failureCount = 0;
  private rateLimitHits = 0;
  private circuitBreakerTrips = 0;
  private readonly responseTimes: number[] = [];

  recordRequest(success: boolean, responseTime: number): void {
    this.requestCount++;
    this.responseTimes.push(responseTime);

    if (success) {
      this.successCount++;
    } else {
      this.failureCount++;
    }
  }

  recordRateLimit(): void {
    this.rateLimitHits++;
  }

  recordCircuitBreakerTrip(): void {
    this.circuitBreakerTrips++;
  }

  getMetrics(): AgentMetrics {
    const avgResponseTime =
      this.responseTimes.length > 0
        ? this.responseTimes.reduce((a, b) => a + b, 0) / this.responseTimes.length
        : 0;

    return {
      requestCount: this.requestCount,
      successCount: this.successCount,
      failureCount: this.failureCount,
      successRate: this.requestCount > 0 ? this.successCount / this.requestCount : 0,
      averageResponseTime: avgResponseTime,
      rateLimitHits: this.rateLimitHits,
      circuitBreakerTrips: this.circuitBreakerTrips,
    };
  }

  getHealthStatus(): { status: "healthy" | "degraded" | "critical"; reason?: string } {
    const metrics = this.getMetrics();

    // Default to healthy when no requests have been made
    if (metrics.requestCount === 0) {
      return { status: "healthy" };
    }

    const successRate = metrics.successRate;

    if (successRate < 0.5) {
      return { status: "critical", reason: "Success rate below 50%" };
    }

    if (successRate < 0.8 || metrics.circuitBreakerTrips > 5) {
      return { status: "degraded", reason: "Elevated error rate or circuit breaker trips" };
    }

    return { status: "healthy" };
  }

  reset(): void {
    this.requestCount = 0;
    this.successCount = 0;
    this.failureCount = 0;
    this.rateLimitHits = 0;
    this.circuitBreakerTrips = 0;
    this.responseTimes.length = 0;
  }
}

describe("Metrics Collector", () => {
  describe("Basic Metrics Collection", () => {
    it("should start with zero metrics", () => {
      const collector = new MetricsCollector();
      const metrics = collector.getMetrics();

      expect(metrics.requestCount).toBe(0);
      expect(metrics.successCount).toBe(0);
      expect(metrics.failureCount).toBe(0);
      expect(metrics.successRate).toBe(0);
      expect(metrics.averageResponseTime).toBe(0);
      expect(metrics.rateLimitHits).toBe(0);
      expect(metrics.circuitBreakerTrips).toBe(0);
    });

    it("should record successful requests", () => {
      const collector = new MetricsCollector();

      collector.recordRequest(true, 100);
      collector.recordRequest(true, 200);
      collector.recordRequest(true, 150);

      const metrics = collector.getMetrics();

      expect(metrics.requestCount).toBe(3);
      expect(metrics.successCount).toBe(3);
      expect(metrics.failureCount).toBe(0);
      expect(metrics.successRate).toBe(1.0);
      expect(metrics.averageResponseTime).toBe(150); // (100 + 200 + 150) / 3
    });

    it("should record failed requests", () => {
      const collector = new MetricsCollector();

      collector.recordRequest(false, 50);
      collector.recordRequest(false, 75);

      const metrics = collector.getMetrics();

      expect(metrics.requestCount).toBe(2);
      expect(metrics.successCount).toBe(0);
      expect(metrics.failureCount).toBe(2);
      expect(metrics.successRate).toBe(0);
      expect(metrics.averageResponseTime).toBe(62.5); // (50 + 75) / 2
    });

    it("should record mixed success and failure", () => {
      const collector = new MetricsCollector();

      collector.recordRequest(true, 100);
      collector.recordRequest(false, 200);
      collector.recordRequest(true, 300);
      collector.recordRequest(false, 400);

      const metrics = collector.getMetrics();

      expect(metrics.requestCount).toBe(4);
      expect(metrics.successCount).toBe(2);
      expect(metrics.failureCount).toBe(2);
      expect(metrics.successRate).toBe(0.5);
      expect(metrics.averageResponseTime).toBe(250); // (100 + 200 + 300 + 400) / 4
    });
  });

  describe("Success Rate Calculation", () => {
    it("should calculate correct success rates", () => {
      const collector = new MetricsCollector();

      // Test different success rates
      const testCases = [
        { successes: 0, failures: 10, expectedRate: 0.0 },
        { successes: 5, failures: 5, expectedRate: 0.5 },
        { successes: 8, failures: 2, expectedRate: 0.8 },
        { successes: 10, failures: 0, expectedRate: 1.0 },
      ];

      testCases.forEach(({ successes, failures, expectedRate }, _index) => {
        collector.reset();

        for (let i = 0; i < successes; i++) {
          collector.recordRequest(true, 100);
        }

        for (let i = 0; i < failures; i++) {
          collector.recordRequest(false, 100);
        }

        const metrics = collector.getMetrics();
        expect(metrics.successRate).toBeCloseTo(expectedRate, 2);
      });
    });

    it("should handle edge case with no requests", () => {
      const collector = new MetricsCollector();
      const metrics = collector.getMetrics();

      expect(metrics.successRate).toBe(0);
      expect(metrics.averageResponseTime).toBe(0);
    });
  });

  describe("Response Time Tracking", () => {
    it("should calculate average response time correctly", () => {
      const collector = new MetricsCollector();

      const responseTimes = [50, 100, 150, 200, 250];
      for (const time of responseTimes) {
        collector.recordRequest(true, time);
      }

      const metrics = collector.getMetrics();
      const expectedAverage = responseTimes.reduce((a, b) => a + b, 0) / responseTimes.length;

      expect(metrics.averageResponseTime).toBe(expectedAverage);
    });

    it("should handle large numbers of response times", () => {
      const collector = new MetricsCollector();

      // Record 1000 response times
      for (let i = 1; i <= 1000; i++) {
        collector.recordRequest(true, i);
      }

      const metrics = collector.getMetrics();

      expect(metrics.requestCount).toBe(1000);
      expect(metrics.averageResponseTime).toBe(500.5); // Average of 1 to 1000
    });

    it("should track response times for both successes and failures", () => {
      const collector = new MetricsCollector();

      collector.recordRequest(true, 100);
      collector.recordRequest(false, 200);
      collector.recordRequest(true, 300);

      const metrics = collector.getMetrics();

      expect(metrics.averageResponseTime).toBe(200); // (100 + 200 + 300) / 3
    });
  });

  describe("Rate Limit and Circuit Breaker Tracking", () => {
    it("should record rate limit hits", () => {
      const collector = new MetricsCollector();

      collector.recordRateLimit();
      collector.recordRateLimit();
      collector.recordRateLimit();

      const metrics = collector.getMetrics();
      expect(metrics.rateLimitHits).toBe(3);
    });

    it("should record circuit breaker trips", () => {
      const collector = new MetricsCollector();

      collector.recordCircuitBreakerTrip();
      collector.recordCircuitBreakerTrip();

      const metrics = collector.getMetrics();
      expect(metrics.circuitBreakerTrips).toBe(2);
    });

    it("should track all metrics independently", () => {
      const collector = new MetricsCollector();

      collector.recordRequest(true, 100);
      collector.recordRequest(false, 200);
      collector.recordRateLimit();
      collector.recordCircuitBreakerTrip();

      const metrics = collector.getMetrics();

      expect(metrics.requestCount).toBe(2);
      expect(metrics.successCount).toBe(1);
      expect(metrics.failureCount).toBe(1);
      expect(metrics.rateLimitHits).toBe(1);
      expect(metrics.circuitBreakerTrips).toBe(1);
    });
  });

  describe("Health Status Calculation", () => {
    it("should report healthy status with high success rate", () => {
      const collector = new MetricsCollector();

      // 90% success rate
      for (let i = 0; i < 9; i++) {
        collector.recordRequest(true, 100);
      }
      collector.recordRequest(false, 100);

      const health = collector.getHealthStatus();
      expect(health.status).toBe("healthy");
      expect(health.reason).toBeUndefined();
    });

    it("should report degraded status with medium success rate", () => {
      const collector = new MetricsCollector();

      // 70% success rate
      for (let i = 0; i < 7; i++) {
        collector.recordRequest(true, 100);
      }
      for (let i = 0; i < 3; i++) {
        collector.recordRequest(false, 100);
      }

      const health = collector.getHealthStatus();
      expect(health.status).toBe("degraded");
      expect(health.reason).toContain("Elevated error rate");
    });

    it("should report critical status with low success rate", () => {
      const collector = new MetricsCollector();

      // 30% success rate
      for (let i = 0; i < 3; i++) {
        collector.recordRequest(true, 100);
      }
      for (let i = 0; i < 7; i++) {
        collector.recordRequest(false, 100);
      }

      const health = collector.getHealthStatus();
      expect(health.status).toBe("critical");
      expect(health.reason).toContain("Success rate below 50%");
    });

    it("should report degraded status with many circuit breaker trips", () => {
      const collector = new MetricsCollector();

      // High success rate but many circuit breaker trips
      for (let i = 0; i < 10; i++) {
        collector.recordRequest(true, 100);
      }

      // 6 circuit breaker trips (threshold is 5)
      for (let i = 0; i < 6; i++) {
        collector.recordCircuitBreakerTrip();
      }

      const health = collector.getHealthStatus();
      expect(health.status).toBe("degraded");
      expect(health.reason).toContain("circuit breaker trips");
    });

    it("should prioritize critical over degraded status", () => {
      const collector = new MetricsCollector();

      // Very low success rate AND many circuit breaker trips
      collector.recordRequest(true, 100);
      for (let i = 0; i < 9; i++) {
        collector.recordRequest(false, 100);
      }

      for (let i = 0; i < 10; i++) {
        collector.recordCircuitBreakerTrip();
      }

      const health = collector.getHealthStatus();
      expect(health.status).toBe("critical"); // Should be critical due to low success rate
      expect(health.reason).toContain("Success rate below 50%");
    });

    it("should handle no requests gracefully", () => {
      const collector = new MetricsCollector();

      const health = collector.getHealthStatus();
      expect(health.status).toBe("healthy"); // Default to healthy when no data
    });
  });

  describe("Reset Functionality", () => {
    it("should reset all metrics to zero", () => {
      const collector = new MetricsCollector();

      // Build up some metrics
      collector.recordRequest(true, 100);
      collector.recordRequest(false, 200);
      collector.recordRateLimit();
      collector.recordCircuitBreakerTrip();

      // Verify metrics exist
      let metrics = collector.getMetrics();
      expect(metrics.requestCount).toBeGreaterThan(0);

      // Reset and verify
      collector.reset();
      metrics = collector.getMetrics();

      expect(metrics.requestCount).toBe(0);
      expect(metrics.successCount).toBe(0);
      expect(metrics.failureCount).toBe(0);
      expect(metrics.successRate).toBe(0);
      expect(metrics.averageResponseTime).toBe(0);
      expect(metrics.rateLimitHits).toBe(0);
      expect(metrics.circuitBreakerTrips).toBe(0);
    });

    it("should report healthy status after reset", () => {
      const collector = new MetricsCollector();

      // Create critical status
      for (let i = 0; i < 10; i++) {
        collector.recordRequest(false, 100);
      }

      expect(collector.getHealthStatus().status).toBe("critical");

      // Reset and check health
      collector.reset();
      expect(collector.getHealthStatus().status).toBe("healthy");
    });
  });

  describe("Performance", () => {
    it("should handle high-frequency metric recording efficiently", () => {
      const collector = new MetricsCollector();

      const iterations = 10000;
      const start = performance.now();

      for (let i = 0; i < iterations; i++) {
        collector.recordRequest(i % 2 === 0, Math.random() * 1000);
      }

      const end = performance.now();
      const avgTime = ((end - start) * 1000) / iterations; // microseconds

      // Should be very fast (< 1μs per operation)
      expect(avgTime).toBeLessThan(1);

      // Verify data integrity
      const metrics = collector.getMetrics();
      expect(metrics.requestCount).toBe(iterations);
    });

    it("should calculate metrics efficiently with large datasets", () => {
      const collector = new MetricsCollector();

      // Record 50,000 requests
      for (let i = 0; i < 50000; i++) {
        collector.recordRequest(true, 100 + i);
      }

      const start = performance.now();
      const metrics = collector.getMetrics();
      const end = performance.now();

      // Metric calculation should be fast (< 10ms)
      expect(end - start).toBeLessThan(10);

      // Verify correctness
      expect(metrics.requestCount).toBe(50000);
      expect(metrics.successRate).toBe(1.0);
      expect(metrics.averageResponseTime).toBe(25099.5); // Average of 100 to 50099
    });
  });
});
