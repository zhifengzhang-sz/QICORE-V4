/**
 * @fileoverview Circuit Breaker Reliability Pattern Tests
 * @purpose Test circuit breaker state transitions and failure handling
 */

import { describe, expect, it, vi } from "vitest";
import { createQiError } from "../../../src/qicore/base/error.js";
import { failure, isFailure, isSuccess, success } from "../../../src/qicore/base/result.js";

// Note: This tests the circuit breaker pattern from our implementation
// We'll need to extract CircuitBreaker class for testing
class CircuitBreaker {
  private state: "closed" | "open" | "half_open" = "closed";
  private failures = 0;
  private lastFailure = 0;
  private successes = 0;

  constructor(
    private readonly config: {
      failureThreshold: number;
      timeout: number;
      monitoringPeriod: number;
    },
    private readonly onTrip?: () => void
  ) {}

  async execute<T>(
    operation: () => Promise<import("../../../src/qicore/base/result.js").Result<T>>
  ): Promise<import("../../../src/qicore/base/result.js").Result<T>> {
    if (this.state === "open") {
      if (Date.now() - this.lastFailure < this.config.timeout) {
        return failure(
          createQiError(
            "CIRCUIT_BREAKER_OPEN",
            "Circuit breaker is open - failing fast",
            "BUSINESS",
            { state: this.state, failures: this.failures }
          )
        );
      }
      this.state = "half_open";
      this.successes = 0;
    }

    try {
      const result = await operation();

      if (isFailure(result)) {
        this.onFailure();
        return result;
      }

      this.onSuccess();
      return result;
    } catch (error) {
      this.onFailure();
      return failure(
        createQiError(
          "CIRCUIT_BREAKER_ERROR",
          `Circuit breaker caught error: ${error}`,
          "NETWORK",
          { error: String(error) }
        )
      );
    }
  }

  private onSuccess(): void {
    this.failures = 0;

    if (this.state === "half_open") {
      this.successes++;
      if (this.successes >= 3) {
        this.state = "closed";
      }
    }
  }

  private onFailure(): void {
    this.failures++;
    this.lastFailure = Date.now();

    if (this.state === "half_open" || this.failures >= this.config.failureThreshold) {
      this.state = "open";
      if (this.onTrip) {
        this.onTrip();
      }
    }
  }

  getState(): { state: "closed" | "open" | "half_open"; failures: number; successes: number } {
    return {
      state: this.state,
      failures: this.failures,
      successes: this.successes,
    };
  }
}

describe("Circuit Breaker Reliability Pattern", () => {
  describe("State Transitions", () => {
    it("should start in closed state", () => {
      const config = { failureThreshold: 3, timeout: 1000, monitoringPeriod: 5000 };
      const circuitBreaker = new CircuitBreaker(config);

      const state = circuitBreaker.getState();
      expect(state.state).toBe("closed");
      expect(state.failures).toBe(0);
      expect(state.successes).toBe(0);
    });

    it("should open after reaching failure threshold", async () => {
      const config = { failureThreshold: 3, timeout: 1000, monitoringPeriod: 5000 };
      const onTrip = vi.fn();
      const circuitBreaker = new CircuitBreaker(config, onTrip);

      // Cause 3 failures to trigger opening
      for (let i = 0; i < 3; i++) {
        await circuitBreaker.execute(async () =>
          failure(createQiError("TEST_ERROR", "Test failure", "NETWORK"))
        );
      }

      const state = circuitBreaker.getState();
      expect(state.state).toBe("open");
      expect(state.failures).toBe(3);
      expect(onTrip).toHaveBeenCalledOnce();
    });

    it("should fail fast when open", async () => {
      const config = { failureThreshold: 2, timeout: 1000, monitoringPeriod: 5000 };
      const circuitBreaker = new CircuitBreaker(config);

      // Open the circuit
      for (let i = 0; i < 2; i++) {
        await circuitBreaker.execute(async () =>
          failure(createQiError("TEST_ERROR", "Test failure", "NETWORK"))
        );
      }

      // Should fail fast without calling operation
      const result = await circuitBreaker.execute(async () => success("should not be called"));

      expect(isFailure(result)).toBe(true);
      if (result._tag === "Left") {
        expect(result.left.code).toBe("CIRCUIT_BREAKER_OPEN");
      }
    });

    it("should transition to half-open after timeout", async () => {
      const config = { failureThreshold: 2, timeout: 100, monitoringPeriod: 5000 };
      const circuitBreaker = new CircuitBreaker(config);

      // Open the circuit
      for (let i = 0; i < 2; i++) {
        await circuitBreaker.execute(async () =>
          failure(createQiError("TEST_ERROR", "Test failure", "NETWORK"))
        );
      }

      // Wait for timeout
      await new Promise((resolve) => setTimeout(resolve, 150));

      // Next call should transition to half-open and succeed
      const result = await circuitBreaker.execute(async () => success("test"));

      expect(isSuccess(result)).toBe(true);
      const state = circuitBreaker.getState();
      expect(state.state).toBe("half_open");
    });

    it("should close after 3 successes in half-open state", async () => {
      const config = { failureThreshold: 2, timeout: 100, monitoringPeriod: 5000 };
      const circuitBreaker = new CircuitBreaker(config);

      // Open the circuit
      for (let i = 0; i < 2; i++) {
        await circuitBreaker.execute(async () =>
          failure(createQiError("TEST_ERROR", "Test failure", "NETWORK"))
        );
      }

      // Wait for timeout
      await new Promise((resolve) => setTimeout(resolve, 150));

      // Execute 3 successful operations
      for (let i = 0; i < 3; i++) {
        const result = await circuitBreaker.execute(async () => success(`test ${i}`));
        expect(isSuccess(result)).toBe(true);
      }

      const state = circuitBreaker.getState();
      expect(state.state).toBe("closed");
      expect(state.failures).toBe(0);
    });

    it("should reopen if failure occurs in half-open state", async () => {
      const config = { failureThreshold: 2, timeout: 100, monitoringPeriod: 5000 };
      const circuitBreaker = new CircuitBreaker(config);

      // Open the circuit
      for (let i = 0; i < 2; i++) {
        await circuitBreaker.execute(async () =>
          failure(createQiError("TEST_ERROR", "Test failure", "NETWORK"))
        );
      }

      // Wait for timeout and transition to half-open
      await new Promise((resolve) => setTimeout(resolve, 150));
      await circuitBreaker.execute(async () => success("test"));

      expect(circuitBreaker.getState().state).toBe("half_open");

      // Fail in half-open state
      await circuitBreaker.execute(async () =>
        failure(createQiError("TEST_ERROR", "Test failure", "NETWORK"))
      );

      const state = circuitBreaker.getState();
      expect(state.state).toBe("open");
    });
  });

  describe("Error Handling", () => {
    it("should catch exceptions and convert to failures", async () => {
      const config = { failureThreshold: 3, timeout: 1000, monitoringPeriod: 5000 };
      const circuitBreaker = new CircuitBreaker(config);

      const result = await circuitBreaker.execute(async () => {
        throw new Error("Test exception");
      });

      expect(isFailure(result)).toBe(true);
      if (result._tag === "Left") {
        expect(result.left.code).toBe("CIRCUIT_BREAKER_ERROR");
        expect(result.left.category).toBe("NETWORK");
      }
    });

    it("should preserve original error information", async () => {
      const config = { failureThreshold: 3, timeout: 1000, monitoringPeriod: 5000 };
      const circuitBreaker = new CircuitBreaker(config);

      const originalError = createQiError("ORIGINAL_ERROR", "Original message", "VALIDATION");
      const result = await circuitBreaker.execute(async () => failure(originalError));

      expect(isFailure(result)).toBe(true);
      if (result._tag === "Left") {
        expect(result.left).toBe(originalError); // Should be the same error object
      }
    });
  });

  describe("Configuration Validation", () => {
    it("should work with different failure thresholds", async () => {
      const configs = [
        { failureThreshold: 1, timeout: 1000, monitoringPeriod: 5000 },
        { failureThreshold: 5, timeout: 1000, monitoringPeriod: 5000 },
        { failureThreshold: 10, timeout: 1000, monitoringPeriod: 5000 },
      ];

      for (const config of configs) {
        const circuitBreaker = new CircuitBreaker(config);

        // Trigger exactly threshold failures
        for (let i = 0; i < config.failureThreshold; i++) {
          await circuitBreaker.execute(async () =>
            failure(createQiError("TEST_ERROR", "Test failure", "NETWORK"))
          );
        }

        const state = circuitBreaker.getState();
        expect(state.state).toBe("open");
        expect(state.failures).toBe(config.failureThreshold);
      }
    });

    it("should respect different timeout values", async () => {
      const shortTimeout = 50;
      const longTimeout = 200;

      const configShort = { failureThreshold: 1, timeout: shortTimeout, monitoringPeriod: 5000 };
      const configLong = { failureThreshold: 1, timeout: longTimeout, monitoringPeriod: 5000 };

      const circuitShort = new CircuitBreaker(configShort);
      const circuitLong = new CircuitBreaker(configLong);

      // Open both circuits
      await circuitShort.execute(async () => failure(createQiError("TEST", "Test", "NETWORK")));
      await circuitLong.execute(async () => failure(createQiError("TEST", "Test", "NETWORK")));

      // Wait for short timeout
      await new Promise((resolve) => setTimeout(resolve, shortTimeout + 10));

      // Short timeout circuit should be half-open, long should still be open
      const resultShort = await circuitShort.execute(async () => success("test"));
      const resultLong = await circuitLong.execute(async () => success("test"));

      expect(isSuccess(resultShort)).toBe(true);
      expect(isFailure(resultLong)).toBe(true);
    });
  });

  describe("Performance", () => {
    it("should have minimal overhead in closed state", async () => {
      const config = { failureThreshold: 5, timeout: 1000, monitoringPeriod: 5000 };
      const circuitBreaker = new CircuitBreaker(config);

      const iterations = 1000;
      const start = performance.now();

      for (let i = 0; i < iterations; i++) {
        await circuitBreaker.execute(async () => success(i));
      }

      const end = performance.now();
      const avgTime = ((end - start) * 1000) / iterations; // microseconds

      // Circuit breaker overhead should be minimal (< 10μs per operation)
      expect(avgTime).toBeLessThan(10);
    });

    it("should fail fast immediately when open", async () => {
      const config = { failureThreshold: 1, timeout: 1000, monitoringPeriod: 5000 };
      const circuitBreaker = new CircuitBreaker(config);

      // Open the circuit
      await circuitBreaker.execute(async () =>
        failure(createQiError("TEST_ERROR", "Test failure", "NETWORK"))
      );

      // Measure fail-fast performance
      const start = performance.now();
      await circuitBreaker.execute(async () => {
        // This should not be called
        await new Promise((resolve) => setTimeout(resolve, 100));
        return success("slow operation");
      });
      const end = performance.now();

      // Should fail fast (< 1ms) without waiting for slow operation
      expect(end - start).toBeLessThan(1);
    });
  });
});
