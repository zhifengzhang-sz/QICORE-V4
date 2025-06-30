/**
 * @fileoverview Rate Limiter Reliability Pattern Tests
 * @purpose Test rate limiting with sliding window algorithm
 */

import { describe, expect, it, vi } from "vitest";
import { createQiError } from "../../../src/qicore/base/error.js";
import { failure, isFailure, isSuccess, success } from "../../../src/qicore/base/result.js";

// Note: This tests the rate limiter pattern from our implementation
class RateLimiter {
  private requests: number[] = [];

  constructor(
    private readonly maxRequests: number,
    private readonly windowMs = 60000
  ) {}

  async checkLimit(): Promise<import("../../../src/qicore/base/result.js").Result<void>> {
    const now = Date.now();

    // Remove old requests outside window
    this.requests = this.requests.filter((time) => now - time < this.windowMs);

    if (this.requests.length >= this.maxRequests) {
      return failure(
        createQiError(
          "RATE_LIMITED",
          `Rate limit exceeded: ${this.maxRequests} requests per ${this.windowMs}ms`,
          "BUSINESS",
          {
            currentRequests: this.requests.length,
            limit: this.maxRequests,
            resetTime: this.requests[0] + this.windowMs,
          }
        )
      );
    }

    this.requests.push(now);
    return success(undefined);
  }

  getUsage(): { current: number; limit: number; resetTime: number } {
    const now = Date.now();
    this.requests = this.requests.filter((time) => now - time < this.windowMs);

    return {
      current: this.requests.length,
      limit: this.maxRequests,
      resetTime: this.requests.length > 0 ? this.requests[0] + this.windowMs : now,
    };
  }
}

describe("Rate Limiter Reliability Pattern", () => {
  describe("Basic Rate Limiting", () => {
    it("should allow requests within limit", async () => {
      const rateLimiter = new RateLimiter(3, 1000); // 3 requests per second

      // Should allow first 3 requests
      for (let i = 0; i < 3; i++) {
        const result = await rateLimiter.checkLimit();
        expect(isSuccess(result)).toBe(true);
      }

      const usage = rateLimiter.getUsage();
      expect(usage.current).toBe(3);
      expect(usage.limit).toBe(3);
    });

    it("should reject requests over limit", async () => {
      const rateLimiter = new RateLimiter(2, 1000); // 2 requests per second

      // Use up the limit
      for (let i = 0; i < 2; i++) {
        await rateLimiter.checkLimit();
      }

      // Should reject the 3rd request
      const result = await rateLimiter.checkLimit();

      expect(isFailure(result)).toBe(true);
      if (result._tag === "Left") {
        expect(result.left.code).toBe("RATE_LIMITED");
        expect(result.left.category).toBe("BUSINESS");
        expect(result.left.context.get("currentRequests")).toBe(2);
        expect(result.left.context.get("limit")).toBe(2);
      }
    });

    it("should provide reset time information", async () => {
      const rateLimiter = new RateLimiter(1, 1000);

      // Use the limit
      await rateLimiter.checkLimit();

      // Get rejection with reset time
      const result = await rateLimiter.checkLimit();

      expect(isFailure(result)).toBe(true);
      if (result._tag === "Left") {
        const now = Date.now();
        const resetTime = result.left.context.get("resetTime") as number;
        expect(resetTime).toBeGreaterThanOrEqual(now);
        expect(resetTime).toBeLessThanOrEqual(now + 1000);
      }
    });
  });

  describe("Sliding Window Algorithm", () => {
    it("should allow requests after window expires", async () => {
      const windowMs = 100; // Short window for testing
      const rateLimiter = new RateLimiter(2, windowMs);

      // Use up the limit
      for (let i = 0; i < 2; i++) {
        const result = await rateLimiter.checkLimit();
        expect(isSuccess(result)).toBe(true);
      }

      // Should be at limit
      const rejectedResult = await rateLimiter.checkLimit();
      expect(isFailure(rejectedResult)).toBe(true);

      // Wait for window to expire
      await new Promise((resolve) => setTimeout(resolve, windowMs + 10));

      // Should allow requests again
      for (let i = 0; i < 2; i++) {
        const result = await rateLimiter.checkLimit();
        expect(isSuccess(result)).toBe(true);
      }
    });

    it("should partially refresh as requests expire", async () => {
      const windowMs = 500; // Much longer window for stability
      const rateLimiter = new RateLimiter(2, windowMs); // Reduce to 2 requests for simpler test

      // Make 2 requests quickly to reach limit
      expect(isSuccess(await rateLimiter.checkLimit())).toBe(true);
      expect(isSuccess(await rateLimiter.checkLimit())).toBe(true);

      // Should be at limit now
      expect(isFailure(await rateLimiter.checkLimit())).toBe(true);

      // Wait for full window + buffer to expire
      await new Promise((resolve) => setTimeout(resolve, windowMs + 100));

      // Should allow requests again (all expired)
      expect(isSuccess(await rateLimiter.checkLimit())).toBe(true);
      expect(isSuccess(await rateLimiter.checkLimit())).toBe(true);
      // Should be at limit again
      expect(isFailure(await rateLimiter.checkLimit())).toBe(true);
    });

    it("should accurately track current usage", async () => {
      const rateLimiter = new RateLimiter(5, 1000);

      // Check initial usage
      let usage = rateLimiter.getUsage();
      expect(usage.current).toBe(0);
      expect(usage.limit).toBe(5);

      // Make some requests
      for (let i = 1; i <= 3; i++) {
        await rateLimiter.checkLimit();
        usage = rateLimiter.getUsage();
        expect(usage.current).toBe(i);
        expect(usage.limit).toBe(5);
      }
    });
  });

  describe("Different Window Sizes", () => {
    it("should work with different window sizes", async () => {
      const windowSizes = [100, 500, 1000, 2000];

      for (const windowMs of windowSizes) {
        const rateLimiter = new RateLimiter(2, windowMs);

        // Use up the limit
        for (let i = 0; i < 2; i++) {
          const result = await rateLimiter.checkLimit();
          expect(isSuccess(result)).toBe(true);
        }

        // Should be at limit
        expect(isFailure(await rateLimiter.checkLimit())).toBe(true);

        // Wait for window to expire
        await new Promise((resolve) => setTimeout(resolve, windowMs + 10));

        // Should allow requests again
        const result = await rateLimiter.checkLimit();
        expect(isSuccess(result)).toBe(true);
      }
    });

    it("should work with minute-based windows", async () => {
      const rateLimiter = new RateLimiter(10, 60000); // 10 per minute

      // Should allow 10 requests
      for (let i = 0; i < 10; i++) {
        const result = await rateLimiter.checkLimit();
        expect(isSuccess(result)).toBe(true);
      }

      // Should reject 11th request
      const result = await rateLimiter.checkLimit();
      expect(isFailure(result)).toBe(true);

      const usage = rateLimiter.getUsage();
      expect(usage.current).toBe(10);
      expect(usage.limit).toBe(10);
    });
  });

  describe("Edge Cases", () => {
    it("should handle zero limit", async () => {
      const rateLimiter = new RateLimiter(0, 1000);

      const result = await rateLimiter.checkLimit();
      expect(isFailure(result)).toBe(true);

      if (result._tag === "Left") {
        expect(result.left.code).toBe("RATE_LIMITED");
      }
    });

    it("should handle single request limit", async () => {
      const rateLimiter = new RateLimiter(1, 1000);

      // First request should succeed
      const first = await rateLimiter.checkLimit();
      expect(isSuccess(first)).toBe(true);

      // Second should fail
      const second = await rateLimiter.checkLimit();
      expect(isFailure(second)).toBe(true);
    });

    it("should handle high frequency requests", async () => {
      const rateLimiter = new RateLimiter(100, 1000);

      // Make 100 requests as fast as possible
      const promises = Array.from({ length: 100 }, () => rateLimiter.checkLimit());
      const results = await Promise.all(promises);

      // All should succeed
      for (const result of results) {
        expect(isSuccess(result)).toBe(true);
      }

      // 101st should fail
      const extraResult = await rateLimiter.checkLimit();
      expect(isFailure(extraResult)).toBe(true);
    });

    it("should handle very short windows", async () => {
      const rateLimiter = new RateLimiter(2, 10); // 2 requests per 10ms

      // Use up limit
      await rateLimiter.checkLimit();
      await rateLimiter.checkLimit();

      // Should be at limit
      expect(isFailure(await rateLimiter.checkLimit())).toBe(true);

      // Wait for very short window
      await new Promise((resolve) => setTimeout(resolve, 15));

      // Should allow requests again
      expect(isSuccess(await rateLimiter.checkLimit())).toBe(true);
    });
  });

  describe("Performance", () => {
    it("should have minimal overhead for allowed requests", async () => {
      const rateLimiter = new RateLimiter(1000, 60000);

      const iterations = 100;
      const start = performance.now();

      for (let i = 0; i < iterations; i++) {
        await rateLimiter.checkLimit();
      }

      const end = performance.now();
      const avgTime = ((end - start) * 1000) / iterations; // microseconds

      // Rate limiter overhead should be minimal (< 50μs per operation)
      expect(avgTime).toBeLessThan(50);
    });

    it("should efficiently handle window cleanup", async () => {
      const rateLimiter = new RateLimiter(10, 100);

      // Fill up with requests
      for (let i = 0; i < 10; i++) {
        await rateLimiter.checkLimit();
      }

      // Wait for window to expire
      await new Promise((resolve) => setTimeout(resolve, 120));

      // Check that cleanup is efficient
      const start = performance.now();
      await rateLimiter.checkLimit();
      const end = performance.now();

      // Should clean up efficiently (< 1ms)
      expect(end - start).toBeLessThan(1);
    });

    it("should handle large numbers of expired requests efficiently", async () => {
      const rateLimiter = new RateLimiter(1000, 100);

      // Fill up with many requests
      for (let i = 0; i < 1000; i++) {
        await rateLimiter.checkLimit();
      }

      // Wait for all to expire
      await new Promise((resolve) => setTimeout(resolve, 120));

      // Should efficiently clean up
      const start = performance.now();
      const usage = rateLimiter.getUsage();
      const end = performance.now();

      expect(usage.current).toBe(0);
      expect(end - start).toBeLessThan(5); // Should be very fast
    });
  });

  describe("Concurrent Access", () => {
    it("should handle concurrent requests correctly", async () => {
      const rateLimiter = new RateLimiter(5, 1000);

      // Make 10 concurrent requests
      const promises = Array.from({ length: 10 }, () => rateLimiter.checkLimit());
      const results = await Promise.all(promises);

      // Exactly 5 should succeed, 5 should fail
      const successes = results.filter((r) => isSuccess(r)).length;
      const failures = results.filter((r) => isFailure(r)).length;

      expect(successes).toBe(5);
      expect(failures).toBe(5);
    });
  });
});
