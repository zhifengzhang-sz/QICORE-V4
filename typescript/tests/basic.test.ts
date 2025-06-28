/**
 * QiCore v4.0 - Basic Integration Tests
 */

import { describe, expect, test } from "vitest";
import { z } from "zod";
import {
  Cache,
  Configuration,
  LogLevel,
  QiError,
  Result,
  StructuredLogger,
  hello,
  version,
} from "../src/index.js";

describe("QiCore TypeScript Integration", () => {
  test("should export version and metadata", () => {
    expect(version).toBe("4.0.1");
    expect(hello()).toBe("Hello from QiCore TypeScript v4.0.1!");
  });

  test("should have all base components", () => {
    expect(Result).toBeDefined();
    expect(QiError).toBeDefined();

    const result = Result.success(42);
    expect(result.isSuccess()).toBe(true);
    expect(result.unwrap()).toBe(42);
  });

  test("should have all core components", () => {
    expect(Configuration).toBeDefined();
    expect(StructuredLogger).toBeDefined();
    expect(Cache).toBeDefined();
  });

  test("should work with real data flow", async () => {
    // Create configuration
    const schema = z.object({
      appName: z.string().default("testapp"),
      logLevel: z.string().default("info"),
      cacheSize: z.number().default(100),
    });

    const config = new Configuration(schema);
    const configResult = config.loadFromObject({
      appName: "QiCore Test",
      logLevel: "debug",
      cacheSize: 500,
    });

    expect(configResult.isSuccess()).toBe(true);

    // Create logger
    const logger = new StructuredLogger({
      level: LogLevel.DEBUG,
      component: "test",
      prettyPrint: false,
    });

    // Create cache
    const cache = new Cache<string, number>({ maxSize: 100 });

    // Test data flow
    await cache.set("test-key", 42);
    const cached = await cache.get("test-key");

    expect(cached.isSuccess()).toBe(true);
    expect(cached.unwrap()).toBe(42);

    // Log the result
    const logResult = logger.info("Cache test completed", {
      value: cached.unwrap(),
      cacheStats: cache.getStats(),
    });

    expect(logResult.isSuccess()).toBe(true);
  });

  test("should handle errors gracefully", async () => {
    const cache = new Cache<string, number>({ maxSize: 1 });

    // Try to get non-existent key
    const missing = await cache.get("nonexistent");
    expect(missing.isFailure()).toBe(true);
    expect(missing.error().category).toBe("ResourceError");

    // Chain operations with error handling
    const result = missing.recover(() => 0).map((x) => x + 10);

    expect(result.isSuccess()).toBe(true);
    expect(result.unwrap()).toBe(10);
  });

  test("should demonstrate mathematical properties", () => {
    // Result monad laws
    const value = 42;
    const f = (x: number) => Result.success(x * 2);
    const g = (x: number) => Result.success(x + 1);

    // Left identity: return a >>= f ≡ f a
    const leftId1 = Result.success(value).flatMap(f);
    const leftId2 = f(value);
    expect(leftId1.unwrap()).toBe(leftId2.unwrap());

    // Right identity: m >>= return ≡ m
    const m = Result.success(value);
    const rightId = m.flatMap((x) => Result.success(x));
    expect(rightId.unwrap()).toBe(m.unwrap());

    // Associativity: (m >>= f) >>= g ≡ m >>= (\\x -> f x >>= g)
    const assoc1 = m.flatMap(f).flatMap(g);
    const assoc2 = m.flatMap((x) => f(x).flatMap(g));
    expect(assoc1.unwrap()).toBe(assoc2.unwrap());
  });
});
