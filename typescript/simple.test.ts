/**
 * Simple QiCore TypeScript test
 */

import { test, expect } from "vitest";

// Test basic imports
test("should import QiCore components", async () => {
  const { Result, QiError } = await import("./src/qicore/base/index.js");
  
  expect(Result).toBeDefined();
  expect(QiError).toBeDefined();
  
  // Test Result monad
  const success = Result.success(42);
  expect(success.isSuccess()).toBe(true);
  expect(success.unwrap()).toBe(42);
  
  // Test QiError
  const error = QiError.validationError("Test error", "field", "value");
  expect(error.category).toBe("ValidationError");
  expect(error.message).toBe("Test error");
  
  const failure = Result.failure(error);
  expect(failure.isFailure()).toBe(true);
  expect(failure.error()).toEqual(error);
});

test("should work with Configuration", async () => {
  const { Configuration } = await import("./src/qicore/core/configuration.js");
  const { z } = await import("zod");
  
  const schema = z.object({
    name: z.string().default("test"),
    port: z.number().default(3000),
  });
  
  const config = new Configuration(schema);
  const result = config.loadFromObject({
    name: "QiCore Test",
    port: 8080,
  });
  
  expect(result.isSuccess()).toBe(true);
  
  const data = config.get();
  expect(data.isSuccess()).toBe(true);
  expect(data.unwrap().name).toBe("QiCore Test");
  expect(data.unwrap().port).toBe(8080);
});

test("should work with Cache", async () => {
  const { Cache } = await import("./src/qicore/core/cache.js");
  
  const cache = new Cache<string, number>({ maxSize: 100 });
  
  await cache.set("test", 42);
  const result = await cache.get("test");
  
  expect(result.isSuccess()).toBe(true);
  expect(result.unwrap()).toBe(42);
  
  const stats = cache.getStats();
  expect(stats.hits).toBe(1);
  expect(stats.sets).toBe(1);
});