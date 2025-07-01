/**
 * Simplified Cache Tests for QiCore v4.0
 *
 * Tests basic cache functionality that we know works
 */

import { getData, isFailure, isSuccess } from "@qicore/base/result";
import { type Cache, createCache, createMemoryCache, createRedisCache } from "@qicore/core/cache";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

describe("Cache Basic Operations", () => {
  let cache: Cache;

  beforeEach(async () => {
    const result = createMemoryCache({ maxSize: 100, ttl: 10000 });
    expect(isSuccess(result)).toBe(true);
    if (isSuccess(result)) {
      const cacheData = getData(result);
      if (cacheData) {
        cache = cacheData;
      }
    }
  });

  afterEach(async () => {
    if (cache) {
      await cache.clear();
    }
  });

  it("should create memory cache successfully", () => {
    const result = createMemoryCache();
    expect(isSuccess(result)).toBe(true);
  });

  it("should set and get values", async () => {
    const setResult = await cache.set("key1", "value1");
    expect(isSuccess(setResult)).toBe(true);

    const getResult = await cache.get("key1");
    expect(isSuccess(getResult)).toBe(true);
    if (isSuccess(getResult)) {
      expect(getData(getResult)).toBe("value1");
    }
  });

  it("should return null for non-existent keys", async () => {
    const result = await cache.get("nonexistent");
    expect(isSuccess(result)).toBe(true);
    if (isSuccess(result)) {
      expect(getData(result)).toBeNull();
    }
  });

  it("should check key existence", async () => {
    await cache.set("key1", "value1");

    const hasResult1 = await cache.has("key1");
    expect(isSuccess(hasResult1)).toBe(true);
    if (isSuccess(hasResult1)) {
      expect(getData(hasResult1)).toBe(true);
    }

    const hasResult2 = await cache.has("nonexistent");
    expect(isSuccess(hasResult2)).toBe(true);
    if (isSuccess(hasResult2)) {
      expect(getData(hasResult2)).toBe(false);
    }
  });

  it("should delete keys", async () => {
    await cache.set("key1", "value1");

    const deleteResult = await cache.delete("key1");
    expect(isSuccess(deleteResult)).toBe(true);

    const getResult = await cache.get("key1");
    expect(isSuccess(getResult)).toBe(true);
    if (isSuccess(getResult)) {
      expect(getData(getResult)).toBeNull();
    }
  });

  it("should clear all keys", async () => {
    await cache.set("key1", "value1");
    await cache.set("key2", "value2");

    const clearResult = await cache.clear();
    expect(isSuccess(clearResult)).toBe(true);

    const getResult1 = await cache.get("key1");
    const getResult2 = await cache.get("key2");

    expect(isSuccess(getResult1)).toBe(true);
    expect(isSuccess(getResult2)).toBe(true);
    if (isSuccess(getResult1) && isSuccess(getResult2)) {
      expect(getData(getResult1)).toBeNull();
      expect(getData(getResult2)).toBeNull();
    }
  });

  it("should handle complex data types", async () => {
    const complexData = {
      id: 123,
      name: "test",
      nested: { value: true },
      array: [1, 2, 3],
    };

    await cache.set("complex", complexData);
    const result = await cache.get("complex");

    expect(isSuccess(result)).toBe(true);
    if (isSuccess(result)) {
      expect(getData(result)).toEqual(complexData);
    }
  });

  it("should get statistics", async () => {
    await cache.set("key1", "value1");
    await cache.get("key1"); // Hit
    await cache.get("nonexistent"); // Miss

    const statsResult = await cache.getStats();
    expect(isSuccess(statsResult)).toBe(true);

    if (isSuccess(statsResult)) {
      const stats = getData(statsResult);
      if (stats) {
        expect(typeof stats.hits).toBe("number");
        expect(typeof stats.misses).toBe("number");
        expect(typeof stats.keys).toBe("number");
        expect(typeof stats.size).toBe("number");
        expect(typeof stats.hitRate).toBe("number");
      }
    }
  });
});

describe("Cache Factory", () => {
  it("should create memory cache by default", async () => {
    const result = await createCache();
    expect(isSuccess(result)).toBe(true);
  });

  it("should create memory cache when specified", async () => {
    const result = await createCache({ type: "memory" });
    expect(isSuccess(result)).toBe(true);
  });

  it("should reject invalid cache types", async () => {
    // @ts-expect-error Testing invalid type
    const result = await createCache({ type: "invalid" });
    expect(isFailure(result)).toBe(true);
  });
});

describe("Redis Cache Error Handling", () => {
  it("should handle Redis connection failure gracefully", async () => {
    // Try to connect to non-existent Redis
    const result = await createRedisCache({
      type: "redis",
      host: "nonexistent-host",
      port: 6379,
      connectTimeout: 100,
    });

    // Should fail gracefully
    expect(isFailure(result)).toBe(true);
  });
});
