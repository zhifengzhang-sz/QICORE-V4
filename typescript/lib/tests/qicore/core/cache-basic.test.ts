/**
 * Basic Cache Tests for QiCore v4.0
 *
 * Tests basic cache functionality that works reliably
 */

import { getData, isFailure, isSuccess } from "@qicore/base/result";
import { type Cache, createCache, createMemoryCache } from "@qicore/core/cache";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

describe("Cache Basic Functionality", () => {
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

  it("should create cache with config", () => {
    const result = createMemoryCache({ maxSize: 50, ttl: 5000 });
    expect(isSuccess(result)).toBe(true);
  });

  it("should set and get string values", async () => {
    const setResult = await cache.set("key1", "value1");
    expect(isSuccess(setResult)).toBe(true);

    const getResult = await cache.get("key1");
    expect(isSuccess(getResult)).toBe(true);
    if (isSuccess(getResult)) {
      expect(getData(getResult)).toBe("value1");
    }
  });

  it("should set and get number values", async () => {
    await cache.set("number", 42);
    const result = await cache.get("number");
    expect(isSuccess(result)).toBe(true);
    if (isSuccess(result)) {
      expect(getData(result)).toBe(42);
    }
  });

  it("should set and get boolean values", async () => {
    await cache.set("boolean", true);
    const result = await cache.get("boolean");
    expect(isSuccess(result)).toBe(true);
    if (isSuccess(result)) {
      expect(getData(result)).toBe(true);
    }
  });

  it("should set and get object values", async () => {
    const obj = { id: 123, name: "test" };
    await cache.set("object", obj);
    const result = await cache.get("object");
    expect(isSuccess(result)).toBe(true);
    if (isSuccess(result)) {
      expect(getData(result)).toEqual(obj);
    }
  });

  it("should set and get array values", async () => {
    const arr = [1, 2, 3, "string"];
    await cache.set("array", arr);
    const result = await cache.get("array");
    expect(isSuccess(result)).toBe(true);
    if (isSuccess(result)) {
      expect(getData(result)).toEqual(arr);
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
    await cache.set("existing", "value");

    const hasResult1 = await cache.has("existing");
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
    await cache.set("toDelete", "value");

    // Verify it exists
    const beforeDelete = await cache.get("toDelete");
    expect(isSuccess(beforeDelete)).toBe(true);
    if (isSuccess(beforeDelete)) {
      expect(getData(beforeDelete)).toBe("value");
    }

    // Delete it
    const deleteResult = await cache.delete("toDelete");
    expect(isSuccess(deleteResult)).toBe(true);

    // Verify it's gone
    const afterDelete = await cache.get("toDelete");
    expect(isSuccess(afterDelete)).toBe(true);
    if (isSuccess(afterDelete)) {
      expect(getData(afterDelete)).toBeNull();
    }
  });

  it("should clear all keys", async () => {
    await cache.set("key1", "value1");
    await cache.set("key2", "value2");
    await cache.set("key3", "value3");

    const clearResult = await cache.clear();
    expect(isSuccess(clearResult)).toBe(true);

    // Verify all are gone
    const results = await Promise.all([cache.get("key1"), cache.get("key2"), cache.get("key3")]);

    for (const result of results) {
      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        expect(getData(result)).toBeNull();
      }
    }
  });

  it("should handle multiple sequential operations", async () => {
    await cache.set("seq1", "value1");
    await cache.set("seq2", "value2");

    const get1 = await cache.get("seq1");
    const get2 = await cache.get("seq2");

    expect(isSuccess(get1) && getData(get1) === "value1").toBe(true);
    expect(isSuccess(get2) && getData(get2) === "value2").toBe(true);

    await cache.delete("seq1");

    const get1After = await cache.get("seq1");
    const get2After = await cache.get("seq2");

    expect(isSuccess(get1After) && getData(get1After) === null).toBe(true);
    expect(isSuccess(get2After) && getData(get2After) === "value2").toBe(true);
  });

  it("should override existing keys", async () => {
    await cache.set("override", "original");

    const first = await cache.get("override");
    expect(isSuccess(first) && getData(first) === "original").toBe(true);

    await cache.set("override", "updated");

    const second = await cache.get("override");
    expect(isSuccess(second) && getData(second) === "updated").toBe(true);
  });

  it("should handle empty string values", async () => {
    await cache.set("empty", "");
    const result = await cache.get("empty");
    expect(isSuccess(result)).toBe(true);
    if (isSuccess(result)) {
      expect(getData(result)).toBe("");
    }
  });

  it("should handle zero values", async () => {
    await cache.set("zero", 0);
    const result = await cache.get("zero");
    expect(isSuccess(result)).toBe(true);
    if (isSuccess(result)) {
      expect(getData(result)).toBe(0);
    }
  });

  it("should handle false values", async () => {
    await cache.set("false", false);
    const result = await cache.get("false");
    expect(isSuccess(result)).toBe(true);
    if (isSuccess(result)) {
      expect(getData(result)).toBe(false);
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

  it("should create cache with custom config", async () => {
    const result = await createCache({
      type: "memory",
      maxSize: 50,
      ttl: 1000,
    });
    expect(isSuccess(result)).toBe(true);
  });
});
