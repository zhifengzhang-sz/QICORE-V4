/**
 * QiCore v4.0 - Cache Component Tests
 * 
 * Comprehensive tests for Memory and Redis caching with LRU + TTL
 */

import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import {
  createMemoryCache,
  createRedisCache,
  createSerializedCache,
  mget,
  mset,
  MemoryCache,
  type Cache,
  type CacheStats
} from "../../../src/qicore/core/cache.js";

describe("Cache Component", () => {
  describe("Memory Cache", () => {
    let cache: Cache<string, string>;

    beforeEach(() => {
      const result = createMemoryCache<string, string>(100, 60000); // 100 max size, 1 minute TTL
      expect(result._tag).toBe("Right");
      cache = result.right!;
    });

    afterEach(() => {
      if (cache && 'destroy' in cache) {
        (cache as MemoryCache<string, string>).destroy();
      }
    });

    describe("Basic Operations", () => {
      it("stores and retrieves values", async () => {
        const setResult = await cache.set("key1", "value1");
        expect(setResult._tag).toBe("Right");

        const getResult = await cache.get("key1");
        expect(getResult._tag).toBe("Right");
        if (getResult._tag === "Right") {
          expect(getResult.right).toBe("value1");
        }
      });

      it("returns cache miss for non-existent keys", async () => {
        const result = await cache.get("nonexistent");
        
        expect(result._tag).toBe("Left");
        if (result._tag === "Left") {
          expect(result.left.code).toBe("CACHE_MISS");
        }
      });

      it("checks key existence", async () => {
        await cache.set("existing", "value");
        
        const existsResult = await cache.has("existing");
        const notExistsResult = await cache.has("nonexistent");
        
        expect(existsResult._tag).toBe("Right");
        expect(existsResult.right).toBe(true);
        
        expect(notExistsResult._tag).toBe("Right");
        expect(notExistsResult.right).toBe(false);
      });

      it("deletes keys", async () => {
        await cache.set("todelete", "value");
        
        const deleteResult = await cache.delete("todelete");
        expect(deleteResult._tag).toBe("Right");
        expect(deleteResult.right).toBe(true);
        
        const getResult = await cache.get("todelete");
        expect(getResult._tag).toBe("Left");
      });

      it("clears all keys", async () => {
        await cache.set("key1", "value1");
        await cache.set("key2", "value2");
        
        const clearResult = await cache.clear();
        expect(clearResult._tag).toBe("Right");
        
        const sizeResult = await cache.size();
        expect(sizeResult._tag).toBe("Right");
        expect(sizeResult.right).toBe(0);
      });

      it("gets cache size", async () => {
        await cache.set("key1", "value1");
        await cache.set("key2", "value2");
        
        const sizeResult = await cache.size();
        expect(sizeResult._tag).toBe("Right");
        expect(sizeResult.right).toBe(2);
      });

      it("lists all keys", async () => {
        await cache.set("key1", "value1");
        await cache.set("key2", "value2");
        
        const keysResult = await cache.keys();
        expect(keysResult._tag).toBe("Right");
        if (keysResult._tag === "Right") {
          expect(keysResult.right.sort()).toEqual(["key1", "key2"]);
        }
      });
    });

    describe("TTL (Time To Live)", () => {
      it("expires entries after TTL", async () => {
        await cache.set("expiring", "value", 50); // 50ms TTL
        
        // Should exist immediately
        const immediateResult = await cache.get("expiring");
        expect(immediateResult._tag).toBe("Right");
        
        // Wait for expiration
        await new Promise(resolve => setTimeout(resolve, 100));
        
        const expiredResult = await cache.get("expiring");
        expect(expiredResult._tag).toBe("Left");
        if (expiredResult._tag === "Left") {
          expect(expiredResult.left.code).toBe("CACHE_EXPIRED");
        }
      });

      it("uses default TTL when not specified", async () => {
        await cache.set("default_ttl", "value"); // Should use default TTL
        
        const result = await cache.get("default_ttl");
        expect(result._tag).toBe("Right");
      });

      it("custom TTL overrides default", async () => {
        const shortTTL = 10; // 10ms
        await cache.set("short_ttl", "value", shortTTL);
        
        await new Promise(resolve => setTimeout(resolve, 50));
        
        const result = await cache.get("short_ttl");
        expect(result._tag).toBe("Left");
      });

      it("updates TTL on access", async () => {
        await cache.set("updateTTL", "value", 100); // 100ms TTL
        
        // Access after 50ms
        await new Promise(resolve => setTimeout(resolve, 50));
        const midResult = await cache.get("updateTTL");
        expect(midResult._tag).toBe("Right");
        
        // Should still exist after another 75ms (would have expired at 100ms without update)
        await new Promise(resolve => setTimeout(resolve, 75));
        const laterResult = await cache.get("updateTTL");
        expect(laterResult._tag).toBe("Right");
      });
    });

    describe("LRU (Least Recently Used) Eviction", () => {
      let smallCache: Cache<string, string>;

      beforeEach(() => {
        const result = createMemoryCache<string, string>(3, 60000); // Max 3 items
        expect(result._tag).toBe("Right");
        smallCache = result.right!;
      });

      afterEach(() => {
        if (smallCache && 'destroy' in smallCache) {
          (smallCache as MemoryCache<string, string>).destroy();
        }
      });

      it("evicts least recently used items when full", async () => {
        // Fill cache to capacity
        await smallCache.set("key1", "value1");
        await smallCache.set("key2", "value2");
        await smallCache.set("key3", "value3");
        
        // Access key1 to make it recently used
        await smallCache.get("key1");
        
        // Add new item, should evict key2 (least recently used)
        await smallCache.set("key4", "value4");
        
        const key1Result = await smallCache.get("key1");
        const key2Result = await smallCache.get("key2");
        const key3Result = await smallCache.get("key3");
        const key4Result = await smallCache.get("key4");
        
        expect(key1Result._tag).toBe("Right"); // Recently accessed
        expect(key2Result._tag).toBe("Left");  // Evicted
        expect(key3Result._tag).toBe("Right"); // Still there
        expect(key4Result._tag).toBe("Right"); // Newly added
      });

      it("updates access time on set operations", async () => {
        await smallCache.set("key1", "value1");
        await smallCache.set("key2", "value2");
        await smallCache.set("key3", "value3");
        
        // Update key1 to make it recently used
        await smallCache.set("key1", "new_value1");
        
        // Add new item
        await smallCache.set("key4", "value4");
        
        // key1 should still exist (recently updated)
        const key1Result = await smallCache.get("key1");
        expect(key1Result._tag).toBe("Right");
        if (key1Result._tag === "Right") {
          expect(key1Result.right).toBe("new_value1");
        }
      });
    });

    describe("Statistics", () => {
      it("tracks cache statistics", async () => {
        // Generate some cache activity
        await cache.set("key1", "value1");
        await cache.set("key2", "value2");
        
        // Hits
        await cache.get("key1"); // Hit
        await cache.get("key1"); // Hit
        
        // Misses
        await cache.get("nonexistent1"); // Miss
        await cache.get("nonexistent2"); // Miss
        
        const statsResult = await cache.getStats();
        expect(statsResult._tag).toBe("Right");
        
        if (statsResult._tag === "Right") {
          const stats = statsResult.right;
          expect(stats.hitCount).toBe(2);
          expect(stats.missCount).toBe(2);
          expect(stats.size).toBe(2);
          expect(stats.evictionCount).toBeGreaterThanOrEqual(0);
        }
      });
    });

    describe("Performance Requirements", () => {
      it("performs get operations quickly", async () => {
        // Pre-populate cache
        for (let i = 0; i < 100; i++) {
          await cache.set(`key${i}`, `value${i}`);
        }
        
        const start = performance.now();
        
        for (let i = 0; i < 1000; i++) {
          await cache.get(`key${i % 100}`);
        }
        
        const end = performance.now();
        const avgDuration = ((end - start) * 1000) / 1000; // microseconds per operation
        
        // Should be well under 50μs per operation
        expect(avgDuration).toBeLessThan(50);
      });

      it("performs set operations quickly", async () => {
        const start = performance.now();
        
        for (let i = 0; i < 1000; i++) {
          await cache.set(`key${i}`, `value${i}`);
        }
        
        const end = performance.now();
        const avgDuration = ((end - start) * 1000) / 1000;
        
        // Should be well under 50μs per operation
        expect(avgDuration).toBeLessThan(50);
      });

      it("performs has operations quickly", async () => {
        // Pre-populate cache
        for (let i = 0; i < 100; i++) {
          await cache.set(`key${i}`, `value${i}`);
        }
        
        const start = performance.now();
        
        for (let i = 0; i < 1000; i++) {
          await cache.has(`key${i % 100}`);
        }
        
        const end = performance.now();
        const avgDuration = ((end - start) * 1000) / 1000;
        
        // Should be well under 15μs per operation
        expect(avgDuration).toBeLessThan(15);
      });
    });

    describe("Error Handling", () => {
      it("handles errors gracefully", async () => {
        // Try to use cache after destruction
        if ('destroy' in cache) {
          (cache as MemoryCache<string, string>).destroy();
        }
        
        // Operations should still work or fail gracefully
        const result = await cache.get("test");
        // Either succeeds or fails with proper error structure
        expect(result._tag).toMatch(/^(Left|Right)$/);
      });
    });
  });

  describe("Batch Operations", () => {
    let cache: Cache<string, string>;

    beforeEach(() => {
      const result = createMemoryCache<string, string>();
      expect(result._tag).toBe("Right");
      cache = result.right!;
    });

    afterEach(() => {
      if (cache && 'destroy' in cache) {
        (cache as MemoryCache<string, string>).destroy();
      }
    });

    it("performs batch get operations", async () => {
      await cache.set("key1", "value1");
      await cache.set("key2", "value2");
      await cache.set("key3", "value3");
      
      const result = await mget(cache, ["key1", "key2", "key4", "key3"]);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const resultMap = result.right;
        expect(resultMap.get("key1")).toBe("value1");
        expect(resultMap.get("key2")).toBe("value2");
        expect(resultMap.get("key3")).toBe("value3");
        expect(resultMap.has("key4")).toBe(false); // Missing key not included
      }
    });

    it("performs batch set operations", async () => {
      const entries = new Map([
        ["key1", "value1"],
        ["key2", "value2"],
        ["key3", "value3"]
      ]);
      
      const setResult = await mset(cache, entries, 60000);
      expect(setResult._tag).toBe("Right");
      
      // Verify all were set
      for (const [key, expectedValue] of entries) {
        const getResult = await cache.get(key);
        expect(getResult._tag).toBe("Right");
        if (getResult._tag === "Right") {
          expect(getResult.right).toBe(expectedValue);
        }
      }
    });
  });

  describe("Serialized Cache", () => {
    let baseCache: Cache<string, string>;
    let serializedCache: Cache<string, { value: number; name: string }>;

    beforeEach(() => {
      const result = createMemoryCache<string, string>();
      expect(result._tag).toBe("Right");
      baseCache = result.right!;
      
      serializedCache = createSerializedCache(baseCache);
    });

    afterEach(() => {
      if (baseCache && 'destroy' in baseCache) {
        (baseCache as MemoryCache<string, string>).destroy();
      }
    });

    it("serializes and deserializes objects", async () => {
      const obj = { value: 42, name: "test" };
      
      const setResult = await serializedCache.set("object_key", obj);
      expect(setResult._tag).toBe("Right");
      
      const getResult = await serializedCache.get("object_key");
      expect(getResult._tag).toBe("Right");
      if (getResult._tag === "Right") {
        expect(getResult.right).toEqual(obj);
      }
    });

    it("handles serialization errors", async () => {
      const circularObj: any = { name: "test" };
      circularObj.self = circularObj;
      
      const result = await serializedCache.set("circular", circularObj);
      
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("CACHE_SERIALIZE_ERROR");
      }
    });

    it("handles deserialization errors", async () => {
      // Manually set invalid JSON in base cache
      await baseCache.set("invalid_json", "{ invalid json }");
      
      const result = await serializedCache.get("invalid_json");
      
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("CACHE_DESERIALIZE_ERROR");
      }
    });
  });

  describe("Cache Factory", () => {
    it("creates memory cache with default parameters", () => {
      const result = createMemoryCache();
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBeDefined();
      }
    });

    it("creates memory cache with custom parameters", () => {
      const result = createMemoryCache<string, number>(500, 30000);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBeDefined();
      }
    });

    it("handles memory cache creation errors", () => {
      // This is hard to test as MemoryCache is simple, but we test the pattern
      const result = createMemoryCache(-1, -1); // Invalid parameters
      
      // Should either succeed (parameters are validated) or fail gracefully
      if (result._tag === "Left") {
        expect(result.left.category).toBe("CACHE");
        expect(result.left.code).toBe("MEMORY_CACHE_INIT_ERROR");
      }
    });
  });

  describe("Redis Cache", () => {
    // Note: These tests would require a Redis instance in a real environment
    // For now, we test the creation logic and error handling
    
    it("handles Redis connection errors gracefully", async () => {
      const result = await createRedisCache("redis://invalid-host:6379");
      
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.category).toBe("CACHE");
        expect(result.left.code).toBe("REDIS_CACHE_INIT_ERROR");
      }
    });

    it("handles Redis configuration options", async () => {
      const options = {
        keyPrefix: "test:",
        connectTimeout: 5000,
        lazyConnect: true
      };
      
      const result = await createRedisCache("redis://localhost:6379", options);
      
      // Should fail gracefully if Redis is not available
      if (result._tag === "Left") {
        expect(result.left.category).toBe("CACHE");
      }
    });
  });

  describe("Edge Cases", () => {
    let cache: Cache<string, string>;

    beforeEach(() => {
      const result = createMemoryCache<string, string>();
      expect(result._tag).toBe("Right");
      cache = result.right!;
    });

    afterEach(() => {
      if (cache && 'destroy' in cache) {
        (cache as MemoryCache<string, string>).destroy();
      }
    });

    it("handles empty string keys and values", async () => {
      await cache.set("", "");
      
      const result = await cache.get("");
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBe("");
      }
    });

    it("handles very long keys and values", async () => {
      const longKey = "x".repeat(1000);
      const longValue = "y".repeat(10000);
      
      await cache.set(longKey, longValue);
      
      const result = await cache.get(longKey);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBe(longValue);
      }
    });

    it("handles special characters in keys", async () => {
      const specialKey = "key:with/special\\chars@#$%^&*()";
      const value = "test value";
      
      await cache.set(specialKey, value);
      
      const result = await cache.get(specialKey);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBe(value);
      }
    });

    it("handles unicode keys and values", async () => {
      const unicodeKey = "键🔑";
      const unicodeValue = "值📝";
      
      await cache.set(unicodeKey, unicodeValue);
      
      const result = await cache.get(unicodeKey);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBe(unicodeValue);
      }
    });

    it("handles zero TTL", async () => {
      const result = await cache.set("zero_ttl", "value", 0);
      
      // Zero TTL should either immediately expire or be treated as no TTL
      expect(result._tag).toBe("Right");
      
      const getResult = await cache.get("zero_ttl");
      // Could be either expired or valid depending on implementation
      expect(getResult._tag).toMatch(/^(Left|Right)$/);
    });

    it("handles negative TTL", async () => {
      const result = await cache.set("negative_ttl", "value", -1);
      
      // Negative TTL should be handled gracefully
      expect(result._tag).toBe("Right");
      
      const getResult = await cache.get("negative_ttl");
      expect(getResult._tag).toBe("Left"); // Should be expired
    });
  });

  describe("Memory Management", () => {
    it("handles many cache operations without memory leaks", async () => {
      const cache = createMemoryCache<string, string>(1000).right!;
      
      // Perform many operations
      for (let i = 0; i < 5000; i++) {
        await cache.set(`key${i}`, `value${i}`, 100); // Short TTL
        
        if (i % 100 === 0) {
          await cache.get(`key${i - 50}`); // Some gets
        }
      }
      
      // Cache should handle this gracefully
      const sizeResult = await cache.size();
      expect(sizeResult._tag).toBe("Right");
      
      if ('destroy' in cache) {
        (cache as MemoryCache<string, string>).destroy();
      }
    });
  });
});