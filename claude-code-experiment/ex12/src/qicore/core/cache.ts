/**
 * QiCore v4.0 - Core Cache Component
 * 
 * Mathematical insight: Cache operations are state transformations with LRU + TTL policies
 * High-performance memory and Redis caching with unified interface
 */

import Redis from "ioredis";
import { createQiError, ErrorCategory } from "../base/error.js";
import { success, failure, type Result, fromAsyncTryCatch } from "../base/result.js";

/**
 * Cache interface - unified API for memory and Redis implementations
 */
export interface Cache<K, V> {
  get(key: K): Promise<Result<V>>;
  set(key: K, value: V, ttl?: number): Promise<Result<void>>;
  has(key: K): Promise<Result<boolean>>;
  delete(key: K): Promise<Result<boolean>>;
  clear(): Promise<Result<void>>;
  size(): Promise<Result<number>>;
  keys(): Promise<Result<K[]>>;
  getStats(): Promise<Result<CacheStats>>;
}

/**
 * Cache statistics interface
 */
export interface CacheStats {
  readonly hitCount: number;
  readonly missCount: number;
  readonly evictionCount: number;
  readonly size: number;
  readonly maxSize?: number;
}

/**
 * Cache entry for memory implementation
 */
interface CacheEntry<V> {
  value: V;
  expiry: number;
  lastUsed: number;
}

/**
 * Memory Cache Implementation with LRU + TTL
 * State monad pattern for predictable state transformations
 */
export class MemoryCache<K, V> implements Cache<K, V> {
  private data = new Map<K, CacheEntry<V>>();
  private maxSize: number;
  private defaultTTL: number;
  private stats: CacheStats;
  private cleanupInterval?: NodeJS.Timeout;

  constructor(maxSize = 1000, defaultTTL = 300000) { // 5 minutes default
    this.maxSize = maxSize;
    this.defaultTTL = defaultTTL;
    this.stats = {
      hitCount: 0,
      missCount: 0,
      evictionCount: 0,
      size: 0,
      maxSize
    };
    
    // Start background cleanup every minute
    this.startCleanup();
  }

  async get(key: K): Promise<Result<V>> {
    try {
      const entry = this.data.get(key);
      
      if (!entry) {
        this.stats = { ...this.stats, missCount: this.stats.missCount + 1 };
        return failure(createQiError(
          "CACHE_MISS",
          `Key not found: ${String(key)}`,
          ErrorCategory.CACHE,
          { key: String(key) }
        ));
      }
      
      const now = Date.now();
      if (now > entry.expiry) {
        this.data.delete(key);
        this.stats = { 
          ...this.stats, 
          missCount: this.stats.missCount + 1,
          size: this.data.size
        };
        return failure(createQiError(
          "CACHE_EXPIRED",
          `Key expired: ${String(key)}`,
          ErrorCategory.CACHE,
          { key: String(key), expiry: entry.expiry, now }
        ));
      }
      
      // Update LRU timestamp
      entry.lastUsed = now;
      this.stats = { ...this.stats, hitCount: this.stats.hitCount + 1 };
      
      return success(entry.value);
    } catch (error) {
      return failure(createQiError(
        "CACHE_GET_ERROR",
        `Failed to get cache value: ${error}`,
        ErrorCategory.CACHE,
        { key: String(key) }
      ));
    }
  }

  async set(key: K, value: V, ttl?: number): Promise<Result<void>> {
    try {
      const now = Date.now();
      const expiry = now + (ttl ?? this.defaultTTL);
      
      // Check if we need to evict for space
      if (this.data.size >= this.maxSize && !this.data.has(key)) {
        this.evictLRU();
      }
      
      this.data.set(key, {
        value,
        expiry,
        lastUsed: now
      });
      
      this.stats = { ...this.stats, size: this.data.size };
      return success(undefined);
    } catch (error) {
      return failure(createQiError(
        "CACHE_SET_ERROR",
        `Failed to set cache value: ${error}`,
        ErrorCategory.CACHE,
        { key: String(key) }
      ));
    }
  }

  async has(key: K): Promise<Result<boolean>> {
    try {
      const entry = this.data.get(key);
      if (!entry) return success(false);
      
      if (Date.now() > entry.expiry) {
        this.data.delete(key);
        this.stats = { ...this.stats, size: this.data.size };
        return success(false);
      }
      
      return success(true);
    } catch (error) {
      return failure(createQiError(
        "CACHE_HAS_ERROR",
        `Failed to check cache key: ${error}`,
        ErrorCategory.CACHE,
        { key: String(key) }
      ));
    }
  }

  async delete(key: K): Promise<Result<boolean>> {
    try {
      const existed = this.data.delete(key);
      this.stats = { ...this.stats, size: this.data.size };
      return success(existed);
    } catch (error) {
      return failure(createQiError(
        "CACHE_DELETE_ERROR",
        `Failed to delete cache key: ${error}`,
        ErrorCategory.CACHE,
        { key: String(key) }
      ));
    }
  }

  async clear(): Promise<Result<void>> {
    try {
      this.data.clear();
      this.stats = {
        ...this.stats,
        size: 0,
        evictionCount: this.stats.evictionCount + this.stats.size
      };
      return success(undefined);
    } catch (error) {
      return failure(createQiError(
        "CACHE_CLEAR_ERROR",
        `Failed to clear cache: ${error}`,
        ErrorCategory.CACHE
      ));
    }
  }

  async size(): Promise<Result<number>> {
    try {
      return success(this.data.size);
    } catch (error) {
      return failure(createQiError(
        "CACHE_SIZE_ERROR",
        `Failed to get cache size: ${error}`,
        ErrorCategory.CACHE
      ));
    }
  }

  async keys(): Promise<Result<K[]>> {
    try {
      return success(Array.from(this.data.keys()));
    } catch (error) {
      return failure(createQiError(
        "CACHE_KEYS_ERROR",
        `Failed to get cache keys: ${error}`,
        ErrorCategory.CACHE
      ));
    }
  }

  async getStats(): Promise<Result<CacheStats>> {
    return success({ ...this.stats, size: this.data.size });
  }

  /**
   * LRU eviction policy
   */
  private evictLRU(): void {
    let oldestKey: K | undefined;
    let oldestTime = Date.now();
    
    for (const [key, entry] of this.data.entries()) {
      if (entry.lastUsed < oldestTime) {
        oldestTime = entry.lastUsed;
        oldestKey = key;
      }
    }
    
    if (oldestKey !== undefined) {
      this.data.delete(oldestKey);
      this.stats = {
        ...this.stats,
        evictionCount: this.stats.evictionCount + 1,
        size: this.data.size
      };
    }
  }

  /**
   * Background cleanup of expired entries
   */
  private startCleanup(): void {
    this.cleanupInterval = setInterval(() => {
      this.cleanupExpired();
    }, 60000); // Every minute
  }

  private cleanupExpired(): void {
    const now = Date.now();
    let evicted = 0;
    
    for (const [key, entry] of this.data.entries()) {
      if (now > entry.expiry) {
        this.data.delete(key);
        evicted++;
      }
    }
    
    if (evicted > 0) {
      this.stats = {
        ...this.stats,
        evictionCount: this.stats.evictionCount + evicted,
        size: this.data.size
      };
    }
  }

  /**
   * Cleanup resources
   */
  destroy(): void {
    if (this.cleanupInterval) {
      clearInterval(this.cleanupInterval);
    }
    this.data.clear();
  }
}

/**
 * Redis Cache Implementation
 */
export class RedisCache<K, V> implements Cache<K, V> {
  private redis: Redis;
  private keyPrefix: string;
  private stats: CacheStats;

  constructor(redis: Redis, keyPrefix = "qicache:") {
    this.redis = redis;
    this.keyPrefix = keyPrefix;
    this.stats = {
      hitCount: 0,
      missCount: 0,
      evictionCount: 0,
      size: 0
    };
  }

  async get(key: K): Promise<Result<V>> {
    return fromAsyncTryCatch(async () => {
      const redisKey = this.getRedisKey(key);
      const value = await this.redis.get(redisKey);
      
      if (value === null) {
        this.stats = { ...this.stats, missCount: this.stats.missCount + 1 };
        throw new Error(`Key not found: ${String(key)}`);
      }
      
      this.stats = { ...this.stats, hitCount: this.stats.hitCount + 1 };
      return JSON.parse(value) as V;
    }).then(result =>
      result._tag === "Left"
        ? failure(createQiError(
            "REDIS_GET_ERROR",
            `Failed to get Redis value: ${result.left.message}`,
            ErrorCategory.CACHE,
            { key: String(key) }
          ))
        : result
    );
  }

  async set(key: K, value: V, ttl?: number): Promise<Result<void>> {
    return fromAsyncTryCatch(async () => {
      const redisKey = this.getRedisKey(key);
      const serialized = JSON.stringify(value);
      
      if (ttl !== undefined) {
        await this.redis.setex(redisKey, Math.ceil(ttl / 1000), serialized);
      } else {
        await this.redis.set(redisKey, serialized);
      }
    }).then(result =>
      result._tag === "Left"
        ? failure(createQiError(
            "REDIS_SET_ERROR",
            `Failed to set Redis value: ${result.left.message}`,
            ErrorCategory.CACHE,
            { key: String(key) }
          ))
        : result
    );
  }

  async has(key: K): Promise<Result<boolean>> {
    return fromAsyncTryCatch(async () => {
      const redisKey = this.getRedisKey(key);
      const exists = await this.redis.exists(redisKey);
      return exists === 1;
    }).then(result =>
      result._tag === "Left"
        ? failure(createQiError(
            "REDIS_HAS_ERROR",
            `Failed to check Redis key: ${result.left.message}`,
            ErrorCategory.CACHE,
            { key: String(key) }
          ))
        : result
    );
  }

  async delete(key: K): Promise<Result<boolean>> {
    return fromAsyncTryCatch(async () => {
      const redisKey = this.getRedisKey(key);
      const deleted = await this.redis.del(redisKey);
      return deleted === 1;
    }).then(result =>
      result._tag === "Left"
        ? failure(createQiError(
            "REDIS_DELETE_ERROR",
            `Failed to delete Redis key: ${result.left.message}`,
            ErrorCategory.CACHE,
            { key: String(key) }
          ))
        : result
    );
  }

  async clear(): Promise<Result<void>> {
    return fromAsyncTryCatch(async () => {
      const pattern = `${this.keyPrefix}*`;
      const keys = await this.redis.keys(pattern);
      if (keys.length > 0) {
        await this.redis.del(...keys);
        this.stats = {
          ...this.stats,
          evictionCount: this.stats.evictionCount + keys.length
        };
      }
    }).then(result =>
      result._tag === "Left"
        ? failure(createQiError(
            "REDIS_CLEAR_ERROR",
            `Failed to clear Redis cache: ${result.left.message}`,
            ErrorCategory.CACHE
          ))
        : result
    );
  }

  async size(): Promise<Result<number>> {
    return fromAsyncTryCatch(async () => {
      const pattern = `${this.keyPrefix}*`;
      const keys = await this.redis.keys(pattern);
      return keys.length;
    }).then(result =>
      result._tag === "Left"
        ? failure(createQiError(
            "REDIS_SIZE_ERROR",
            `Failed to get Redis cache size: ${result.left.message}`,
            ErrorCategory.CACHE
          ))
        : result
    );
  }

  async keys(): Promise<Result<K[]>> {
    return fromAsyncTryCatch(async () => {
      const pattern = `${this.keyPrefix}*`;
      const redisKeys = await this.redis.keys(pattern);
      return redisKeys.map(key => 
        key.slice(this.keyPrefix.length) as K
      );
    }).then(result =>
      result._tag === "Left"
        ? failure(createQiError(
            "REDIS_KEYS_ERROR",
            `Failed to get Redis cache keys: ${result.left.message}`,
            ErrorCategory.CACHE
          ))
        : result
    );
  }

  async getStats(): Promise<Result<CacheStats>> {
    const sizeResult = await this.size();
    if (sizeResult._tag === "Left") return sizeResult;
    
    return success({
      ...this.stats,
      size: sizeResult.right
    });
  }

  private getRedisKey(key: K): string {
    return `${this.keyPrefix}${String(key)}`;
  }
}

/**
 * Cache factory functions
 */

/**
 * Creates a memory cache with LRU and TTL
 */
export const createMemoryCache = <K, V>(
  maxSize?: number,
  defaultTTL?: number
): Result<Cache<K, V>> => {
  try {
    return success(new MemoryCache<K, V>(maxSize, defaultTTL));
  } catch (error) {
    return failure(createQiError(
      "MEMORY_CACHE_INIT_ERROR",
      `Failed to create memory cache: ${error}`,
      ErrorCategory.CACHE,
      { maxSize, defaultTTL }
    ));
  }
};

/**
 * Creates a Redis cache
 */
export const createRedisCache = async <K, V>(
  connectionString: string,
  options?: {
    keyPrefix?: string;
    connectTimeout?: number;
    lazyConnect?: boolean;
  }
): Promise<Result<Cache<K, V>>> => {
  return fromAsyncTryCatch(async () => {
    const redis = new Redis(connectionString, {
      connectTimeout: options?.connectTimeout || 10000,
      lazyConnect: options?.lazyConnect || true,
      retryDelayOnFailover: 100,
      maxRetriesPerRequest: 3
    });
    
    // Test connection
    await redis.ping();
    
    return new RedisCache<K, V>(redis, options?.keyPrefix);
  }).then(result =>
    result._tag === "Left"
      ? failure(createQiError(
          "REDIS_CACHE_INIT_ERROR",
          `Failed to create Redis cache: ${result.left.message}`,
          ErrorCategory.CACHE,
          { connectionString, options }
        ))
      : result
  );
};

/**
 * Cache utility operations
 */

/**
 * Batch get operation
 */
export const mget = async <K, V>(
  cache: Cache<K, V>,
  keys: K[]
): Promise<Result<Map<K, V>>> => {
  try {
    const results = new Map<K, V>();
    
    for (const key of keys) {
      const result = await cache.get(key);
      if (result._tag === "Right") {
        results.set(key, result.right);
      }
    }
    
    return success(results);
  } catch (error) {
    return failure(createQiError(
      "CACHE_MGET_ERROR",
      `Failed to perform batch get: ${error}`,
      ErrorCategory.CACHE,
      { keys: keys.map(String) }
    ));
  }
};

/**
 * Batch set operation
 */
export const mset = async <K, V>(
  cache: Cache<K, V>,
  entries: Map<K, V>,
  ttl?: number
): Promise<Result<void>> => {
  try {
    for (const [key, value] of entries) {
      const result = await cache.set(key, value, ttl);
      if (result._tag === "Left") {
        return result;
      }
    }
    
    return success(undefined);
  } catch (error) {
    return failure(createQiError(
      "CACHE_MSET_ERROR",
      `Failed to perform batch set: ${error}`,
      ErrorCategory.CACHE,
      { entryCount: entries.size }
    ));
  }
};

/**
 * Cache with automatic serialization
 */
export const createSerializedCache = <K, V>(
  baseCache: Cache<string, string>
): Cache<K, V> => ({
  async get(key: K): Promise<Result<V>> {
    const result = await baseCache.get(String(key));
    if (result._tag === "Left") return result;
    
    try {
      const value = JSON.parse(result.right) as V;
      return success(value);
    } catch (error) {
      return failure(createQiError(
        "CACHE_DESERIALIZE_ERROR",
        `Failed to deserialize cache value: ${error}`,
        ErrorCategory.CACHE,
        { key: String(key) }
      ));
    }
  },

  async set(key: K, value: V, ttl?: number): Promise<Result<void>> {
    try {
      const serialized = JSON.stringify(value);
      return await baseCache.set(String(key), serialized, ttl);
    } catch (error) {
      return failure(createQiError(
        "CACHE_SERIALIZE_ERROR",
        `Failed to serialize cache value: ${error}`,
        ErrorCategory.CACHE,
        { key: String(key) }
      ));
    }
  },

  async has(key: K): Promise<Result<boolean>> {
    return await baseCache.has(String(key));
  },

  async delete(key: K): Promise<Result<boolean>> {
    return await baseCache.delete(String(key));
  },

  async clear(): Promise<Result<void>> {
    return await baseCache.clear();
  },

  async size(): Promise<Result<number>> {
    return await baseCache.size();
  },

  async keys(): Promise<Result<K[]>> {
    const result = await baseCache.keys();
    if (result._tag === "Left") return result;
    
    return success(result.right.map(key => key as K));
  },

  async getStats(): Promise<Result<CacheStats>> {
    return await baseCache.getStats();
  }
});