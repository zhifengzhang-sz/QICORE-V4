/**
 * QiCore v4.0 Core Component - High-Performance Cache
 *
 * Mathematical Foundation:
 * - State Management: Consistent cache operations with TTL and eviction
 * - Performance Tier: TypeScript (interpreted) = 100× baseline
 *
 * Package Implementation:
 * - Uses node-cache@5.1.2 for high-performance in-memory caching
 * - Uses ioredis@5.3.2 for distributed caching scenarios
 * - Production-ready with comprehensive error handling
 */

import Redis, { type RedisOptions } from "ioredis";
// eslint-disable-next-line @typescript-eslint/no-require-imports
const NodeCache = require("node-cache");
import { createQiError } from "../base/error.js";
import { type Result, Result as ResultClass, failure, isSuccess, success } from "../base/result.js";

// ============================================================================
// Cache Types and Configuration
// ============================================================================

/**
 * Simple interface for NodeCache to avoid import issues
 */
interface NodeCacheInterface {
  get(key: string): unknown;
  set(key: string, value: unknown, ttl?: number): boolean;
  del(key: string): number;
  has(key: string): boolean;
  keys(): string[];
  flushAll(): void;
  getStats(): {
    hits: number;
    misses: number;
    keys: number;
    ksize: number;
    vsize: number;
  };
  close(): void;
  on(event: string, callback: (...args: unknown[]) => void): void;
}

/**
 * Cache Configuration Options
 */
export interface CacheConfig {
  readonly type: "memory" | "redis";
  readonly maxSize?: number; // Maximum number of entries
  readonly ttl?: number; // Default TTL in seconds
  readonly checkPeriod?: number; // Check period for expired keys (seconds)
  // Redis-specific options
  readonly redisUrl?: string;
  readonly redisHost?: string;
  readonly redisPort?: number;
  readonly redisPassword?: string;
  readonly redisDb?: number;
}

/**
 * Cache Statistics
 */
export interface CacheStats {
  readonly hits: number;
  readonly misses: number;
  readonly keys: number;
  readonly size: number;
  readonly hitRate: number;
  readonly sets?: number;
}

/**
 * Cache Interface
 */
export interface CacheInterface {
  readonly get: <T>(key: string) => Promise<Result<T | null>>;
  readonly set: <T>(key: string, value: T, ttl?: number) => Promise<Result<void>>;
  readonly delete: (key: string) => Promise<Result<boolean>>;
  readonly has: (key: string) => Promise<Result<boolean>>;
  readonly clear: () => Promise<Result<void>>;
  readonly keys: () => Promise<Result<string[]>>;
  readonly getStats: () => Promise<Result<CacheStats>>;
  readonly close: () => Promise<Result<void>>;
}

// ============================================================================
// Memory Cache Implementation (NodeCache)
// ============================================================================

class MemoryCache implements CacheInterface {
  private readonly cache: NodeCacheInterface;
  private stats = { hits: 0, misses: 0 };

  constructor(config: CacheConfig) {
    this.cache = new NodeCache({
      stdTTL: config.ttl || 600, // Default 10 minutes
      checkperiod: config.checkPeriod || 120, // Check every 2 minutes
      maxKeys: config.maxSize || 1000,
      useClones: false, // Performance optimization
    });

    // Track statistics
    this.cache.on("hit", () => {
      this.stats.hits++;
    });

    this.cache.on("missed", () => {
      this.stats.misses++;
    });
  }

  async get<T>(key: string): Promise<Result<T | null>> {
    try {
      const value = this.cache.get(key) as T | undefined;
      return success(value ?? null);
    } catch (error) {
      return failure(
        createQiError("CACHE_GET_ERROR", `Failed to get key ${key}: ${error}`, "SYSTEM", {
          key,
          error: String(error),
        })
      );
    }
  }

  async set<T>(key: string, value: T, ttl?: number): Promise<Result<void>> {
    try {
      const success_result = this.cache.set(key, value, ttl || 0);
      if (success_result) {
        return success(undefined);
      }
      return failure(
        createQiError("CACHE_SET_ERROR", `Failed to set key ${key}`, "SYSTEM", { key })
      );
    } catch (error) {
      return failure(
        createQiError("CACHE_SET_ERROR", `Failed to set key ${key}: ${error}`, "SYSTEM", {
          key,
          error: String(error),
        })
      );
    }
  }

  async delete(key: string): Promise<Result<boolean>> {
    try {
      const deleteCount = this.cache.del(key);
      return success(deleteCount > 0);
    } catch (error) {
      return failure(
        createQiError("CACHE_DELETE_ERROR", `Failed to delete key ${key}: ${error}`, "SYSTEM", {
          key,
          error: String(error),
        })
      );
    }
  }

  async has(key: string): Promise<Result<boolean>> {
    try {
      const exists = this.cache.has(key);
      return success(exists);
    } catch (error) {
      return failure(
        createQiError("CACHE_HAS_ERROR", `Failed to check key ${key}: ${error}`, "SYSTEM", {
          key,
          error: String(error),
        })
      );
    }
  }

  async clear(): Promise<Result<void>> {
    try {
      this.cache.flushAll();
      return success(undefined);
    } catch (error) {
      return failure(
        createQiError("CACHE_CLEAR_ERROR", `Failed to clear cache: ${error}`, "SYSTEM", {
          error: String(error),
        })
      );
    }
  }

  async keys(): Promise<Result<string[]>> {
    try {
      const allKeys = this.cache.keys();
      return success(allKeys);
    } catch (error) {
      return failure(
        createQiError("CACHE_KEYS_ERROR", `Failed to get keys: ${error}`, "SYSTEM", {
          error: String(error),
        })
      );
    }
  }

  async getStats(): Promise<Result<CacheStats>> {
    try {
      const stats = this.cache.getStats();
      const hitRate =
        this.stats.hits + this.stats.misses > 0
          ? this.stats.hits / (this.stats.hits + this.stats.misses)
          : 0;

      return success({
        hits: this.stats.hits,
        misses: this.stats.misses,
        keys: stats.keys,
        size: stats.ksize + stats.vsize,
        hitRate,
      });
    } catch (error) {
      return failure(
        createQiError("CACHE_STATS_ERROR", `Failed to get stats: ${error}`, "SYSTEM", {
          error: String(error),
        })
      );
    }
  }

  async close(): Promise<Result<void>> {
    try {
      this.cache.close();
      return success(undefined);
    } catch (error) {
      return failure(
        createQiError("CACHE_CLOSE_ERROR", `Failed to close cache: ${error}`, "SYSTEM", {
          error: String(error),
        })
      );
    }
  }
}

// ============================================================================
// Redis Cache Implementation (IORedis)
// ============================================================================

class RedisCache implements CacheInterface {
  private readonly redis: Redis;
  private stats = { hits: 0, misses: 0 };

  constructor(config: CacheConfig) {
    const redisConfig: RedisOptions = {
      host: config.redisHost || "localhost",
      port: config.redisPort || 6379,
      password: config.redisPassword,
      db: config.redisDb || 0,
      maxRetriesPerRequest: 3,
      lazyConnect: true,
    };

    if (config.redisUrl) {
      this.redis = new Redis(config.redisUrl, redisConfig);
    } else {
      this.redis = new Redis(redisConfig);
    }
  }

  async get<T>(key: string): Promise<Result<T | null>> {
    try {
      const value = await this.redis.get(key);
      if (value === null) {
        this.stats.misses++;
        return success(null);
      }

      this.stats.hits++;
      const parsed = JSON.parse(value) as T;
      return success(parsed);
    } catch (error) {
      return failure(
        createQiError("REDIS_GET_ERROR", `Failed to get key ${key}: ${error}`, "NETWORK", {
          key,
          error: String(error),
        })
      );
    }
  }

  async set<T>(key: string, value: T, ttl?: number): Promise<Result<void>> {
    try {
      const serialized = JSON.stringify(value);

      if (ttl && ttl > 0) {
        await this.redis.setex(key, ttl, serialized);
      } else {
        await this.redis.set(key, serialized);
      }

      return success(undefined);
    } catch (error) {
      return failure(
        createQiError("REDIS_SET_ERROR", `Failed to set key ${key}: ${error}`, "NETWORK", {
          key,
          error: String(error),
        })
      );
    }
  }

  async delete(key: string): Promise<Result<boolean>> {
    try {
      const deleteCount = await this.redis.del(key);
      return success(deleteCount > 0);
    } catch (error) {
      return failure(
        createQiError("REDIS_DELETE_ERROR", `Failed to delete key ${key}: ${error}`, "NETWORK", {
          key,
          error: String(error),
        })
      );
    }
  }

  async has(key: string): Promise<Result<boolean>> {
    try {
      const exists = await this.redis.exists(key);
      return success(exists === 1);
    } catch (error) {
      return failure(
        createQiError("REDIS_HAS_ERROR", `Failed to check key ${key}: ${error}`, "NETWORK", {
          key,
          error: String(error),
        })
      );
    }
  }

  async clear(): Promise<Result<void>> {
    try {
      await this.redis.flushdb();
      return success(undefined);
    } catch (error) {
      return failure(
        createQiError("REDIS_CLEAR_ERROR", `Failed to clear cache: ${error}`, "NETWORK", {
          error: String(error),
        })
      );
    }
  }

  async keys(): Promise<Result<string[]>> {
    try {
      const allKeys = await this.redis.keys("*");
      return success(allKeys);
    } catch (error) {
      return failure(
        createQiError("REDIS_KEYS_ERROR", `Failed to get keys: ${error}`, "NETWORK", {
          error: String(error),
        })
      );
    }
  }

  async getStats(): Promise<Result<CacheStats>> {
    try {
      const info = await this.redis.info("keyspace");
      const dbInfo = info.match(/db0:keys=(\d+),expires=(\d+),avg_ttl=(\d+)/);
      const keyCount = dbInfo ? Number.parseInt(dbInfo[1], 10) : 0;

      const hitRate =
        this.stats.hits + this.stats.misses > 0
          ? this.stats.hits / (this.stats.hits + this.stats.misses)
          : 0;

      return success({
        hits: this.stats.hits,
        misses: this.stats.misses,
        keys: keyCount,
        size: 0, // Redis doesn't easily provide memory usage per DB
        hitRate,
      });
    } catch (error) {
      return failure(
        createQiError("REDIS_STATS_ERROR", `Failed to get stats: ${error}`, "NETWORK", {
          error: String(error),
        })
      );
    }
  }

  async close(): Promise<Result<void>> {
    try {
      this.redis.disconnect();
      return success(undefined);
    } catch (error) {
      return failure(
        createQiError(
          "REDIS_CLOSE_ERROR",
          `Failed to close Redis connection: ${error}`,
          "NETWORK",
          {
            error: String(error),
          }
        )
      );
    }
  }

  /**
   * Test Redis connection
   * @internal Used by cache factory for connection testing
   */
  async ping(): Promise<void> {
    await this.redis.ping();
  }
}

// ============================================================================
// Cache Factory Functions
// ============================================================================

/**
 * createMemoryCache: CacheConfig → Result<Cache>
 * Create high-performance in-memory cache
 * Performance: < 50μs per operation (TypeScript interpreted tier)
 */
export const createMemoryCache = (config: Partial<CacheConfig> = {}): Result<CacheInterface> => {
  try {
    const cacheConfig: CacheConfig = {
      type: "memory",
      maxSize: 1000,
      ttl: 600,
      checkPeriod: 120,
      ...config,
    };

    const cache = new MemoryCache(cacheConfig);
    return success(cache);
  } catch (error) {
    return failure(
      createQiError(
        "MEMORY_CACHE_CREATE_ERROR",
        `Failed to create memory cache: ${error}`,
        "SYSTEM",
        {
          config,
          error: String(error),
        }
      )
    );
  }
};

/**
 * createRedisCache: CacheConfig → Promise<Result<Cache>>
 * Create distributed Redis cache
 * Performance: < 2ms per operation (network bound)
 */
export const createRedisCache = async (
  config: Partial<CacheConfig> = {}
): Promise<Result<CacheInterface>> => {
  try {
    const cacheConfig: CacheConfig = {
      type: "redis",
      redisHost: "localhost",
      redisPort: 6379,
      redisDb: 0,
      ...config,
    };

    const cache = new RedisCache(cacheConfig);

    // Test connection
    try {
      await (cache as RedisCache).ping();
    } catch (error) {
      return failure(
        createQiError("REDIS_CONNECTION_ERROR", `Failed to connect to Redis: ${error}`, "NETWORK", {
          config: cacheConfig,
          error: String(error),
        })
      );
    }

    return success(cache);
  } catch (error) {
    return failure(
      createQiError(
        "REDIS_CACHE_CREATE_ERROR",
        `Failed to create Redis cache: ${error}`,
        "SYSTEM",
        {
          config,
          error: String(error),
        }
      )
    );
  }
};

/**
 * createCache: CacheConfig → Promise<Result<Cache>>
 * Auto-select cache implementation based on configuration
 */
export const createCache = async (
  config: Partial<CacheConfig> = {}
): Promise<Result<CacheInterface>> => {
  const cacheType = config.type || "memory";

  switch (cacheType) {
    case "memory":
      return createMemoryCache(config);
    case "redis":
      return createRedisCache(config);
    default:
      return failure(
        createQiError("INVALID_CACHE_TYPE", `Unknown cache type: ${cacheType}`, "VALIDATION", {
          type: cacheType,
          config,
        })
      );
  }
};

// ============================================================================
// Complete Cache API
// ============================================================================

/**
 * Cache API following QiCore v4 mathematical specification
 */
export const QiCache = {
  // Factory functions
  create: createCache,
  createMemory: createMemoryCache,
  createRedis: createRedisCache,
} as const;

// Export types for external use
// Types already exported above

// ============================================================================
// Cache Class for Test Compatibility
// ============================================================================

/**
 * Cache class interface that provides constructor with generic types and config
 * for compatibility with test expectations
 */
export class Cache<K = string, V = unknown> {
  private cache: MemoryCache;
  private stats = { hits: 0, misses: 0, sets: 0 };

  constructor(config: { maxSize?: number; ttl?: number } = {}) {
    const cacheConfig: CacheConfig = {
      type: "memory" as const,
      maxSize: config.maxSize || 1000,
      ttl: config.ttl || 600,
    };
    this.cache = new MemoryCache(cacheConfig);
  }

  /**
   * Set a value in the cache
   */
  async set(key: K, value: V, ttl?: number): Promise<ResultClass<void>> {
    const keyStr = String(key);
    const result = await this.cache.set(keyStr, value, ttl);
    if (isSuccess(result)) {
      this.stats.sets++;
      return ResultClass.success(undefined);
    }
    return ResultClass.failure(result.left);
  }

  /**
   * Get a value from the cache
   */
  async get(key: K): Promise<ResultClass<V | null>> {
    const keyStr = String(key);
    const result = await this.cache.get<V>(keyStr);
    if (isSuccess(result)) {
      if (result.right !== null) {
        this.stats.hits++;
      } else {
        this.stats.misses++;
      }
      return ResultClass.success(result.right);
    }
    return ResultClass.failure(result.left);
  }

  /**
   * Get cache statistics
   */
  getStats(): CacheStats {
    const hitRate =
      this.stats.hits + this.stats.misses > 0
        ? this.stats.hits / (this.stats.hits + this.stats.misses)
        : 0;

    return {
      hits: this.stats.hits,
      misses: this.stats.misses,
      keys: 0, // Would need to track separately or get from cache
      size: 0, // Would need to track separately or get from cache
      hitRate,
      sets: this.stats.sets,
    };
  }
}
