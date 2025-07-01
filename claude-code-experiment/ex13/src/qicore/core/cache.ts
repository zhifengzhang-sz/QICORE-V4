/**
 * QiCore v4.0 - High-Performance Caching with LRU + TTL
 * 
 * Mathematical Foundation: Cache operations are state transformations with LRU + TTL policies
 * Implementation: Custom memory cache + ioredis for distributed scenarios
 * Performance Target: ~10-30μs for memory operations, ~1-2ms for Redis operations
 */

import type Redis from "ioredis";
import type { Result } from "../base/result.js";
import { success, failure } from "../base/result.js";
import { CacheError } from "../base/error.js";

/**
 * Cache entry with metadata for LRU and TTL policies
 */
interface CacheEntry<V> {
  readonly value: V;
  readonly expiry: number;
  readonly lastUsed: number;
  readonly size: number; // For memory management
}

/**
 * Cache statistics for monitoring
 */
export interface CacheStats {
  readonly hits: number;
  readonly misses: number;
  readonly hitRate: number;
  readonly size: number;
  readonly maxSize: number;
  readonly memoryUsage: number; // Approximate bytes
}

/**
 * Cache configuration options
 */
export interface CacheConfig {
  readonly maxSize: number;
  readonly defaultTTL: number; // milliseconds
  readonly cleanupInterval: number; // milliseconds
  readonly enableStats: boolean;
}

/**
 * Generic cache interface for consistent API across implementations
 */
export interface Cache<K, V> {
  readonly get: (key: K) => Promise<Result<V>>;
  readonly set: (key: K, value: V, ttl?: number) => Promise<Result<void>>;
  readonly has: (key: K) => Promise<boolean>;
  readonly delete: (key: K) => Promise<Result<void>>;
  readonly clear: () => Promise<Result<void>>;
  readonly size: () => Promise<number>;
  readonly stats: () => Promise<CacheStats>;
  readonly close: () => Promise<void>;
}

/**
 * Default cache configuration
 */
const defaultConfig: CacheConfig = {
  maxSize: 1000,
  defaultTTL: 5 * 60 * 1000, // 5 minutes
  cleanupInterval: 60 * 1000, // 1 minute
  enableStats: true
};

/**
 * High-performance memory cache with LRU eviction and TTL expiration
 * Optimized for V8 engine performance
 */
export class MemoryCache<K, V> implements Cache<K, V> {
  private readonly config: CacheConfig;
  private readonly data: Map<K, CacheEntry<V>>;
  private readonly accessOrder: K[]; // For LRU tracking
  private cleanupTimer?: NodeJS.Timeout;
  
  // Statistics tracking
  private stats = {
    hits: 0,
    misses: 0,
    approximateMemory: 0
  };

  constructor(config: Partial<CacheConfig> = {}) {
    this.config = { ...defaultConfig, ...config };
    this.data = new Map();
    this.accessOrder = [];
    
    // Start background cleanup if interval specified
    if (this.config.cleanupInterval > 0) {
      this.startCleanup();
    }
  }

  /**
   * Get value from cache
   * Performance: ~10-30μs for memory operations
   */
  async get(key: K): Promise<Result<V>> {
    const entry = this.data.get(key);
    
    if (!entry) {
      this.stats.misses++;
      return failure(CacheError(
        "CACHE_MISS",
        `Key not found in cache: ${key}`,
        new Map([["key", key]])
      ));
    }
    
    // Check TTL expiration
    const now = Date.now();
    if (now > entry.expiry) {
      this.data.delete(key);
      this.removeFromAccessOrder(key);
      this.stats.misses++;
      return failure(CacheError(
        "CACHE_EXPIRED",
        `Key expired in cache: ${key}`,
        new Map([["key", key], ["expiry", entry.expiry], ["now", now]])
      ));
    }
    
    // Update LRU order
    this.updateAccessOrder(key);
    this.stats.hits++;
    
    return success(entry.value);
  }

  /**
   * Set value in cache with optional TTL
   * Performance: ~15-35μs including LRU eviction
   */
  async set(key: K, value: V, ttl?: number): Promise<Result<void>> {
    try {
      const now = Date.now();
      const expiry = now + (ttl ?? this.config.defaultTTL);
      const size = this.estimateSize(value);
      
      const entry: CacheEntry<V> = {
        value,
        expiry,
        lastUsed: now,
        size
      };
      
      // Check if we need to evict entries
      if (this.data.size >= this.config.maxSize && !this.data.has(key)) {
        this.evictLRU();
      }
      
      // Update existing or add new entry
      const wasUpdate = this.data.has(key);
      this.data.set(key, entry);
      
      if (!wasUpdate) {
        this.accessOrder.push(key);
        this.stats.approximateMemory += size;
      } else {
        this.updateAccessOrder(key);
      }
      
      return success(undefined);
    } catch (error) {
      return failure(CacheError(
        "CACHE_SET_ERROR",
        `Failed to set cache entry: ${error}`,
        new Map([["key", key], ["error", error]])
      ));
    }
  }

  /**
   * Check if key exists in cache (without updating LRU)
   */
  async has(key: K): Promise<boolean> {
    const entry = this.data.get(key);
    if (!entry) return false;
    
    // Check TTL without updating access
    const now = Date.now();
    if (now > entry.expiry) {
      this.data.delete(key);
      this.removeFromAccessOrder(key);
      return false;
    }
    
    return true;
  }

  /**
   * Delete specific key from cache
   */
  async delete(key: K): Promise<Result<void>> {
    try {
      const entry = this.data.get(key);
      if (entry) {
        this.stats.approximateMemory -= entry.size;
      }
      
      this.data.delete(key);
      this.removeFromAccessOrder(key);
      
      return success(undefined);
    } catch (error) {
      return failure(CacheError(
        "CACHE_DELETE_ERROR",
        `Failed to delete cache entry: ${error}`,
        new Map([["key", key], ["error", error]])
      ));
    }
  }

  /**
   * Clear all entries from cache
   */
  async clear(): Promise<Result<void>> {
    try {
      this.data.clear();
      this.accessOrder.length = 0;
      this.stats.approximateMemory = 0;
      return success(undefined);
    } catch (error) {
      return failure(CacheError(
        "CACHE_CLEAR_ERROR",
        `Failed to clear cache: ${error}`,
        new Map([["error", error]])
      ));
    }
  }

  /**
   * Get current cache size
   */
  async size(): Promise<number> {
    return this.data.size;
  }

  /**
   * Get cache statistics
   */
  async stats(): Promise<CacheStats> {
    const totalOperations = this.stats.hits + this.stats.misses;
    const hitRate = totalOperations > 0 ? this.stats.hits / totalOperations : 0;
    
    return {
      hits: this.stats.hits,
      misses: this.stats.misses,
      hitRate,
      size: this.data.size,
      maxSize: this.config.maxSize,
      memoryUsage: this.stats.approximateMemory
    };
  }

  /**
   * Close cache and cleanup resources
   */
  async close(): Promise<void> {
    if (this.cleanupTimer) {
      clearInterval(this.cleanupTimer);
      this.cleanupTimer = undefined;
    }
    await this.clear();
  }

  /**
   * Start background cleanup timer
   */
  private startCleanup(): void {
    this.cleanupTimer = setInterval(() => {
      this.cleanupExpired();
    }, this.config.cleanupInterval);
  }

  /**
   * Remove expired entries (background cleanup)
   */
  private cleanupExpired(): void {
    const now = Date.now();
    const keysToDelete: K[] = [];
    
    for (const [key, entry] of this.data) {
      if (now > entry.expiry) {
        keysToDelete.push(key);
      }
    }
    
    for (const key of keysToDelete) {
      this.data.delete(key);
      this.removeFromAccessOrder(key);
    }
  }

  /**
   * Evict least recently used entry
   */
  private evictLRU(): void {
    if (this.accessOrder.length === 0) return;
    
    const lruKey = this.accessOrder.shift();
    if (lruKey) {
      const entry = this.data.get(lruKey);
      if (entry) {
        this.stats.approximateMemory -= entry.size;
      }
      this.data.delete(lruKey);
    }
  }

  /**
   * Update access order for LRU tracking
   */
  private updateAccessOrder(key: K): void {
    this.removeFromAccessOrder(key);
    this.accessOrder.push(key);
  }

  /**
   * Remove key from access order array
   */
  private removeFromAccessOrder(key: K): void {
    const index = this.accessOrder.indexOf(key);
    if (index >= 0) {
      this.accessOrder.splice(index, 1);
    }
  }

  /**
   * Estimate memory size of value (rough approximation)
   */
  private estimateSize(value: V): number {
    if (value === null || value === undefined) return 8;
    if (typeof value === "string") return value.length * 2; // UTF-16
    if (typeof value === "number") return 8;
    if (typeof value === "boolean") return 4;
    if (typeof value === "object") {
      return JSON.stringify(value).length * 2; // Rough estimate
    }
    return 16; // Default size
  }
}

/**
 * Redis-based distributed cache
 * Provides consistent interface with memory cache
 */
export class RedisCache<K, V> implements Cache<K, V> {
  private readonly redis: Redis;
  private readonly keyPrefix: string;
  private readonly defaultTTL: number;
  
  // Statistics tracking
  private stats = {
    hits: 0,
    misses: 0
  };

  constructor(
    redis: Redis,
    options: {
      keyPrefix?: string;
      defaultTTL?: number;
    } = {}
  ) {
    this.redis = redis;
    this.keyPrefix = options.keyPrefix ?? "qicore:cache:";
    this.defaultTTL = options.defaultTTL ?? 5 * 60; // 5 minutes in seconds
  }

  /**
   * Get value from Redis cache
   * Performance: ~1-2ms including network latency
   */
  async get(key: K): Promise<Result<V>> {
    try {
      const redisKey = this.buildKey(key);
      const value = await this.redis.get(redisKey);
      
      if (value === null) {
        this.stats.misses++;
        return failure(CacheError(
          "CACHE_MISS",
          `Key not found in Redis cache: ${key}`,
          new Map([["key", key], ["redisKey", redisKey]])
        ));
      }
      
      this.stats.hits++;
      const parsed = JSON.parse(value) as V;
      return success(parsed);
    } catch (error) {
      return failure(CacheError(
        "REDIS_GET_ERROR",
        `Failed to get from Redis cache: ${error}`,
        new Map([["key", key], ["error", error]])
      ));
    }
  }

  /**
   * Set value in Redis cache with TTL
   */
  async set(key: K, value: V, ttl?: number): Promise<Result<void>> {
    try {
      const redisKey = this.buildKey(key);
      const serialized = JSON.stringify(value);
      const ttlSeconds = ttl ? Math.ceil(ttl / 1000) : this.defaultTTL;
      
      await this.redis.setex(redisKey, ttlSeconds, serialized);
      return success(undefined);
    } catch (error) {
      return failure(CacheError(
        "REDIS_SET_ERROR",
        `Failed to set Redis cache entry: ${error}`,
        new Map([["key", key], ["error", error]])
      ));
    }
  }

  /**
   * Check if key exists in Redis cache
   */
  async has(key: K): Promise<boolean> {
    try {
      const redisKey = this.buildKey(key);
      const exists = await this.redis.exists(redisKey);
      return exists === 1;
    } catch {
      return false;
    }
  }

  /**
   * Delete key from Redis cache
   */
  async delete(key: K): Promise<Result<void>> {
    try {
      const redisKey = this.buildKey(key);
      await this.redis.del(redisKey);
      return success(undefined);
    } catch (error) {
      return failure(CacheError(
        "REDIS_DELETE_ERROR",
        `Failed to delete Redis cache entry: ${error}`,
        new Map([["key", key], ["error", error]])
      ));
    }
  }

  /**
   * Clear all cache entries with prefix
   */
  async clear(): Promise<Result<void>> {
    try {
      const pattern = `${this.keyPrefix}*`;
      const keys = await this.redis.keys(pattern);
      if (keys.length > 0) {
        await this.redis.del(...keys);
      }
      return success(undefined);
    } catch (error) {
      return failure(CacheError(
        "REDIS_CLEAR_ERROR",
        `Failed to clear Redis cache: ${error}`,
        new Map([["error", error]])
      ));
    }
  }

  /**
   * Get approximate cache size
   */
  async size(): Promise<number> {
    try {
      const pattern = `${this.keyPrefix}*`;
      const keys = await this.redis.keys(pattern);
      return keys.length;
    } catch {
      return 0;
    }
  }

  /**
   * Get cache statistics
   */
  async stats(): Promise<CacheStats> {
    const totalOperations = this.stats.hits + this.stats.misses;
    const hitRate = totalOperations > 0 ? this.stats.hits / totalOperations : 0;
    const currentSize = await this.size();
    
    return {
      hits: this.stats.hits,
      misses: this.stats.misses,
      hitRate,
      size: currentSize,
      maxSize: -1, // Redis doesn't have a fixed max size
      memoryUsage: -1 // Would need Redis MEMORY USAGE command
    };
  }

  /**
   * Close Redis connection
   */
  async close(): Promise<void> {
    await this.redis.quit();
  }

  /**
   * Build Redis key with prefix
   */
  private buildKey(key: K): string {
    return `${this.keyPrefix}${String(key)}`;
  }
}

/**
 * Create memory cache with default configuration
 */
export const createMemoryCache = <K, V>(
  config?: Partial<CacheConfig>
): Result<Cache<K, V>> => {
  try {
    const cache = new MemoryCache<K, V>(config);
    return success(cache);
  } catch (error) {
    return failure(CacheError(
      "MEMORY_CACHE_CREATE_ERROR",
      `Failed to create memory cache: ${error}`,
      new Map([["config", config], ["error", error]])
    ));
  }
};

/**
 * Create Redis cache with connection
 */
export const createRedisCache = <K, V>(
  redis: Redis,
  options?: {
    keyPrefix?: string;
    defaultTTL?: number;
  }
): Result<Cache<K, V>> => {
  try {
    const cache = new RedisCache<K, V>(redis, options);
    return success(cache);
  } catch (error) {
    return failure(CacheError(
      "REDIS_CACHE_CREATE_ERROR",
      `Failed to create Redis cache: ${error}`,
      new Map([["options", options], ["error", error]])
    ));
  }
}; 