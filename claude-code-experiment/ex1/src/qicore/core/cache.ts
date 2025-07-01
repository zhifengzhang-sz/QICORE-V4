import { ErrorCategory, type QiError, createQiError } from "../base/error.js";
import {
	type Result,
	failure,
	fromAsyncTryCatch,
	success,
} from "../base/result.js";

export interface CacheConfig {
	maxSize: number;
	evictionPolicy?: "LRU" | "FIFO";
	defaultTTL?: number;
	persistenceInterval?: number;
}

export interface CacheEntry<T> {
	value: T;
	timestamp: number;
	ttl?: number;
	accessCount: number;
	lastAccessed: number;
}

export interface Cache<K = string, V = unknown> {
	get(key: K): Result<V>;
	set(key: K, value: V, ttl?: number): Result<void>;
	has(key: K): boolean;
	remove(key: K): boolean;
	clear(): void;
	size(): number;
	getOrSet(key: K, factory: () => V | Promise<V>): Promise<Result<V>>;
}

class MemoryCache<K = string, V = unknown> implements Cache<K, V> {
	private store: Map<K, CacheEntry<V>>;
	private config: CacheConfig;
	private accessOrder: K[]; // For LRU tracking

	constructor(config: CacheConfig) {
		this.store = new Map();
		this.config = config;
		this.accessOrder = [];

		// Start background cleanup for expired entries
		this.startCleanupTimer();
	}

	get(key: K): Result<V> {
		const entry = this.store.get(key);

		if (!entry) {
			return failure(
				createQiError(
					"KEY_NOT_FOUND",
					`Cache key not found: ${key}`,
					ErrorCategory.CACHE,
					{ key },
				),
			);
		}

		// Check TTL expiration
		if (this.isExpired(entry)) {
			this.store.delete(key);
			this.removeFromAccessOrder(key);
			return failure(
				createQiError(
					"KEY_EXPIRED",
					`Cache key has expired: ${key}`,
					ErrorCategory.CACHE,
					{ key, ttl: entry.ttl },
				),
			);
		}

		// Update access tracking
		entry.accessCount++;
		entry.lastAccessed = Date.now();
		this.updateAccessOrder(key);

		return success(entry.value);
	}

	set(key: K, value: V, ttl?: number): Result<void> {
		try {
			const now = Date.now();
			const entry: CacheEntry<V> = {
				value,
				timestamp: now,
				ttl: ttl ?? this.config.defaultTTL,
				accessCount: 1,
				lastAccessed: now,
			};

			// Check if cache is at capacity and evict if necessary
			if (!this.store.has(key) && this.store.size >= this.config.maxSize) {
				this.evictEntry();
			}

			this.store.set(key, entry);
			this.updateAccessOrder(key);

			return success(undefined);
		} catch (error) {
			return failure(
				createQiError(
					"CACHE_SET_ERROR",
					`Failed to set cache entry: ${error}`,
					ErrorCategory.CACHE,
					{ key, originalError: error },
				),
			);
		}
	}

	has(key: K): boolean {
		const entry = this.store.get(key);
		return entry !== undefined && !this.isExpired(entry);
	}

	remove(key: K): boolean {
		const existed = this.store.has(key);
		this.store.delete(key);
		this.removeFromAccessOrder(key);
		return existed;
	}

	clear(): void {
		this.store.clear();
		this.accessOrder = [];
	}

	size(): number {
		// Clean up expired entries first
		this.cleanupExpired();
		return this.store.size;
	}

	async getOrSet(key: K, factory: () => V | Promise<V>): Promise<Result<V>> {
		const existing = this.get(key);
		if (existing._tag === "Right") {
			return existing;
		}

		try {
			const value = await factory();
			const setResult = this.set(key, value);

			if (setResult._tag === "Left") {
				return setResult;
			}

			return success(value);
		} catch (error) {
			return failure(
				createQiError(
					"CACHE_FACTORY_ERROR",
					`Cache factory function failed: ${error}`,
					ErrorCategory.CACHE,
					{ key, originalError: error },
				),
			);
		}
	}

	private isExpired(entry: CacheEntry<V>): boolean {
		if (!entry.ttl) return false;
		return Date.now() - entry.timestamp > entry.ttl;
	}

	private evictEntry(): void {
		if (this.store.size === 0) return;

		if (this.config.evictionPolicy === "FIFO") {
			// Remove oldest entry by timestamp
			let oldestKey: K | undefined;
			let oldestTime = Number.POSITIVE_INFINITY;

			for (const [key, entry] of this.store) {
				if (entry.timestamp < oldestTime) {
					oldestTime = entry.timestamp;
					oldestKey = key;
				}
			}

			if (oldestKey !== undefined) {
				this.remove(oldestKey);
			}
		} else {
			// LRU - remove least recently used
			if (this.accessOrder.length > 0) {
				const lruKey = this.accessOrder[0];
				this.remove(lruKey);
			}
		}
	}

	private updateAccessOrder(key: K): void {
		this.removeFromAccessOrder(key);
		this.accessOrder.push(key);
	}

	private removeFromAccessOrder(key: K): void {
		const index = this.accessOrder.indexOf(key);
		if (index > -1) {
			this.accessOrder.splice(index, 1);
		}
	}

	private cleanupExpired(): void {
		const expiredKeys: K[] = [];

		for (const [key, entry] of this.store) {
			if (this.isExpired(entry)) {
				expiredKeys.push(key);
			}
		}

		for (const key of expiredKeys) {
			this.remove(key);
		}
	}

	private startCleanupTimer(): void {
		// Run cleanup every 60 seconds
		setInterval(() => {
			this.cleanupExpired();
		}, 60000);
	}
}

// Redis cache implementation (simplified without actual Redis dependency)
class RedisCache<K = string, V = unknown> implements Cache<K, V> {
	private config: CacheConfig;
	private connected = false;

	constructor(config: CacheConfig) {
		this.config = config;
		// In a real implementation, this would connect to Redis
		this.connected = true;
	}

	get(key: K): Result<V> {
		if (!this.connected) {
			return failure(
				createQiError(
					"CACHE_CONNECTION_ERROR",
					"Redis cache not connected",
					ErrorCategory.CACHE,
				),
			);
		}

		// Simulate Redis get operation
		return failure(
			createQiError(
				"KEY_NOT_FOUND",
				`Redis key not found: ${key}`,
				ErrorCategory.CACHE,
				{ key },
			),
		);
	}

	set(key: K, value: V, ttl?: number): Result<void> {
		if (!this.connected) {
			return failure(
				createQiError(
					"CACHE_CONNECTION_ERROR",
					"Redis cache not connected",
					ErrorCategory.CACHE,
				),
			);
		}

		// Simulate Redis set operation
		return success(undefined);
	}

	has(key: K): boolean {
		return this.connected && false; // Simulate key not found
	}

	remove(key: K): boolean {
		return this.connected && false; // Simulate key not found
	}

	clear(): void {
		// Simulate Redis flush
	}

	size(): number {
		return 0; // Simulate empty cache
	}

	async getOrSet(key: K, factory: () => V | Promise<V>): Promise<Result<V>> {
		const existing = this.get(key);
		if (existing._tag === "Right") {
			return existing;
		}

		try {
			const value = await factory();
			const setResult = this.set(key, value);

			if (setResult._tag === "Left") {
				return setResult;
			}

			return success(value);
		} catch (error) {
			return failure(
				createQiError(
					"CACHE_FACTORY_ERROR",
					`Cache factory function failed: ${error}`,
					ErrorCategory.CACHE,
					{ key, originalError: error },
				),
			);
		}
	}
}

export const createMemoryCache = <K = string, V = unknown>(
	config?: Partial<CacheConfig>,
): Result<Cache<K, V>> => {
	try {
		const defaultConfig: CacheConfig = {
			maxSize: 1000,
			evictionPolicy: "LRU",
			defaultTTL: undefined,
		};

		const finalConfig = { ...defaultConfig, ...config };

		if (finalConfig.maxSize <= 0) {
			return failure(
				createQiError(
					"INVALID_CACHE_CONFIG",
					"Cache maxSize must be greater than 0",
					ErrorCategory.CONFIGURATION,
					{ maxSize: finalConfig.maxSize },
				),
			);
		}

		return success(new MemoryCache<K, V>(finalConfig));
	} catch (error) {
		return failure(
			createQiError(
				"CACHE_CREATION_ERROR",
				`Failed to create memory cache: ${error}`,
				ErrorCategory.CACHE,
				{ config, originalError: error },
			),
		);
	}
};

export const createRedisCache = async <K = string, V = unknown>(
	connectionString: string,
	config?: Partial<CacheConfig>,
): Promise<Result<Cache<K, V>>> => {
	return fromAsyncTryCatch(
		async () => {
			const defaultConfig: CacheConfig = {
				maxSize: 10000,
				evictionPolicy: "LRU",
				defaultTTL: undefined,
			};

			const finalConfig = { ...defaultConfig, ...config };

			// In a real implementation, this would connect to Redis using ioredis
			// For now, return a mock Redis cache
			return new RedisCache<K, V>(finalConfig);
		},
		(error) =>
			createQiError(
				"REDIS_CONNECTION_ERROR",
				`Failed to connect to Redis: ${error}`,
				ErrorCategory.NETWORK,
				{ connectionString, originalError: error },
			),
	);
};
