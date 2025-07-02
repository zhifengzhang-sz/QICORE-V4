import { beforeEach, describe, expect, it } from "vitest";
import { MemoryManager } from "../../../lib/src/qimcp/tools/memory-simple.js";

describe("MemoryManager", () => {
	let memoryManager: MemoryManager;

	beforeEach(() => {
		memoryManager = new MemoryManager();
	});

	describe("Basic Operations", () => {
		it("should create a memory manager instance", () => {
			expect(memoryManager).toBeInstanceOf(MemoryManager);
		});

		it("should store and retrieve memory", () => {
			const key = "test-key";
			const value = "test-value";

			memoryManager.store(key, value);
			const retrieved = memoryManager.get(key);

			expect(retrieved).toBe(value);
		});

		it("should return undefined for non-existent keys", () => {
			const result = memoryManager.get("non-existent-key");
			expect(result).toBeUndefined();
		});

		it("should check if key exists", () => {
			const key = "test-key";

			expect(memoryManager.has(key)).toBe(false);

			memoryManager.store(key, "value");
			expect(memoryManager.has(key)).toBe(true);
		});

		it("should delete memory", () => {
			const key = "test-key";

			memoryManager.store(key, "value");
			expect(memoryManager.has(key)).toBe(true);

			const deleted = memoryManager.delete(key);
			expect(deleted).toBe(true);
			expect(memoryManager.has(key)).toBe(false);
		});

		it("should return false when deleting non-existent key", () => {
			const deleted = memoryManager.delete("non-existent-key");
			expect(deleted).toBe(false);
		});
	});

	describe("Metadata Operations", () => {
		it("should store memory with metadata", () => {
			const key = "test-key";
			const value = "test-value";
			const metadata = { category: "test", importance: "high" };

			memoryManager.storeWithMetadata(key, value, metadata);

			const retrieved = memoryManager.get(key);
			const retrievedMetadata = memoryManager.getMetadata(key);

			expect(retrieved).toBe(value);
			expect(retrievedMetadata).toEqual(expect.objectContaining(metadata));
			expect(retrievedMetadata?.timestamp).toBeInstanceOf(Date);
		});

		it("should return undefined metadata for non-existent keys", () => {
			const metadata = memoryManager.getMetadata("non-existent-key");
			expect(metadata).toBeUndefined();
		});

		it("should update existing memory metadata", () => {
			const key = "test-key";

			memoryManager.storeWithMetadata(key, "value1", { category: "old" });
			memoryManager.storeWithMetadata(key, "value2", { category: "new", extra: "data" });

			const metadata = memoryManager.getMetadata(key);
			expect(metadata?.category).toBe("new");
			expect(metadata?.extra).toBe("data");
		});
	});

	describe("Bulk Operations", () => {
		it("should list all keys", () => {
			memoryManager.store("key1", "value1");
			memoryManager.store("key2", "value2");
			memoryManager.store("key3", "value3");

			const keys = memoryManager.listKeys();

			expect(keys).toContain("key1");
			expect(keys).toContain("key2");
			expect(keys).toContain("key3");
			expect(keys).toHaveLength(3);
		});

		it("should return empty array when no keys exist", () => {
			const keys = memoryManager.listKeys();
			expect(keys).toEqual([]);
		});

		it("should clear all memory", () => {
			memoryManager.store("key1", "value1");
			memoryManager.store("key2", "value2");

			expect(memoryManager.listKeys()).toHaveLength(2);

			memoryManager.clear();

			expect(memoryManager.listKeys()).toHaveLength(0);
			expect(memoryManager.has("key1")).toBe(false);
			expect(memoryManager.has("key2")).toBe(false);
		});

		it("should get all memory entries", () => {
			memoryManager.store("key1", "value1");
			memoryManager.storeWithMetadata("key2", "value2", { category: "test" });

			const entries = memoryManager.getAll();

			expect(entries).toHaveLength(2);
			expect(entries[0]).toEqual({
				key: "key1",
				value: "value1",
				metadata: expect.objectContaining({
					timestamp: expect.any(Date),
				}),
			});
			expect(entries[1]).toEqual({
				key: "key2",
				value: "value2",
				metadata: expect.objectContaining({
					category: "test",
					timestamp: expect.any(Date),
				}),
			});
		});
	});

	describe("Search Operations", () => {
		beforeEach(() => {
			memoryManager.storeWithMetadata("user-123", "John Doe", { category: "user", role: "admin" });
			memoryManager.storeWithMetadata("user-456", "Jane Smith", { category: "user", role: "user" });
			memoryManager.storeWithMetadata("config-db", "localhost:5432", {
				category: "config",
				type: "database",
			});
			memoryManager.storeWithMetadata("config-api", "api.example.com", {
				category: "config",
				type: "api",
			});
		});

		it("should search by key pattern", () => {
			const userKeys = memoryManager.searchByKey("user-");

			expect(userKeys).toContain("user-123");
			expect(userKeys).toContain("user-456");
			expect(userKeys).not.toContain("config-db");
			expect(userKeys).toHaveLength(2);
		});

		it("should search by metadata", () => {
			const configEntries = memoryManager.searchByMetadata({ category: "config" });

			expect(configEntries).toHaveLength(2);
			expect(configEntries.map((e) => e.key)).toContain("config-db");
			expect(configEntries.map((e) => e.key)).toContain("config-api");
		});

		it("should search by multiple metadata criteria", () => {
			const adminUsers = memoryManager.searchByMetadata({ category: "user", role: "admin" });

			expect(adminUsers).toHaveLength(1);
			expect(adminUsers[0].key).toBe("user-123");
			expect(adminUsers[0].value).toBe("John Doe");
		});

		it("should return empty results for no matches", () => {
			const results = memoryManager.searchByKey("nonexistent-");
			expect(results).toEqual([]);

			const metadataResults = memoryManager.searchByMetadata({ category: "nonexistent" });
			expect(metadataResults).toEqual([]);
		});
	});

	describe("Statistics", () => {
		it("should return memory statistics", () => {
			memoryManager.store("key1", "value1");
			memoryManager.store("key2", "longer value 2");
			memoryManager.storeWithMetadata("key3", "value3", { category: "test" });

			const stats = memoryManager.getStats();

			expect(stats.totalKeys).toBe(3);
			expect(stats.totalSize).toBeGreaterThan(0);
			expect(stats.categories).toEqual({ test: 1, default: 2 });
			expect(stats.oldestEntry).toBeInstanceOf(Date);
			expect(stats.newestEntry).toBeInstanceOf(Date);
		});

		it("should return empty stats for empty memory", () => {
			const stats = memoryManager.getStats();

			expect(stats.totalKeys).toBe(0);
			expect(stats.totalSize).toBe(0);
			expect(stats.categories).toEqual({});
			expect(stats.oldestEntry).toBeNull();
			expect(stats.newestEntry).toBeNull();
		});
	});

	describe("Data Types", () => {
		it("should handle different value types", () => {
			const stringValue = "string value";
			const numberValue = 42;
			const booleanValue = true;
			const objectValue = { name: "test", count: 5 };
			const arrayValue = [1, 2, 3];

			memoryManager.store("string", stringValue);
			memoryManager.store("number", numberValue);
			memoryManager.store("boolean", booleanValue);
			memoryManager.store("object", objectValue);
			memoryManager.store("array", arrayValue);

			expect(memoryManager.get("string")).toBe(stringValue);
			expect(memoryManager.get("number")).toBe(numberValue);
			expect(memoryManager.get("boolean")).toBe(booleanValue);
			expect(memoryManager.get("object")).toEqual(objectValue);
			expect(memoryManager.get("array")).toEqual(arrayValue);
		});

		it("should handle null and undefined values", () => {
			memoryManager.store("null", null);
			memoryManager.store("undefined", undefined);

			expect(memoryManager.get("null")).toBeNull();
			expect(memoryManager.get("undefined")).toBeUndefined();
			expect(memoryManager.has("null")).toBe(true);
			expect(memoryManager.has("undefined")).toBe(true);
		});
	});

	describe("Edge Cases", () => {
		it("should handle empty string keys", () => {
			memoryManager.store("", "empty key value");

			expect(memoryManager.get("")).toBe("empty key value");
			expect(memoryManager.has("")).toBe(true);
		});

		it("should handle empty string values", () => {
			memoryManager.store("empty-value", "");

			expect(memoryManager.get("empty-value")).toBe("");
			expect(memoryManager.has("empty-value")).toBe(true);
		});

		it("should handle special characters in keys", () => {
			const specialKey = "key-with-!@#$%^&*()_+{}|:<>?`~";

			memoryManager.store(specialKey, "special value");

			expect(memoryManager.get(specialKey)).toBe("special value");
			expect(memoryManager.has(specialKey)).toBe(true);
		});

		it("should handle very large values", () => {
			const largeValue = "x".repeat(10000);

			memoryManager.store("large", largeValue);

			expect(memoryManager.get("large")).toBe(largeValue);
			expect(memoryManager.get("large")?.length).toBe(10000);
		});
	});

	describe("Memory Persistence", () => {
		it("should maintain memory across operations", () => {
			// Store initial data
			memoryManager.store("persistent", "initial value");

			// Perform other operations
			memoryManager.store("temp1", "temp");
			memoryManager.store("temp2", "temp");
			memoryManager.delete("temp1");

			// Original data should still exist
			expect(memoryManager.get("persistent")).toBe("initial value");
			expect(memoryManager.has("persistent")).toBe(true);
		});

		it("should handle rapid successive operations", () => {
			const operations = 100;

			// Rapid store operations
			for (let i = 0; i < operations; i++) {
				memoryManager.store(`key-${i}`, `value-${i}`);
			}

			// Verify all data is stored
			for (let i = 0; i < operations; i++) {
				expect(memoryManager.get(`key-${i}`)).toBe(`value-${i}`);
			}

			expect(memoryManager.listKeys()).toHaveLength(operations);
		});
	});
});
