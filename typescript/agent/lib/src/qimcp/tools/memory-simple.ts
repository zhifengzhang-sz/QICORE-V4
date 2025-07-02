/**
 * Simple in-memory implementation for testing
 */

export interface MemoryMetadata {
	timestamp: Date;
	category?: string;
	[key: string]: unknown;
}

export interface MemoryEntry {
	key: string;
	value: unknown;
	metadata: MemoryMetadata;
}

export interface MemoryStats {
	totalKeys: number;
	totalSize: number;
	categories: Record<string, number>;
	oldestEntry: Date | null;
	newestEntry: Date | null;
}

export class MemoryManager {
	private memory = new Map<string, { value: unknown; metadata: MemoryMetadata }>();

	store(key: string, value: unknown): void {
		this.memory.set(key, {
			value,
			metadata: {
				timestamp: new Date(),
				category: "default",
			},
		});
	}

	storeWithMetadata(key: string, value: unknown, metadata: Partial<MemoryMetadata>): void {
		this.memory.set(key, {
			value,
			metadata: {
				timestamp: new Date(),
				category: "default",
				...metadata,
			},
		});
	}

	get(key: string): unknown {
		return this.memory.get(key)?.value;
	}

	getMetadata(key: string): MemoryMetadata | undefined {
		return this.memory.get(key)?.metadata;
	}

	has(key: string): boolean {
		return this.memory.has(key);
	}

	delete(key: string): boolean {
		return this.memory.delete(key);
	}

	listKeys(): string[] {
		return Array.from(this.memory.keys());
	}

	clear(): void {
		this.memory.clear();
	}

	getAll(): MemoryEntry[] {
		return Array.from(this.memory.entries()).map(([key, { value, metadata }]) => ({
			key,
			value,
			metadata,
		}));
	}

	searchByKey(pattern: string): string[] {
		return this.listKeys().filter((key) => key.includes(pattern));
	}

	searchByMetadata(criteria: Record<string, unknown>): MemoryEntry[] {
		return this.getAll().filter((entry) => {
			return Object.entries(criteria).every(([key, value]) => {
				return entry.metadata[key] === value;
			});
		});
	}

	getStats(): MemoryStats {
		const entries = this.getAll();

		if (entries.length === 0) {
			return {
				totalKeys: 0,
				totalSize: 0,
				categories: {},
				oldestEntry: null,
				newestEntry: null,
			};
		}

		const categories: Record<string, number> = {};
		let totalSize = 0;
		let oldestEntry = entries[0].metadata.timestamp;
		let newestEntry = entries[0].metadata.timestamp;

		for (const entry of entries) {
			const category = (entry.metadata.category as string) || "default";
			categories[category] = (categories[category] || 0) + 1;

			totalSize += JSON.stringify(entry.value).length;

			if (entry.metadata.timestamp < oldestEntry) {
				oldestEntry = entry.metadata.timestamp;
			}
			if (entry.metadata.timestamp > newestEntry) {
				newestEntry = entry.metadata.timestamp;
			}
		}

		return {
			totalKeys: entries.length,
			totalSize,
			categories,
			oldestEntry,
			newestEntry,
		};
	}
}
