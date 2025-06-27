/**
 * QiCore v4.0 - Configuration Component
 * 
 * Mathematical Contract-Based TypeScript Library
 * Component 3: Configuration - Type-safe config with monoid merge (9 operations)
 */

import { z, type ZodSchema } from "zod";
import { readFile } from "fs/promises";
import { Result } from "../base/result.js";
import { QiError } from "../base/error.js";

/**
 * Configuration class provides type-safe configuration management with validation,
 * environment variable loading, and monoid merge operations.
 */
export class Configuration<T> {
  private data: T | null = null;
  private originalData: Record<string, unknown> | null = null;

  constructor(private readonly schema: ZodSchema<T>) {}

  /**
   * Operation 1: Load configuration from object
   */
  loadFromObject(obj: unknown): Result<T> {
    try {
      const parsed = this.schema.parse(obj);
      this.data = parsed;
      // Store the original object for merge operations
      this.originalData = obj as Record<string, unknown>;
      return Result.success(parsed);
    } catch (error) {
      if (error instanceof z.ZodError) {
        return Result.failure(
          QiError.validationError(
            `Configuration validation failed: ${error.issues.map(i => i.message).join(", ")}`,
            "config",
            obj,
          ),
        );
      }
      return Result.failure(
        QiError.configurationError(
          `Configuration parsing failed: ${error}`,
          "unknown",
          "object",
        ),
      );
    }
  }

  /**
   * Operation 2: Load configuration from environment variables
   */
  loadFromEnv(prefix = ""): Result<T> {
    try {
      const envObj = this.extractEnvVars(prefix);
      return this.loadFromObject(envObj);
    } catch (error) {
      return Result.failure(
        QiError.configurationError(
          `Environment loading failed: ${error}`,
          "env",
          "environment",
        ),
      );
    }
  }

  /**
   * Operation 3: Load configuration from JSON string
   */
  loadFromJson(json: string): Result<T> {
    try {
      const obj = JSON.parse(json);
      return this.loadFromObject(obj);
    } catch (error) {
      return Result.failure(
        QiError.configurationError(
          `JSON parsing failed: ${error}`,
          "json",
          "string",
        ),
      );
    }
  }

  /**
   * Operation 4: Load configuration from file (for Node.js/Bun environments)
   */
  async loadFromFile(filePath: string): Promise<Result<T>> {
    try {
      const content = await readFile(filePath, "utf-8");
      
      if (filePath.endsWith(".json")) {
        return this.loadFromJson(content);
      } else {
        // Try to parse as JSON anyway
        return this.loadFromJson(content);
      }
    } catch (error) {
      return Result.failure(
        QiError.resourceError(
          `File loading failed: ${error}`,
          "file",
          filePath,
        ),
      );
    }
  }

  /**
   * Operation 5: Merge with another configuration (Monoid operation)
   */
  merge(other: Configuration<T>): Result<Configuration<T>> {
    if (!this.data || !other.data || !this.originalData || !other.originalData) {
      return Result.failure(
        QiError.stateError(
          "Cannot merge uninitialized configurations",
          "uninitialized",
          "initialized",
        ),
      );
    }

    try {
      // Merge the original data (without defaults applied)
      // This ensures that defaults from the second config don't override explicit values from the first
      const mergedOriginal = this.deepMerge(
        (this.originalData ?? {}) as T, 
        (other.originalData ?? {}) as T
      );
      const result = new Configuration(this.schema);
      // Load the merged original data, which will apply defaults only where needed
      return result.loadFromObject(mergedOriginal).map(() => result);
    } catch (error) {
      return Result.failure(
        QiError.configurationError(
          `Merge failed: ${error}`,
          "merge",
          "object",
        ),
      );
    }
  }

  /**
   * Operation 6: Get current configuration
   */
  get(): Result<T> {
    if (this.data) {
      return Result.success(this.data);
    }
    return Result.failure(
      QiError.stateError(
        "Configuration not loaded",
        "uninitialized",
        "initialized",
      ),
    );
  }

  /**
   * Operation 7: Validate current configuration
   */
  validate(): Result<T> {
    if (!this.data) {
      return Result.failure(
        QiError.stateError(
          "No configuration to validate",
          "uninitialized",
          "initialized",
        ),
      );
    }

    try {
      const validated = this.schema.parse(this.data);
      return Result.success(validated);
    } catch (error) {
      if (error instanceof z.ZodError) {
        return Result.failure(
          QiError.validationError(
            `Validation failed: ${error.issues.map(i => i.message).join(", ")}`,
            "config",
            this.data,
          ),
        );
      }
      return Result.failure(
        QiError.configurationError(
          `Validation error: ${error}`,
          "validation",
          "object",
        ),
      );
    }
  }

  /**
   * Operation 8: Reset configuration
   */
  reset(): Result<null> {
    this.data = null;
    this.originalData = null;
    return Result.success(null);
  }

  /**
   * Operation 9: Clone configuration
   */
  clone(): Result<Configuration<T>> {
    if (!this.data || !this.originalData) {
      return Result.failure(
        QiError.stateError(
          "Cannot clone uninitialized configuration",
          "uninitialized",
          "initialized",
        ),
      );
    }

    const cloned = new Configuration(this.schema);
    return cloned.loadFromObject(this.originalData).map(() => cloned);
  }

  /**
   * Check if configuration is loaded
   */
  isLoaded(): boolean {
    return this.data !== null;
  }

  /**
   * Get configuration with default fallback
   */
  getOrDefault(defaultValue: T): T {
    return this.data ?? defaultValue;
  }

  /**
   * Extract environment variables with optional prefix
   */
  private extractEnvVars(prefix: string): Record<string, unknown> {
    const result: Record<string, unknown> = {};
    const envPrefix = prefix ? `${prefix}_` : "";

    for (const [key, value] of Object.entries(process.env)) {
      if (key.startsWith(envPrefix)) {
        const configKey = prefix ? key.slice(envPrefix.length) : key;
        result[configKey.toLowerCase()] = this.parseEnvValue(value || "");
      }
    }

    return result;
  }

  /**
   * Parse environment variable value with type inference
   */
  private parseEnvValue(value: string): unknown {
    // Try boolean
    if (value.toLowerCase() === "true") return true;
    if (value.toLowerCase() === "false") return false;

    // Try number
    if (/^-?\\d+$/.test(value)) return parseInt(value, 10);
    if (/^-?\\d+\\.\\d+$/.test(value)) return parseFloat(value);

    // Try JSON
    if ((value.startsWith("{") && value.endsWith("}")) || 
        (value.startsWith("[") && value.endsWith("]"))) {
      try {
        return JSON.parse(value);
      } catch {
        // Fall through to string
      }
    }

    // Return as string
    return value;
  }

  /**
   * Deep merge two objects (monoid operation)
   */
  private deepMerge(target: T, source: T): T {
    if (this.isObject(target) && this.isObject(source)) {
      const result = { ...target } as any;
      
      for (const key in source) {
        if (this.isObject((source as any)[key]) && this.isObject((target as any)[key])) {
          (result as any)[key] = this.deepMerge((target as any)[key], (source as any)[key]);
        } else {
          (result as any)[key] = (source as any)[key];
        }
      }
      
      return result;
    }
    
    return source;
  }

  /**
   * Check if value is a plain object
   */
  private isObject(item: unknown): item is Record<string, unknown> {
    return item !== null && typeof item === "object" && !Array.isArray(item);
  }
}

/**
 * Configuration builder for fluent API
 */
export class ConfigurationBuilder<T> {
  private config: Configuration<T>;
  private schema: ZodSchema<T>;

  constructor(schema: ZodSchema<T>) {
    this.schema = schema;
    this.config = new Configuration(schema);
  }

  /**
   * Load from object
   */
  fromObject(obj: unknown): this {
    if (this.config.isLoaded()) {
      // If config is already loaded, merge with existing data
      const tempConfig = new Configuration(this.schema);
      const result = tempConfig.loadFromObject(obj);
      if (result.isFailure()) {
        throw new Error(`Configuration loading failed: ${result.error().message}`);
      }
      const merged = this.config.merge(tempConfig);
      if (merged.isFailure()) {
        throw new Error(`Configuration merge failed: ${merged.error().message}`);
      }
      this.config = merged.unwrap();
    } else {
      // First load, just load directly
      const result = this.config.loadFromObject(obj);
      if (result.isFailure()) {
        throw new Error(`Configuration loading failed: ${result.error().message}`);
      }
    }
    return this;
  }

  /**
   * Load from environment
   */
  fromEnv(prefix?: string): this {
    if (this.config.isLoaded()) {
      // If config is already loaded, merge with existing data
      const tempConfig = new Configuration(this.schema);
      const result = tempConfig.loadFromEnv(prefix);
      if (result.isFailure()) {
        throw new Error(`Environment loading failed: ${result.error().message}`);
      }
      const merged = this.config.merge(tempConfig);
      if (merged.isFailure()) {
        throw new Error(`Configuration merge failed: ${merged.error().message}`);
      }
      this.config = merged.unwrap();
    } else {
      // First load, just load directly
      const result = this.config.loadFromEnv(prefix);
      if (result.isFailure()) {
        throw new Error(`Environment loading failed: ${result.error().message}`);
      }
    }
    return this;
  }

  /**
   * Load from JSON
   */
  fromJson(json: string): this {
    if (this.config.isLoaded()) {
      // If config is already loaded, merge with existing data
      const tempConfig = new Configuration(this.schema);
      const result = tempConfig.loadFromJson(json);
      if (result.isFailure()) {
        throw new Error(`JSON loading failed: ${result.error().message}`);
      }
      const merged = this.config.merge(tempConfig);
      if (merged.isFailure()) {
        throw new Error(`Configuration merge failed: ${merged.error().message}`);
      }
      this.config = merged.unwrap();
    } else {
      // First load, just load directly
      const result = this.config.loadFromJson(json);
      if (result.isFailure()) {
        throw new Error(`JSON loading failed: ${result.error().message}`);
      }
    }
    return this;
  }

  /**
   * Load from file
   */
  async fromFile(filePath: string): Promise<this> {
    if (this.config.isLoaded()) {
      // If config is already loaded, merge with existing data
      const tempConfig = new Configuration(this.schema);
      const result = await tempConfig.loadFromFile(filePath);
      if (result.isFailure()) {
        throw new Error(`File loading failed: ${result.error().message}`);
      }
      const merged = this.config.merge(tempConfig);
      if (merged.isFailure()) {
        throw new Error(`Configuration merge failed: ${merged.error().message}`);
      }
      this.config = merged.unwrap();
    } else {
      // First load, just load directly
      const result = await this.config.loadFromFile(filePath);
      if (result.isFailure()) {
        throw new Error(`File loading failed: ${result.error().message}`);
      }
    }
    return this;
  }

  /**
   * Set a specific value (for fluent API)
   */
  set<K extends keyof T>(key: K, value: T[K]): this {
    const currentData = this.config.isLoaded() ? this.config.get().unwrap() : {} as T;
    const newData = { ...currentData, [key]: value };
    const result = this.config.loadFromObject(newData);
    if (result.isFailure()) {
      throw new Error(`Configuration set failed: ${result.error().message}`);
    }
    return this;
  }

  /**
   * Build the configuration
   */
  build(): Configuration<T> {
    return this.config;
  }
}

/**
 * Utility functions for configuration management
 */
export namespace ConfigurationUtils {
  /**
   * Create a configuration from schema
   */
  export function create<T>(schema: ZodSchema<T>): Configuration<T> {
    return new Configuration(schema);
  }

  /**
   * Create a configuration builder
   */
  export function builder<T>(schema: ZodSchema<T>): ConfigurationBuilder<T> {
    return new ConfigurationBuilder(schema);
  }

  /**
   * Merge multiple configurations
   */
  export function mergeAll<T>(configs: Configuration<T>[]): Result<Configuration<T>> {
    if (configs.length === 0) {
      return Result.failure(
        QiError.validationError("Cannot merge empty configuration list", "configs", configs),
      );
    }

    let result = configs[0];
    for (let i = 1; i < configs.length; i++) {
      const mergeResult = result.merge(configs[i]);
      if (mergeResult.isFailure()) {
        return mergeResult;
      }
      result = mergeResult.unwrap();
    }

    return Result.success(result);
  }
}