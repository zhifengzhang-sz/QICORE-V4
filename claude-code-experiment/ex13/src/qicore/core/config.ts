/**
 * QiCore v4.0 - Configuration Management with Monoid Semantics
 * 
 * Mathematical Foundation: Configuration merge is associative operation with identity
 * Implementation: Custom monoid ensuring mathematical laws with right-biased merge
 * Performance Target: ~200-500μs for merge operations, ~100μs for object parsing
 */

import type { Result } from "../base/result.js";
import { success, failure, map, flatMap } from "../base/result.js";
import { createQiError, ConfigurationError } from "../base/error.js";

/**
 * Configuration data with Map-based storage for optimal V8 performance
 * Immutable structure preserving monoid properties
 */
export interface ConfigData {
  readonly data: Map<string, unknown>;
}

/**
 * Configuration schema for validation
 */
export interface ConfigSchema {
  readonly [key: string]: {
    readonly type: "string" | "number" | "boolean" | "object";
    readonly required?: boolean;
    readonly default?: unknown;
  };
}

/**
 * Identity element for configuration monoid
 * Performance: ~10μs
 * 
 * Monoid Law: empty ⊕ a = a ⊕ empty = a
 */
export const empty = (): ConfigData => ({
  data: new Map()
});

/**
 * Associative merge operation (right-biased)
 * Performance: ~200-500μs for typical configurations
 * 
 * Monoid Laws:
 * - Associativity: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
 * - Identity: empty ⊕ a = a ⊕ empty = a
 * - Right-bias: a ⊕ b overwrites a's values with b's values
 */
export const merge = (a: ConfigData, b: ConfigData): ConfigData => ({
  data: new Map([...a.data, ...b.data])
});

/**
 * Merge multiple configurations (left-to-right precedence)
 * Uses fold to apply merge associatively
 */
export const mergeAll = (configs: ConfigData[]): ConfigData =>
  configs.reduce(merge, empty());

/**
 * Create configuration from object
 * Performance: ~100μs for typical objects
 */
export const fromObject = (obj: Record<string, unknown>): Result<ConfigData> => {
  try {
    return success({
      data: new Map(Object.entries(obj))
    });
  } catch (error) {
    return failure(ConfigurationError(
      "OBJECT_PARSE_ERROR",
      `Failed to parse object: ${error}`,
      new Map([["object", obj]])
    ));
  }
};

/**
 * Create configuration from JSON string
 */
export const fromString = (jsonString: string): Result<ConfigData> => {
  try {
    const obj = JSON.parse(jsonString);
    return fromObject(obj);
  } catch (error) {
    return failure(ConfigurationError(
      "JSON_PARSE_ERROR",
      `Failed to parse JSON: ${error}`,
      new Map([["json", jsonString]])
    ));
  }
};

/**
 * Create configuration from environment variables with prefix
 * Converts UPPER_CASE to lower.case dot notation
 */
export const fromEnvironment = (prefix?: string): Result<ConfigData> => {
  try {
    const env = process.env;
    const configMap = new Map<string, unknown>();
    
    for (const [key, value] of Object.entries(env)) {
      if (prefix && !key.startsWith(prefix + "_")) {
        continue;
      }
      
      // Convert UPPER_CASE to lower.case
      const configKey = prefix 
        ? key.slice(prefix.length + 1).toLowerCase().replace(/_/g, ".")
        : key.toLowerCase().replace(/_/g, ".");
      
      // Try to parse as JSON, fallback to string
      try {
        configMap.set(configKey, JSON.parse(value ?? ""));
      } catch {
        configMap.set(configKey, value);
      }
    }
    
    return success({ data: configMap });
  } catch (error) {
    return failure(ConfigurationError(
      "ENV_PARSE_ERROR",
      `Failed to parse environment: ${error}`,
      new Map([["prefix", prefix]])
    ));
  }
};

/**
 * Create configuration from file (JSON)
 */
export const fromFile = async (filePath: string): Promise<Result<ConfigData>> => {
  try {
    const fs = await import("fs/promises");
    const content = await fs.readFile(filePath, "utf-8");
    return fromString(content);
  } catch (error) {
    return failure(ConfigurationError(
      "FILE_READ_ERROR",
      `Failed to read config file: ${error}`,
      new Map([["filePath", filePath]])
    ));
  }
};

/**
 * Get configuration value by key (supports dot notation)
 * Returns Result<T> for type-safe access
 */
export const get = <T = unknown>(config: ConfigData, key: string): Result<T> => {
  const value = config.data.get(key);
  
  if (value === undefined) {
    return failure(ConfigurationError(
      "KEY_NOT_FOUND",
      `Configuration key not found: ${key}`,
      new Map([["key", key], ["availableKeys", Array.from(config.data.keys())]])
    ));
  }
  
  return success(value as T);
};

/**
 * Check if configuration has a key
 */
export const has = (config: ConfigData, key: string): boolean =>
  config.data.has(key);

/**
 * Get all keys in configuration
 */
export const keys = (config: ConfigData): string[] =>
  Array.from(config.data.keys());

/**
 * Get all values in configuration
 */
export const values = (config: ConfigData): unknown[] =>
  Array.from(config.data.values());

/**
 * Convert configuration to plain object
 */
export const toObject = (config: ConfigData): Record<string, unknown> =>
  Object.fromEntries(config.data);

/**
 * Convert configuration to JSON string
 */
export const toString = (config: ConfigData): string =>
  JSON.stringify(toObject(config), null, 2);

/**
 * Validate configuration against schema
 */
export const validate = (config: ConfigData, schema: ConfigSchema): Result<ConfigData> => {
  const errors: string[] = [];
  const validatedData = new Map<string, unknown>();
  
  // Check required fields and types
  for (const [key, fieldSchema] of Object.entries(schema)) {
    const value = config.data.get(key);
    
    if (value === undefined) {
      if (fieldSchema.required) {
        errors.push(`Required field missing: ${key}`);
      } else if (fieldSchema.default !== undefined) {
        validatedData.set(key, fieldSchema.default);
      }
      continue;
    }
    
    // Type validation
    const actualType = typeof value;
    if (actualType !== fieldSchema.type) {
      errors.push(`Type mismatch for ${key}: expected ${fieldSchema.type}, got ${actualType}`);
      continue;
    }
    
    validatedData.set(key, value);
  }
  
  // Copy non-schema fields as-is
  for (const [key, value] of config.data) {
    if (!schema[key]) {
      validatedData.set(key, value);
    }
  }
  
  if (errors.length > 0) {
    return failure(ConfigurationError(
      "VALIDATION_ERROR",
      `Configuration validation failed: ${errors.join(", ")}`,
      new Map([
        ["errors", errors],
        ["schema", schema],
        ["config", toObject(config)]
      ] as [string, unknown][])
    ));
  }
  
  return success({ data: validatedData });
};

/**
 * Filter configuration by key predicate
 */
export const filter = (
  config: ConfigData,
  predicate: (key: string, value: unknown) => boolean
): ConfigData => ({
  data: new Map(
    Array.from(config.data.entries()).filter(([key, value]) => predicate(key, value))
  )
});

/**
 * Map configuration values
 */
export const mapValues = (
  config: ConfigData,
  mapper: (value: unknown, key: string) => unknown
): ConfigData => ({
  data: new Map(
    Array.from(config.data.entries()).map(([key, value]) => [key, mapper(value, key)])
  )
});

/**
 * Extract subset of configuration by key prefix
 */
export const subset = (config: ConfigData, prefix: string): ConfigData => ({
  data: new Map(
    Array.from(config.data.entries())
      .filter(([key]) => key.startsWith(prefix))
      .map(([key, value]) => [key.slice(prefix.length + 1), value])
  )
});

/**
 * Deep merge for nested object values (custom merge strategy)
 */
export const deepMerge = (a: ConfigData, b: ConfigData): ConfigData => {
  const merged = new Map(a.data);
  
  for (const [key, bValue] of b.data) {
    const aValue = merged.get(key);
    
    if (
      aValue && 
      typeof aValue === "object" && 
      typeof bValue === "object" &&
      !Array.isArray(aValue) && 
      !Array.isArray(bValue) &&
      aValue !== null &&
      bValue !== null
    ) {
      // Deep merge objects
      merged.set(key, { ...(aValue as object), ...(bValue as object) });
    } else {
      // Right-bias for non-objects
      merged.set(key, bValue);
    }
  }
  
  return { data: merged };
}; 