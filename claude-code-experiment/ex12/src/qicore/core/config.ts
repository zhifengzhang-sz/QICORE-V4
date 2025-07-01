/**
 * QiCore v4.0 - Core Configuration Component
 * 
 * Mathematical insight: Configuration merge is monoid operation
 * Custom monoid implementation ensuring mathematical laws with right-biased merge
 */

import { promises as fs } from "fs";
import { createQiError, type QiError, ErrorCategory } from "../base/error.js";
import { success, failure, type Result, fromAsyncTryCatch } from "../base/result.js";

/**
 * Configuration data structure using Map for optimal V8 performance
 */
export interface ConfigData {
  readonly data: Map<string, unknown>;
}

/**
 * Identity element for monoid
 * Satisfies: merge(empty(), a) = merge(a, empty()) = a
 */
export const empty = (): ConfigData => ({
  data: new Map()
});

/**
 * Associative merge operation (right-biased)
 * Satisfies: merge(merge(a, b), c) = merge(a, merge(b, c))
 * Right-biased: later values override earlier ones
 */
export const merge = (a: ConfigData, b: ConfigData): ConfigData => ({
  data: new Map([...a.data, ...b.data])
});

/**
 * Deep merge for nested objects
 * Recursively merges object values
 */
export const deepMerge = (a: ConfigData, b: ConfigData): ConfigData => {
  const result = new Map(a.data);
  
  for (const [key, bValue] of b.data) {
    const aValue = result.get(key);
    
    if (aValue && typeof aValue === 'object' && !Array.isArray(aValue) &&
        bValue && typeof bValue === 'object' && !Array.isArray(bValue)) {
      // Recursively merge objects
      const aConfig: ConfigData = { data: new Map(Object.entries(aValue as Record<string, unknown>)) };
      const bConfig: ConfigData = { data: new Map(Object.entries(bValue as Record<string, unknown>)) };
      const merged = deepMerge(aConfig, bConfig);
      result.set(key, Object.fromEntries(merged.data));
    } else {
      // Right-biased: b overrides a
      result.set(key, bValue);
    }
  }
  
  return { data: result };
};

/**
 * Creates ConfigData from plain object
 */
export const fromObject = (obj: Record<string, unknown>): Result<ConfigData> => {
  try {
    const flattenedEntries = flattenObject(obj);
    return success({
      data: new Map(flattenedEntries)
    });
  } catch (error) {
    return failure(createQiError(
      "CONFIG_PARSE_ERROR",
      `Failed to parse configuration object: ${error}`,
      ErrorCategory.CONFIGURATION,
      { object: obj }
    ));
  }
};

/**
 * Creates ConfigData from environment variables with prefix
 */
export const fromEnvironment = (prefix: string): Result<ConfigData> => {
  try {
    const envVars = Object.entries(process.env)
      .filter(([key]) => key.startsWith(prefix))
      .map(([key, value]) => {
        const cleanKey = key.slice(prefix.length + 1).toLowerCase().replace(/_/g, '.');
        return [cleanKey, parseEnvValue(value)];
      })
      .filter(([, value]) => value !== undefined);
    
    return success({
      data: new Map(envVars)
    });
  } catch (error) {
    return failure(createQiError(
      "ENV_LOAD_ERROR",
      `Failed to load environment variables with prefix "${prefix}": ${error}`,
      ErrorCategory.CONFIGURATION,
      { prefix }
    ));
  }
};

/**
 * Loads configuration from JSON file
 */
export const fromFile = async (path: string): Promise<Result<ConfigData>> => {
  return fromAsyncTryCatch(async () => {
    const content = await fs.readFile(path, 'utf-8');
    const parsed = JSON.parse(content);
    
    if (typeof parsed !== 'object' || parsed === null) {
      throw new Error('Configuration file must contain a JSON object');
    }
    
    const result = fromObject(parsed);
    if (result._tag === "Left") {
      throw new Error(`Invalid configuration structure: ${result.left.message}`);
    }
    
    return result.right;
  }).then(result => 
    result._tag === "Left" 
      ? failure(createQiError(
          "CONFIG_FILE_ERROR",
          `Failed to load configuration from file: ${result.left.message}`,
          ErrorCategory.FILESYSTEM,
          { path }
        ))
      : result
  );
};

/**
 * Gets configuration value by key with type safety
 */
export const get = <T = unknown>(config: ConfigData, key: string): Result<T> => {
  if (!config.data.has(key)) {
    return failure(createQiError(
      "CONFIG_KEY_NOT_FOUND",
      `Configuration key "${key}" not found`,
      ErrorCategory.CONFIGURATION,
      { key, availableKeys: Array.from(config.data.keys()) }
    ));
  }
  
  return success(config.data.get(key) as T);
};

/**
 * Gets configuration value with default
 */
export const getOr = <T>(defaultValue: T) => (config: ConfigData, key: string): T => {
  const result = get<T>(config, key);
  return result._tag === "Right" ? result.right : defaultValue;
};

/**
 * Checks if configuration has key
 */
export const has = (config: ConfigData, key: string): boolean =>
  config.data.has(key);

/**
 * Gets all configuration keys
 */
export const keys = (config: ConfigData): string[] =>
  Array.from(config.data.keys());

/**
 * Gets all configuration values
 */
export const values = (config: ConfigData): unknown[] =>
  Array.from(config.data.values());

/**
 * Gets all configuration entries
 */
export const entries = (config: ConfigData): [string, unknown][] =>
  Array.from(config.data.entries());

/**
 * Converts ConfigData to plain object
 */
export const toObject = (config: ConfigData): Record<string, unknown> =>
  Object.fromEntries(config.data);

/**
 * Maps over configuration values
 */
export const mapValues = <T, U>(
  f: (value: T, key: string) => U
) => (config: ConfigData): ConfigData => ({
  data: new Map(
    Array.from(config.data.entries()).map(([key, value]) => [
      key,
      f(value as T, key)
    ])
  )
});

/**
 * Filters configuration by predicate
 */
export const filter = (
  predicate: (value: unknown, key: string) => boolean
) => (config: ConfigData): ConfigData => ({
  data: new Map(
    Array.from(config.data.entries()).filter(([key, value]) =>
      predicate(value, key)
    )
  )
});

/**
 * Validates configuration against schema
 */
export interface ConfigSchema<T> {
  readonly validate: (data: unknown) => Result<T>;
}

export const validate = <T>(
  config: ConfigData,
  schema: ConfigSchema<T>
): Result<T> => {
  const obj = toObject(config);
  return schema.validate(obj);
};

/**
 * Creates configuration chain with precedence
 * Later configs override earlier ones (right-biased merge)
 */
export const chain = (...configs: ConfigData[]): ConfigData =>
  configs.reduce(merge, empty());

/**
 * Creates deep configuration chain with precedence
 */
export const deepChain = (...configs: ConfigData[]): ConfigData =>
  configs.reduce(deepMerge, empty());

/**
 * Configuration size (number of keys)
 */
export const size = (config: ConfigData): number =>
  config.data.size;

/**
 * Checks if configuration is empty
 */
export const isEmpty = (config: ConfigData): boolean =>
  config.data.size === 0;

/**
 * Configuration equality check
 */
export const equals = (a: ConfigData, b: ConfigData): boolean => {
  if (a.data.size !== b.data.size) return false;
  
  for (const [key, value] of a.data) {
    if (!b.data.has(key) || b.data.get(key) !== value) {
      return false;
    }
  }
  
  return true;
};

// Helper Functions

/**
 * Flattens nested object to dot notation keys
 */
function flattenObject(
  obj: Record<string, unknown>,
  prefix = '',
  result: [string, unknown][] = []
): [string, unknown][] {
  for (const [key, value] of Object.entries(obj)) {
    const newKey = prefix ? `${prefix}.${key}` : key;
    
    if (value && typeof value === 'object' && !Array.isArray(value)) {
      flattenObject(value as Record<string, unknown>, newKey, result);
    } else {
      result.push([newKey, value]);
    }
  }
  
  return result;
}

/**
 * Parses environment variable value with type coercion
 */
function parseEnvValue(value: string | undefined): unknown {
  if (value === undefined) return undefined;
  
  // Boolean parsing
  if (value.toLowerCase() === 'true') return true;
  if (value.toLowerCase() === 'false') return false;
  
  // Number parsing
  if (/^-?\d+$/.test(value)) return parseInt(value, 10);
  if (/^-?\d*\.\d+$/.test(value)) return parseFloat(value);
  
  // Array parsing (comma-separated)
  if (value.includes(',')) {
    return value.split(',').map(v => parseEnvValue(v.trim()));
  }
  
  // Default to string
  return value;
}

/**
 * Creates a simple configuration schema
 */
export const createSchema = <T>(
  validator: (data: unknown) => T | never
): ConfigSchema<T> => ({
  validate: (data: unknown): Result<T> => {
    try {
      const result = validator(data);
      return success(result);
    } catch (error) {
      return failure(createQiError(
        "CONFIG_VALIDATION_ERROR",
        `Configuration validation failed: ${error}`,
        ErrorCategory.VALIDATION,
        { data }
      ));
    }
  }
});

/**
 * Loads configuration from multiple sources with precedence
 * Order: defaults → file → environment → overrides
 */
export const loadConfiguration = async (options: {
  defaults?: Record<string, unknown>;
  configFile?: string;
  envPrefix?: string;
  overrides?: Record<string, unknown>;
}): Promise<Result<ConfigData>> => {
  try {
    let config = empty();
    
    // Apply defaults
    if (options.defaults) {
      const defaultsResult = fromObject(options.defaults);
      if (defaultsResult._tag === "Left") return defaultsResult;
      config = merge(config, defaultsResult.right);
    }
    
    // Apply file configuration
    if (options.configFile) {
      const fileResult = await fromFile(options.configFile);
      if (fileResult._tag === "Right") {
        config = merge(config, fileResult.right);
      }
      // File loading errors are non-fatal - continue with other sources
    }
    
    // Apply environment variables
    if (options.envPrefix) {
      const envResult = fromEnvironment(options.envPrefix);
      if (envResult._tag === "Right") {
        config = merge(config, envResult.right);
      }
    }
    
    // Apply overrides
    if (options.overrides) {
      const overrideResult = fromObject(options.overrides);
      if (overrideResult._tag === "Left") return overrideResult;
      config = merge(config, overrideResult.right);
    }
    
    return success(config);
  } catch (error) {
    return failure(createQiError(
      "CONFIG_LOAD_ERROR",
      `Failed to load configuration: ${error}`,
      ErrorCategory.CONFIGURATION,
      { options }
    ));
  }
};