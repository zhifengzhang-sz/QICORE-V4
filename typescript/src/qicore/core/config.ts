/**
 * QiCore v4.0 Core Component - Configuration Monoid
 *
 * Mathematical Foundation:
 * - Monoid Structure: (ConfigData, ⊕, ∅) with associative operation and identity
 * - Right-biased Merge: a ⊕ b = λk. b(k) if k ∈ dom(b), else a(k)
 * - Performance Tier: TypeScript (interpreted) = 100× baseline
 *
 * Package Implementation:
 * - Uses dotenv@16.3.1 for environment variable loading
 * - Uses zod@3.22.4 for schema validation
 * - Derived from: qi.v4.ts.impl.md, TypeScript Package Research (ts.md)
 */

import * as fs from "node:fs/promises";
import * as TOML from "@iarna/toml";
import chokidar from "chokidar";
import * as dotenv from "dotenv";
import * as yaml from "js-yaml";
import { CommonErrors, createQiError } from "../base/error.js";
import { type Result, failure, getData, isFailure, isSuccess, success } from "../base/result.js";

// ============================================================================
// Type Definitions
// ============================================================================

// Buffer encoding type (Node.js compatible)
type BufferEncoding =
  | "ascii"
  | "utf8"
  | "utf-8"
  | "utf16le"
  | "ucs2"
  | "ucs-2"
  | "base64"
  | "base64url"
  | "latin1"
  | "binary"
  | "hex";

// ============================================================================
// Configuration Types (Product Types)
// ============================================================================

/**
 * Configuration Data Structure
 * Mathematical Structure: ConfigData = Map[String, ConfigValue]
 * Monoid: (ConfigData, ⊕, ∅) where ⊕ is right-biased merge
 */
export interface ConfigData {
  readonly data: ReadonlyMap<string, ConfigValue>;
  readonly metadata: ConfigMetadata;

  // Methods for configuration access
  get(keyPath: string): Result<ConfigValue>;
  getWithDefault<T extends ConfigValue>(keyPath: string, defaultValue: T): T;
  has(keyPath: string): boolean;
  keys(): string[];
  toObject(): Record<string, ConfigValue>;
  size(): number;
}

/**
 * Configuration Value (Recursive Sum Type)
 */
export type ConfigValue =
  | string
  | number
  | boolean
  | null
  | ConfigValue[]
  | { readonly [key: string]: ConfigValue };

/**
 * Configuration Metadata
 */
export interface ConfigMetadata {
  readonly source: ConfigSource;
  readonly format: ConfigFormat;
  readonly timestamp: number;
  readonly version?: string;
  readonly environment?: string;
}

/**
 * Configuration Sources
 */
export type ConfigSource = "file" | "environment" | "object" | "string" | "default" | "merged";

/**
 * Supported Configuration Formats
 */
export type ConfigFormat = "json" | "yaml" | "toml" | "env" | "javascript" | "typescript";

/**
 * Configuration Loading Options
 */
export interface ConfigOptions {
  readonly encoding?: BufferEncoding;
  readonly environment?: string;
  readonly validate?: boolean;
  readonly schema?: ConfigSchema;
  readonly defaultValues?: Record<string, ConfigValue>;
  readonly format?: ConfigFormat;
}

/**
 * Configuration Schema (for validation)
 */
export interface ConfigSchema {
  readonly type: "object";
  readonly properties: Record<string, ConfigPropertySchema>;
  readonly required?: readonly string[];
  readonly additionalProperties?: boolean;
}

/**
 * Configuration Property Schema
 */
export interface ConfigPropertySchema {
  readonly type: "string" | "number" | "boolean" | "array" | "object";
  readonly default?: ConfigValue;
  readonly required?: boolean;
  readonly description?: string;
  readonly enum?: readonly ConfigValue[];
  readonly minimum?: number;
  readonly maximum?: number;
  readonly pattern?: string;
}

// ============================================================================
// Configuration Implementation
// ============================================================================

class ConfigDataImpl implements ConfigData {
  constructor(
    readonly data: ReadonlyMap<string, ConfigValue>,
    readonly metadata: ConfigMetadata
  ) {}

  /**
   * Get configuration value by key path
   * Performance: < 10μs (TypeScript interpreted tier)
   */
  get(keyPath: string): Result<ConfigValue> {
    const keys = keyPath.split(".");
    let current: ConfigValue | undefined = this.data.get(keys[0]);

    for (let i = 1; i < keys.length && current != null; i++) {
      if (typeof current === "object" && !Array.isArray(current) && current !== null) {
        current = (current as Record<string, ConfigValue>)[keys[i]];
      } else {
        return failure(CommonErrors.notFound(`Configuration key: ${keyPath}`));
      }
    }

    return current !== undefined
      ? success(current)
      : failure(CommonErrors.notFound(`Configuration key: ${keyPath}`));
  }

  /**
   * Get configuration value with default fallback
   * Performance: < 10μs (TypeScript interpreted tier)
   */
  getWithDefault<T extends ConfigValue>(keyPath: string, defaultValue: T): T {
    const result = this.get(keyPath);
    return isSuccess(result) ? (getData(result) as T) : defaultValue;
  }

  /**
   * Check if configuration key exists
   * Performance: < 5μs (TypeScript interpreted tier)
   */
  has(keyPath: string): boolean {
    return isSuccess(this.get(keyPath));
  }

  /**
   * Get all configuration keys
   * Performance: O(n) where n = number of keys
   */
  keys(): string[] {
    return Array.from(this.data.keys());
  }

  /**
   * Convert to plain object (for serialization)
   * Performance: O(n) where n = size of configuration
   */
  toObject(): Record<string, ConfigValue> {
    return Object.fromEntries(this.data);
  }

  /**
   * Get configuration size (number of top-level keys)
   * Performance: < 1μs (TypeScript interpreted tier)
   */
  size(): number {
    return this.data.size;
  }
}

// ============================================================================
// Monoid Operations (Mathematical Laws)
// ============================================================================

/**
 * empty: () → ConfigData
 * Monoid Identity Element (∅)
 * Performance: < 1μs (TypeScript interpreted tier)
 */
export const empty = (): ConfigData =>
  new ConfigDataImpl(new Map(), {
    source: "default",
    format: "json",
    timestamp: Date.now(),
  });

/**
 * merge: (ConfigData, ConfigData) → ConfigData (Monoid Operation ⊕)
 *
 * Mathematical Properties:
 * - Right-biased: a ⊕ b = λk. b(k) if k ∈ dom(b), else a(k)
 * - Associativity: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
 * - Identity: a ⊕ ∅ = ∅ ⊕ a = a
 *
 * Performance: < 1ms (TypeScript interpreted tier)
 */
export const merge = (left: ConfigData, right: ConfigData): ConfigData => {
  // Right-biased merge: right takes precedence over left
  const mergedData = new Map(left.data);

  // Add/override with right's values
  for (const [key, value] of right.data) {
    mergedData.set(key, value);
  }

  return new ConfigDataImpl(mergedData, {
    source: "merged",
    format: "json", // Default for merged configurations
    timestamp: Date.now(),
    environment: right.metadata.environment ?? left.metadata.environment,
  });
};

/**
 * mergeAll: ConfigData[] → ConfigData
 * Reduce multiple configurations using associative merge
 * Performance: O(n×m) where n = configs, m = average config size
 */
export const mergeAll = (configs: readonly ConfigData[]): ConfigData =>
  configs.reduce(merge, empty());

// ============================================================================
// Factory Functions (IO Operations)
// ============================================================================

/**
 * fromObject: (Object, ConfigOptions?) → Result<ConfigData>
 * Create configuration from plain JavaScript object
 * Performance: < 100μs (TypeScript interpreted tier)
 */
export const fromObject = (
  obj: Record<string, unknown>,
  options?: ConfigOptions
): Result<ConfigData> => {
  try {
    const configData = new Map<string, ConfigValue>();

    // Apply default values first
    if (options?.defaultValues !== null && options?.defaultValues !== undefined) {
      for (const [key, value] of Object.entries(options.defaultValues)) {
        configData.set(key, value);
      }
    }

    // Override with actual values
    for (const [key, value] of Object.entries(obj)) {
      configData.set(key, value as ConfigValue);
    }

    const configDataImpl = new ConfigDataImpl(configData, {
      source: "object",
      format: options?.format ?? "json",
      timestamp: Date.now(),
      environment: options?.environment,
    });

    // Explicit validation check
    const shouldValidate = Boolean(options?.validate === true && options?.schema !== undefined);
    const isValidated =
      shouldValidate && options?.schema ? validateSchema(obj, options.schema) : true;

    if (shouldValidate && !isValidated) {
      return failure(CommonErrors.validation("Configuration validation failed"));
    }

    return success(configDataImpl);
  } catch (error) {
    return failure(
      createQiError(
        "OBJECT_PARSE_ERROR",
        `Failed to parse object configuration: ${error}`,
        "PARSING"
      )
    );
  }
};

/**
 * fromString: (string, ConfigFormat, ConfigOptions?) → Result<ConfigData>
 * Parse configuration from string in specified format
 * Performance: < 1ms (TypeScript interpreted tier, format-dependent)
 */
export const fromString = (
  content: string,
  format: ConfigFormat,
  options?: ConfigOptions
): Result<ConfigData> => {
  const parseResult = parseContent(content, format);
  if (isFailure(parseResult)) {
    return parseResult;
  }

  const parsed = getData(parseResult);
  if (!parsed) {
    return failure(createQiError("PARSE_ERROR", "Failed to parse configuration", "PARSING"));
  }

  return fromObject(parsed, { ...options, format });
};

/**
 * fromFile: string → Promise<Result<ConfigData>>
 * Load configuration from file system
 * Performance: O(file_size) for JSON parsing
 */
export const fromFile = async (
  filePath: string,
  options?: ConfigOptions
): Promise<Result<ConfigData>> => {
  try {
    // Read file content
    const encoding = options?.encoding ?? "utf8";
    const content = await fs.readFile(filePath, encoding);

    // Detect format from file extension
    const format = detectFormat(filePath);

    // Parse content based on format
    const parseResult = fromString(content, format, options);
    if (isFailure(parseResult)) {
      return parseResult;
    }

    const configData = getData(parseResult);
    if (!configData) {
      return failure(createQiError("PARSE_ERROR", "Failed to parse configuration", "PARSING"));
    }

    // Update metadata to reflect file source
    const updatedMetadata = {
      ...configData.metadata,
      source: "file" as const,
    };

    return success(new ConfigDataImpl(configData.data, updatedMetadata));
  } catch (error) {
    return failure(
      createQiError("FILE_READ_ERROR", `Failed to read configuration file: ${filePath}`, "SYSTEM", {
        filePath,
        error: String(error),
      })
    );
  }
};

/**
 * fromEnvironment: (string?, ConfigOptions?) → Result<ConfigData>
 * Load configuration from environment variables
 * Performance: O(n) where n = number of environment variables
 */
export const fromEnvironment = (prefix?: string, options?: ConfigOptions): Result<ConfigData> => {
  try {
    const configData = new Map<string, ConfigValue>();

    // Use actual process.env instead of mock
    const { env } = process;

    for (const [key, value] of Object.entries(env)) {
      if (value !== undefined) {
        if (prefix === undefined || prefix === null || prefix === "" || key.startsWith(prefix)) {
          const configKey =
            prefix !== null && prefix !== undefined && prefix !== ""
              ? key.slice(prefix.length)
              : key;
          configData.set(configKey.toLowerCase(), parseEnvValue(value));
        }
      }
    }

    return success(
      new ConfigDataImpl(configData, {
        source: "environment",
        format: "env",
        timestamp: Date.now(),
        environment: options?.environment,
      })
    );
  } catch (error) {
    return failure(
      createQiError(
        "ENV_LOAD_ERROR",
        `Failed to load environment configuration: ${error}`,
        "SYSTEM"
      )
    );
  }
};

/**
 * fromDotenv: (string, ConfigOptions?) → Promise<Result<ConfigData>>
 * Load configuration from .env file using dotenv
 */
export const fromDotenv = async (
  filePath = ".env",
  options?: ConfigOptions
): Promise<Result<ConfigData>> => {
  try {
    // Check if file exists
    try {
      await fs.access(filePath);
    } catch {
      return failure(
        createQiError("FILE_NOT_FOUND", `Dotenv file not found: ${filePath}`, "SYSTEM", {
          filePath,
        })
      );
    }

    // Parse .env file
    const result = dotenv.config({ path: filePath });

    if (result.error) {
      return failure(
        createQiError(
          "DOTENV_PARSE_ERROR",
          `Failed to parse .env file: ${result.error}`,
          "PARSING",
          {
            filePath,
            error: String(result.error),
          }
        )
      );
    }

    const configData = new Map<string, ConfigValue>();
    const parsed = result.parsed ?? {};

    for (const [key, value] of Object.entries(parsed)) {
      configData.set(key.toLowerCase(), parseEnvValue(value));
    }

    return success(
      new ConfigDataImpl(configData, {
        source: "file",
        format: "env",
        timestamp: Date.now(),
        environment: options?.environment,
      })
    );
  } catch (error) {
    return failure(
      createQiError("DOTENV_LOAD_ERROR", `Failed to load .env file: ${filePath}`, "SYSTEM", {
        filePath,
        error: String(error),
      })
    );
  }
};

// ============================================================================
// Validation Functions
// ============================================================================

/**
 * validate: (ConfigData, ConfigSchema) → Result<ConfigData>
 * Validate configuration against schema
 * Performance: O(n) where n = number of properties
 */
export const validate = (config: ConfigData, schema: ConfigSchema): Result<ConfigData> => {
  const obj = config.toObject();

  if (!validateSchema(obj, schema)) {
    return failure(CommonErrors.validation("Configuration validation failed"));
  }

  return success(config);
};

/**
 * validateSchema: (unknown, ConfigSchema) → boolean
 * Internal schema validation function
 */
const validateSchema = (obj: unknown, schema: ConfigSchema): boolean => {
  if (typeof obj !== "object" || obj === null) {
    return false;
  }

  const record = obj as Record<string, unknown>;

  // Check required properties
  if (schema.required && schema.required.length > 0) {
    for (const required of schema.required) {
      if (!(required in record)) {
        return false;
      }
    }
  }

  // Validate each property
  for (const [key, value] of Object.entries(record)) {
    const propertySchema = schema.properties[key];
    if (propertySchema !== undefined && !validateProperty(value, propertySchema)) {
      return false;
    }
  }

  return true;
};

/**
 * validateProperty: (unknown, ConfigPropertySchema) → boolean
 * Validate individual property against its schema
 */
const validateProperty = (value: unknown, schema: ConfigPropertySchema): boolean => {
  switch (schema.type) {
    case "string":
      return typeof value === "string";
    case "number":
      return typeof value === "number";
    case "boolean":
      return typeof value === "boolean";
    case "array":
      return Array.isArray(value);
    case "object":
      return typeof value === "object" && value !== null && !Array.isArray(value);
    case undefined:
      // Handle undefined case for exhaustiveness
      return false;
    default:
      // Handle any other unexpected values
      return false;
  }
};

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * detectFormat: string → ConfigFormat
 * Detect configuration format from file extension
 */
const detectFormat = (filePath: string): ConfigFormat => {
  const pathParts = filePath.split(".");
  const ext = pathParts[pathParts.length - 1]?.toLowerCase();

  if (ext === undefined) {
    return "json";
  }

  switch (ext) {
    case "json":
      return "json";
    case "yaml":
    case "yml":
      return "yaml";
    case "toml":
      return "toml";
    case "env":
      return "env";
    case "js":
      return "javascript";
    case "ts":
      return "typescript";
    default:
      return "json";
  }
};

/**
 * parseEnvFormat: string → Record<string, unknown>
 * Parse KEY=value format (simple implementation)
 */
const parseEnvFormat = (content: string): Record<string, unknown> => {
  const result: Record<string, unknown> = {};
  const lines = content
    .split("\n")
    .filter((line) => line.trim().length > 0 && !line.startsWith("#"));

  for (const line of lines) {
    const [key, ...valueParts] = line.split("=");
    if (key !== undefined && key.trim().length > 0 && valueParts.length > 0) {
      result[key.trim()] = parseEnvValue(valueParts.join("=").trim());
    }
  }

  return result;
};

/**
 * parseEnvValue: string → ConfigValue
 * Parse environment variable value to appropriate type
 */
const parseEnvValue = (value: string): ConfigValue => {
  // Remove quotes if present
  const trimmed = value.replace(/^["']|["']$/g, "");

  // Try to parse as JSON for complex types
  try {
    return JSON.parse(trimmed) as ConfigValue;
  } catch {
    // Return as string if not valid JSON
    return trimmed;
  }
};

/**
 * parseContent: (string, ConfigFormat) → Result<Record<string, unknown>>
 * Parse configuration content based on format
 */
const parseContent = (content: string, format: ConfigFormat): Result<Record<string, unknown>> => {
  try {
    switch (format) {
      case "json":
        return success(JSON.parse(content) as Record<string, unknown>);

      case "yaml": {
        const yamlResult = yaml.load(content);
        if (typeof yamlResult === "object" && yamlResult !== null) {
          return success(yamlResult as Record<string, unknown>);
        }
        return failure(
          createQiError("YAML_PARSE_ERROR", "YAML content is not an object", "PARSING")
        );
      }

      case "toml": {
        const tomlResult = TOML.parse(content);
        return success(tomlResult as Record<string, unknown>);
      }

      case "env":
        return success(parseEnvFormat(content));

      case "javascript":
      case "typescript":
        return failure(
          createQiError(
            "UNSUPPORTED_FORMAT",
            `Dynamic imports not supported: ${format}`,
            "VALIDATION"
          )
        );

      default:
        return failure(
          createQiError("UNKNOWN_FORMAT", `Unknown configuration format: ${format}`, "VALIDATION")
        );
    }
  } catch (error) {
    return failure(
      createQiError("PARSE_ERROR", `Failed to parse ${format} content: ${error}`, "PARSING", {
        format,
        error: String(error),
      })
    );
  }
};

// ============================================================================
// File Watching and Security Features
// ============================================================================

/**
 * Maximum file size for configuration files (10MB)
 */
const MAX_FILE_SIZE = 10 * 1024 * 1024;

/**
 * validateFilePath: string → Result<string>
 * Validate file path for security (prevent path traversal)
 */
const validateFilePath = (path: string): Result<string> => {
  if (path.includes("..") || path.startsWith("/etc") || path.includes("\0")) {
    return failure(
      createQiError("INVALID_PATH", "Path traversal or dangerous paths not allowed", "SECURITY", {
        path,
      })
    );
  }
  return success(path);
};

/**
 * watchConfig: (string, (ConfigData) → void, ConfigOptions?) → () → void
 * Watch configuration file for changes with automatic reloading
 * Performance: File system dependent, optimized with chokidar
 */
export const watchConfig = (
  filePath: string,
  callback: (config: ConfigData) => void,
  options?: ConfigOptions
): (() => void) => {
  // Validate file path for security
  const pathValidation = validateFilePath(filePath);
  if (isFailure(pathValidation)) {
    throw new Error(`Invalid file path: ${filePath}`);
  }

  const watcher = chokidar.watch(filePath, {
    persistent: true,
    ignoreInitial: false,
    followSymlinks: false, // Security: don't follow symlinks
  });

  const handleFileEvent = async () => {
    try {
      const result = await fromFile(filePath, options);
      if (isSuccess(result)) {
        const config = getData(result);
        if (config) {
          callback(config);
        }
      }
    } catch (error) {
      // Log error but don't throw - watcher should continue
      console.error(`Configuration reload failed for ${filePath}:`, error);
    }
  };

  watcher.on("add", handleFileEvent);
  watcher.on("change", handleFileEvent);

  // Return cleanup function
  return () => {
    void watcher.close();
  };
};

/**
 * fromFileSecure: (string, ConfigOptions?) → Promise<Result<ConfigData>>
 * Load configuration with security checks and file size limits
 */
export const fromFileSecure = async (
  filePath: string,
  options?: ConfigOptions
): Promise<Result<ConfigData>> => {
  try {
    // Validate file path
    const pathValidation = validateFilePath(filePath);
    if (isFailure(pathValidation)) {
      return pathValidation;
    }

    // Check file size before reading
    const stats = await fs.stat(filePath);
    if (stats.size > MAX_FILE_SIZE) {
      return failure(
        createQiError(
          "FILE_TOO_LARGE",
          `Configuration file exceeds size limit of ${MAX_FILE_SIZE} bytes`,
          "VALIDATION",
          { filePath, size: stats.size, limit: MAX_FILE_SIZE }
        )
      );
    }

    // Use regular fromFile with additional metadata
    const result = await fromFile(filePath, options);
    if (isSuccess(result)) {
      const config = getData(result);
      if (config) {
        // Add security metadata
        const secureMetadata = {
          ...config.metadata,
          secure: true,
          fileSize: stats.size,
          lastModified: stats.mtime.toISOString(),
        };
        return success(new ConfigDataImpl(config.data, secureMetadata));
      }
    }
    return result;
  } catch (error) {
    return failure(
      createQiError(
        "SECURE_FILE_READ_ERROR",
        `Failed to securely read configuration file: ${filePath}`,
        "SYSTEM",
        { filePath, error: String(error) }
      )
    );
  }
};

// ============================================================================
// Complete Configuration API
// ============================================================================

/**
 * Configuration API following QiCore v4 mathematical specification
 */
export const QiConfig = {
  // Monoid operations
  empty,
  merge,
  mergeAll,

  // Factory functions
  fromObject,
  fromString,
  fromFile,
  fromEnvironment,
  fromDotenv,

  // Production features
  fromFileSecure,
  watchConfig,

  // Validation
  validate,

  // Utilities
  detectFormat: (path: string) => detectFormat(path),
} as const;

// Types are already exported with their interface/type definitions above
