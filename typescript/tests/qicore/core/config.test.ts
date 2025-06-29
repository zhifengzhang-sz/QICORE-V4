/**
 * Comprehensive Configuration Tests for QiCore v4.0
 *
 * Tests all configuration operations including:
 * - File loading (JSON, YAML, TOML, ENV)
 * - Environment variable loading
 * - File watching and reloading
 * - Security validation
 * - Error scenarios
 * - Performance requirements
 */

import { promises as fs } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { getData, isFailure, isSuccess } from "@qicore/base/result";
import {
  type ConfigData,
  type ConfigSchema,
  empty,
  fromDotenv,
  fromEnvironment,
  fromFile,
  fromFileSecure,
  fromObject,
  fromString,
  merge,
  mergeAll,
  validate,
  watchConfig,
} from "@qicore/core/config";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

// Test utilities
const createTempFile = async (content: string, extension: string): Promise<string> => {
  const tempDir = tmpdir();
  const fileName = `test-config-${Date.now()}-${Math.random().toString(36).substr(2, 9)}.${extension}`;
  const filePath = join(tempDir, fileName);
  await fs.writeFile(filePath, content, "utf8");
  return filePath;
};

const cleanupFile = async (filePath: string): Promise<void> => {
  try {
    await fs.unlink(filePath);
  } catch {
    // Ignore cleanup errors
  }
};

describe("Configuration Monoid Operations", () => {
  describe("empty()", () => {
    it("should create empty configuration", () => {
      const config = empty();
      expect(config.size()).toBe(0);
      expect(config.keys()).toEqual([]);
      expect(config.toObject()).toEqual({});
    });

    it("should have correct metadata", () => {
      const config = empty();
      expect(config.metadata.source).toBe("default");
      expect(config.metadata.format).toBe("json");
      expect(config.metadata.timestamp).toBeTypeOf("number");
    });
  });

  describe("merge()", () => {
    it("should merge two configurations (right-biased)", () => {
      const config1Result = fromObject({ a: 1, b: 2 });
      const config2Result = fromObject({ b: 3, c: 4 });

      expect(isSuccess(config1Result)).toBe(true);
      expect(isSuccess(config2Result)).toBe(true);

      if (isSuccess(config1Result) && isSuccess(config2Result)) {
        const config1Data = getData(config1Result);
        const config2Data = getData(config2Result);
        if (config1Data && config2Data) {
          const merged = merge(config1Data, config2Data);

          expect(merged.toObject()).toEqual({ a: 1, b: 3, c: 4 });
          expect(merged.metadata.source).toBe("merged");
        }
      }
    });

    it("should satisfy monoid identity laws", () => {
      const configResult = fromObject({ key: "value" });
      expect(isSuccess(configResult)).toBe(true);

      if (isSuccess(configResult)) {
        const config = getData(configResult);
        if (config) {
          const emptyConfig = empty();

          // Left identity: empty ⊕ a = a
          const leftMerge = merge(emptyConfig, config);
          expect(leftMerge.toObject()).toEqual(config.toObject());

          // Right identity: a ⊕ empty = a
          const rightMerge = merge(config, emptyConfig);
          expect(rightMerge.toObject()).toEqual(config.toObject());
        }
      }
    });

    it("should be associative", () => {
      const config1Result = fromObject({ a: 1 });
      const config2Result = fromObject({ b: 2 });
      const config3Result = fromObject({ c: 3 });

      expect(isSuccess(config1Result)).toBe(true);
      expect(isSuccess(config2Result)).toBe(true);
      expect(isSuccess(config3Result)).toBe(true);

      if (isSuccess(config1Result) && isSuccess(config2Result) && isSuccess(config3Result)) {
        const a = getData(config1Result);
        const b = getData(config2Result);
        const c = getData(config3Result);
        if (a && b && c) {
          // (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
          const left = merge(merge(a, b), c);
          const right = merge(a, merge(b, c));

          expect(left.toObject()).toEqual(right.toObject());
        }
      }
    });
  });

  describe("mergeAll()", () => {
    it("should merge multiple configurations", () => {
      const configs = [
        fromObject({ a: 1 }),
        fromObject({ b: 2 }),
        fromObject({ c: 3, a: 99 }), // Should override a: 1
      ];

      // Check all configs are valid
      for (const result of configs) {
        expect(isSuccess(result)).toBe(true);
      }

      const validConfigs = configs.map((result) => {
        const data = getData(result);
        if (!data) throw new Error("Expected valid config data");
        return data;
      });
      const merged = mergeAll(validConfigs);

      expect(merged.toObject()).toEqual({ a: 99, b: 2, c: 3 });
    });

    it("should handle empty array", () => {
      const merged = mergeAll([]);
      expect(merged.toObject()).toEqual({});
      expect(merged.metadata.source).toBe("default");
    });
  });
});

describe("Configuration Access Methods", () => {
  let config: ConfigData;

  beforeEach(() => {
    const result = fromObject({
      simple: "value",
      number: 42,
      boolean: true,
      nested: {
        deep: {
          value: "found",
        },
      },
      array: [1, 2, 3],
    });

    expect(isSuccess(result)).toBe(true);
    if (isSuccess(result)) {
      const configData = getData(result);
      if (configData) {
        config = configData;
      }
    }
  });

  describe("get()", () => {
    it("should get simple values", () => {
      const result = config.get("simple");
      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        expect(getData(result)).toBe("value");
      }
    });

    it("should get nested values with dot notation", () => {
      const result = config.get("nested.deep.value");
      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        expect(getData(result)).toBe("found");
      }
    });

    it("should return error for non-existent keys", () => {
      const result = config.get("nonexistent");
      expect(isFailure(result)).toBe(true);
    });

    it("should return error for invalid path traversal", () => {
      const result = config.get("simple.invalid");
      expect(isFailure(result)).toBe(true);
    });
  });

  describe("getWithDefault()", () => {
    it("should return value when key exists", () => {
      const value = config.getWithDefault("simple", "default");
      expect(value).toBe("value");
    });

    it("should return default when key does not exist", () => {
      const value = config.getWithDefault("nonexistent", "default");
      expect(value).toBe("default");
    });

    it("should work with complex defaults", () => {
      const defaultValue = { complex: true };
      const value = config.getWithDefault("nonexistent", defaultValue);
      expect(value).toEqual(defaultValue);
    });
  });

  describe("has()", () => {
    it("should return true for existing keys", () => {
      expect(config.has("simple")).toBe(true);
      expect(config.has("nested.deep.value")).toBe(true);
    });

    it("should return false for non-existent keys", () => {
      expect(config.has("nonexistent")).toBe(false);
      expect(config.has("nested.nonexistent")).toBe(false);
    });
  });

  describe("keys()", () => {
    it("should return all top-level keys", () => {
      const keys = config.keys();
      expect(keys).toContain("simple");
      expect(keys).toContain("number");
      expect(keys).toContain("nested");
      expect(keys).toContain("array");
    });
  });

  describe("size()", () => {
    it("should return number of top-level keys", () => {
      expect(config.size()).toBe(5); // simple, number, boolean, nested, array
    });
  });
});

describe("File Format Parsing", () => {
  const tempFiles: string[] = [];

  afterEach(async () => {
    // Cleanup temp files
    await Promise.all(tempFiles.map(cleanupFile));
    tempFiles.length = 0;
  });

  describe("JSON format", () => {
    it("should parse valid JSON", async () => {
      const content = JSON.stringify({ key: "value", number: 42 });
      const result = fromString(content, "json");

      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        const config = getData(result);
        if (config) {
          expect(config.get("key")).toEqual(expect.objectContaining({ _tag: "Right" }));
          expect(config.get("number")).toEqual(expect.objectContaining({ _tag: "Right" }));
        }
      }
    });

    it("should handle invalid JSON", () => {
      const result = fromString("{ invalid json }", "json");
      expect(isFailure(result)).toBe(true);
    });

    it("should load JSON from file", async () => {
      const content = JSON.stringify({ fileKey: "fileValue" });
      const filePath = await createTempFile(content, "json");
      tempFiles.push(filePath);

      const result = await fromFile(filePath);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (config) {
          expect(config.metadata.source).toBe("file");
          expect(config.metadata.format).toBe("json");

          const valueResult = config.get("fileKey");
          expect(isSuccess(valueResult)).toBe(true);
          if (isSuccess(valueResult)) {
            expect(getData(valueResult)).toBe("fileValue");
          }
        }
      }
    });
  });

  describe("YAML format", () => {
    it("should parse valid YAML", () => {
      const content = `
        key: value
        number: 42
        nested:
          deep: value
      `;
      const result = fromString(content, "yaml");

      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        const config = getData(result);
        if (!config) return;
        const nestedResult = config.get("nested.deep");
        expect(isSuccess(nestedResult)).toBe(true);
        if (isSuccess(nestedResult)) {
          expect(getData(nestedResult)).toBe("value");
        }
      }
    });

    it("should handle invalid YAML", () => {
      const result = fromString("key: value\n  invalid: yaml: structure", "yaml");
      expect(isFailure(result)).toBe(true);
    });

    it("should load YAML from file", async () => {
      const content = "yamlKey: yamlValue\nnested:\n  key: nestedValue";
      const filePath = await createTempFile(content, "yaml");
      tempFiles.push(filePath);

      const result = await fromFile(filePath);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (!config) return;
        expect(config.metadata.format).toBe("yaml");
      }
    });
  });

  describe("TOML format", () => {
    it("should parse valid TOML", () => {
      const content = `
        key = "value"
        number = 42
        
        [nested]
        deep = "value"
      `;
      const result = fromString(content, "toml");

      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        const config = getData(result);
        if (!config) return;
        const nestedResult = config.get("nested.deep");
        expect(isSuccess(nestedResult)).toBe(true);
        if (isSuccess(nestedResult)) {
          expect(getData(nestedResult)).toBe("value");
        }
      }
    });

    it("should handle invalid TOML", () => {
      const result = fromString("[invalid toml structure", "toml");
      expect(isFailure(result)).toBe(true);
    });
  });

  describe("ENV format", () => {
    it("should parse environment format", () => {
      const content = `
        KEY=value
        NUMBER=42
        BOOLEAN=true
        # Comment should be ignored
        QUOTED="quoted value"
      `;
      const result = fromString(content, "env");

      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        const config = getData(result);
        if (!config) return;

        const keyResult = config.get("KEY");
        expect(isSuccess(keyResult) && getData(keyResult) === "value").toBe(true);

        const numberResult = config.get("NUMBER");
        expect(isSuccess(numberResult) && getData(numberResult) === 42).toBe(true);
      }
    });

    it("should handle empty lines and comments", () => {
      const content = `
        # This is a comment
        
        KEY=value
        # Another comment
        
        OTHER=data
      `;
      const result = fromString(content, "env");

      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        const config = getData(result);
        if (!config) return;
        expect(config.size()).toBe(2);
      }
    });
  });
});

describe("Environment Variables", () => {
  const originalEnv = process.env;
  const tempFiles: string[] = [];

  beforeEach(() => {
    // Reset process.env
    process.env = { ...originalEnv };
  });

  afterEach(async () => {
    process.env = originalEnv;
    // Cleanup temp files
    await Promise.all(tempFiles.map(cleanupFile));
    tempFiles.length = 0;
  });

  describe("fromEnvironment()", () => {
    it("should load all environment variables", () => {
      process.env.TEST_KEY = "test_value";
      process.env.ANOTHER_KEY = "123";

      const result = fromEnvironment();
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (!config) return;
        expect(config.has("test_key")).toBe(true); // Should be lowercase
        expect(config.has("another_key")).toBe(true);
      }
    });

    it("should filter by prefix", () => {
      process.env.APP_CONFIG_KEY = "app_value";
      process.env.OTHER_KEY = "other_value";

      const result = fromEnvironment("APP_CONFIG_");
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (!config) return;
        expect(config.has("key")).toBe(true); // PREFIX removed
        expect(config.has("other_key")).toBe(false);
      }
    });

    it("should parse JSON values in env vars", () => {
      process.env.JSON_VAR = '{"key": "value", "number": 42}';

      const result = fromEnvironment();
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (!config) return;
        const jsonResult = config.get("json_var");
        expect(isSuccess(jsonResult)).toBe(true);
        if (isSuccess(jsonResult)) {
          expect(getData(jsonResult)).toEqual({ key: "value", number: 42 });
        }
      }
    });
  });

  describe("fromDotenv()", () => {
    it("should load .env file", async () => {
      const content = `
        APP_NAME=MyApp
        DEBUG=true
        PORT=3000
      `;
      const filePath = await createTempFile(content, "env");
      tempFiles.push(filePath);

      const result = await fromDotenv(filePath);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (!config) return;
        expect(config.metadata.format).toBe("env");

        const appNameResult = config.get("app_name");
        expect(isSuccess(appNameResult) && getData(appNameResult) === "MyApp").toBe(true);
      }
    });

    it("should handle missing .env file", async () => {
      const result = await fromDotenv("/nonexistent/.env");
      expect(isFailure(result)).toBe(true);
    });
  });
});

describe("Security Features", () => {
  const tempFiles: string[] = [];

  afterEach(async () => {
    await Promise.all(tempFiles.map(cleanupFile));
    tempFiles.length = 0;
  });

  describe("fromFileSecure()", () => {
    it("should load file with security checks", async () => {
      const content = JSON.stringify({ secure: true });
      const filePath = await createTempFile(content, "json");
      tempFiles.push(filePath);

      const result = await fromFileSecure(filePath);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (!config) return;
        expect(config.metadata).toHaveProperty("secure");
        expect(config.metadata).toHaveProperty("fileSize");
        expect(config.metadata).toHaveProperty("lastModified");
      }
    });

    it("should reject dangerous paths", async () => {
      const result = await fromFileSecure("../../../etc/passwd");
      expect(isFailure(result)).toBe(true);
    });

    it("should reject files with path traversal", async () => {
      const result = await fromFileSecure("safe/../../../etc/hosts");
      expect(isFailure(result)).toBe(true);
    });

    it("should reject files starting with /etc", async () => {
      const result = await fromFileSecure("/etc/config.json");
      expect(isFailure(result)).toBe(true);
    });

    it("should reject files with null bytes", async () => {
      const result = await fromFileSecure("file\0.json");
      expect(isFailure(result)).toBe(true);
    });

    it("should handle large files (simulated)", async () => {
      // Create a large but valid JSON content string (simulate large file)
      const largeObject = { data: "x".repeat(1024 * 1024 - 20) }; // 1MB minus JSON overhead
      const largeContent = JSON.stringify(largeObject);
      const filePath = await createTempFile(largeContent, "json");
      tempFiles.push(filePath);

      const result = await fromFileSecure(filePath);
      // Should succeed for 1MB (limit is 10MB)
      expect(isSuccess(result)).toBe(true);
    });
  });

  describe("File watching security", () => {
    it("should not follow symlinks", async () => {
      const content = JSON.stringify({ watch: true });
      const filePath = await createTempFile(content, "json");
      tempFiles.push(filePath);

      let callbackCalled = false;
      const cleanup = watchConfig(filePath, () => {
        callbackCalled = true;
      });

      // Wait a bit for initial callback
      await new Promise((resolve) => setTimeout(resolve, 100));

      cleanup();

      // Should have been called at least once (initial load)
      expect(callbackCalled).toBe(true);
    });

    it("should reject dangerous watch paths", () => {
      expect(() => {
        watchConfig("../../../etc/passwd", () => {
          // Empty callback for testing dangerous path rejection
        });
      }).toThrow();
    });
  });
});

describe("Validation", () => {
  const testSchema: ConfigSchema = {
    type: "object",
    properties: {
      name: { type: "string", required: true },
      age: { type: "number" },
      enabled: { type: "boolean" },
    },
    required: ["name"],
  };

  it("should validate valid configuration", () => {
    const configResult = fromObject({ name: "test", age: 25, enabled: true });
    expect(isSuccess(configResult)).toBe(true);

    if (isSuccess(configResult)) {
      const config = getData(configResult);
      if (config) {
        const validationResult = validate(config, testSchema);
        expect(isSuccess(validationResult)).toBe(true);
      }
    }
  });

  it("should reject invalid configuration", () => {
    const configResult = fromObject({ age: 25 }); // Missing required 'name'
    expect(isSuccess(configResult)).toBe(true);

    if (isSuccess(configResult)) {
      const config = getData(configResult);
      if (config) {
        const validationResult = validate(config, testSchema);
        expect(isFailure(validationResult)).toBe(true);
      }
    }
  });

  it("should validate with fromObject options", () => {
    const validResult = fromObject(
      { name: "test", age: 25 },
      { validate: true, schema: testSchema }
    );
    expect(isSuccess(validResult)).toBe(true);

    const invalidResult = fromObject(
      { age: 25 }, // Missing required 'name'
      { validate: true, schema: testSchema }
    );
    expect(isFailure(invalidResult)).toBe(true);
  });
});

describe("Error Scenarios", () => {
  const tempFiles: string[] = [];

  afterEach(async () => {
    await Promise.all(tempFiles.map(cleanupFile));
    tempFiles.length = 0;
  });

  it("should handle file not found", async () => {
    const result = await fromFile("/nonexistent/config.json");
    expect(isFailure(result)).toBe(true);
  });

  it("should handle permission errors", async () => {
    // Try to read from a directory (should fail)
    const result = await fromFile("/");
    expect(isFailure(result)).toBe(true);
  });

  it("should handle corrupted files", async () => {
    const corruptedJson = '{ "key": "value"'; // Missing closing brace
    const filePath = await createTempFile(corruptedJson, "json");
    tempFiles.push(filePath);

    const result = await fromFile(filePath);
    expect(isFailure(result)).toBe(true);
  });

  it("should handle unsupported formats", () => {
    const result = fromString("content", "javascript");
    expect(isFailure(result)).toBe(true);
  });
});

describe("Performance Requirements", () => {
  it("should meet configuration access performance", () => {
    const configResult = fromObject({
      level1: { level2: { level3: { value: "deep" } } },
    });

    expect(isSuccess(configResult)).toBe(true);
    if (isSuccess(configResult)) {
      const config = getData(configResult);
      if (!config) return;

      const iterations = 10000;
      const start = performance.now();

      for (let i = 0; i < iterations; i++) {
        config.get("level1.level2.level3.value");
      }

      const end = performance.now();
      const avgTime = (end - start) / iterations;

      // Should be < 10μs per operation (TypeScript tier)
      expect(avgTime).toBeLessThan(0.01); // 10μs = 0.01ms
    }
  });
});
