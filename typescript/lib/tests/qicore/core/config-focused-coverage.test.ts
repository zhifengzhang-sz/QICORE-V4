/**
 * Focused Config Coverage Tests for QiCore v4.0 Core
 *
 * Carefully targets specific uncovered lines in config.ts
 * Builds on working patterns from config-basic.test.ts
 */

import * as fs from "node:fs/promises";
import * as path from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { getData, isFailure, isSuccess } from "../../../src/qicore/base/result.js";
import {
  type ConfigData,
  type ConfigSchema,
  empty,
  fromEnvironment,
  fromFile,
  fromObject,
  fromString,
  merge,
  mergeAll,
  validate,
} from "../../../src/qicore/core/config.js";

describe("Config Coverage Enhancement - Targeted Tests", () => {
  const testDir = "/tmp/qicore-config-focused";

  beforeEach(async () => {
    try {
      await fs.mkdir(testDir, { recursive: true });
    } catch {
      // Directory might already exist
    }
  });

  afterEach(async () => {
    try {
      await fs.rm(testDir, { recursive: true, force: true });
    } catch {
      // Ignore cleanup errors
    }
  });

  describe("ConfigData.get() edge cases", () => {
    it("should handle null values properly", () => {
      const result = fromObject({
        nullValue: null,
        nested: {
          alsoNull: null,
          validValue: "test",
        },
      });

      if (isSuccess(result)) {
        const config = getData(result)!;

        // Test getting null value directly
        const nullResult = config.get("nullValue");
        expect(isSuccess(nullResult)).toBe(true);
        if (isSuccess(nullResult)) {
          expect(getData(nullResult)).toBeNull();
        }

        // Test getting nested null value
        const nestedNullResult = config.get("nested.alsoNull");
        expect(isSuccess(nestedNullResult)).toBe(true);
        if (isSuccess(nestedNullResult)) {
          expect(getData(nestedNullResult)).toBeNull();
        }

        // Test that we can still get valid nested values
        const validResult = config.get("nested.validValue");
        expect(isSuccess(validResult)).toBe(true);
        if (isSuccess(validResult)) {
          expect(getData(validResult)).toBe("test");
        }
      }
    });

    it("should fail gracefully on deep invalid paths", () => {
      const result = fromObject({
        level1: {
          level2: "string_value",
        },
      });

      if (isSuccess(result)) {
        const config = getData(result)!;

        // Try to access property of string (should fail)
        const invalidResult = config.get("level1.level2.level3");
        expect(isFailure(invalidResult)).toBe(true);
      }
    });
  });

  describe("File operations with different formats", () => {
    it("should handle JSON files correctly", async () => {
      const jsonPath = path.join(testDir, "test.json");
      const jsonData = {
        app: {
          name: "test-app",
          version: "1.0.0",
          settings: {
            debug: true,
            port: 3000,
          },
        },
      };

      await fs.writeFile(jsonPath, JSON.stringify(jsonData, null, 2));

      const result = await fromFile(jsonPath);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result)!;

        const nameResult = config.get("app.name");
        expect(isSuccess(nameResult)).toBe(true);
        if (isSuccess(nameResult)) {
          expect(getData(nameResult)).toBe("test-app");
        }

        const debugResult = config.get("app.settings.debug");
        expect(isSuccess(debugResult)).toBe(true);
        if (isSuccess(debugResult)) {
          expect(getData(debugResult)).toBe(true);
        }
      }
    });

    it("should handle YAML files correctly", async () => {
      const yamlPath = path.join(testDir, "test.yaml");
      const yamlContent = `
database:
  host: localhost
  port: 5432
  credentials:
    username: testuser
    password: testpass
features:
  - authentication
  - logging
  - monitoring
`;

      await fs.writeFile(yamlPath, yamlContent);

      const result = await fromFile(yamlPath);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result)!;

        const hostResult = config.get("database.host");
        expect(isSuccess(hostResult)).toBe(true);
        if (isSuccess(hostResult)) {
          expect(getData(hostResult)).toBe("localhost");
        }

        const usernameResult = config.get("database.credentials.username");
        expect(isSuccess(usernameResult)).toBe(true);
        if (isSuccess(usernameResult)) {
          expect(getData(usernameResult)).toBe("testuser");
        }
      }
    });

    it("should handle TOML files correctly", async () => {
      const tomlPath = path.join(testDir, "test.toml");
      const tomlContent = `
title = "Test Configuration"

[server]
host = "127.0.0.1"
port = 8080

[server.ssl]
enabled = true
cert_path = "/path/to/cert"

[database]
url = "postgresql://localhost:5432/test"
max_connections = 10
`;

      await fs.writeFile(tomlPath, tomlContent);

      const result = await fromFile(tomlPath);
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result)!;

        const titleResult = config.get("title");
        expect(isSuccess(titleResult)).toBe(true);
        if (isSuccess(titleResult)) {
          expect(getData(titleResult)).toBe("Test Configuration");
        }

        const sslResult = config.get("server.ssl.enabled");
        expect(isSuccess(sslResult)).toBe(true);
        if (isSuccess(sslResult)) {
          expect(getData(sslResult)).toBe(true);
        }
      }
    });

    it("should handle file read errors gracefully", async () => {
      // Test non-existent file
      const result = await fromFile("/totally/nonexistent/path/file.json");
      expect(isFailure(result)).toBe(true);

      if (isFailure(result)) {
        // Check that error has appropriate structure (don't assume specific code)
        expect(result.left.message).toBeDefined();
        expect(result.left.code).toBeDefined();
      }
    });

    it("should handle invalid JSON gracefully", async () => {
      const invalidJsonPath = path.join(testDir, "invalid.json");
      await fs.writeFile(invalidJsonPath, '{ "invalid": json content }');

      const result = await fromFile(invalidJsonPath);
      expect(isFailure(result)).toBe(true);

      if (isFailure(result)) {
        expect(result.left.message).toBeDefined();
        expect(result.left.code).toBeDefined();
      }
    });
  });

  describe("String parsing operations", () => {
    it("should parse JSON strings correctly", () => {
      const jsonString = JSON.stringify({
        service: {
          name: "api-service",
          endpoints: ["/health", "/metrics", "/api/v1"],
        },
      });

      const result = fromString(jsonString, "json");
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result)!;

        const nameResult = config.get("service.name");
        expect(isSuccess(nameResult)).toBe(true);
        if (isSuccess(nameResult)) {
          expect(getData(nameResult)).toBe("api-service");
        }
      }
    });

    it("should parse YAML strings correctly", () => {
      const yamlString = `
logging:
  level: info
  format: json
  outputs:
    - console
    - file
  file:
    path: /var/log/app.log
    max_size: 100MB
`;

      const result = fromString(yamlString, "yaml");
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result)!;

        const levelResult = config.get("logging.level");
        expect(isSuccess(levelResult)).toBe(true);
        if (isSuccess(levelResult)) {
          expect(getData(levelResult)).toBe("info");
        }

        const pathResult = config.get("logging.file.path");
        expect(isSuccess(pathResult)).toBe(true);
        if (isSuccess(pathResult)) {
          expect(getData(pathResult)).toBe("/var/log/app.log");
        }
      }
    });

    it("should handle invalid JSON strings gracefully", () => {
      const invalidJson = '{ "key": invalid_value }';

      const result = fromString(invalidJson, "json");
      expect(isFailure(result)).toBe(true);

      if (isFailure(result)) {
        expect(result.left.message).toBeDefined();
        expect(result.left.code).toBeDefined();
      }
    });
  });

  describe("Environment variable operations", () => {
    const originalEnv = process.env;

    beforeEach(() => {
      // Set test environment variables
      process.env.APP_NAME = "test-app";
      process.env.APP_PORT = "3000";
      process.env.APP_DEBUG = "true";
      process.env.DB_HOST = "localhost";
      process.env.DB_PORT = "5432";
      process.env.OTHER_VAR = "should-not-appear";
    });

    afterEach(() => {
      process.env = originalEnv;
    });

    it("should read environment variables with prefix filtering", () => {
      const result = fromEnvironment("APP_");
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result)!;

        // Should have APP_ variables without the prefix (converted to lowercase)
        const nameResult = config.get("name");
        expect(isSuccess(nameResult)).toBe(true);
        if (isSuccess(nameResult)) {
          expect(getData(nameResult)).toBe("test-app");
        }

        const portResult = config.get("port");
        expect(isSuccess(portResult)).toBe(true);
        if (isSuccess(portResult)) {
          expect(getData(portResult)).toBe(3000); // parseEnvValue converts "3000" to number
        }

        // Should not have OTHER_VAR since it doesn't match prefix
        const otherResult = config.get("other_var");
        expect(isFailure(otherResult)).toBe(true);
      }
    });

    it("should read all environment variables when no prefix provided", () => {
      const result = fromEnvironment();
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result)!;

        // Should have all our test variables (converted to lowercase)
        const appNameResult = config.get("app_name");
        expect(isSuccess(appNameResult)).toBe(true);
        if (isSuccess(appNameResult)) {
          expect(getData(appNameResult)).toBe("test-app");
        }

        const dbHostResult = config.get("db_host");
        expect(isSuccess(dbHostResult)).toBe(true);
        if (isSuccess(dbHostResult)) {
          expect(getData(dbHostResult)).toBe("localhost");
        }
      }
    });
  });

  describe("Configuration validation", () => {
    it("should validate against schema successfully", () => {
      const config = fromObject({
        api: {
          host: "localhost",
          port: 8080,
          ssl: true,
        },
        database: {
          url: "postgresql://localhost:5432/test",
          pool_size: 10,
        },
      });

      if (isSuccess(config)) {
        // For flat config objects, use plain keys not dotted paths
        const schema: ConfigSchema = {
          type: "object",
          properties: {
            api: { type: "object" },
            database: { type: "object" },
          },
          required: ["api", "database"],
        };

        const validationResult = validate(getData(config)!, schema);
        expect(isSuccess(validationResult)).toBe(true);
      }
    });

    it("should fail validation for missing required fields", () => {
      const config = fromObject({
        api: {
          host: "localhost",
          // Missing required port
        },
      });

      if (isSuccess(config)) {
        const schema: ConfigSchema = {
          type: "object",
          properties: {
            "api.host": { type: "string" },
            "api.port": { type: "number" },
          },
          required: ["api.host", "api.port"],
        };

        const validationResult = validate(getData(config)!, schema);
        expect(isFailure(validationResult)).toBe(true);
      }
    });

    it("should fail validation for wrong types", () => {
      const config = fromObject({
        api: {
          host: "localhost",
          port: "not-a-number", // Wrong type
        },
      });

      if (isSuccess(config)) {
        const schema: ConfigSchema = {
          type: "object",
          properties: {
            "api.host": { type: "string" },
            "api.port": { type: "number" },
          },
          required: ["api.host", "api.port"],
        };

        const validationResult = validate(getData(config)!, schema);
        expect(isFailure(validationResult)).toBe(true);
      }
    });
  });

  describe("Complex merge operations", () => {
    it("should handle mergeAll with many configurations", () => {
      const configs = [
        fromObject({ app_name: "base", database_host: "localhost" }),
        fromObject({ app_version: "1.0", database_port: 5432 }),
        fromObject({ app_debug: true, cache_enabled: true }),
        fromObject({ app_name: "override", cache_ttl: 3600 }),
      ];

      const validConfigs = configs.filter(isSuccess).map(getData).filter(Boolean) as ConfigData[];

      if (validConfigs.length === 4) {
        const merged = mergeAll(validConfigs);

        // Should be right-biased for conflicts
        const nameResult = merged.get("app_name");
        expect(isSuccess(nameResult)).toBe(true);
        if (isSuccess(nameResult)) {
          expect(getData(nameResult)).toBe("override"); // Last one wins
        }

        // Should merge non-conflicting values
        const versionResult = merged.get("app_version");
        expect(isSuccess(versionResult)).toBe(true);
        if (isSuccess(versionResult)) {
          expect(getData(versionResult)).toBe("1.0");
        }

        const hostResult = merged.get("database_host");
        expect(isSuccess(hostResult)).toBe(true);
        if (isSuccess(hostResult)) {
          expect(getData(hostResult)).toBe("localhost");
        }

        const ttlResult = merged.get("cache_ttl");
        expect(isSuccess(ttlResult)).toBe(true);
        if (isSuccess(ttlResult)) {
          expect(getData(ttlResult)).toBe(3600);
        }
      }
    });

    it("should handle empty configuration arrays in mergeAll", () => {
      const merged = mergeAll([]);
      expect(merged.size()).toBe(0);
      expect(merged.keys()).toEqual([]);
    });

    it("should handle single configuration in mergeAll", () => {
      const singleConfig = fromObject({ key: "value", number: 42 });

      if (isSuccess(singleConfig)) {
        const merged = mergeAll([getData(singleConfig)!]);

        const keyResult = merged.get("key");
        expect(isSuccess(keyResult)).toBe(true);
        if (isSuccess(keyResult)) {
          expect(getData(keyResult)).toBe("value");
        }

        const numberResult = merged.get("number");
        expect(isSuccess(numberResult)).toBe(true);
        if (isSuccess(numberResult)) {
          expect(getData(numberResult)).toBe(42);
        }
      }
    });
  });
});
