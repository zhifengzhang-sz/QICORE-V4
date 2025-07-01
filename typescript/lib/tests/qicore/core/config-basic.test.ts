/**
 * Basic Configuration Tests for QiCore v4.0
 *
 * Tests core config functionality that definitely works
 */

import { getData, isFailure, isSuccess } from "@qicore/base/result";
import {
  type ConfigData,
  type ConfigSchema,
  empty,
  fromEnvironment,
  fromObject,
  fromString,
  merge,
  mergeAll,
  validate,
} from "@qicore/core/config";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

describe("Configuration Core Operations", () => {
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
      expect(typeof config.metadata.timestamp).toBe("number");
    });
  });

  describe("fromObject()", () => {
    it("should create config from object", () => {
      const result = fromObject({ key: "value", number: 42 });
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (config) {
          expect(config.size()).toBe(2);
          expect(config.has("key")).toBe(true);
          expect(config.has("number")).toBe(true);
        }
      }
    });

    it("should handle nested objects", () => {
      const result = fromObject({
        simple: "value",
        nested: {
          deep: {
            value: "found",
          },
        },
      });

      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        const config = getData(result);
        if (config) {
          const nestedResult = config.get("nested.deep.value");
          expect(isSuccess(nestedResult)).toBe(true);
          if (isSuccess(nestedResult)) {
            expect(getData(nestedResult)).toBe("found");
          }
        }
      }
    });

    it("should handle arrays", () => {
      const result = fromObject({ array: [1, 2, 3] });
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (config) {
          const arrayResult = config.get("array");
          expect(isSuccess(arrayResult)).toBe(true);
          if (isSuccess(arrayResult)) {
            expect(getData(arrayResult)).toEqual([1, 2, 3]);
          }
        }
      }
    });
  });

  describe("Configuration Access", () => {
    let config: ConfigData;

    beforeEach(() => {
      const result = fromObject({
        simple: "value",
        number: 42,
        boolean: true,
        nested: { key: "nested_value" },
      });

      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        const configData = getData(result);
        if (configData) {
          config = configData;
        }
      }
    });

    it("should get simple values", () => {
      const result = config.get("simple");
      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        expect(getData(result)).toBe("value");
      }
    });

    it("should get nested values", () => {
      const result = config.get("nested.key");
      expect(isSuccess(result)).toBe(true);
      if (isSuccess(result)) {
        expect(getData(result)).toBe("nested_value");
      }
    });

    it("should return error for non-existent keys", () => {
      const result = config.get("nonexistent");
      expect(isFailure(result)).toBe(true);
    });

    it("should check key existence with has()", () => {
      expect(config.has("simple")).toBe(true);
      expect(config.has("nested.key")).toBe(true);
      expect(config.has("nonexistent")).toBe(false);
    });

    it("should get with defaults", () => {
      expect(config.getWithDefault("simple", "default")).toBe("value");
      expect(config.getWithDefault("nonexistent", "default")).toBe("default");
    });

    it("should list keys", () => {
      const keys = config.keys();
      expect(keys).toContain("simple");
      expect(keys).toContain("number");
      expect(keys).toContain("boolean");
      expect(keys).toContain("nested");
    });

    it("should convert to object", () => {
      const obj = config.toObject();
      expect(obj.simple).toBe("value");
      expect(obj.number).toBe(42);
      expect(obj.boolean).toBe(true);
    });
  });

  describe("merge()", () => {
    it("should merge configurations (right-biased)", () => {
      const config1Result = fromObject({ a: 1, b: 2 });
      const config2Result = fromObject({ b: 3, c: 4 });

      expect(isSuccess(config1Result)).toBe(true);
      expect(isSuccess(config2Result)).toBe(true);

      if (isSuccess(config1Result) && isSuccess(config2Result)) {
        const config1Data = getData(config1Result);
        const config2Data = getData(config2Result);
        if (config1Data && config2Data) {
          const merged = merge(config1Data, config2Data);

          const obj = merged.toObject();
          expect(obj.a).toBe(1);
          expect(obj.b).toBe(3); // Right-biased
          expect(obj.c).toBe(4);
          expect(merged.metadata.source).toBe("merged");
        }
      }
    });

    it("should satisfy identity laws", () => {
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
  });

  describe("mergeAll()", () => {
    it("should merge multiple configurations", () => {
      const configs = [fromObject({ a: 1 }), fromObject({ b: 2 }), fromObject({ c: 3 })];

      for (const result of configs) {
        expect(isSuccess(result)).toBe(true);
      }

      const validConfigs = configs.map((result) => {
        const data = getData(result);
        if (!data) throw new Error("Expected valid config data");
        return data;
      });
      const merged = mergeAll(validConfigs);

      const obj = merged.toObject();
      expect(obj.a).toBe(1);
      expect(obj.b).toBe(2);
      expect(obj.c).toBe(3);
    });
  });

  describe("String parsing", () => {
    it("should parse JSON strings", () => {
      const result = fromString('{"key": "value", "number": 42}', "json");
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (config) {
          expect(config.has("key")).toBe(true);
          expect(config.has("number")).toBe(true);
        }
      }
    });

    it("should handle invalid JSON", () => {
      const result = fromString("{ invalid json }", "json");
      expect(isFailure(result)).toBe(true);
    });

    it("should parse YAML strings", () => {
      const result = fromString("key: value\nnumber: 42", "yaml");
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (config) {
          expect(config.has("key")).toBe(true);
          expect(config.has("number")).toBe(true);
        }
      }
    });

    it("should parse TOML strings", () => {
      const result = fromString('key = "value"\nnumber = 42', "toml");
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (config) {
          expect(config.has("key")).toBe(true);
          expect(config.has("number")).toBe(true);
        }
      }
    });
  });

  describe("Environment variables", () => {
    const originalEnv = process.env;

    afterEach(() => {
      process.env = originalEnv;
    });

    it("should load environment variables", () => {
      process.env.TEST_KEY = "test_value";
      process.env.ANOTHER_KEY = "123";

      const result = fromEnvironment();
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (config) {
          expect(config.has("test_key")).toBe(true);
          expect(config.has("another_key")).toBe(true);
        }
      }
    });

    it("should filter by prefix", () => {
      process.env.APP_CONFIG = "app_value";
      process.env.OTHER_KEY = "other_value";

      const result = fromEnvironment("APP_");
      expect(isSuccess(result)).toBe(true);

      if (isSuccess(result)) {
        const config = getData(result);
        if (config) {
          expect(config.has("config")).toBe(true);
          expect(config.has("other_key")).toBe(false);
        }
      }
    });
  });

  describe("Validation", () => {
    const testSchema: ConfigSchema = {
      type: "object",
      properties: {
        name: { type: "string", required: true },
        age: { type: "number" },
      },
      required: ["name"],
    };

    it("should validate valid configuration", () => {
      const configResult = fromObject({ name: "test", age: 25 });
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
  });
});
