/**
 * QiCore v4.0 - Configuration Component Tests
 * 
 * Comprehensive tests for configuration with monoid laws verification
 */

import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { promises as fs } from "fs";
import {
  empty,
  merge,
  deepMerge,
  fromObject,
  fromEnvironment,
  fromFile,
  get,
  getOr,
  has,
  keys,
  values,
  entries,
  toObject,
  mapValues,
  filter,
  validate,
  createSchema,
  chain,
  deepChain,
  size,
  isEmpty,
  equals,
  loadConfiguration,
  type ConfigData
} from "../../../src/qicore/core/config.js";

describe("Configuration Component", () => {
  const tempFilePath = "/tmp/qicore-test-config.json";

  afterEach(async () => {
    try {
      await fs.unlink(tempFilePath);
    } catch {
      // File may not exist
    }
  });

  describe("Basic Operations", () => {
    it("creates empty configuration", () => {
      const config = empty();
      
      expect(config.data.size).toBe(0);
      expect(isEmpty(config)).toBe(true);
      expect(size(config)).toBe(0);
    });

    it("creates configuration from object", () => {
      const obj = { database: { host: "localhost", port: 5432 }, debug: true };
      const result = fromObject(obj);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const config = result.right;
        expect(has(config, "database.host")).toBe(true);
        expect(has(config, "database.port")).toBe(true);
        expect(has(config, "debug")).toBe(true);
        expect(size(config)).toBe(3);
      }
    });

    it("handles null and undefined values", () => {
      const obj = { nullValue: null, undefinedValue: undefined, zeroValue: 0 };
      const result = fromObject(obj);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const config = result.right;
        expect(has(config, "nullValue")).toBe(true);
        expect(has(config, "undefinedValue")).toBe(true);
        expect(has(config, "zeroValue")).toBe(true);
      }
    });
  });

  describe("Monoid Laws", () => {
    let testConfigs: ConfigData[];

    beforeEach(() => {
      testConfigs = [
        fromObject({ a: 1 }).right!,
        fromObject({ b: 2 }).right!,
        fromObject({ c: 3 }).right!,
        fromObject({ a: 10, d: 4 }).right!,
        empty()
      ];
    });

    it("satisfies left identity: ∅ ⊕ a = a", () => {
      for (const config of testConfigs) {
        const result = merge(empty(), config);
        expect(equals(result, config)).toBe(true);
      }
    });

    it("satisfies right identity: a ⊕ ∅ = a", () => {
      for (const config of testConfigs) {
        const result = merge(config, empty());
        expect(equals(result, config)).toBe(true);
      }
    });

    it("satisfies associativity: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)", () => {
      for (let i = 0; i < testConfigs.length; i++) {
        for (let j = 0; j < testConfigs.length; j++) {
          for (let k = 0; k < testConfigs.length; k++) {
            const a = testConfigs[i];
            const b = testConfigs[j];
            const c = testConfigs[k];
            
            const leftSide = merge(merge(a, b), c);
            const rightSide = merge(a, merge(b, c));
            
            expect(equals(leftSide, rightSide)).toBe(true);
          }
        }
      }
    });

    it("implements right-biased merge", () => {
      const config1 = fromObject({ key: "value1", keep: "original" }).right!;
      const config2 = fromObject({ key: "value2", new: "added" }).right!;
      
      const merged = merge(config1, config2);
      
      expect(merged.data.get("key")).toBe("value2"); // Right wins
      expect(merged.data.get("keep")).toBe("original"); // Left preserved
      expect(merged.data.get("new")).toBe("added"); // Right added
    });
  });

  describe("Deep Merge Operations", () => {
    it("merges nested objects deeply", () => {
      const config1 = fromObject({
        database: { host: "localhost", port: 5432 },
        cache: { ttl: 300 }
      }).right!;
      
      const config2 = fromObject({
        database: { port: 3306, ssl: true },
        logging: { level: "info" }
      }).right!;
      
      const merged = deepMerge(config1, config2);
      
      // Check that nested objects are merged
      const dbConfig = merged.data.get("database") as Record<string, unknown>;
      expect(dbConfig.host).toBe("localhost"); // From config1
      expect(dbConfig.port).toBe(3306); // From config2 (right-biased)
      expect(dbConfig.ssl).toBe(true); // From config2
      
      expect(merged.data.get("cache")).toEqual({ ttl: 300 });
      expect(merged.data.get("logging")).toEqual({ level: "info" });
    });

    it("handles arrays as atomic values", () => {
      const config1 = fromObject({ items: [1, 2, 3] }).right!;
      const config2 = fromObject({ items: [4, 5] }).right!;
      
      const merged = deepMerge(config1, config2);
      
      expect(merged.data.get("items")).toEqual([4, 5]); // Right replaces left
    });
  });

  describe("Environment Variable Loading", () => {
    const originalEnv = process.env;

    beforeEach(() => {
      // Set up test environment variables
      process.env.TEST_APP_DATABASE_HOST = "env-host";
      process.env.TEST_APP_DATABASE_PORT = "5432";
      process.env.TEST_APP_DEBUG = "true";
      process.env.TEST_APP_ITEMS = "item1,item2,item3";
      process.env.OTHER_VAR = "should-not-be-included";
    });

    afterEach(() => {
      process.env = originalEnv;
    });

    it("loads environment variables with prefix", () => {
      const result = fromEnvironment("TEST_APP");
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const config = result.right;
        
        expect(config.data.get("database.host")).toBe("env-host");
        expect(config.data.get("database.port")).toBe(5432); // Parsed as number
        expect(config.data.get("debug")).toBe(true); // Parsed as boolean
        expect(config.data.get("items")).toEqual(["item1", "item2", "item3"]); // Parsed as array
        expect(config.data.has("other")).toBe(false); // Different prefix
      }
    });

    it("handles missing prefix gracefully", () => {
      const result = fromEnvironment("NONEXISTENT");
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.data.size).toBe(0);
      }
    });
  });

  describe("File Loading", () => {
    it("loads configuration from JSON file", async () => {
      const testConfig = {
        server: { port: 8080, host: "0.0.0.0" },
        database: { url: "postgres://localhost/test" }
      };
      
      await fs.writeFile(tempFilePath, JSON.stringify(testConfig));
      
      const result = await fromFile(tempFilePath);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const config = result.right;
        expect(config.data.get("server.port")).toBe(8080);
        expect(config.data.get("server.host")).toBe("0.0.0.0");
        expect(config.data.get("database.url")).toBe("postgres://localhost/test");
      }
    });

    it("handles file not found", async () => {
      const result = await fromFile("/nonexistent/file.json");
      
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.category).toBe("FILESYSTEM");
      }
    });

    it("handles invalid JSON", async () => {
      await fs.writeFile(tempFilePath, "{ invalid json }");
      
      const result = await fromFile(tempFilePath);
      
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.category).toBe("FILESYSTEM");
      }
    });
  });

  describe("Configuration Access", () => {
    let config: ConfigData;

    beforeEach(() => {
      config = fromObject({
        string: "value",
        number: 42,
        boolean: true,
        nested: { key: "nested-value" },
        array: [1, 2, 3]
      }).right!;
    });

    it("gets values by key", () => {
      expect(get<string>(config, "string")).toEqual({ _tag: "Right", right: "value" });
      expect(get<number>(config, "number")).toEqual({ _tag: "Right", right: 42 });
      expect(get<boolean>(config, "boolean")).toEqual({ _tag: "Right", right: true });
      expect(get(config, "nested")).toEqual({ _tag: "Right", right: { key: "nested-value" } });
    });

    it("handles missing keys", () => {
      const result = get(config, "nonexistent");
      
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("CONFIG_KEY_NOT_FOUND");
      }
    });

    it("gets values with defaults", () => {
      expect(getOr("default")(config, "string")).toBe("value");
      expect(getOr("default")(config, "nonexistent")).toBe("default");
    });

    it("checks key existence", () => {
      expect(has(config, "string")).toBe(true);
      expect(has(config, "nonexistent")).toBe(false);
    });

    it("lists keys and values", () => {
      const configKeys = keys(config).sort();
      const configValues = values(config);
      const configEntries = entries(config);
      
      expect(configKeys).toEqual(["array", "boolean", "nested", "number", "string"]);
      expect(configValues).toHaveLength(5);
      expect(configEntries).toHaveLength(5);
    });

    it("converts back to object", () => {
      const obj = toObject(config);
      
      expect(obj.string).toBe("value");
      expect(obj.number).toBe(42);
      expect(obj.boolean).toBe(true);
      expect(obj.nested).toEqual({ key: "nested-value" });
      expect(obj.array).toEqual([1, 2, 3]);
    });
  });

  describe("Configuration Transformations", () => {
    let config: ConfigData;

    beforeEach(() => {
      config = fromObject({
        count: 5,
        name: "test",
        active: true
      }).right!;
    });

    it("maps over values", () => {
      const mapped = mapValues((value: unknown, key: string) => `${key}:${value}`)(config);
      
      expect(mapped.data.get("count")).toBe("count:5");
      expect(mapped.data.get("name")).toBe("name:test");
      expect(mapped.data.get("active")).toBe("active:true");
    });

    it("filters by predicate", () => {
      const filtered = filter((value: unknown, key: string) => 
        typeof value === "string"
      )(config);
      
      expect(filtered.data.size).toBe(1);
      expect(filtered.data.get("name")).toBe("test");
    });
  });

  describe("Configuration Validation", () => {
    it("validates with schema", () => {
      const config = fromObject({
        port: 8080,
        host: "localhost"
      }).right!;
      
      const schema = createSchema((data: unknown) => {
        const obj = data as Record<string, unknown>;
        if (typeof obj.port !== "number" || typeof obj.host !== "string") {
          throw new Error("Invalid server config");
        }
        return { port: obj.port, host: obj.host };
      });
      
      const result = validate(config, schema);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.port).toBe(8080);
        expect(result.right.host).toBe("localhost");
      }
    });

    it("fails validation with invalid data", () => {
      const config = fromObject({
        port: "invalid",
        host: 123
      }).right!;
      
      const schema = createSchema((data: unknown) => {
        const obj = data as Record<string, unknown>;
        if (typeof obj.port !== "number" || typeof obj.host !== "string") {
          throw new Error("Invalid server config");
        }
        return { port: obj.port, host: obj.host };
      });
      
      const result = validate(config, schema);
      
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("CONFIG_VALIDATION_ERROR");
      }
    });
  });

  describe("Configuration Chaining", () => {
    it("chains configurations with precedence", () => {
      const base = fromObject({ a: 1, b: 2 }).right!;
      const override1 = fromObject({ b: 20, c: 3 }).right!;
      const override2 = fromObject({ c: 30, d: 4 }).right!;
      
      const chained = chain(base, override1, override2);
      
      expect(chained.data.get("a")).toBe(1); // From base
      expect(chained.data.get("b")).toBe(20); // From override1
      expect(chained.data.get("c")).toBe(30); // From override2 (latest wins)
      expect(chained.data.get("d")).toBe(4); // From override2
    });

    it("deep chains configurations", () => {
      const base = fromObject({ db: { host: "localhost", port: 5432 } }).right!;
      const override = fromObject({ db: { port: 3306 } }).right!;
      
      const chained = deepChain(base, override);
      
      const dbConfig = chained.data.get("db") as Record<string, unknown>;
      expect(dbConfig.host).toBe("localhost"); // Preserved
      expect(dbConfig.port).toBe(3306); // Overridden
    });
  });

  describe("Configuration Loading", () => {
    const originalEnv = process.env;

    beforeEach(() => {
      process.env.TEST_CONFIG_PORT = "9000";
      process.env.TEST_CONFIG_DEBUG = "true";
    });

    afterEach(() => {
      process.env = originalEnv;
    });

    it("loads from multiple sources", async () => {
      const fileConfig = {
        server: { port: 8080, host: "localhost" },
        database: { url: "postgres://localhost" }
      };
      await fs.writeFile(tempFilePath, JSON.stringify(fileConfig));
      
      const result = await loadConfiguration({
        defaults: { server: { port: 3000, workers: 4 } },
        configFile: tempFilePath,
        envPrefix: "TEST_CONFIG",
        overrides: { server: { workers: 8 } }
      });
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const config = result.right;
        
        // Defaults, then file, then env, then overrides
        expect(config.data.get("server.port")).toBe(9000); // From env
        expect(config.data.get("server.host")).toBe("localhost"); // From file
        expect(config.data.get("server.workers")).toBe(8); // From overrides
        expect(config.data.get("debug")).toBe(true); // From env
        expect(config.data.get("database.url")).toBe("postgres://localhost"); // From file
      }
    });

    it("continues on file loading errors", async () => {
      const result = await loadConfiguration({
        defaults: { port: 3000 },
        configFile: "/nonexistent/file.json",
        overrides: { debug: true }
      });
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const config = result.right;
        expect(config.data.get("port")).toBe(3000);
        expect(config.data.get("debug")).toBe(true);
      }
    });
  });

  describe("Performance Requirements", () => {
    it("merges configurations quickly", () => {
      const config1 = fromObject(Object.fromEntries(
        Array.from({ length: 100 }, (_, i) => [`key_${i}`, `value_${i}`])
      )).right!;
      
      const config2 = fromObject(Object.fromEntries(
        Array.from({ length: 100 }, (_, i) => [`key_${i + 50}`, `value_${i + 50}_new`])
      )).right!;
      
      const start = performance.now();
      
      for (let i = 0; i < 100; i++) {
        merge(config1, config2);
      }
      
      const end = performance.now();
      const avgDuration = ((end - start) * 1000) / 100; // microseconds per operation
      
      // Should be well under 1ms per operation
      expect(avgDuration).toBeLessThan(1000);
    });

    it("accesses values quickly", () => {
      const config = fromObject(Object.fromEntries(
        Array.from({ length: 1000 }, (_, i) => [`key_${i}`, `value_${i}`])
      )).right!;
      
      const start = performance.now();
      
      for (let i = 0; i < 1000; i++) {
        get(config, `key_${i % 1000}`);
        has(config, `key_${i % 1000}`);
      }
      
      const end = performance.now();
      const avgDuration = ((end - start) * 1000) / 2000; // microseconds per operation
      
      // Should be well under 10μs per operation
      expect(avgDuration).toBeLessThan(10);
    });
  });

  describe("Edge Cases", () => {
    it("handles circular object references gracefully", () => {
      const obj: any = { a: 1 };
      obj.self = obj; // Circular reference
      
      // fromObject should handle this without infinite recursion
      expect(() => fromObject(obj)).not.toThrow();
    });

    it("handles very deep objects", () => {
      const createDeepObject = (depth: number): Record<string, unknown> => {
        let obj: Record<string, unknown> = { value: "deep" };
        for (let i = 0; i < depth; i++) {
          obj = { [`level_${i}`]: obj };
        }
        return obj;
      };
      
      const deepObj = createDeepObject(100);
      const result = fromObject(deepObj);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.data.size).toBeGreaterThan(0);
      }
    });

    it("handles special values correctly", () => {
      const specialValues = {
        nan: NaN,
        infinity: Infinity,
        negativeInfinity: -Infinity,
        emptyString: "",
        zero: 0,
        negativeZero: -0
      };
      
      const result = fromObject(specialValues);
      
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const config = result.right;
        
        expect(Number.isNaN(config.data.get("nan"))).toBe(true);
        expect(config.data.get("infinity")).toBe(Infinity);
        expect(config.data.get("negativeInfinity")).toBe(-Infinity);
        expect(config.data.get("emptyString")).toBe("");
        expect(config.data.get("zero")).toBe(0);
        expect(Object.is(config.data.get("negativeZero"), -0)).toBe(true);
      }
    });
  });
});