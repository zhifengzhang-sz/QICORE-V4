/**
 * QiCore v4.0 - Configuration Tests
 */

import { test, expect, describe } from "vitest";
import { z } from "zod";
import { Configuration, ConfigurationBuilder, ConfigurationUtils } from "../../src/qicore/core/index.js";

const TestSchema = z.object({
  name: z.string().default("test"),
  port: z.number().default(3000),
  debug: z.boolean().default(false),
  features: z.array(z.string()).default([]),
});

type TestConfig = z.infer<typeof TestSchema>;

describe("Configuration", () => {
  test("should load from object", () => {
    const config = new Configuration(TestSchema);
    const result = config.loadFromObject({
      name: "myapp",
      port: 8080,
      debug: true,
    });
    
    expect(result.isSuccess()).toBe(true);
    
    const data = config.get();
    expect(data.isSuccess()).toBe(true);
    expect(data.unwrap().name).toBe("myapp");
    expect(data.unwrap().port).toBe(8080);
    expect(data.unwrap().debug).toBe(true);
  });

  test("should validate configuration", () => {
    const config = new Configuration(TestSchema);
    
    // Valid config
    const valid = config.loadFromObject({
      name: "test",
      port: 3000,
    });
    expect(valid.isSuccess()).toBe(true);
    
    // Invalid config
    const invalid = config.loadFromObject({
      name: 123, // Should be string
      port: "invalid", // Should be number
    });
    expect(invalid.isFailure()).toBe(true);
  });

  test("should load from JSON", () => {
    const config = new Configuration(TestSchema);
    const json = JSON.stringify({
      name: "jsonapp",
      port: 9000,
    });
    
    const result = config.loadFromJson(json);
    expect(result.isSuccess()).toBe(true);
    
    const data = config.get().unwrap();
    expect(data.name).toBe("jsonapp");
    expect(data.port).toBe(9000);
  });

  test("should handle invalid JSON", () => {
    const config = new Configuration(TestSchema);
    const result = config.loadFromJson("invalid json");
    
    expect(result.isFailure()).toBe(true);
    expect(result.error().category).toBe("ConfigurationError");
  });

  test("should merge configurations", () => {
    const config1 = new Configuration(TestSchema);
    const config2 = new Configuration(TestSchema);
    
    config1.loadFromObject({
      name: "app1",
      port: 3000,
      debug: false,
    });
    
    config2.loadFromObject({
      port: 8080,
      debug: true,
      features: ["feature1"],
    });
    
    const merged = config1.merge(config2);
    expect(merged.isSuccess()).toBe(true);
    
    const data = merged.unwrap().get().unwrap();
    expect(data.name).toBe("app1"); // From config1
    expect(data.port).toBe(8080); // From config2 (overrides)
    expect(data.debug).toBe(true); // From config2 (overrides)
    expect(data.features).toEqual(["feature1"]); // From config2
  });

  test("should clone configuration", () => {
    const config = new Configuration(TestSchema);
    config.loadFromObject({
      name: "original",
      port: 3000,
    });
    
    const cloned = config.clone();
    expect(cloned.isSuccess()).toBe(true);
    
    const clonedData = cloned.unwrap().get().unwrap();
    expect(clonedData.name).toBe("original");
    expect(clonedData.port).toBe(3000);
  });

  test("should reset configuration", () => {
    const config = new Configuration(TestSchema);
    config.loadFromObject({ name: "test" });
    
    expect(config.isLoaded()).toBe(true);
    
    config.reset();
    expect(config.isLoaded()).toBe(false);
    expect(config.get().isFailure()).toBe(true);
  });

  test("should validate current configuration", () => {
    const config = new Configuration(TestSchema);
    config.loadFromObject({
      name: "test",
      port: 3000,
    });
    
    const validation = config.validate();
    expect(validation.isSuccess()).toBe(true);
  });

  test("should handle uninitialized configuration", () => {
    const config = new Configuration(TestSchema);
    
    expect(config.isLoaded()).toBe(false);
    expect(config.get().isFailure()).toBe(true);
    expect(config.validate().isFailure()).toBe(true);
    expect(config.clone().isFailure()).toBe(true);
  });

  test("should handle merge with uninitialized", () => {
    const config1 = new Configuration(TestSchema);
    const config2 = new Configuration(TestSchema);
    
    config1.loadFromObject({ name: "test" });
    // config2 not loaded
    
    const merged = config1.merge(config2);
    expect(merged.isFailure()).toBe(true);
  });
});

describe("ConfigurationBuilder", () => {
  test("should build configuration fluently", async () => {
    const config = await new ConfigurationBuilder(TestSchema)
      .fromObject({ name: "builder-test" })
      .fromJson('{"port": 8080}')
      .build();
    
    const data = config.get().unwrap();
    expect(data.name).toBe("builder-test");
    expect(data.port).toBe(8080);
  });

  test("should handle builder errors", () => {
    expect(() => {
      new ConfigurationBuilder(TestSchema)
        .fromObject({ name: 123 }) // Invalid
        .build();
    }).toThrow();
  });

  test("should support set method", () => {
    const config = new ConfigurationBuilder(TestSchema)
      .fromObject({ name: "builder-test" })
      .set("port", 9090)
      .set("debug", true)
      .build();
    
    const data = config.get().unwrap();
    expect(data.name).toBe("builder-test");
    expect(data.port).toBe(9090);
    expect(data.debug).toBe(true);
  });
});

describe("ConfigurationUtils", () => {
  test("should create configuration", () => {
    const config = ConfigurationUtils.create(TestSchema);
    expect(config).toBeInstanceOf(Configuration);
  });

  test("should create builder", () => {
    const builder = ConfigurationUtils.builder(TestSchema);
    expect(builder).toBeInstanceOf(ConfigurationBuilder);
  });

  test("should merge multiple configurations", () => {
    const config1 = ConfigurationUtils.create(TestSchema);
    const config2 = ConfigurationUtils.create(TestSchema);
    const config3 = ConfigurationUtils.create(TestSchema);
    
    config1.loadFromObject({ name: "app1", port: 3000 });
    config2.loadFromObject({ port: 4000, debug: true });
    config3.loadFromObject({ features: ["feat1"] });
    
    const merged = ConfigurationUtils.mergeAll([config1, config2, config3]);
    expect(merged.isSuccess()).toBe(true);
    
    const data = merged.unwrap().get().unwrap();
    expect(data.name).toBe("app1");
    expect(data.port).toBe(4000);
    expect(data.debug).toBe(true);
    expect(data.features).toEqual(["feat1"]);
  });

  test("should handle empty merge list", () => {
    const merged = ConfigurationUtils.mergeAll([]);
    expect(merged.isFailure()).toBe(true);
  });
});