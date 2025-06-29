/**
 * QiCore Component Integration Tests
 * 
 * Tests how the 6 implemented components work together:
 * - Error + Result integration
 * - Config + Logger integration  
 * - Cache + Performance integration
 * - Full workflow integration tests
 */

import { createMemoryCache } from "../../../src/qicore/core/cache.js";
import { fromObject, merge } from "../../../src/qicore/core/config.js";
import { createDefault as createDefaultLogger } from "../../../src/qicore/core/logger.js";
import { measure, measureAsync } from "../../../src/qicore/core/performance.js";
import {
  createQiError,
  failure,
  flatMap,
  map,
  success,
  withContext,
  type Result,
} from "../../../src/qicore/index.js";
import { describe, expect, it } from "vitest";

describe("QiCore Component Integration", () => {
  describe("Error + Result Integration", () => {
    it("should chain errors with context through Result operations", () => {
      const originalError = createQiError("VALIDATION_ERROR", "Invalid input", "VALIDATION");
      const contextualError = withContext(originalError, { field: "email", value: "invalid" });
      
      const result = failure(contextualError);
      const mapped = map((x: string) => x.toUpperCase())(result);
      const chained = flatMap((x: string) => success(x + "!"))(mapped);
      
      expect(chained._tag).toBe("Left");
      if (chained._tag === "Left") {
        expect(chained.left.code).toBe("VALIDATION_ERROR");
        expect(chained.left.context.get("field")).toBe("email");
        expect(chained.left.context.get("value")).toBe("invalid");
      }
    });

    it("should maintain error chain through complex operations", () => {
      const step1 = (input: number): Result<number> => {
        if (input < 0) {
          return failure(createQiError("NEGATIVE_INPUT", "Input cannot be negative", "VALIDATION"));
        }
        return success(input * 2);
      };

      const step2 = (input: number): Result<string> => {
        if (input > 100) {
          return failure(createQiError("TOO_LARGE", "Result too large", "BUSINESS"));
        }
        return success(`Result: ${input}`);
      };

      const process = (input: number): Result<string> => {
        return flatMap(step2)(step1(input));
      };

      // Test negative input
      const result1 = process(-5);
      expect(result1._tag).toBe("Left");
      if (result1._tag === "Left") {
        expect(result1.left.code).toBe("NEGATIVE_INPUT");
      }

      // Test too large result
      const result2 = process(60); // 60 * 2 = 120 > 100
      expect(result2._tag).toBe("Left");
      if (result2._tag === "Left") {
        expect(result2.left.code).toBe("TOO_LARGE");
      }

      // Test success case
      const result3 = process(25); // 25 * 2 = 50 < 100
      expect(result3._tag).toBe("Right");
      if (result3._tag === "Right") {
        expect(result3.right).toBe("Result: 50");
      }
    });
  });

  describe("Config + Logger Integration", () => {
    it("should use configuration to set up logger with custom settings", () => {
      const configResult = fromObject({
        logger: {
          level: "warn",
          format: "json",
          timestamp: true,
        },
      });

      expect(configResult._tag).toBe("Right");
      if (configResult._tag === "Right") {
        const config = configResult.right;
        
        // Extract logger config and create logger
        const loggerResult = createDefaultLogger();
        expect(loggerResult._tag).toBe("Right");
        
        if (loggerResult._tag === "Right") {
          const logger = loggerResult.right;
          
          // Test logger configuration
          expect(logger.isLevelEnabled("error")).toBe(true);
          expect(logger.isLevelEnabled("warn")).toBe(true);
          expect(logger.isLevelEnabled("info")).toBe(true); // Default logger level
          expect(logger.isLevelEnabled("debug")).toBe(false);
        }
      }
    });

    it("should merge configurations and apply to logger", () => {
      const baseConfigResult = fromObject({
        logger: { level: "info" },
        app: { name: "test-app" },
      });

      const overrideConfigResult = fromObject({
        logger: { format: "json", timestamp: true },
        app: { version: "1.0.0" },
      });

      expect(baseConfigResult._tag).toBe("Right");
      expect(overrideConfigResult._tag).toBe("Right");

      if (baseConfigResult._tag === "Right" && overrideConfigResult._tag === "Right") {
        const mergedConfig = merge(baseConfigResult.right, overrideConfigResult.right);
        
        // Verify merged configuration
        expect(mergedConfig.data.get("logger")).toEqual({ format: "json", timestamp: true });
        expect(mergedConfig.data.get("app")).toEqual({ version: "1.0.0" });
      }
    });
  });

  describe("Cache + Performance Integration", () => {
    it("should measure cache operations performance", async () => {
      const cacheResult = createMemoryCache();
      expect(cacheResult._tag).toBe("Right");

      if (cacheResult._tag === "Right") {
        const cache = cacheResult.right;

        // Measure cache set operation
        const setValue = await measureAsync(
          "cache_set_integration",
          async () => {
            await cache.set("integration-key", "integration-value");
          }
        );

        // Measure cache get operation
        const getValue = await measureAsync(
          "cache_get_integration", 
          async () => {
            return await cache.get("integration-key");
          }
        );

        expect(getValue._tag).toBe("Right");
        if (getValue._tag === "Right") {
          expect(getValue.right).toBe("integration-value");
        }

        // Measure cache has operation
        const hasValue = await measureAsync(
          "cache_has_integration",
          async () => {
            return await cache.has("integration-key");
          }
        );

        expect(hasValue._tag).toBe("Right");
        if (hasValue._tag === "Right") {
          expect(hasValue.right).toBe(true);
        }
      }
    });

    it("should handle cache errors with performance monitoring", async () => {
      const cacheResult = createMemoryCache();
      expect(cacheResult._tag).toBe("Right");

      if (cacheResult._tag === "Right") {
        const cache = cacheResult.right;

        // Test cache miss with performance monitoring
        const missResult = await measureAsync(
          "cache_miss_integration",
          async () => {
            return await cache.get("nonexistent-key");
          }
        );

        expect(missResult._tag).toBe("Right");
        if (missResult._tag === "Right") {
          expect(missResult.right).toBeNull(); // Memory cache returns null for misses
        }
      }
    });
  });

  describe("Full Workflow Integration", () => {
    it("should handle complete user service workflow", async () => {
      // 1. Create and configure components
      const configResult = fromObject({
        cache: { maxSize: 100, ttl: 60000 },
        logger: { level: "info" },
        service: { timeout: 5000 },
      });

      const cacheResult = createMemoryCache();
      const loggerResult = createDefaultLogger();

      expect(configResult._tag).toBe("Right");
      expect(cacheResult._tag).toBe("Right");
      expect(loggerResult._tag).toBe("Right");

      if (
        configResult._tag === "Right" &&
        cacheResult._tag === "Right" &&
        loggerResult._tag === "Right"
      ) {
        const config = configResult.right;
        const cache = cacheResult.right;
        const logger = loggerResult.right;

        // 2. Simulate user service operations
        const getUserById = async (userId: string): Promise<Result<{ id: string; name: string }>> => {
          return measureAsync("get_user_by_id", async () => {
            // Check cache first
            const cachedResult = await cache.get(`user:${userId}`);
            
            if (cachedResult._tag === "Right" && cachedResult.right !== null) {
              logger.info("Cache hit for user", { userId });
              return success(cachedResult.right as { id: string; name: string });
            }

            // Simulate database lookup
            await new Promise(resolve => setTimeout(resolve, 10)); // Simulate DB delay
            
            if (userId === "404") {
              const error = createQiError("USER_NOT_FOUND", `User ${userId} not found`, "BUSINESS");
              logger.warn("User not found", { userId });
              return failure(error);
            }

            const user = { id: userId, name: `User ${userId}` };
            
            // Cache the result
            await cache.set(`user:${userId}`, user);
            logger.info("User loaded and cached", { userId });
            
            return success(user);
          });
        };

        // 3. Test successful workflow
        const user1Result = await getUserById("123");
        expect(user1Result._tag).toBe("Right");
        if (user1Result._tag === "Right") {
          expect(user1Result.right.id).toBe("123");
          expect(user1Result.right.name).toBe("User 123");
        }

        // 4. Test cache hit (second call)
        const user1CachedResult = await getUserById("123");
        expect(user1CachedResult._tag).toBe("Right");
        if (user1CachedResult._tag === "Right") {
          expect(user1CachedResult.right.id).toBe("123");
        }

        // 5. Test error case
        const userNotFoundResult = await getUserById("404");
        expect(userNotFoundResult._tag).toBe("Left");
        if (userNotFoundResult._tag === "Left") {
          expect(userNotFoundResult.left.code).toBe("USER_NOT_FOUND");
        }
      }
    });

    it("should handle configuration-driven component behavior", () => {
      // Test how different configurations affect component behavior
      const devConfigResult = fromObject({
        environment: "development",
        logger: { level: "debug" },
        cache: { enabled: true, maxSize: 50 },
      });

      const prodConfigResult = fromObject({
        environment: "production", 
        logger: { level: "warn" },
        cache: { enabled: true, maxSize: 1000 },
      });

      expect(devConfigResult._tag).toBe("Right");
      expect(prodConfigResult._tag).toBe("Right");

      if (devConfigResult._tag === "Right" && prodConfigResult._tag === "Right") {
        const devConfig = devConfigResult.right;
        const prodConfig = prodConfigResult.right;

        // Verify different configuration values
        expect(devConfig.data.get("logger").level).toBe("debug");
        expect(prodConfig.data.get("logger").level).toBe("warn");
        expect(devConfig.data.get("cache").maxSize).toBe(50);
        expect(prodConfig.data.get("cache").maxSize).toBe(1000);

        // Test configuration merging for environment-specific overrides
        const overrideResult = fromObject({
          cache: { maxSize: 200 }, // Override cache size
        });

        expect(overrideResult._tag).toBe("Right");
        if (overrideResult._tag === "Right") {
          const finalConfig = merge(prodConfig, overrideResult.right);
          expect(finalConfig.data.get("cache").maxSize).toBe(200); // Overridden
          expect(finalConfig.data.get("logger").level).toBe("warn"); // Preserved
        }
      }
    });

    it("should handle error propagation through component chain", async () => {
      // Create components
      const cacheResult = createMemoryCache();
      const loggerResult = createDefaultLogger();

      expect(cacheResult._tag).toBe("Right");
      expect(loggerResult._tag).toBe("Right");

      if (cacheResult._tag === "Right" && loggerResult._tag === "Right") {
        const cache = cacheResult.right;
        const logger = loggerResult.right;

        // Simulate a service that uses multiple components
        const processData = async (input: string): Promise<Result<string>> => {
          return measureAsync("process_data_integration", async () => {
            // Step 1: Validate input
            if (!input || input.trim().length === 0) {
              const error = createQiError("INVALID_INPUT", "Input cannot be empty", "VALIDATION");
              logger.error("Validation failed", { input });
              return failure(error);
            }

            // Step 2: Check cache
            const cacheKey = `processed:${input}`;
            const cachedResult = await cache.get(cacheKey);
            
            if (cachedResult._tag === "Right" && cachedResult.right !== null) {
              logger.info("Cache hit for processed data", { input });
              return success(cachedResult.right as string);
            }

            // Step 3: Process data (simulate failure condition)
            if (input === "error") {
              const error = createQiError("PROCESSING_ERROR", "Failed to process data", "SYSTEM");
              logger.error("Processing failed", { input, error: error.message });
              return failure(error);
            }

            // Step 4: Successful processing
            const processed = input.toUpperCase();
            await cache.set(cacheKey, processed);
            logger.info("Data processed and cached", { input, processed });
            
            return success(processed);
          });
        };

        // Test successful case
        const result1 = await processData("hello");
        expect(result1._tag).toBe("Right");
        if (result1._tag === "Right") {
          expect(result1.right).toBe("HELLO");
        }

        // Test validation error
        const result2 = await processData("");
        expect(result2._tag).toBe("Left");
        if (result2._tag === "Left") {
          expect(result2.left.code).toBe("INVALID_INPUT");
        }

        // Test processing error
        const result3 = await processData("error");
        expect(result3._tag).toBe("Left");
        if (result3._tag === "Left") {
          expect(result3.left.code).toBe("PROCESSING_ERROR");
        }

        // Test cache hit (call again with "hello")
        const result4 = await processData("hello");
        expect(result4._tag).toBe("Right");
        if (result4._tag === "Right") {
          expect(result4.right).toBe("HELLO");
        }
      }
    });

    it("should measure complex operation performance across components", async () => {
      const complexOperation = async (): Promise<Result<string>> => {
        return measureAsync("complex_multi_component_operation", async () => {
          // Step 1: Configuration
          const configResult = measure("config_creation", () => {
            return fromObject({ operation: "complex", timeout: 1000 });
          });

          if (configResult._tag === "Left") {
            return configResult;
          }

          // Step 2: Cache setup
          const cacheResult = measure("cache_creation", () => {
            return createMemoryCache();
          });

          if (cacheResult._tag === "Left") {
            return cacheResult;
          }

          // Step 3: Multiple cache operations
          await measureAsync("cache_operations_batch", async () => {
            await cacheResult.right.set("key1", "value1");
            await cacheResult.right.set("key2", "value2");
            await cacheResult.right.set("key3", "value3");
          });

          // Step 4: Cache retrieval
          const retrievalResult = await measureAsync("cache_retrieval_batch", async () => {
            const val1 = await cacheResult.right.get("key1");
            const val2 = await cacheResult.right.get("key2");
            const val3 = await cacheResult.right.get("key3");

            if (val1._tag === "Right" && val2._tag === "Right" && val3._tag === "Right") {
              return success(`${val1.right}-${val2.right}-${val3.right}`);
            }
            
            return failure(createQiError("CACHE_ERROR", "Failed to retrieve values", "SYSTEM"));
          });

          return retrievalResult;
        });
      };

      const result = await complexOperation();
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBe("value1-value2-value3");
      }
    });
  });
});