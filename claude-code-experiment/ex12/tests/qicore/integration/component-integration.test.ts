/**
 * QiCore v4.0 - Component Integration Tests
 * 
 * Tests that verify all components work together correctly
 */

import { describe, it, expect, beforeEach, afterEach } from "vitest";
import {
  success,
  failure,
  map,
  flatMap,
  fold
} from "../../../src/qicore/base/result.js";
import { createQiError, ErrorCategory, withContext } from "../../../src/qicore/base/error.js";
import {
  fromObject,
  merge,
  loadConfiguration,
  get
} from "../../../src/qicore/core/config.js";
import { createDefault as createLogger } from "../../../src/qicore/core/logger.js";
import { createMemoryCache } from "../../../src/qicore/core/cache.js";
import {
  measure,
  measureAsync,
  benchmark,
  getStats,
  clearMeasurements
} from "../../../src/qicore/core/performance.js";
import { promises as fs } from "fs";

describe("Component Integration", () => {
  const tempConfigFile = "/tmp/qicore-integration-test.json";

  beforeEach(() => {
    clearMeasurements();
  });

  afterEach(async () => {
    clearMeasurements();
    try {
      await fs.unlink(tempConfigFile);
    } catch {
      // File may not exist
    }
  });

  describe("Result + Error Integration", () => {
    it("chains operations with error context accumulation", () => {
      const processUser = (userData: Record<string, unknown>) => {
        // Validation step
        if (!userData.email) {
          return failure(createQiError(
            "MISSING_EMAIL",
            "Email is required",
            ErrorCategory.VALIDATION,
            { userData }
          ));
        }

        // Transform step
        return flatMap((data: Record<string, unknown>) => {
          if (!(data.normalizedEmail as string).includes("@")) {
            return failure(withContext(
              createQiError(
                "INVALID_EMAIL_FORMAT",
                "Email must contain @ symbol",
                ErrorCategory.VALIDATION
              ),
              { email: data.normalizedEmail, step: "normalization" }
            ));
          }
          return success(data);
        })(
          map((data: Record<string, unknown>) => ({
            ...data,
            normalizedEmail: (data.email as string).toLowerCase()
          }))(success(userData))
        );
      };

      // Test success case
      const validUser = { email: "Test@Example.com", name: "Test User" };
      const successResult = processUser(validUser);
      
      expect(successResult._tag).toBe("Right");
      if (successResult._tag === "Right") {
        expect(successResult.right.normalizedEmail).toBe("test@example.com");
      }

      // Test failure case with context
      const invalidUser = { email: "invalid-email", name: "Test User" };
      const failureResult = processUser(invalidUser);
      
      expect(failureResult._tag).toBe("Left");
      if (failureResult._tag === "Left") {
        expect(failureResult.left.code).toBe("INVALID_EMAIL_FORMAT");
        expect(failureResult.left.context.get("email")).toBe("invalid-email");
        expect(failureResult.left.context.get("step")).toBe("normalization");
      }

      // Test missing email case
      const missingEmailUser = { name: "Test User" };
      const missingResult = processUser(missingEmailUser);
      
      expect(missingResult._tag).toBe("Left");
      if (missingResult._tag === "Left") {
        expect(missingResult.left.code).toBe("MISSING_EMAIL");
      }
    });
  });

  describe("Config + Result Integration", () => {
    it("loads and validates configuration with error handling", async () => {
      // Create test config file
      const testConfig = {
        server: { port: 8080, host: "localhost" },
        database: { url: "postgres://localhost/test" },
        cache: { maxSize: 1000, ttl: 300000 }
      };
      await fs.writeFile(tempConfigFile, JSON.stringify(testConfig));

      // Set environment variables
      const originalEnv = process.env;
      process.env.INTEGRATION_TEST_SERVER_PORT = "9000";
      process.env.INTEGRATION_TEST_DEBUG = "true";

      try {
        const configResult = await loadConfiguration({
          defaults: { server: { workers: 4 } },
          configFile: tempConfigFile,
          envPrefix: "INTEGRATION_TEST",
          overrides: { app: { name: "QiCore Test" } }
        });

        expect(configResult._tag).toBe("Right");
        if (configResult._tag === "Right") {
          const config = configResult.right;
          
          // Verify configuration loading with precedence
          expect(get<number>(config, "server.port")).toEqual({ _tag: "Right", right: 9000 }); // From env
          expect(get<string>(config, "server.host")).toEqual({ _tag: "Right", right: "localhost" }); // From file
          expect(get<number>(config, "server.workers")).toEqual({ _tag: "Right", right: 4 }); // From defaults
          expect(get<boolean>(config, "debug")).toEqual({ _tag: "Right", right: true }); // From env
          expect(get<string>(config, "app.name")).toEqual({ _tag: "Right", right: "QiCore Test" }); // From overrides
        }
      } finally {
        process.env = originalEnv;
      }
    });

    it("validates configuration with custom schema", () => {
      const configResult = fromObject({
        port: 8080,
        host: "localhost",
        debug: true
      });

      expect(configResult._tag).toBe("Right");
      if (configResult._tag === "Right") {
        const config = configResult.right;
        
        // Extract and validate specific configuration
        const portResult = get<number>(config, "port");
        const hostResult = get<string>(config, "host");
        const debugResult = get<boolean>(config, "debug");

        const validateServerConfig = () => {
          if (portResult._tag === "Left") return portResult;
          if (hostResult._tag === "Left") return hostResult;
          if (debugResult._tag === "Left") return debugResult;

          const port = portResult.right;
          const host = hostResult.right;
          const debug = debugResult.right;

          if (port < 1 || port > 65535) {
            return failure(createQiError(
              "INVALID_PORT",
              "Port must be between 1 and 65535",
              ErrorCategory.VALIDATION,
              { port }
            ));
          }

          return success({ port, host, debug });
        };

        const validationResult = validateServerConfig();
        expect(validationResult._tag).toBe("Right");
      }
    });
  });

  describe("Cache + Logger Integration", () => {
    it("implements cached service with logging", async () => {
      const loggerResult = createLogger();
      const cacheResult = createMemoryCache<string, any>();

      expect(loggerResult._tag).toBe("Right");
      expect(cacheResult._tag).toBe("Right");

      const logger = loggerResult.right!;
      const cache = cacheResult.right!;

      // Simulate a service that fetches user data with caching and logging
      const fetchUserData = async (userId: string) => {
        logger.info("Attempting to fetch user data", { userId });

        // Check cache first
        const cacheResult = await cache.get(userId);
        if (cacheResult._tag === "Right") {
          logger.info("User data found in cache", { userId });
          return success(cacheResult.right);
        }

        logger.debug("Cache miss, fetching from database", { userId });
        
        // Simulate database fetch
        const userData = await measureAsync("database_fetch", async () => {
          await new Promise(resolve => setTimeout(resolve, 10)); // Simulate DB latency
          return { id: userId, name: `User ${userId}`, email: `user${userId}@example.com` };
        });

        // Cache the result
        const setCacheResult = await cache.set(userId, userData, 60000); // 1 minute TTL
        if (setCacheResult._tag === "Left") {
          logger.warn("Failed to cache user data", { userId, error: setCacheResult.left });
        } else {
          logger.debug("User data cached successfully", { userId });
        }

        logger.info("User data fetched successfully", { userId });
        return success(userData);
      };

      // Test the integrated service
      const user1Result = await fetchUserData("123");
      expect(user1Result._tag).toBe("Right");
      if (user1Result._tag === "Right") {
        expect(user1Result.right.id).toBe("123");
      }

      // Second call should hit cache
      const user1CachedResult = await fetchUserData("123");
      expect(user1CachedResult._tag).toBe("Right");

      // Verify performance measurements were recorded
      const dbFetchStats = getStats("database_fetch");
      expect(dbFetchStats._tag).toBe("Right");
    });
  });

  describe("Performance + All Components Integration", () => {
    it("benchmarks complete workflow with all components", async () => {
      const workflowResult = await benchmark(
        "complete_workflow",
        async () => {
          // 1. Load configuration
          const configResult = fromObject({
            cache: { maxSize: 100, ttl: 60000 },
            logging: { level: "info" }
          });

          if (configResult._tag === "Left") throw new Error("Config failed");

          // 2. Create logger
          const loggerResult = createLogger();
          if (loggerResult._tag === "Left") throw new Error("Logger failed");

          // 3. Create cache
          const cacheResult = createMemoryCache<string, number>();
          if (cacheResult._tag === "Left") throw new Error("Cache failed");

          const cache = cacheResult.right;

          // 4. Perform cached computation
          const computeValue = (key: string): Promise<number> =>
            measureAsync("computation", async () => {
              await new Promise(resolve => setTimeout(resolve, 1)); // Simulate work
              return parseInt(key) * 2;
            });

          const getCachedValue = async (key: string) => {
            const cacheResult = await cache.get(key);
            if (cacheResult._tag === "Right") {
              return success(cacheResult.right);
            }

            const computed = await computeValue(key);
            await cache.set(key, computed);
            return success(computed);
          };

          // 5. Process multiple values
          const results = await Promise.all([
            getCachedValue("42"),
            getCachedValue("84"),
            getCachedValue("42") // Should hit cache
          ]);

          return results.every(r => r._tag === "Right");
        },
        50 // Fewer iterations due to complexity
      );

      expect(workflowResult._tag).toBe("Right");
      if (workflowResult._tag === "Right") {
        expect(workflowResult.right.sampleCount).toBe(50);
        expect(workflowResult.right.mean).toBeGreaterThan(0);
        
        // Verify that computation was measured
        const computationStats = getStats("computation");
        expect(computationStats._tag).toBe("Right");
      }
    });
  });

  describe("Error Propagation Across Components", () => {
    it("propagates errors through component boundaries", async () => {
      // Create a workflow that can fail at multiple points
      const createApplication = async () => {
        // Step 1: Configuration (could fail)
        const configResult = fromObject({
          database: { url: "invalid://url" },
          cache: { maxSize: -1 } // Invalid
        });

        if (configResult._tag === "Left") return configResult;
        const config = configResult.right;

        // Step 2: Validate database URL
        const dbUrlResult = get<string>(config, "database.url");
        if (dbUrlResult._tag === "Left") return dbUrlResult;

        const dbUrl = dbUrlResult.right;
        if (!dbUrl.startsWith("postgres://") && !dbUrl.startsWith("mysql://")) {
          return failure(withContext(
            createQiError("INVALID_DB_URL", "Invalid database URL", ErrorCategory.CONFIGURATION),
            { url: dbUrl, step: "database_validation" }
          ));
        }

        // Step 3: Validate cache size
        const cacheSizeResult = get<number>(config, "cache.maxSize");
        if (cacheSizeResult._tag === "Left") return cacheSizeResult;

        const cacheSize = cacheSizeResult.right;
        if (cacheSize <= 0) {
          return failure(withContext(
            createQiError("INVALID_CACHE_SIZE", "Cache size must be positive", ErrorCategory.VALIDATION),
            { size: cacheSize, step: "cache_validation" }
          ));
        }

        return success({ dbUrl, cacheSize });
      };

      const appResult = await createApplication();
      
      expect(appResult._tag).toBe("Left");
      if (appResult._tag === "Left") {
        // Should fail at database URL validation
        expect(appResult.left.code).toBe("INVALID_DB_URL");
        expect(appResult.left.context.get("step")).toBe("database_validation");
      }
    });
  });

  describe("Resource Management Integration", () => {
    it("manages resources across component lifecycle", async () => {
      const resources = {
        logger: undefined as any,
        cache: undefined as any,
        cleanupCalled: false
      };

      try {
        // Initialize resources
        const loggerResult = createLogger();
        const cacheResult = createMemoryCache<string, string>();

        expect(loggerResult._tag).toBe("Right");
        expect(cacheResult._tag).toBe("Right");

        resources.logger = loggerResult.right;
        resources.cache = cacheResult.right;

        // Use resources together
        await resources.cache.set("test", "value");
        resources.logger.info("Cache initialized", { size: await resources.cache.size() });

        const value = await resources.cache.get("test");
        expect(value._tag).toBe("Right");

        // Verify resource state
        const cacheSize = await resources.cache.size();
        expect(cacheSize._tag).toBe("Right");
        if (cacheSize._tag === "Right") {
          expect(cacheSize.right).toBe(1);
        }

      } finally {
        // Cleanup resources
        if (resources.cache && 'destroy' in resources.cache) {
          resources.cache.destroy();
          resources.cleanupCalled = true;
        }
      }

      expect(resources.cleanupCalled).toBe(true);
    });
  });

  describe("Concurrent Operations", () => {
    it("handles concurrent operations across components safely", async () => {
      const loggerResult = createLogger();
      const cacheResult = createMemoryCache<string, number>();

      expect(loggerResult._tag).toBe("Right");
      expect(cacheResult._tag).toBe("Right");

      const logger = loggerResult.right!;
      const cache = cacheResult.right!;

      // Simulate concurrent operations
      const concurrentOperations = Array.from({ length: 10 }, (_, i) =>
        Promise.resolve().then(async () => {
          const key = `concurrent_${i}`;
          const value = i * 10;

          logger.debug("Starting concurrent operation", { key, value });

          // Measured cache operation
          const result = await measureAsync(`concurrent_cache_${i}`, async () => {
            await cache.set(key, value);
            return await cache.get(key);
          });

          logger.debug("Completed concurrent operation", { key, result: result._tag });
          return result;
        })
      );

      const results = await Promise.all(concurrentOperations);

      // All operations should succeed
      expect(results.every(r => r._tag === "Right")).toBe(true);

      // Cache should have all values
      const finalSize = await cache.size();
      expect(finalSize._tag).toBe("Right");
      if (finalSize._tag === "Right") {
        expect(finalSize.right).toBe(10);
      }

      // Performance measurements should be recorded
      for (let i = 0; i < 10; i++) {
        const stats = getStats(`concurrent_cache_${i}`);
        expect(stats._tag).toBe("Right");
      }
    });
  });

  describe("Complex Data Flow", () => {
    it("processes complex data flow through all components", async () => {
      // Setup components
      const loggerResult = createLogger();
      const cacheResult = createMemoryCache<string, any>();
      const configResult = fromObject({
        processing: { batchSize: 5, timeout: 1000 },
        cache: { ttl: 60000 }
      });

      expect(loggerResult._tag).toBe("Right");
      expect(cacheResult._tag).toBe("Right");
      expect(configResult._tag).toBe("Right");

      const logger = loggerResult.right!;
      const cache = cacheResult.right!;
      const config = configResult.right!;

      // Complex data processing pipeline
      const processDataBatch = async (data: number[]) => {
        logger.info("Processing data batch", { size: data.length });

        return fold(
          (error) => {
            logger.error("Batch processing failed", { error: error.code });
            return failure(error);
          },
          (batchSize: number) => {
            logger.debug("Using batch size", { batchSize });

            const processItem = async (item: number) => {
              const cacheKey = `processed_${item}`;
              
              // Check cache first
              const cached = await cache.get(cacheKey);
              if (cached._tag === "Right") {
                logger.debug("Cache hit", { item });
                return success(cached.right);
              }

              // Process item
              const processed = await measureAsync("item_processing", async () => {
                await new Promise(resolve => setTimeout(resolve, 1)); // Simulate processing
                return item * item;
              });

              // Cache result
              const ttlResult = get<number>(config, "cache.ttl");
              const ttl = ttlResult._tag === "Right" ? ttlResult.right : 60000;
              
              await cache.set(cacheKey, processed, ttl);
              logger.debug("Item processed and cached", { item, processed });
              
              return success(processed);
            };

            // Process batch
            return Promise.all(data.slice(0, batchSize).map(processItem))
              .then(results => {
                const allSuccessful = results.every(r => r._tag === "Right");
                if (allSuccessful) {
                  const values = results.map(r => r._tag === "Right" ? r.right : 0);
                  logger.info("Batch processed successfully", { processedCount: values.length });
                  return success(values);
                } else {
                  const firstError = results.find(r => r._tag === "Left");
                  return firstError || failure(createQiError("UNKNOWN", "Unknown error", ErrorCategory.SYSTEM));
                }
              });
          }
        )(get<number>(config, "processing.batchSize"));
      };

      // Test the pipeline
      const testData = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
      const result = await processDataBatch(testData);

      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toEqual([1, 4, 9, 16, 25]); // First 5 items squared
      }

      // Verify performance measurements
      const processingStats = getStats("item_processing");
      expect(processingStats._tag).toBe("Right");

      // Verify cache contains processed items
      const cacheSize = await cache.size();
      expect(cacheSize._tag).toBe("Right");
      if (cacheSize._tag === "Right") {
        expect(cacheSize.right).toBe(5);
      }

      // Test cache hits on second run
      const cachedResult = await processDataBatch(testData);
      expect(cachedResult._tag).toBe("Right");
      if (cachedResult._tag === "Right") {
        expect(cachedResult.right).toEqual([1, 4, 9, 16, 25]);
      }
    });
  });
});