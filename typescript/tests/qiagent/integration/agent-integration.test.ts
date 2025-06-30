/**
 * @fileoverview QiAgent Integration Tests
 * @purpose Test complete agent workflows and reliability patterns
 */

import { beforeEach, describe, expect, it } from "vitest";
import {
  type Agent,
  type GenerationRequest,
  QiAgent,
  createAgent,
  createClaudeCodeConfig,
  createOpenAIConfig,
} from "../../../src/qiagent/index.js";
import { withContext } from "../../../src/qicore/base/error.js";
import { isFailure, isSuccess } from "../../../src/qicore/base/result.js";

describe("QiAgent Integration Tests", () => {
  const validRequest: GenerationRequest = {
    model: {
      id: "test-model",
      name: "Test Model",
      provider: "test",
      modelName: "test-model-name",
      temperature: 0.7,
      maxTokens: 100,
    },
    systemPrompt: "You are a helpful assistant.",
    userPrompt: "Say hello.",
  };

  describe("Agent Creation and Configuration", () => {
    it("should create Claude Code agent with full configuration", () => {
      const config = createClaudeCodeConfig("test-api-key");

      // Add reliability configurations
      config.rateLimit = 5;
      config.circuitBreaker = {
        failureThreshold: 3,
        timeout: 5000,
        monitoringPeriod: 10000,
      };
      config.retryBackoff = {
        initialDelay: 500,
        maxDelay: 5000,
        multiplier: 2,
        jitter: true,
      };

      const agentResult = createAgent(config);

      expect(isSuccess(agentResult)).toBe(true);

      if (agentResult._tag === "Right") {
        const agent = agentResult.right;

        // Verify configuration is applied
        const validation = agent.validateConfiguration();
        expect(isSuccess(validation)).toBe(true);

        const providerInfo = agent.getProviderInfo();
        expect(providerInfo.provider).toBe("claude-code");
        expect(providerInfo.capabilities).toContain("code-generation");

        const metrics = agent.getMetrics();
        expect(metrics.requestCount).toBe(0);
        expect(metrics.successRate).toBe(0);

        const health = agent.getHealthStatus();
        expect(health.status).toBe("healthy");
      }
    });

    it("should create OpenAI agent with different configuration", () => {
      const config = createOpenAIConfig("test-api-key");
      config.timeout = 45000; // Different timeout
      config.maxRetries = 5; // More retries

      const agentResult = createAgent(config);

      expect(isSuccess(agentResult)).toBe(true);

      if (agentResult._tag === "Right") {
        const agent = agentResult.right;

        const providerInfo = agent.getProviderInfo();
        expect(providerInfo.provider).toBe("openai");
        expect(providerInfo.capabilities).toContain("function-calling");
      }
    });

    it("should validate configuration on agent creation", () => {
      // Invalid configuration - missing API key
      const config = createClaudeCodeConfig();
      config.authentication.apiKey = undefined;

      const agentResult = createAgent(config);

      expect(isSuccess(agentResult)).toBe(true); // Agent creation succeeds

      if (agentResult._tag === "Right") {
        const agent = agentResult.right;

        // But validation should fail
        const validation = agent.validateConfiguration();
        expect(isFailure(validation)).toBe(true);

        if (validation._tag === "Left") {
          expect(validation.left.code).toBe("MISSING_AUTH");
          expect(validation.left.category).toBe("SECURITY");
        }
      }
    });
  });

  describe("Generation Request Handling", () => {
    let mockAgent: Agent;

    beforeEach(() => {
      // Create a mock agent for testing (since we don't have real API keys)
      const config = createClaudeCodeConfig("mock-key");
      config.timeout = 2000; // Short timeout for tests
      const agentResult = createAgent(config);

      if (agentResult._tag === "Right") {
        mockAgent = agentResult.right;
      }
    });

    it("should validate generation requests before processing", async () => {
      if (!mockAgent) return;

      // Invalid request - empty prompt
      const invalidRequest: GenerationRequest = {
        ...validRequest,
        userPrompt: "",
      };

      // Note: This would fail at validation level before hitting the API
      // In a real scenario, we'd mock the API call
      const result = await mockAgent.generate(invalidRequest);

      // Should fail due to validation or API error (mock key)
      expect(isFailure(result)).toBe(true);

      if (result._tag === "Left") {
        // Could be validation error or auth error
        expect(["INVALID_REQUEST", "MISSING_AUTH", "GENERATION_FAILED"]).toContain(
          result.left.code
        );
      }
    });

    it("should handle API authentication errors gracefully", async () => {
      if (!mockAgent) return;

      // This will fail due to invalid API key
      const result = await mockAgent.generate(validRequest);

      expect(isFailure(result)).toBe(true);

      if (result._tag === "Left") {
        // Should be authentication or generation error
        expect(["MISSING_AUTH", "GENERATION_FAILED", "SERVICE_UNAVAILABLE"]).toContain(
          result.left.code
        );
      }
    }, 15000);

    it("should update metrics after generation attempts", async () => {
      if (!mockAgent) return;

      const initialMetrics = mockAgent.getMetrics();
      expect(initialMetrics.requestCount).toBe(0);

      // Make a generation request (will fail due to mock key)
      await mockAgent.generate(validRequest);

      const updatedMetrics = mockAgent.getMetrics();
      expect(updatedMetrics.requestCount).toBe(1);
      expect(updatedMetrics.failureCount).toBe(1);
      expect(updatedMetrics.successRate).toBe(0);
      expect(updatedMetrics.averageResponseTime).toBeGreaterThan(0);
    }, 15000);

    it("should update health status based on failures", async () => {
      if (!mockAgent) return;

      const initialHealth = mockAgent.getHealthStatus();
      expect(initialHealth.status).toBe("healthy");

      // Make multiple failed requests
      for (let i = 0; i < 5; i++) {
        await mockAgent.generate(validRequest);
      }

      const finalHealth = mockAgent.getHealthStatus();
      // Should be degraded or critical due to high failure rate
      expect(["degraded", "critical"]).toContain(finalHealth.status);
      expect(finalHealth.reason).toBeDefined();
    }, 60000);
  });

  describe("Multi-Provider Support", () => {
    it("should create multiple agents with different providers", async () => {
      const claudeConfig = createClaudeCodeConfig("test-claude-key");
      const openaiConfig = createOpenAIConfig("test-openai-key");

      const claudeResult = createAgent(claudeConfig);
      const openaiResult = createAgent(openaiConfig);

      expect(isSuccess(claudeResult)).toBe(true);
      expect(isSuccess(openaiResult)).toBe(true);

      if (claudeResult._tag === "Right" && openaiResult._tag === "Right") {
        const claudeAgent = claudeResult.right;
        const openaiAgent = openaiResult.right;

        const claudeInfo = claudeAgent.getProviderInfo();
        const openaiInfo = openaiAgent.getProviderInfo();

        expect(claudeInfo.provider).toBe("claude-code");
        expect(openaiInfo.provider).toBe("openai");

        // Should have different capabilities
        expect(claudeInfo.capabilities).toContain("reasoning");
        expect(openaiInfo.capabilities).toContain("function-calling");
      }
    });

    it("should handle provider-specific configurations", () => {
      const claudeConfig = createClaudeCodeConfig("test-key");
      const openaiConfig = createOpenAIConfig("test-key");

      // Claude Code typically has different defaults
      expect(claudeConfig.timeout).toBe(30000);
      expect(openaiConfig.timeout).toBe(30000);

      // But should be configurable differently
      claudeConfig.maxRetries = 3;
      openaiConfig.maxRetries = 5;

      expect(claudeConfig.maxRetries).toBe(3);
      expect(openaiConfig.maxRetries).toBe(5);
    });
  });

  describe("Error Recovery and Resilience", () => {
    it("should provide detailed error context", async () => {
      const config = createClaudeCodeConfig("invalid-key");
      config.timeout = 2000;
      const agentResult = createAgent(config);

      if (agentResult._tag === "Right") {
        const agent = agentResult.right;
        const result = await agent.generate(validRequest);

        expect(isFailure(result)).toBe(true);

        if (result._tag === "Left") {
          const error = result.left;

          // Should have structured error information
          expect(error.code).toBeDefined();
          expect(error.message).toBeDefined();
          expect(error.category).toBeDefined();
          expect(error.timestamp).toBeGreaterThan(0);

          // Should have context information
          expect(error.context.size).toBeGreaterThan(0);

          // Should be serializable
          const structured = error.toStructuredData();
          expect(structured.code).toBe(error.code);
          expect(structured.message).toBe(error.message);
          expect(structured.category).toBe(error.category);
        }
      }
    }, 15000);

    it("should maintain error context through reliability patterns", async () => {
      const config = createClaudeCodeConfig("invalid-key");
      config.timeout = 2000;
      config.maxRetries = 2; // Will retry failures

      const agentResult = createAgent(config);

      if (agentResult._tag === "Right") {
        const agent = agentResult.right;
        const result = await agent.generate(validRequest);

        expect(isFailure(result)).toBe(true);

        if (result._tag === "Left") {
          // Error should include context about retries
          const error = result.left;
          expect(error.toString()).toBeDefined();

          // Should be able to chain additional context
          const enrichedError = withContext(error, {
            experimentalContext: "integration-test",
            timestamp: Date.now(),
          });

          expect(enrichedError.context.get("experimentalContext")).toBe("integration-test");
        }
      }
    }, 15000);
  });

  describe("Performance and Monitoring", () => {
    it("should track performance metrics accurately", async () => {
      const config = createClaudeCodeConfig("test-key");
      config.timeout = 2000;
      const agentResult = createAgent(config);

      if (agentResult._tag === "Right") {
        const agent = agentResult.right;

        // Make multiple requests to build metrics
        const requests = Array.from({ length: 3 }, () => agent.generate(validRequest));
        await Promise.all(requests);

        const metrics = agent.getMetrics();

        expect(metrics.requestCount).toBe(3);
        expect(metrics.failureCount).toBe(3); // All fail due to mock key
        expect(metrics.successCount).toBe(0);
        expect(metrics.successRate).toBe(0);
        expect(metrics.averageResponseTime).toBeGreaterThan(0);
      }
    }, 15000);

    it("should provide health status based on performance", async () => {
      const config = createClaudeCodeConfig("test-key");
      config.timeout = 2000;
      const agentResult = createAgent(config);

      if (agentResult._tag === "Right") {
        const agent = agentResult.right;

        // Initial health should be healthy
        let health = agent.getHealthStatus();
        expect(health.status).toBe("healthy");

        // Make some failing requests
        for (let i = 0; i < 10; i++) {
          await agent.generate(validRequest);
        }

        // Health should degrade
        health = agent.getHealthStatus();
        expect(["degraded", "critical"]).toContain(health.status);
        expect(health.reason).toContain("Success rate");
      }
    }, 60000);

    it("should handle concurrent requests safely", async () => {
      const config = createClaudeCodeConfig("test-key");
      config.timeout = 2000;
      const agentResult = createAgent(config);

      if (agentResult._tag === "Right") {
        const agent = agentResult.right;

        // Make 5 concurrent requests
        const promises = Array.from({ length: 5 }, () => agent.generate(validRequest));
        const results = await Promise.all(promises);

        // All should complete (though fail due to mock key)
        expect(results).toHaveLength(5);
        for (const result of results) {
          expect(isFailure(result)).toBe(true);
        }

        const metrics = agent.getMetrics();
        expect(metrics.requestCount).toBe(5);
      }
    }, 15000);
  });

  describe("QiAgent Namespace Integration", () => {
    it("should provide consistent interface through namespace", () => {
      // Test that all factory methods work through QiAgent namespace
      expect(typeof QiAgent.createAgent).toBe("function");
      expect(typeof QiAgent.createClaudeCodeAgent).toBe("function");
      expect(typeof QiAgent.createOpenAIAgent).toBe("function");
      expect(typeof QiAgent.createLocalAgent).toBe("function");

      const config = QiAgent.createClaudeCodeConfig("test-key");
      const agentResult = QiAgent.createAgent(config);

      expect(isSuccess(agentResult)).toBe(true);

      if (agentResult._tag === "Right") {
        const agent = agentResult.right;
        expect(agent.getProviderInfo().provider).toBe("claude-code");
      }
    });

    it("should validate requests consistently", () => {
      const validationResult = QiAgent.validateGenerationRequest(validRequest);
      expect(isSuccess(validationResult)).toBe(true);

      const invalidRequest = { ...validRequest, userPrompt: "" };
      const invalidResult = QiAgent.validateGenerationRequest(invalidRequest);
      expect(isFailure(invalidResult)).toBe(true);
    });
  });
});
