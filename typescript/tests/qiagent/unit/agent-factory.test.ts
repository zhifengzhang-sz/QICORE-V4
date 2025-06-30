/**
 * @fileoverview QiAgent Factory Unit Tests
 * @purpose Test agent creation and configuration validation
 */

import { describe, expect, it } from "vitest";
import {
  type AgentConfig,
  type AgentProvider,
  QiAgent,
  createAgent,
  createClaudeCodeConfig,
  createLocalConfig,
  createOpenAIConfig,
  validateGenerationRequest,
} from "../../../src/qiagent/index.js";
import type { GenerationRequest, ModelConfig } from "../../../src/qiagent/types.js";
import { isFailure, isSuccess } from "../../../src/qicore/base/result.js";

describe("QiAgent Factory", () => {
  describe("createAgent", () => {
    it("should create Claude Code agent with valid config", () => {
      const config = createClaudeCodeConfig("test-api-key");
      const result = createAgent(config);

      expect(isSuccess(result)).toBe(true);

      if (result._tag === "Right") {
        const agent = result.right;
        expect(typeof agent.generate).toBe("function");
        expect(typeof agent.validateConfiguration).toBe("function");
        expect(typeof agent.getProviderInfo).toBe("function");
        expect(typeof agent.getMetrics).toBe("function");
        expect(typeof agent.getHealthStatus).toBe("function");

        const providerInfo = agent.getProviderInfo();
        expect(providerInfo.provider).toBe("claude-code");
        expect(providerInfo.version).toBe("2.0.0");
        expect(providerInfo.capabilities).toContain("code-generation");
      }
    });

    it("should create OpenAI agent with valid config", () => {
      const config = createOpenAIConfig("test-api-key");
      const result = createAgent(config);

      expect(isSuccess(result)).toBe(true);

      if (result._tag === "Right") {
        const agent = result.right;
        const providerInfo = agent.getProviderInfo();
        expect(providerInfo.provider).toBe("openai");
        expect(providerInfo.capabilities).toContain("function-calling");
      }
    });

    it("should create local agent with valid config", () => {
      const config = createLocalConfig("http://localhost:11434");
      const result = createAgent(config);

      expect(isSuccess(result)).toBe(true);

      if (result._tag === "Right") {
        const agent = result.right;
        const providerInfo = agent.getProviderInfo();
        expect(providerInfo.provider).toBe("local");
      }
    });

    it("should fail with unknown provider", () => {
      const config: AgentConfig = {
        provider: "unknown" as AgentProvider,
        timeout: 30000,
        maxRetries: 3,
        authentication: {},
      };

      const result = createAgent(config);

      expect(isFailure(result)).toBe(true);

      if (result._tag === "Left") {
        expect(result.left.code).toBe("UNKNOWN_PROVIDER");
        expect(result.left.category).toBe("VALIDATION");
      }
    });

    it("should fail with not implemented providers", () => {
      const config: AgentConfig = {
        provider: "bedrock",
        timeout: 30000,
        maxRetries: 3,
        authentication: {},
      };

      const result = createAgent(config);

      expect(isFailure(result)).toBe(true);

      if (result._tag === "Left") {
        expect(result.left.code).toBe("NOT_IMPLEMENTED");
        expect(result.left.category).toBe("SYSTEM");
      }
    });
  });

  describe("Configuration Factories", () => {
    it("should create Claude Code config with defaults", () => {
      const config = createClaudeCodeConfig("test-key");

      expect(config.provider).toBe("claude-code");
      expect(config.timeout).toBe(30000);
      expect(config.maxRetries).toBe(3);
      expect(config.authentication.apiKey).toBe("test-key");
      expect(config.circuitBreaker).toBeDefined();
      expect(config.retryBackoff).toBeDefined();
    });

    it("should create OpenAI config with defaults", () => {
      const config = createOpenAIConfig("test-key");

      expect(config.provider).toBe("openai");
      expect(config.timeout).toBe(30000);
      expect(config.maxRetries).toBe(3);
      expect(config.authentication.apiKey).toBe("test-key");
    });

    it("should create local config with endpoint", () => {
      const endpoint = "http://localhost:11434";
      const config = createLocalConfig(endpoint);

      expect(config.provider).toBe("local");
      expect(config.timeout).toBe(60000); // Higher timeout for local
      expect(config.maxRetries).toBe(2); // Fewer retries for local
      expect(config.authentication.endpoint).toBe(endpoint);
    });
  });

  describe("Request Validation", () => {
    const validRequest: GenerationRequest = {
      model: {
        id: "test-model",
        name: "Test Model",
        provider: "claude-code",
        modelName: "claude-3-5-sonnet-20241022",
        temperature: 0.7,
        maxTokens: 1000,
      },
      userPrompt: "Test prompt",
    };

    it("should validate valid request", () => {
      const result = validateGenerationRequest(validRequest);

      expect(isSuccess(result)).toBe(true);

      if (result._tag === "Right") {
        expect(result.right).toEqual(validRequest);
      }
    });

    it("should fail with empty user prompt", () => {
      const invalidRequest = {
        ...validRequest,
        userPrompt: "",
      };

      const result = validateGenerationRequest(invalidRequest);

      expect(isFailure(result)).toBe(true);

      if (result._tag === "Left") {
        expect(result.left.code).toBe("INVALID_REQUEST");
        expect(result.left.category).toBe("VALIDATION");
      }
    });

    it("should fail with missing model", () => {
      const invalidRequest = {
        ...validRequest,
        model: undefined as unknown as ModelConfig,
      };

      const result = validateGenerationRequest(invalidRequest);

      expect(isFailure(result)).toBe(true);

      if (result._tag === "Left") {
        expect(result.left.code).toBe("INVALID_MODEL");
      }
    });

    it("should fail with invalid temperature", () => {
      const invalidRequest = {
        ...validRequest,
        model: {
          ...validRequest.model,
          temperature: 3.0, // Invalid: > 2
        },
      };

      const result = validateGenerationRequest(invalidRequest);

      expect(isFailure(result)).toBe(true);

      if (result._tag === "Left") {
        expect(result.left.code).toBe("INVALID_TEMPERATURE");
      }
    });

    it("should fail with invalid max tokens", () => {
      const invalidRequest = {
        ...validRequest,
        model: {
          ...validRequest.model,
          maxTokens: 20000, // Invalid: > 10000
        },
      };

      const result = validateGenerationRequest(invalidRequest);

      expect(isFailure(result)).toBe(true);

      if (result._tag === "Left") {
        expect(result.left.code).toBe("INVALID_MAX_TOKENS");
      }
    });

    it("should accept valid temperature range", () => {
      const requests = [0, 0.5, 1.0, 1.5, 2.0].map((temp) => ({
        ...validRequest,
        model: { ...validRequest.model, temperature: temp },
      }));

      for (const request of requests) {
        const result = validateGenerationRequest(request);
        expect(isSuccess(result)).toBe(true);
      }
    });

    it("should accept valid max tokens range", () => {
      const requests = [1, 100, 1000, 4000, 10000].map((maxTokens) => ({
        ...validRequest,
        model: { ...validRequest.model, maxTokens },
      }));

      for (const request of requests) {
        const result = validateGenerationRequest(request);
        expect(isSuccess(result)).toBe(true);
      }
    });
  });

  describe("QiAgent Namespace", () => {
    it("should export all factory functions", () => {
      expect(typeof QiAgent.createAgent).toBe("function");
      expect(typeof QiAgent.createClaudeCodeAgent).toBe("function");
      expect(typeof QiAgent.createOpenAIAgent).toBe("function");
      expect(typeof QiAgent.createLocalAgent).toBe("function");
      expect(typeof QiAgent.createDefaultAgentConfig).toBe("function");
      expect(typeof QiAgent.createClaudeCodeConfig).toBe("function");
      expect(typeof QiAgent.createOpenAIConfig).toBe("function");
      expect(typeof QiAgent.createLocalConfig).toBe("function");
      expect(typeof QiAgent.validateGenerationRequest).toBe("function");
    });
  });
});
