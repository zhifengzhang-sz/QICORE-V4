/**
 * @fileoverview QiAgent Component Tests - Production Quality Test Suite
 * @purpose Comprehensive testing of AI agent interactions, resilience patterns, and performance
 * @dependencies Vitest, Fast-Check, QiCore Base, QiAgent
 */

import { performance } from 'node:perf_hooks';
import {
  type AgentConfig,
  type AgentProvider,
  createClaudeCodeAgent,
  createLocalAgent,
  createOpenAIAgent,
  validateGenerationRequest,
} from '@/qiagent/index';
import { getData, getError, isFailure, isSuccess } from '@/qicore/base/result';
import fc from 'fast-check';
import { describe, expect, it, vi } from 'vitest';
import type { GenerationRequest } from '../../src/modules/types';

// ============================================================================
// TEST UTILITIES & FIXTURES
// ============================================================================

const createMockGenerationRequest = (): GenerationRequest => ({
  model: {
    id: 'claude-3.5',
    name: 'Claude 3.5 Sonnet',
    provider: 'anthropic',
    modelName: 'claude-3-5-sonnet-20241022',
    temperature: 0.7,
    maxTokens: 4000,
  },
  systemPrompt: 'You are an expert Haskell programmer.',
  userPrompt: 'Implement a pure function for list reversal.',
});

const createMockAgentConfig = (provider: AgentProvider): AgentConfig => ({
  provider,
  timeout: 30000,
  maxRetries: 3,
  rateLimit: 10,
  authentication: {
    apiKey: provider === 'local' ? undefined : 'test-key',
    endpoint: provider === 'local' ? 'http://localhost:11434' : undefined,
  },
  circuitBreaker: {
    failureThreshold: 5,
    timeout: 30000,
    monitoringPeriod: 60000,
  },
  retryBackoff: {
    initialDelay: 1000,
    maxDelay: 30000,
    multiplier: 2,
    jitter: true,
  },
});

// Mock network delay for testing
const _delay = (ms: number): Promise<void> => new Promise((resolve) => setTimeout(resolve, ms));

// ============================================================================
// UNIT TESTS - VALIDATION
// ============================================================================

describe('QiAgent - Validation', () => {
  // biome-ignore lint/nursery/noSecrets: Test function name, not a secret
  describe('validateGenerationRequest', () => {
    it('should accept valid generation request', () => {
      const request = createMockGenerationRequest();
      const result = validateGenerationRequest(request);

      expect(isSuccess(result)).toBe(true);
      expect(getData(result)).toEqual(request);
    });

    it('should reject empty user prompt', () => {
      const request = { ...createMockGenerationRequest(), userPrompt: '' };
      const result = validateGenerationRequest(request);

      expect(isFailure(result)).toBe(true);
      const error = getError(result);
      expect(error?.code).toBe('INVALID_REQUEST');
      expect(error?.message).toContain('User prompt is required');
    });

    it('should reject missing model configuration', () => {
      const request = {
        ...createMockGenerationRequest(),
        model: undefined as unknown as GenerationRequest['model'],
      };
      const result = validateGenerationRequest(request);

      expect(isFailure(result)).toBe(true);
      const error = getError(result);
      expect(error?.code).toBe('INVALID_MODEL');
    });

    it('should reject invalid temperature', () => {
      const request: GenerationRequest = {
        ...createMockGenerationRequest(),
        model: {
          ...createMockGenerationRequest().model,
          temperature: 3.0, // Invalid - too high
        },
      };
      const result = validateGenerationRequest(request);

      expect(isFailure(result)).toBe(true);
      const error = getError(result);
      expect(error?.code).toBe('INVALID_TEMPERATURE');
    });

    it('should reject invalid max tokens', () => {
      const request: GenerationRequest = {
        ...createMockGenerationRequest(),
        model: {
          ...createMockGenerationRequest().model,
          maxTokens: 20000, // Invalid - too high
        },
      };
      const result = validateGenerationRequest(request);

      expect(isFailure(result)).toBe(true);
      const error = getError(result);
      expect(error?.code).toBe('INVALID_MAX_TOKENS');
    });
  });
});

// ============================================================================
// UNIT TESTS - AGENT CREATION
// ============================================================================

describe('QiAgent - Agent Creation', () => {
  // biome-ignore lint/nursery/noSecrets: Test function name, not a secret
  describe('createClaudeCodeAgent', () => {
    it('should create agent with valid configuration', () => {
      const config = createMockAgentConfig('claude-code');
      const agent = createClaudeCodeAgent(config);

      expect(agent).toBeDefined();
      expect(agent.generate).toBeInstanceOf(Function);
      expect(agent.validateConfiguration).toBeInstanceOf(Function);
      expect(agent.getProviderInfo).toBeInstanceOf(Function);
      expect(agent.getMetrics).toBeInstanceOf(Function);
      expect(agent.getHealthStatus).toBeInstanceOf(Function);
    });

    it('should provide correct provider info', () => {
      const config = createMockAgentConfig('claude-code');
      const agent = createClaudeCodeAgent(config);
      const info = agent.getProviderInfo();

      expect(info.provider).toBe('claude-code');
      expect(info.version).toBe('2.0.0');
      expect(info.capabilities).toContain('code-generation');
      expect(info.modelSupport).toContain('claude-3-5-sonnet-20241022');
    });

    it('should validate configuration correctly', () => {
      const config = createMockAgentConfig('claude-code');
      const agent = createClaudeCodeAgent(config);
      const result = agent.validateConfiguration();

      expect(isSuccess(result)).toBe(true);
    });

    it('should reject invalid provider in config', () => {
      const config = createMockAgentConfig('openai'); // Wrong provider
      const agent = createClaudeCodeAgent(config);
      const result = agent.validateConfiguration();

      expect(isFailure(result)).toBe(true);
      const error = getError(result);
      expect(error?.code).toBe('INVALID_PROVIDER');
    });

    it('should start with healthy status', () => {
      const config = createMockAgentConfig('claude-code');
      const agent = createClaudeCodeAgent(config);
      const health = agent.getHealthStatus();

      expect(health.status).toBe('healthy');
    });

    it('should start with zero metrics', () => {
      const config = createMockAgentConfig('claude-code');
      const agent = createClaudeCodeAgent(config);
      const metrics = agent.getMetrics();

      expect(metrics.requestCount).toBe(0);
      expect(metrics.successCount).toBe(0);
      expect(metrics.failureCount).toBe(0);
    });
  });

  // biome-ignore lint/nursery/noSecrets: Test function name, not a secret
  describe('createOpenAIAgent', () => {
    it('should create agent with valid configuration', () => {
      const config = createMockAgentConfig('openai');
      const agent = createOpenAIAgent(config);

      expect(agent).toBeDefined();
      expect(agent.getProviderInfo().provider).toBe('openai');
    });
  });

  describe('createLocalAgent', () => {
    it('should create agent with valid configuration', () => {
      const config = createMockAgentConfig('local');
      const agent = createLocalAgent(config);

      expect(agent).toBeDefined();
      expect(agent.getProviderInfo().provider).toBe('local');
    });

    it('should validate endpoint requirement', () => {
      const config = { ...createMockAgentConfig('local'), authentication: { endpoint: undefined } };
      const agent = createLocalAgent(config);
      const result = agent.validateConfiguration();

      expect(isFailure(result)).toBe(true);
      const error = getError(result);
      expect(error?.code).toBe('MISSING_ENDPOINT');
    });
  });
});

// ============================================================================
// INTEGRATION TESTS - CIRCUIT BREAKER
// ============================================================================

describe('QiAgent - Circuit Breaker Integration', () => {
  it('should handle multiple failures and open circuit', async () => {
    // Mock the Anthropic SDK to always fail
    const _mockAnthropicCreate = vi.fn().mockRejectedValue(new Error('Service unavailable'));

    const config: AgentConfig = {
      ...createMockAgentConfig('claude-code'),
      circuitBreaker: { failureThreshold: 2, timeout: 1000, monitoringPeriod: 5000 },
    };

    const agent = createClaudeCodeAgent(config);
    const request = createMockGenerationRequest();

    // Make multiple requests that should fail
    const _results = await Promise.all([
      agent.generate(request),
      agent.generate(request),
      agent.generate(request), // This should fail fast due to circuit breaker
    ]);

    const metrics = agent.getMetrics();
    expect(metrics.failureCount).toBeGreaterThan(0);
    expect(metrics.circuitBreakerTrips).toBeGreaterThan(0);

    const health = agent.getHealthStatus();
    expect(health.status).not.toBe('healthy');
  });
});

// ============================================================================
// INTEGRATION TESTS - RATE LIMITING
// ============================================================================

describe('QiAgent - Rate Limiting Integration', () => {
  it('should enforce rate limits', async () => {
    const config: AgentConfig = {
      ...createMockAgentConfig('claude-code'),
      rateLimit: 2, // Very low limit for testing
    };

    const agent = createClaudeCodeAgent(config);
    const request = createMockGenerationRequest();

    // Make requests rapidly to exceed rate limit
    const results = await Promise.all([
      agent.generate(request),
      agent.generate(request),
      agent.generate(request), // This should be rate limited
    ]);

    // At least one should be rate limited
    const rateLimitedResults = results.filter(
      (result) => isFailure(result) && getError(result)?.code === 'RATE_LIMITED'
    );

    expect(rateLimitedResults.length).toBeGreaterThan(0);

    const metrics = agent.getMetrics();
    expect(metrics.rateLimitHits).toBeGreaterThan(0);
  });
});

// ============================================================================
// PROPERTY-BASED TESTS
// ============================================================================

describe('QiAgent - Property-Based Tests', () => {
  describe('Agent Configuration Properties', () => {
    it('should create valid agents for any supported provider', () => {
      fc.assert(
        fc.property(
          fc.constantFrom('claude-code' as const, 'openai' as const, 'local' as const),
          fc.integer({ min: 5000, max: 300000 }), // timeout
          fc.integer({ min: 1, max: 10 }), // maxRetries
          (provider: AgentProvider, timeout: number, maxRetries: number) => {
            const baseConfig = createMockAgentConfig(provider);
            const config: AgentConfig = {
              ...baseConfig,
              timeout,
              maxRetries,
            };

            let agent: ReturnType<typeof createClaudeCodeAgent>;
            switch (provider) {
              case 'claude-code':
                agent = createClaudeCodeAgent(config);
                break;
              case 'openai':
                agent = createOpenAIAgent(config);
                break;
              case 'local':
                agent = createLocalAgent(config);
                break;
              case 'anthropic':
              case 'bedrock':
              case 'vertex':
                throw new Error(`Provider ${provider} not implemented in tests`);
              default:
                throw new Error(`Unsupported provider: ${provider}`);
            }

            expect(agent).toBeDefined();
            expect(agent.getProviderInfo().provider).toBe(provider);
            expect(typeof agent.generate).toBe('function');
            expect(typeof agent.validateConfiguration).toBe('function');
          }
        )
      );
    });
  });
});

// ============================================================================
// PERFORMANCE TESTS
// ============================================================================

describe('QiAgent - Performance Tests', () => {
  it('should create agents within performance requirements', () => {
    const startTime = performance.now();

    const config = createMockAgentConfig('claude-code');
    const agent = createClaudeCodeAgent(config);

    const endTime = performance.now();
    const creationTime = endTime - startTime;

    // Should create agent in < 10ms (specification requirement)
    expect(creationTime).toBeLessThan(10);
    expect(agent).toBeDefined();
  });

  it('should validate requests within performance requirements', () => {
    const request = createMockGenerationRequest();

    const startTime = performance.now();
    const result = validateGenerationRequest(request);
    const endTime = performance.now();

    const validationTime = endTime - startTime;

    // Should validate in < 1ms (specification requirement)
    expect(validationTime).toBeLessThan(1);
    expect(isSuccess(result)).toBe(true);
  });

  it('should handle concurrent validation efficiently', async () => {
    const request = createMockGenerationRequest();
    const concurrentRequests = 100;

    const startTime = performance.now();

    const results = await Promise.all(
      new Array(concurrentRequests).fill(null).map(() => validateGenerationRequest(request))
    );

    const endTime = performance.now();
    const totalTime = endTime - startTime;
    const avgTime = totalTime / concurrentRequests;

    expect(results.every(isSuccess)).toBe(true);
    expect(avgTime).toBeLessThan(0.1); // Should average < 0.1ms per validation
  });
});

// ============================================================================
// ERROR HANDLING TESTS
// ============================================================================

describe('QiAgent - Error Handling', () => {
  it('should properly categorize different error types', async () => {
    const config = createMockAgentConfig('claude-code');
    const agent = createClaudeCodeAgent(config);

    // Test with invalid request
    const invalidRequest = { ...createMockGenerationRequest(), userPrompt: '' };
    const result = await agent.generate(invalidRequest);

    expect(isFailure(result)).toBe(true);
    const error = getError(result);
    expect(error?.category).toBe('VALIDATION');
  });

  it('should provide detailed error context', async () => {
    const request: GenerationRequest = {
      ...createMockGenerationRequest(),
      model: {
        ...createMockGenerationRequest().model,
        temperature: 5.0, // Invalid
      },
    };

    const result = validateGenerationRequest(request);

    expect(isFailure(result)).toBe(true);
    const error = getError(result);
    expect(error?.context).toBeDefined();

    // QiCore context is a Map, not an object
    const context = error?.context as Map<string, unknown>;
    expect(context).toBeInstanceOf(Map);
    expect(context.has('temperature')).toBe(true);
    expect(context.get('temperature')).toBe(5.0);
  });
});
