# QiAgent v4.0 Component Implementation Guide

## Implementation Strategy

**Production Reliability at Implementation Level**: Apply enterprise reliability patterns through TypeScript implementations, achieving production readiness through proven patterns.

## Component Implementation Order

### 1. Foundation Components (Reliability Patterns)

#### Step 1.1: Circuit Breaker Implementation
```typescript
// File: src/qiagent/reliability/circuit-breaker.ts
import { Result, success, failure } from '../qicore/base/result.js';
import { createQiError } from '../qicore/base/error.js';

// Mathematical insight: Circuit breaker is state machine with failure counting
type CircuitState = 'closed' | 'open' | 'half_open';

export interface CircuitBreakerConfig {
  readonly failureThreshold: number;
  readonly timeout: number;
  readonly monitoringPeriod: number;
}

export class CircuitBreaker {
  private state: CircuitState = 'closed';
  private failures = 0;
  private lastFailure = 0;
  private successes = 0;

  constructor(
    private readonly config: CircuitBreakerConfig,
    private readonly onTrip?: () => void
  ) {}

  async execute<T>(operation: () => Promise<Result<T>>): Promise<Result<T>> {
    // State machine logic
    if (this.state === 'open') {
      if (Date.now() - this.lastFailure < this.config.timeout) {
        return failure(createQiError(
          'CIRCUIT_BREAKER_OPEN',
          'Circuit breaker is open - failing fast',
          'BUSINESS',
          { state: this.state, failures: this.failures }
        ));
      }
      this.state = 'half_open';
      this.successes = 0;
    }

    try {
      const result = await operation();
      
      if (result._tag === "Left") {
        this.onFailure();
        return result;
      }
      
      this.onSuccess();
      return result;
    } catch (error) {
      this.onFailure();
      return failure(createQiError(
        'CIRCUIT_BREAKER_ERROR',
        `Circuit breaker caught error: ${error}`,
        'NETWORK',
        { error: String(error) }
      ));
    }
  }

  private onSuccess(): void {
    this.failures = 0;
    
    if (this.state === 'half_open') {
      this.successes++;
      if (this.successes >= 3) {
        this.state = 'closed';
      }
    }
  }

  private onFailure(): void {
    this.failures++;
    this.lastFailure = Date.now();
    
    if (this.failures >= this.config.failureThreshold) {
      this.state = 'open';
      if (this.onTrip) {
        this.onTrip();
      }
    }
  }

  getState(): { state: CircuitState; failures: number; successes: number } {
    return {
      state: this.state,
      failures: this.failures,
      successes: this.successes
    };
  }
}
```

**Key Implementation Decisions:**
- ✅ State machine with clear transitions
- ✅ Configurable thresholds and timeouts
- ✅ Result<T> integration preserves error information
- ✅ Observable state for monitoring

#### Step 1.2: Rate Limiter Implementation
```typescript
// File: src/qiagent/reliability/rate-limiter.ts

// Mathematical insight: Token bucket with sliding window
export class RateLimiter {
  private requests: number[] = [];

  constructor(
    private readonly maxRequests: number,
    private readonly windowMs = 60000
  ) {}

  async checkLimit(): Promise<Result<void>> {
    const now = Date.now();
    
    // Sliding window implementation
    this.requests = this.requests.filter(time => now - time < this.windowMs);
    
    if (this.requests.length >= this.maxRequests) {
      return failure(createQiError(
        'RATE_LIMITED',
        `Rate limit exceeded: ${this.maxRequests} requests per ${this.windowMs}ms`,
        'BUSINESS',
        { 
          currentRequests: this.requests.length, 
          limit: this.maxRequests,
          resetTime: this.requests[0] + this.windowMs
        }
      ));
    }
    
    this.requests.push(now);
    return success(undefined);
  }

  getUsage(): { current: number; limit: number; resetTime: number } {
    const now = Date.now();
    this.requests = this.requests.filter(time => now - time < this.windowMs);
    
    return {
      current: this.requests.length,
      limit: this.maxRequests,
      resetTime: this.requests.length > 0 ? this.requests[0] + this.windowMs : now
    };
  }
}
```

#### Step 1.3: Retry Handler Implementation
```typescript
// File: src/qiagent/reliability/retry-handler.ts

// Mathematical insight: Exponential backoff with jitter
export interface RetryConfig {
  readonly initialDelay: number;
  readonly maxDelay: number;
  readonly multiplier: number;
  readonly jitter: boolean;
}

export class RetryHandler {
  constructor(private readonly config: RetryConfig) {}

  async executeWithRetry<T>(
    operation: () => Promise<Result<T>>,
    maxRetries: number
  ): Promise<Result<T>> {
    let lastError: Result<T> | null = null;

    for (let attempt = 0; attempt <= maxRetries; attempt++) {
      if (attempt > 0) {
        const delay = this.calculateDelay(attempt);
        await this.sleep(delay);
      }

      const result = await operation();
      
      if (result._tag === "Right") {
        return result;
      }

      const error = result.left;
      lastError = result;

      // Smart error classification
      if (!this.isRetryableError(error.category)) {
        break;
      }
    }

    return lastError || failure(createQiError(
      'RETRY_EXHAUSTED', 
      'All retry attempts failed', 
      'NETWORK'
    ));
  }

  private calculateDelay(attempt: number): number {
    const delay = Math.min(
      this.config.initialDelay * Math.pow(this.config.multiplier, attempt - 1),
      this.config.maxDelay
    );

    if (this.config.jitter) {
      return delay * (0.5 + Math.random() * 0.5);
    }

    return delay;
  }

  private isRetryableError(category: string): boolean {
    return ['NETWORK', 'TIMEOUT', 'SYSTEM'].includes(category);
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}
```

### 2. Agent Interface and Types

#### Step 2.1: Core Types Implementation
```typescript
// File: src/qiagent/types.ts

export type AgentProvider = 'claude-code' | 'openai' | 'anthropic' | 'local' | 'bedrock' | 'vertex';

export interface AgentConfig {
  readonly provider: AgentProvider;
  readonly timeout: number;
  readonly maxRetries: number;
  readonly rateLimit?: number;
  readonly authentication: AgentAuthentication;
  readonly circuitBreaker?: CircuitBreakerConfig;
  readonly retryBackoff?: RetryConfig;
}

export interface AgentAuthentication {
  readonly apiKey?: string;
  readonly projectId?: string;
  readonly region?: string;
  readonly endpoint?: string;
}

export interface GenerationRequest {
  readonly model: ModelConfig;
  readonly systemPrompt?: string;
  readonly userPrompt: string;
  readonly context?: Record<string, unknown>;
  readonly timeout?: number;
  readonly stream?: boolean;
}

export interface AgentResponse {
  readonly content: string;
  readonly model: string;
  readonly usage?: TokenUsage;
  readonly finishReason: string;
  readonly id?: string;
  readonly metadata?: Record<string, unknown>;
}

export interface Agent {
  readonly generate: (request: GenerationRequest) => Promise<Result<AgentResponse>>;
  readonly generateStream?: (request: GenerationRequest) => AsyncIterableIterator<Result<string>>;
  readonly validateConfiguration: () => Result<void>;
  readonly getProviderInfo: () => ProviderInfo;
  readonly getMetrics: () => AgentMetrics;
  readonly getHealthStatus: () => HealthStatus;
}
```

#### Step 2.2: Metrics Collection Implementation
```typescript
// File: src/qiagent/metrics/metrics-collector.ts

export interface AgentMetrics {
  requestCount: number;
  successCount: number;
  failureCount: number;
  successRate: number;
  averageResponseTime: number;
  rateLimitHits: number;
  circuitBreakerTrips: number;
}

export class MetricsCollector {
  private requestCount = 0;
  private successCount = 0;
  private failureCount = 0;
  private rateLimitHits = 0;
  private circuitBreakerTrips = 0;
  private readonly responseTimes: number[] = [];

  recordRequest(success: boolean, responseTime: number): void {
    this.requestCount++;
    this.responseTimes.push(responseTime);
    
    if (success) {
      this.successCount++;
    } else {
      this.failureCount++;
    }
  }

  recordRateLimit(): void {
    this.rateLimitHits++;
  }

  recordCircuitBreakerTrip(): void {
    this.circuitBreakerTrips++;
  }

  getMetrics(): AgentMetrics {
    const avgResponseTime = this.responseTimes.length > 0
      ? this.responseTimes.reduce((a, b) => a + b, 0) / this.responseTimes.length
      : 0;

    return {
      requestCount: this.requestCount,
      successCount: this.successCount,
      failureCount: this.failureCount,
      successRate: this.requestCount > 0 ? this.successCount / this.requestCount : 0,
      averageResponseTime: avgResponseTime,
      rateLimitHits: this.rateLimitHits,
      circuitBreakerTrips: this.circuitBreakerTrips
    };
  }

  getHealthStatus(): { status: 'healthy' | 'degraded' | 'critical'; reason?: string } {
    const metrics = this.getMetrics();
    const successRate = metrics.successRate;

    if (successRate < 0.5) {
      return { status: 'critical', reason: 'Success rate below 50%' };
    }
    
    if (successRate < 0.8 || metrics.circuitBreakerTrips > 5) {
      return { status: 'degraded', reason: 'Elevated error rate or circuit breaker trips' };
    }
    
    return { status: 'healthy' };
  }
}
```

### 3. Provider Implementations

#### Step 3.1: Claude Code Agent Implementation
```typescript
// File: src/qiagent/providers/claude-code-agent.ts
import Anthropic from '@anthropic-ai/sdk';

export const createClaudeCodeAgent = (config: AgentConfig): Agent => {
  const anthropic = new Anthropic({
    apiKey: config.authentication.apiKey || process.env.ANTHROPIC_API_KEY,
    timeout: config.timeout,
  });

  // Initialize reliability patterns
  const circuitBreaker = new CircuitBreaker(
    config.circuitBreaker || defaultCircuitBreakerConfig,
    () => metrics.recordCircuitBreakerTrip()
  );
  
  const rateLimiter = config.rateLimit 
    ? new RateLimiter(config.rateLimit) 
    : null;
    
  const retryHandler = new RetryHandler(
    config.retryBackoff || defaultRetryConfig
  );
  
  const metrics = new MetricsCollector();

  const validateConfig = (): Result<void> => {
    if (config.provider !== 'claude-code') {
      return failure(createQiError(
        'INVALID_PROVIDER', 
        'Expected claude-code provider', 
        'VALIDATION',
        { provider: config.provider }
      ));
    }

    if (!config.authentication.apiKey && !process.env.ANTHROPIC_API_KEY) {
      return failure(createQiError(
        'MISSING_AUTH', 
        'Claude Code requires API key', 
        'SECURITY',
        { configPath: 'authentication.apiKey' }
      ));
    }

    if (config.timeout < 1000 || config.timeout > 300000) {
      return failure(createQiError(
        'INVALID_TIMEOUT', 
        'Timeout must be between 1s and 5min', 
        'VALIDATION',
        { timeout: config.timeout }
      ));
    }

    return success(undefined);
  };

  const generate = async (request: GenerationRequest): Promise<Result<AgentResponse>> => {
    const startTime = Date.now();

    try {
      // Validate configuration and request
      const configValidation = validateConfig();
      if (configValidation._tag === "Left") {
        metrics.recordRequest(false, Date.now() - startTime);
        return configValidation;
      }

      const requestValidation = validateGenerationRequest(request);
      if (requestValidation._tag === "Left") {
        metrics.recordRequest(false, Date.now() - startTime);
        return requestValidation;
      }

      // Apply reliability patterns: CircuitBreaker -> Retry -> RateLimit -> API
      const result = await circuitBreaker.execute(async () => 
        await retryHandler.executeWithRetry(async () => {
          // Check rate limit
          if (rateLimiter) {
            const rateLimitCheck = await rateLimiter.checkLimit();
            if (rateLimitCheck._tag === "Left") {
              metrics.recordRateLimit();
              return rateLimitCheck;
            }
          }

          // Make API call
          try {
            const messages = [{ role: 'user' as const, content: request.userPrompt }];
            
            const response = await anthropic.messages.create({
              model: request.model.modelName || 'claude-3-5-sonnet-20241022',
              max_tokens: request.model.maxTokens || 4000,
              temperature: request.model.temperature || 0.7,
              messages,
              ...(request.systemPrompt && { system: request.systemPrompt }),
            });

            const content = response.content
              .filter(block => block.type === 'text')
              .map(block => block.text)
              .join('\n');

            const agentResponse: AgentResponse = {
              content,
              model: response.model,
              usage: {
                promptTokens: response.usage.input_tokens,
                completionTokens: response.usage.output_tokens,
                totalTokens: response.usage.input_tokens + response.usage.output_tokens,
              },
              finishReason: response.stop_reason || 'unknown',
              id: response.id,
              metadata: {
                role: response.role,
                type: response.type,
              },
            };

            return success(agentResponse);
          } catch (error: unknown) {
            const err = error as { status?: number; message?: string };

            if (err.status === 429) {
              return failure(createQiError(
                'RATE_LIMITED', 
                'Claude API rate limit exceeded', 
                'BUSINESS',
                { error: err.message }
              ));
            }

            if (err.status && err.status >= 500) {
              return failure(createQiError(
                'SERVICE_UNAVAILABLE', 
                'Claude API service error', 
                'NETWORK',
                { status: err.status, error: err.message }
              ));
            }

            return failure(createQiError(
              'GENERATION_FAILED',
              `Claude Code generation failed: ${err.message || String(error)}`,
              'NETWORK',
              { error: err.message || String(error), status: err.status }
            ));
          }
        }, config.maxRetries)
      );

      const responseTime = Date.now() - startTime;
      metrics.recordRequest(result._tag === "Right", responseTime);

      return result;
    } catch (error: unknown) {
      const responseTime = Date.now() - startTime;
      metrics.recordRequest(false, responseTime);

      return failure(createQiError(
        'GENERATION_FAILED', 
        `Unexpected error: ${String(error)}`, 
        'SYSTEM',
        { error: String(error) }
      ));
    }
  };

  return {
    generate,
    validateConfiguration: validateConfig,
    getProviderInfo: (): ProviderInfo => ({
      provider: 'claude-code',
      version: '2.0.0',
      capabilities: ['text-generation', 'code-generation', 'reasoning'],
      modelSupport: [
        'claude-3-5-sonnet-20241022',
        'claude-3-opus-20240229',
        'claude-3-haiku-20240307',
      ],
    }),
    getMetrics: () => metrics.getMetrics(),
    getHealthStatus: () => metrics.getHealthStatus(),
  };
};
```

#### Step 3.2: OpenAI Agent Implementation
```typescript
// File: src/qiagent/providers/openai-agent.ts
import OpenAI from 'openai';

export const createOpenAIAgent = (config: AgentConfig): Agent => {
  const openai = new OpenAI({
    apiKey: config.authentication.apiKey || process.env.OPENAI_API_KEY,
    timeout: config.timeout,
  });

  // Same reliability pattern setup as Claude
  const circuitBreaker = new CircuitBreaker(/* ... */);
  const rateLimiter = config.rateLimit ? new RateLimiter(config.rateLimit) : null;
  const retryHandler = new RetryHandler(/* ... */);
  const metrics = new MetricsCollector();

  const generate = async (request: GenerationRequest): Promise<Result<AgentResponse>> => {
    // Same reliability pattern composition
    return await circuitBreaker.execute(async () => 
      await retryHandler.executeWithRetry(async () => {
        // OpenAI-specific API call
        const messages: OpenAI.ChatCompletionMessageParam[] = [];
        
        if (request.systemPrompt) {
          messages.push({ role: 'system', content: request.systemPrompt });
        }
        
        messages.push({ role: 'user', content: request.userPrompt });

        const response = await openai.chat.completions.create({
          model: request.model.modelName || 'gpt-4',
          messages,
          max_tokens: request.model.maxTokens || 4000,
          temperature: request.model.temperature || 0.7,
        });

        const choice = response.choices[0];
        if (!choice?.message?.content) {
          return failure(createQiError(
            'EMPTY_RESPONSE', 
            'OpenAI returned empty response', 
            'NETWORK'
          ));
        }

        return success({
          content: choice.message.content,
          model: response.model,
          usage: response.usage ? {
            promptTokens: response.usage.prompt_tokens,
            completionTokens: response.usage.completion_tokens,
            totalTokens: response.usage.total_tokens,
          } : undefined,
          finishReason: choice.finish_reason || 'unknown',
          id: response.id,
        });
      }, config.maxRetries)
    );
  };

  return {
    generate,
    validateConfiguration: () => /* validation */,
    getProviderInfo: () => ({
      provider: 'openai',
      version: '2.0.0',
      capabilities: ['text-generation', 'code-generation', 'reasoning', 'function-calling'],
      modelSupport: ['gpt-4', 'gpt-4-turbo', 'gpt-3.5-turbo'],
    }),
    getMetrics: () => metrics.getMetrics(),
    getHealthStatus: () => metrics.getHealthStatus(),
  };
};
```

### 4. Agent Factory Implementation

#### Step 4.1: Factory with Provider Routing
```typescript
// File: src/qiagent/factory/agent-factory.ts

export const createAgent = (config: AgentConfig): Result<Agent> => {
  // Validate configuration first
  const validation = validateAgentConfig(config);
  if (validation._tag === "Left") {
    return validation;
  }

  switch (config.provider) {
    case 'claude-code':
      return success(createClaudeCodeAgent(config));
    
    case 'openai':
      return success(createOpenAIAgent(config));
    
    case 'local':
      return success(createLocalAgent(config));
    
    case 'anthropic':
    case 'bedrock':
    case 'vertex':
      return failure(createQiError(
        'NOT_IMPLEMENTED',
        `Provider ${config.provider} not yet implemented`,
        'SYSTEM',
        { provider: config.provider }
      ));
    
    default:
      return failure(createQiError(
        'UNKNOWN_PROVIDER',
        `Unknown provider: ${config.provider}`,
        'VALIDATION',
        { provider: config.provider }
      ));
  }
};

// Configuration helpers
export const createClaudeCodeConfig = (apiKey?: string): AgentConfig => ({
  provider: 'claude-code',
  timeout: 30000,
  maxRetries: 3,
  rateLimit: 10,
  authentication: {
    apiKey: apiKey || process.env.ANTHROPIC_API_KEY,
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
```

## Component Integration Patterns

### Pattern 1: Reliability Composition
```typescript
// Mathematical insight: Reliability patterns compose through function composition
const composeReliabilityPatterns = <T>(
  circuitBreaker: CircuitBreaker,
  retryHandler: RetryHandler,
  rateLimiter: RateLimiter | null,
  operation: () => Promise<Result<T>>
): Promise<Result<T>> => {
  return circuitBreaker.execute(async () =>
    retryHandler.executeWithRetry(async () => {
      if (rateLimiter) {
        const rateCheck = await rateLimiter.checkLimit();
        if (rateCheck._tag === "Left") return rateCheck;
      }
      return operation();
    }, 3)
  );
};
```

### Pattern 2: Error Context Enrichment
```typescript
// Mathematical insight: Error enrichment preserves original error while adding context
const enrichErrorWithProviderContext = (
  error: QiError,
  provider: AgentProvider,
  operationContext: Record<string, unknown>
): QiError => {
  return error.withContext({
    provider,
    operationTimestamp: Date.now(),
    ...operationContext
  });
};
```

## Testing Implementation Strategy

### Reliability Pattern Tests
```typescript
import { describe, it, expect } from "vitest";

describe("QiAgent Reliability Patterns", () => {
  it("verifies circuit breaker prevents cascade failures", async () => {
    const config = { failureThreshold: 3, timeout: 1000, monitoringPeriod: 5000 };
    const circuitBreaker = new CircuitBreaker(config);
    
    // Trigger circuit breaker opening
    for (let i = 0; i < 3; i++) {
      await circuitBreaker.execute(async () => 
        failure(createQiError('TEST_ERROR', 'Test failure', 'NETWORK'))
      );
    }
    
    // Verify circuit is open and fails fast
    const result = await circuitBreaker.execute(async () => success("test"));
    expect(result._tag).toBe("Left");
    expect(result.left?.code).toBe("CIRCUIT_BREAKER_OPEN");
  });

  it("verifies rate limiter sliding window", async () => {
    const rateLimiter = new RateLimiter(3, 1000); // 3 per second
    
    // Should allow 3 requests
    for (let i = 0; i < 3; i++) {
      const result = await rateLimiter.checkLimit();
      expect(result._tag).toBe("Right");
    }
    
    // Should reject 4th request
    const result = await rateLimiter.checkLimit();
    expect(result._tag).toBe("Left");
    expect(result.left?.code).toBe("RATE_LIMITED");
  });

  it("verifies exponential backoff progression", () => {
    const config = { initialDelay: 100, maxDelay: 10000, multiplier: 2, jitter: false };
    const retryHandler = new RetryHandler(config);
    
    // Verify exponential progression: 100, 200, 400, 800...
    expect(retryHandler.calculateDelay(1)).toBe(100);
    expect(retryHandler.calculateDelay(2)).toBe(200);
    expect(retryHandler.calculateDelay(3)).toBe(400);
    expect(retryHandler.calculateDelay(4)).toBe(800);
  });
});
```

### Integration Tests
```typescript
describe("QiAgent Integration", () => {
  it("creates agent with all reliability patterns", async () => {
    const config = createClaudeCodeConfig("test-key");
    const agentResult = createAgent(config);
    
    expect(agentResult._tag).toBe("Right");
    
    if (agentResult._tag === "Right") {
      const agent = agentResult.right;
      
      // Verify agent interface
      expect(typeof agent.generate).toBe("function");
      expect(typeof agent.validateConfiguration).toBe("function");
      expect(typeof agent.getHealthStatus).toBe("function");
      expect(typeof agent.getMetrics).toBe("function");
      
      // Verify provider info
      const providerInfo = agent.getProviderInfo();
      expect(providerInfo.provider).toBe("claude-code");
      expect(providerInfo.capabilities).toContain("code-generation");
    }
  });

  it("handles generation request with full error handling", async () => {
    const config = createClaudeCodeConfig("invalid-key");
    const agentResult = createAgent(config);
    
    if (agentResult._tag === "Right") {
      const agent = agentResult.right;
      
      const request: GenerationRequest = {
        model: {
          id: 'test',
          name: 'Test Model',
          provider: 'claude-code',
          modelName: 'claude-3-5-sonnet-20241022',
        },
        userPrompt: 'Test prompt'
      };
      
      const result = await agent.generate(request);
      
      // Should fail with authentication error
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.category).toBe("SECURITY");
      }
    }
  });
});
```

---

**This implementation guide ensures production-grade reliability through well-structured TypeScript implementations with comprehensive error handling, observability, and testing strategies.**