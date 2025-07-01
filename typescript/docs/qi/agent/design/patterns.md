# QiAgent v4.0 TypeScript Design Patterns

> **Stage 3: Design Patterns and Reliability Implementations**  
> **Based on**: [Interface Contracts](../../../../docs/qi/agent/) and production reliability requirements  
> **Implements**: TypeScript-specific patterns for AI agent reliability and observability  
> Version: v4.0  
> Date: June 29, 2025  
> Status: Production-Proven Patterns  
> Purpose: Enterprise-grade AI agent patterns with comprehensive reliability and monitoring

## Design Philosophy

**Production Reliability at Implementation Level**: We apply enterprise reliability patterns directly through TypeScript implementations, achieving production readiness through:

1. **Reliability Pattern Recognition**: Implementing circuit breakers, rate limiting, and retry strategies
2. **QiCore Integration**: "Every operation returns Result<T> for consistent error handling"
3. **Provider Abstraction**: "AI providers are interchangeable through unified interfaces"
4. **Observability First**: "Metrics and health monitoring are built-in, not added later"

## Core Design Patterns

### 1. Agent Factory Pattern (Provider Abstraction)

**Design Insight**: AI providers should be interchangeable through configuration

**Pattern Recognition from Contracts**:
```typescript
// Interface hints: createAgent, provider config → Factory pattern
interface AgentFactory {
  createAgent(config: AgentConfig): Result<Agent>
  createClaudeCodeAgent(config: AgentConfig): Agent
  createOpenAIAgent(config: AgentConfig): Agent
}
```

**Implementation Pattern**:
```typescript
export const createAgent = (config: AgentConfig): Result<Agent> => {
  // Validate configuration first
  const validation = validateAgentConfig(config);
  if (validation._tag === "Left") {
    return validation;
  }

  // Factory dispatch based on provider
  switch (config.provider) {
    case 'claude-code':
      return success(createClaudeCodeAgent(config));
    
    case 'openai':
      return success(createOpenAIAgent(config));
    
    case 'local':
      return success(createLocalAgent(config));
    
    default:
      return failure(createQiError(
        'UNKNOWN_PROVIDER',
        `Unknown provider: ${config.provider}`,
        'VALIDATION',
        { provider: config.provider }
      ));
  }
};

// Provider-specific factories with consistent interface
export const createClaudeCodeAgent = (config: AgentConfig): Agent => {
  const anthropic = new Anthropic({
    apiKey: config.authentication.apiKey || process.env.ANTHROPIC_API_KEY,
    timeout: config.timeout,
  });

  // All agents implement the same interface
  return {
    generate: (request) => generateWithClaude(anthropic, request),
    validateConfiguration: () => validateClaudeConfig(config),
    getProviderInfo: () => ({
      provider: 'claude-code',
      version: '2.0.0',
      capabilities: ['text-generation', 'code-generation', 'reasoning'],
      modelSupport: ['claude-3-5-sonnet-20241022', 'claude-3-opus-20240229']
    }),
    getMetrics: () => metrics.getMetrics(),
    getHealthStatus: () => calculateHealthStatus(metrics.getMetrics())
  };
};
```

**Why This Pattern Works**:
- ✅ **Unified Interface**: All providers implement the same Agent interface
- ✅ **Configuration Driven**: Provider selection through configuration, not code changes
- ✅ **Type Safety**: Factory validates configuration and returns typed agents
- ✅ **Result<T> Integration**: Factory operations return Result<T> for error handling

### 2. Circuit Breaker Pattern (Reliability)

**Design Insight**: Prevent cascade failures during provider outages

**Pattern Recognition from Contracts**:
```typescript
// Interface hints: failure thresholds, state management → Circuit Breaker pattern
interface CircuitBreakerContract {
  execute<T>(operation: () => Promise<Result<T>>): Promise<Result<T>>
  getState(): { state: CircuitState; failures: number }
}
```

**Implementation Pattern**:
```typescript
type CircuitState = 'closed' | 'open' | 'half_open';

class CircuitBreaker {
  private state: CircuitState = 'closed';
  private failures = 0;
  private lastFailure = 0;
  private successes = 0;

  constructor(
    private readonly config: CircuitBreakerConfig,
    private readonly onTrip?: () => void
  ) {}

  async execute<T>(operation: () => Promise<Result<T>>): Promise<Result<T>> {
    // Fail fast when circuit is open
    if (this.state === 'open') {
      if (Date.now() - this.lastFailure < this.config.timeout) {
        return failure(createQiError(
          'CIRCUIT_BREAKER_OPEN',
          'Circuit breaker is open - failing fast',
          'BUSINESS',
          { state: this.state, failures: this.failures }
        ));
      }
      // Transition to half-open for testing
      this.state = 'half_open';
      this.successes = 0;
    }

    try {
      const result = await operation();
      
      if (isFailure(result)) {
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
        this.state = 'closed'; // Require 3 successes to close
      }
    }
  }

  private onFailure(): void {
    this.failures++;
    this.lastFailure = Date.now();
    
    if (this.failures >= this.config.failureThreshold) {
      this.state = 'open';
      if (this.onTrip) {
        this.onTrip(); // Notify metrics collector
      }
    }
  }
}
```

**Why This Pattern Works**:
- ✅ **Prevents Cascade Failures**: Stops calling failing services automatically
- ✅ **Automatic Recovery**: Tests service health and recovers automatically
- ✅ **Observable**: Provides state information for monitoring and alerting
- ✅ **Result<T> Integration**: Preserves error information while adding circuit state

### 3. Rate Limiter Pattern (Resource Protection)

**Design Insight**: Protect against rate limiting from AI providers

**Pattern Recognition from Contracts**:
```typescript
// Interface hints: request limits, time windows → Rate Limiter pattern
interface RateLimiterContract {
  checkLimit(): Promise<Result<void>>
  getUsage(): { current: number; limit: number; resetTime: number }
}
```

**Implementation Pattern**:
```typescript
class RateLimiter {
  private requests: number[] = [];

  constructor(
    private readonly maxRequests: number,
    private readonly windowMs = 60000 // 1 minute default window
  ) {}

  async checkLimit(): Promise<Result<void>> {
    const now = Date.now();
    
    // Sliding window: remove old requests
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
    
    // Record this request
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

**Why This Pattern Works**:
- ✅ **Sliding Window**: More accurate than fixed window rate limiting
- ✅ **Predictive Protection**: Prevents hitting provider rate limits
- ✅ **Observable**: Provides usage metrics for monitoring
- ✅ **Graceful Degradation**: Returns meaningful errors with retry timing

### 4. Retry with Exponential Backoff Pattern (Resilience)

**Design Insight**: Transient failures should be retried with increasing delays

**Pattern Recognition from Contracts**:
```typescript
// Interface hints: retry attempts, backoff strategies → Retry pattern
interface RetryContract {
  executeWithRetry<T>(operation: () => Promise<Result<T>>, maxRetries: number): Promise<Result<T>>
}
```

**Implementation Pattern**:
```typescript
class RetryHandler {
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
      
      if (!isFailure(result)) {
        return result; // Success!
      }

      const error = getError(result);
      lastError = result;

      // Smart error classification - don't retry certain errors
      if (error && !this.isRetryableError(error.category)) {
        break; // Non-retryable error
      }
    }

    return lastError || failure(createQiError(
      'RETRY_EXHAUSTED', 
      'All retry attempts failed', 
      'NETWORK'
    ));
  }

  private calculateDelay(attempt: number): number {
    // Exponential backoff: delay = initial * multiplier^(attempt-1)
    const delay = Math.min(
      this.config.initialDelay * Math.pow(this.config.multiplier, attempt - 1),
      this.config.maxDelay
    );

    // Add jitter to prevent thundering herd
    if (this.config.jitter) {
      return delay * (0.5 + Math.random() * 0.5); // 50-100% of calculated delay
    }

    return delay;
  }

  private isRetryableError(category: string): boolean {
    // Only retry transient errors
    return ['NETWORK', 'TIMEOUT', 'SYSTEM'].includes(category);
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}
```

**Why This Pattern Works**:
- ✅ **Smart Classification**: Only retries transient errors
- ✅ **Exponential Backoff**: Reduces load on failing services
- ✅ **Jitter**: Prevents thundering herd problems
- ✅ **Error Preservation**: Maintains original error context

### 5. Metrics Collection Pattern (Observability)

**Design Insight**: Production systems need comprehensive observability

**Pattern Recognition from Contracts**:
```typescript
// Interface hints: performance tracking, health status → Metrics pattern
interface MetricsContract {
  recordRequest(success: boolean, responseTime: number): void
  getMetrics(): AgentMetrics
  getHealthStatus(): HealthStatus
}
```

**Implementation Pattern**:
```typescript
interface AgentMetrics {
  requestCount: number;
  successCount: number;
  failureCount: number;
  successRate: number;
  averageResponseTime: number;
  rateLimitHits: number;
  circuitBreakerTrips: number;
}

class MetricsCollector {
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

**Why This Pattern Works**:
- ✅ **Real-time Metrics**: Tracks performance and reliability in real-time
- ✅ **Health Determination**: Automatically calculates health status
- ✅ **Production Ready**: Provides data needed for alerting and monitoring
- ✅ **Minimal Overhead**: Efficient metric collection with minimal performance impact

## Integration Patterns

### 1. Reliability Composition Pattern

**Design Insight**: Reliability patterns should compose cleanly

```typescript
export const createClaudeCodeAgent = (config: AgentConfig): Agent => {
  // Create reliability pattern instances
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

  const generate = async (request: GenerationRequest): Promise<Result<AgentResponse>> => {
    const startTime = Date.now();

    try {
      // Compose reliability patterns: CircuitBreaker -> Retry -> RateLimit -> Operation
      const result = await circuitBreaker.execute(async () => 
        await retryHandler.executeWithRetry(async () => {
          // Check rate limit before making request
          if (rateLimiter) {
            const rateLimitCheck = await rateLimiter.checkLimit();
            if (isFailure(rateLimitCheck)) {
              metrics.recordRateLimit();
              return rateLimitCheck;
            }
          }

          // Actual API call
          return await callAnthropicAPI(request);
        }, config.maxRetries)
      );

      const responseTime = Date.now() - startTime;
      metrics.recordRequest(!isFailure(result), responseTime);
      
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

  return { generate, validateConfiguration, getProviderInfo, getMetrics, getHealthStatus };
};
```

### 2. Configuration Pattern

**Design Insight**: Configuration should have sensible defaults with override capability

```typescript
// Default configurations with production-ready values
const defaultCircuitBreakerConfig: CircuitBreakerConfig = {
  failureThreshold: 5,      // Open after 5 failures
  timeout: 30000,           // Stay open for 30s
  monitoringPeriod: 60000   // Monitor for 60s
};

const defaultRetryConfig: RetryConfig = {
  initialDelay: 1000,       // Start with 1s delay
  maxDelay: 30000,          // Max 30s delay
  multiplier: 2,            // Double each retry
  jitter: true              // Add randomization
};

// Configuration factory functions
export const createClaudeCodeConfig = (apiKey?: string): AgentConfig => ({
  provider: 'claude-code',
  timeout: 30000,
  maxRetries: 3,
  rateLimit: 10,           // 10 requests per minute
  authentication: {
    apiKey: apiKey || process.env.ANTHROPIC_API_KEY
  },
  circuitBreaker: defaultCircuitBreakerConfig,
  retryBackoff: defaultRetryConfig
});

// Override specific settings while keeping defaults
export const createProductionClaudeConfig = (apiKey: string): AgentConfig => {
  const config = createClaudeCodeConfig(apiKey);
  
  // Production overrides
  config.circuitBreaker = {
    ...defaultCircuitBreakerConfig,
    failureThreshold: 3,    // More sensitive in production
    timeout: 60000          // Longer recovery time
  };
  
  config.rateLimit = 20;    // Higher rate limit for production
  
  return config;
};
```

### 3. Error Recovery Pattern

**Design Insight**: Errors should be recoverable with meaningful context

```typescript
// Error recovery with graceful degradation
export const generateWithFallback = async (
  primaryAgent: Agent,
  fallbackAgent: Agent,
  request: GenerationRequest
): Promise<Result<AgentResponse>> => {
  // Try primary agent first
  const primaryResult = await primaryAgent.generate(request);
  
  if (primaryResult._tag === "Right") {
    return primaryResult;
  }

  const primaryError = primaryResult.left;
  
  // Check if error is recoverable
  if (isRecoverableError(primaryError)) {
    console.log(`Primary agent failed (${primaryError.code}), trying fallback...`);
    
    const fallbackResult = await fallbackAgent.generate(request);
    
    if (fallbackResult._tag === "Right") {
      // Add metadata about fallback usage
      return success({
        ...fallbackResult.right,
        metadata: {
          ...fallbackResult.right.metadata,
          usedFallback: true,
          primaryError: primaryError.toStructuredData()
        }
      });
    }
  }

  // Return original error with additional context
  return failure(primaryError.withContext({
    fallbackAttempted: isRecoverableError(primaryError),
    fallbackProvider: fallbackAgent.getProviderInfo().provider
  }));
};

const isRecoverableError = (error: QiError): boolean => {
  const recoverableCodes = [
    'RATE_LIMITED',
    'SERVICE_UNAVAILABLE', 
    'TIMEOUT_ERROR',
    'CIRCUIT_BREAKER_OPEN'
  ];
  
  return recoverableCodes.includes(error.code);
};
```

## Mathematical Verification Patterns

### Property Testing for Reliability Patterns

```typescript
import { describe, it, expect } from "vitest";

describe("Reliability Pattern Verification", () => {
  it("verifies circuit breaker state transitions", () => {
    const config = { failureThreshold: 3, timeout: 1000, monitoringPeriod: 5000 };
    const circuitBreaker = new CircuitBreaker(config);
    
    // Property: Circuit should open after threshold failures
    for (let i = 0; i < config.failureThreshold; i++) {
      circuitBreaker.onFailure();
    }
    
    expect(circuitBreaker.getState().state).toBe('open');
    
    // Property: Circuit should fail fast when open
    const result = circuitBreaker.execute(async () => success("test"));
    expect(isFailure(result)).toBe(true);
  });

  it("verifies exponential backoff calculation", () => {
    const config = { initialDelay: 100, maxDelay: 10000, multiplier: 2, jitter: false };
    const retryHandler = new RetryHandler(config);
    
    // Property: Delays should follow exponential progression
    const delays = [100, 200, 400, 800, 1600, 3200, 6400, 10000];
    
    for (let attempt = 1; attempt <= 8; attempt++) {
      const calculatedDelay = retryHandler.calculateDelay(attempt);
      expect(calculatedDelay).toBe(delays[attempt - 1]);
    }
  });

  it("verifies rate limiter sliding window", () => {
    const rateLimiter = new RateLimiter(5, 1000); // 5 requests per second
    
    // Property: Should allow requests within limit
    for (let i = 0; i < 5; i++) {
      const result = rateLimiter.checkLimit();
      expect(isSuccess(result)).toBe(true);
    }
    
    // Property: Should reject requests over limit
    const result = rateLimiter.checkLimit();
    expect(isFailure(result)).toBe(true);
  });
});
```

## Success Criteria for Pattern Application

### Reliability Compliance
- ✅ **Circuit Breaker**: Prevents cascade failures with automatic recovery
- ✅ **Rate Limiting**: Protects against provider rate limits
- ✅ **Retry Logic**: Handles transient failures with exponential backoff
- ✅ **Metrics Collection**: Provides production observability

### QiCore Integration
- ✅ **Result<T> Pattern**: All operations return Result<QiError, T>
- ✅ **Error Context**: Rich error information with chaining
- ✅ **Type Safety**: Full TypeScript coverage with runtime validation
- ✅ **Functional Composition**: Pure functions where possible

### Production Quality
- ✅ **Observability**: Health status and performance metrics
- ✅ **Configuration**: Sensible defaults with override capability
- ✅ **Error Recovery**: Graceful degradation and fallback strategies
- ✅ **Performance**: Minimal overhead with comprehensive functionality

---

**These patterns enable production-grade AI agent reliability through TypeScript implementations, proving that enterprise reliability can be achieved through well-designed patterns and QiCore integration.**