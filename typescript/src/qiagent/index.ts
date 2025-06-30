/**
 * @fileoverview QiAgent - AI Agent Interaction Component (Production Quality)
 * @purpose Handles all AI model interactions with full Claude Code SDK integration
 * @dependencies QiCore Base, Anthropic SDK, OpenAI SDK, Study Types, QiPrompt
 * @version 2.0.0 - Production Implementation
 */

import Anthropic from "@anthropic-ai/sdk";
import OpenAI from "openai";
import { createQiError } from "../qicore/base/error.js";
import { type Result, failure, getError, isFailure, success } from "../qicore/base/result.js";
import type { GenerationRequest } from "./types.js";

// ============================================================================
// PROVIDER TYPES
// ============================================================================

export type AgentProvider = "claude-code" | "openai" | "anthropic" | "local" | "bedrock" | "vertex";

export interface AgentConfig {
  readonly provider: AgentProvider;
  readonly timeout: number;
  readonly maxRetries: number;
  readonly rateLimit?: number;
  readonly authentication: AgentAuthentication;
  readonly circuitBreaker?: CircuitBreakerConfig;
  readonly retryBackoff?: RetryConfig;
}

export interface CircuitBreakerConfig {
  readonly failureThreshold: number;
  readonly timeout: number;
  readonly monitoringPeriod: number;
}

export interface RetryConfig {
  readonly initialDelay: number;
  readonly maxDelay: number;
  readonly multiplier: number;
  readonly jitter: boolean;
}

export interface AgentAuthentication {
  readonly apiKey?: string;
  readonly projectId?: string;
  readonly region?: string;
  readonly endpoint?: string;
}

export interface AgentResponse {
  readonly content: string;
  readonly model: string;
  readonly usage?: {
    readonly promptTokens: number;
    readonly completionTokens: number;
    readonly totalTokens: number;
  };
  readonly finishReason: string;
  readonly id?: string;
  readonly metadata?: Record<string, unknown>;
}

export interface ProviderInfo {
  readonly provider: AgentProvider;
  readonly version: string;
  readonly capabilities: string[];
  readonly modelSupport: string[];
}

// ============================================================================
// CIRCUIT BREAKER IMPLEMENTATION
// ============================================================================

type CircuitState = "closed" | "open" | "half_open";

class CircuitBreaker {
  private state: CircuitState = "closed";
  private failures = 0;
  private lastFailure = 0;
  private successes = 0;

  constructor(
    private readonly config: CircuitBreakerConfig,
    private readonly onTrip?: () => void
  ) {}

  async execute<T>(operation: () => Promise<Result<T>>): Promise<Result<T>> {
    if (this.state === "open") {
      if (Date.now() - this.lastFailure < this.config.timeout) {
        return failure(
          createQiError(
            "CIRCUIT_BREAKER_OPEN",
            "Circuit breaker is open - failing fast",
            "BUSINESS",
            { state: this.state, failures: this.failures }
          )
        );
      }
      this.state = "half_open";
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
      return failure(
        createQiError(
          "CIRCUIT_BREAKER_ERROR",
          `Circuit breaker caught error: ${error}`,
          "NETWORK",
          { error: String(error) }
        )
      );
    }
  }

  private onSuccess(): void {
    this.failures = 0;

    if (this.state === "half_open") {
      this.successes++;
      if (this.successes >= 3) {
        // Require 3 successes to close
        this.state = "closed";
      }
    }
  }

  private onFailure(): void {
    this.failures++;
    this.lastFailure = Date.now();

    if (this.state === "half_open" || this.failures >= this.config.failureThreshold) {
      this.state = "open";
      // Notify metrics collector about circuit breaker trip
      if (this.onTrip) {
        this.onTrip();
      }
    }
  }

  getState(): { state: CircuitState; failures: number; successes: number } {
    return {
      state: this.state,
      failures: this.failures,
      successes: this.successes,
    };
  }
}

// ============================================================================
// RATE LIMITER IMPLEMENTATION
// ============================================================================

class RateLimiter {
  private requests: number[] = [];

  constructor(
    private readonly maxRequests: number,
    private readonly windowMs = 60000
  ) {}

  async checkLimit(): Promise<Result<void>> {
    const now = Date.now();

    // Remove old requests outside window
    this.requests = this.requests.filter((time) => now - time < this.windowMs);

    if (this.requests.length >= this.maxRequests) {
      const resetTime =
        this.requests.length > 0 ? this.requests[0] + this.windowMs : now + this.windowMs;
      return failure(
        createQiError(
          "RATE_LIMITED",
          `Rate limit exceeded: ${this.maxRequests} requests per ${this.windowMs}ms`,
          "BUSINESS",
          { currentRequests: this.requests.length, limit: this.maxRequests, resetTime }
        )
      );
    }

    this.requests.push(now);
    return success(undefined);
  }

  getUsage(): { current: number; limit: number; resetTime: number } {
    const now = Date.now();
    this.requests = this.requests.filter((time) => now - time < this.windowMs);

    return {
      current: this.requests.length,
      limit: this.maxRequests,
      resetTime: this.requests.length > 0 ? this.requests[0] + this.windowMs : now,
    };
  }
}

// ============================================================================
// RETRY LOGIC WITH EXPONENTIAL BACKOFF
// ============================================================================

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
        return result;
      }

      const error = getError(result);
      lastError = result;

      // Don't retry certain error types
      if (error && !this.isRetryableError(error.category)) {
        break;
      }
    }

    return (
      lastError || failure(createQiError("RETRY_EXHAUSTED", "All retry attempts failed", "NETWORK"))
    );
  }

  private calculateDelay(attempt: number): number {
    const delay = Math.min(
      this.config.initialDelay * this.config.multiplier ** (attempt - 1),
      this.config.maxDelay
    );

    if (this.config.jitter) {
      return delay * (0.5 + Math.random() * 0.5); // Add 0-50% jitter
    }

    return delay;
  }

  private isRetryableError(category: string): boolean {
    return ["NETWORK", "TIMEOUT", "SYSTEM"].includes(category);
  }

  private sleep(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }
}

// ============================================================================
// PERFORMANCE MONITORING
// ============================================================================

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
    const avgResponseTime =
      this.responseTimes.length > 0
        ? this.responseTimes.reduce((a, b) => a + b, 0) / this.responseTimes.length
        : 0;

    return {
      requestCount: this.requestCount,
      successCount: this.successCount,
      failureCount: this.failureCount,
      successRate: this.requestCount > 0 ? this.successCount / this.requestCount : 0,
      averageResponseTime: avgResponseTime,
      rateLimitHits: this.rateLimitHits,
      circuitBreakerTrips: this.circuitBreakerTrips,
    };
  }

  getHealthStatus(): { status: "healthy" | "degraded" | "critical"; reason?: string } {
    const successRate = this.requestCount > 0 ? this.successCount / this.requestCount : 1;

    if (successRate < 0.5) {
      return { status: "critical", reason: "Success rate below 50%" };
    }
    if (successRate < 0.8 || this.circuitBreakerTrips > 5) {
      return { status: "degraded", reason: "Elevated error rate or circuit breaker trips" };
    }
    return { status: "healthy" };
  }

  reset(): void {
    this.requestCount = 0;
    this.successCount = 0;
    this.failureCount = 0;
    this.rateLimitHits = 0;
    this.circuitBreakerTrips = 0;
    this.responseTimes.length = 0;
  }
}

// ============================================================================
// AGENT INTERFACE
// ============================================================================

export interface Agent {
  readonly generate: (request: GenerationRequest) => Promise<Result<AgentResponse>>;
  readonly generateStream?: (request: GenerationRequest) => AsyncIterableIterator<Result<string>>;
  readonly validateConfiguration: () => Result<void>;
  readonly getProviderInfo: () => ProviderInfo;
  readonly getMetrics: () => AgentMetrics;
  readonly getHealthStatus: () => { status: "healthy" | "degraded" | "critical"; reason?: string };
}

// ============================================================================
// CLAUDE CODE AGENT (PRODUCTION IMPLEMENTATION)
// ============================================================================

/**
 * Claude Code Agent implementation with full SDK integration
 * Uses Anthropic Claude Code SDK for AI-powered code generation
 */

// Define proper interfaces for Anthropic SDK
interface AnthropicMessage {
  readonly role: "user" | "assistant" | "system";
  readonly content: string;
}

interface AnthropicResponse {
  readonly content: Array<{ type: string; text: string }>;
  readonly model: string;
  readonly usage: {
    readonly input_tokens: number;
    readonly output_tokens: number;
  };
  readonly stop_reason: string;
  readonly id: string;
  readonly role: string;
  readonly type: string;
}

export const createClaudeCodeAgent = (config: AgentConfig): Agent => {
  const anthropic = new Anthropic({
    apiKey: config.authentication.apiKey || process.env.ANTHROPIC_API_KEY,
    timeout: config.timeout,
  });

  const circuitBreaker = config.circuitBreaker
    ? new CircuitBreaker(config.circuitBreaker, () => metrics.recordCircuitBreakerTrip())
    : new CircuitBreaker({ failureThreshold: 5, timeout: 30000, monitoringPeriod: 60000 }, () =>
        metrics.recordCircuitBreakerTrip()
      );

  const rateLimiter = config.rateLimit ? new RateLimiter(config.rateLimit) : null;

  const retryHandler = new RetryHandler(
    config.retryBackoff || {
      initialDelay: 1000,
      maxDelay: 30000,
      multiplier: 2,
      jitter: true,
    }
  );

  const metrics = new MetricsCollector();

  const validateConfig = (): Result<void> => {
    if (config.provider !== "claude-code") {
      return failure(
        createQiError("INVALID_PROVIDER", "Expected claude-code provider", "VALIDATION", {
          provider: config.provider,
        })
      );
    }

    if (!config.authentication.apiKey && !process.env.ANTHROPIC_API_KEY) {
      return failure(
        createQiError("MISSING_AUTH", "Claude Code requires API key", "SECURITY", {
          configPath: "authentication.apiKey",
        })
      );
    }

    if (config.timeout < 1000 || config.timeout > 300000) {
      return failure(
        createQiError("INVALID_TIMEOUT", "Timeout must be between 1s and 5min", "VALIDATION", {
          timeout: config.timeout,
        })
      );
    }

    return success(undefined);
  };

  const generate = async (request: GenerationRequest): Promise<Result<AgentResponse>> => {
    const startTime = Date.now();

    try {
      // Validate configuration
      const configValidation = validateConfig();
      if (isFailure(configValidation)) {
        metrics.recordRequest(false, Date.now() - startTime);
        const error = getError(configValidation);
        return failure(
          error ||
            createQiError("VALIDATION_ERROR", "Configuration validation failed", "VALIDATION")
        );
      }

      // Validate request
      const requestValidation = validateGenerationRequest(request);
      if (isFailure(requestValidation)) {
        metrics.recordRequest(false, Date.now() - startTime);
        const error = getError(requestValidation);
        return failure(
          error || createQiError("VALIDATION_ERROR", "Request validation failed", "VALIDATION")
        );
      }

      // Check rate limit
      if (rateLimiter) {
        const rateLimitCheck = await rateLimiter.checkLimit();
        if (isFailure(rateLimitCheck)) {
          metrics.recordRateLimit();
          metrics.recordRequest(false, Date.now() - startTime);
          const error = getError(rateLimitCheck);
          return failure(error || createQiError("RATE_LIMITED", "Rate limit exceeded", "BUSINESS"));
        }
      }

      // Execute with circuit breaker and retry logic
      const result = await circuitBreaker.execute(
        async () =>
          await retryHandler.executeWithRetry(async () => {
            try {
              const messages: AnthropicMessage[] = [
                {
                  role: "user",
                  content: request.userPrompt,
                },
              ];

              // Use proper interface instead of any for better type safety
              interface AnthropicSDK {
                messages: {
                  create: (params: {
                    model: string;
                    messages: AnthropicMessage[];
                    max_tokens: number;
                    temperature?: number;
                    system?: string;
                  }) => Promise<AnthropicResponse>;
                };
              }

              const anthropicAPI = anthropic as unknown as AnthropicSDK;
              const response: AnthropicResponse = await anthropicAPI.messages.create({
                model: request.model.modelName || "claude-3-5-sonnet-20241022",
                max_tokens: request.model.maxTokens || 4000,
                temperature: request.model.temperature || 0.7,
                messages,
                ...(request.systemPrompt && { system: request.systemPrompt }),
              });

              const content = response.content
                .filter((block) => block.type === "text")
                .map((block) => block.text)
                .join("\n");

              const agentResponse: AgentResponse = {
                content,
                model: response.model,
                usage: {
                  promptTokens: response.usage.input_tokens,
                  completionTokens: response.usage.output_tokens,
                  totalTokens: response.usage.input_tokens + response.usage.output_tokens,
                },
                finishReason: response.stop_reason || "unknown",
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
                return failure(
                  createQiError("RATE_LIMITED", "Claude API rate limit exceeded", "BUSINESS", {
                    error: err.message,
                  })
                );
              }

              if (err.status && err.status >= 500) {
                return failure(
                  createQiError("SERVICE_UNAVAILABLE", "Claude API service error", "NETWORK", {
                    status: err.status,
                    error: err.message,
                  })
                );
              }

              return failure(
                createQiError(
                  "GENERATION_FAILED",
                  `Claude Code generation failed: ${err.message || String(error)}`,
                  "NETWORK",
                  { error: err.message || String(error), status: err.status }
                )
              );
            }
          }, config.maxRetries)
      );

      const responseTime = Date.now() - startTime;
      metrics.recordRequest(!isFailure(result), responseTime);

      return result;
    } catch (error: unknown) {
      const responseTime = Date.now() - startTime;
      metrics.recordRequest(false, responseTime);

      return failure(
        createQiError("GENERATION_FAILED", `Unexpected error: ${String(error)}`, "SYSTEM", {
          error: String(error),
        })
      );
    }
  };

  return {
    generate,
    validateConfiguration: validateConfig,
    getProviderInfo: (): ProviderInfo => ({
      provider: "claude-code",
      version: "2.0.0",
      capabilities: ["text-generation", "code-generation", "reasoning"],
      modelSupport: [
        "claude-3-5-sonnet-20241022",
        "claude-3-opus-20240229",
        "claude-3-haiku-20240307",
      ],
    }),
    getMetrics: () => metrics.getMetrics(),
    getHealthStatus: () => {
      const currentMetrics = metrics.getMetrics();
      const successRate =
        currentMetrics.requestCount > 0
          ? currentMetrics.successCount / currentMetrics.requestCount
          : 1;
      if (successRate < 0.5) {
        return { status: "critical", reason: "Success rate below 50%" };
      }
      if (successRate < 0.8 || currentMetrics.circuitBreakerTrips > 5) {
        return { status: "degraded", reason: "Elevated error rate or circuit breaker trips" };
      }
      return { status: "healthy" };
    },
  };
};

// ============================================================================
// OPENAI AGENT (PRODUCTION IMPLEMENTATION)
// ============================================================================

/**
 * OpenAI Agent implementation with real SDK integration
 */
export const createOpenAIAgent = (config: AgentConfig): Agent => {
  const openai = new OpenAI({
    apiKey: config.authentication.apiKey || process.env.OPENAI_API_KEY,
    timeout: config.timeout,
  });

  const circuitBreaker = config.circuitBreaker
    ? new CircuitBreaker(config.circuitBreaker, () => metrics.recordCircuitBreakerTrip())
    : new CircuitBreaker({ failureThreshold: 5, timeout: 30000, monitoringPeriod: 60000 }, () =>
        metrics.recordCircuitBreakerTrip()
      );

  const rateLimiter = config.rateLimit ? new RateLimiter(config.rateLimit) : null;
  const metrics = new MetricsCollector();

  const validateConfig = (): Result<void> => {
    if (config.provider !== "openai") {
      return failure(
        createQiError("INVALID_PROVIDER", "Expected openai provider", "VALIDATION", {
          provider: config.provider,
        })
      );
    }

    if (!config.authentication.apiKey && !process.env.OPENAI_API_KEY) {
      return failure(
        createQiError("MISSING_AUTH", "OpenAI requires API key", "SECURITY", {
          configPath: "authentication.apiKey",
        })
      );
    }

    return success(undefined);
  };

  const generate = async (request: GenerationRequest): Promise<Result<AgentResponse>> => {
    const startTime = Date.now();

    try {
      const configValidation = validateConfig();
      if (isFailure(configValidation)) {
        metrics.recordRequest(false, Date.now() - startTime);
        const error = getError(configValidation);
        return failure(
          error ||
            createQiError("VALIDATION_ERROR", "Configuration validation failed", "VALIDATION")
        );
      }

      const requestValidation = validateGenerationRequest(request);
      if (isFailure(requestValidation)) {
        metrics.recordRequest(false, Date.now() - startTime);
        const error = getError(requestValidation);
        return failure(
          error || createQiError("VALIDATION_ERROR", "Request validation failed", "VALIDATION")
        );
      }

      if (rateLimiter) {
        const rateLimitCheck = await rateLimiter.checkLimit();
        if (isFailure(rateLimitCheck)) {
          metrics.recordRateLimit();
          metrics.recordRequest(false, Date.now() - startTime);
          const error = getError(rateLimitCheck);
          return failure(error || createQiError("RATE_LIMITED", "Rate limit exceeded", "BUSINESS"));
        }
      }

      const result = await circuitBreaker.execute(async () => {
        try {
          const messages: OpenAI.ChatCompletionMessageParam[] = [];

          if (request.systemPrompt) {
            messages.push({ role: "system", content: request.systemPrompt });
          }

          messages.push({ role: "user", content: request.userPrompt });

          const response = await openai.chat.completions.create({
            model: request.model.modelName || "gpt-4",
            messages,
            max_tokens: request.model.maxTokens || 4000,
            temperature: request.model.temperature || 0.7,
          });

          const choice = response.choices[0];
          if (!choice?.message?.content) {
            return failure(
              createQiError("EMPTY_RESPONSE", "OpenAI returned empty response", "NETWORK")
            );
          }

          const agentResponse: AgentResponse = {
            content: choice.message.content,
            model: response.model,
            usage: response.usage
              ? {
                  promptTokens: response.usage.prompt_tokens,
                  completionTokens: response.usage.completion_tokens,
                  totalTokens: response.usage.total_tokens,
                }
              : undefined,
            finishReason: choice.finish_reason || "unknown",
            id: response.id,
          };

          return success(agentResponse);
        } catch (error: unknown) {
          const err = error as { status?: number; message?: string };

          if (err.status === 429) {
            return failure(
              createQiError("RATE_LIMITED", "OpenAI API rate limit exceeded", "BUSINESS", {
                error: err.message,
              })
            );
          }

          return failure(
            createQiError(
              "GENERATION_FAILED",
              `OpenAI generation failed: ${err.message || String(error)}`,
              "NETWORK",
              { error: err.message || String(error), status: err.status }
            )
          );
        }
      });

      const responseTime = Date.now() - startTime;
      metrics.recordRequest(!isFailure(result), responseTime);

      return result;
    } catch (error: unknown) {
      const responseTime = Date.now() - startTime;
      metrics.recordRequest(false, responseTime);

      return failure(
        createQiError("GENERATION_FAILED", `Unexpected OpenAI error: ${String(error)}`, "SYSTEM", {
          error: String(error),
        })
      );
    }
  };

  return {
    generate,
    validateConfiguration: validateConfig,
    getProviderInfo: (): ProviderInfo => ({
      provider: "openai",
      version: "2.0.0",
      capabilities: ["text-generation", "code-generation", "reasoning", "function-calling"],
      modelSupport: ["gpt-4", "gpt-4-turbo", "gpt-3.5-turbo"],
    }),
    getMetrics: () => metrics.getMetrics(),
    getHealthStatus: () => {
      const currentMetrics = metrics.getMetrics();
      const successRate =
        currentMetrics.requestCount > 0
          ? currentMetrics.successCount / currentMetrics.requestCount
          : 1;
      if (successRate < 0.5) {
        return { status: "critical", reason: "Success rate below 50%" };
      }
      if (successRate < 0.8 || currentMetrics.circuitBreakerTrips > 5) {
        return { status: "degraded", reason: "Elevated error rate or circuit breaker trips" };
      }
      return { status: "healthy" };
    },
  };
};

// ============================================================================
// LOCAL MODEL AGENT (OLLAMA IMPLEMENTATION)
// ============================================================================

/**
 * Local Model Agent implementation for Ollama and other local endpoints
 */
export const createLocalAgent = (config: AgentConfig): Agent => {
  const metrics = new MetricsCollector();

  const validateConfig = (): Result<void> => {
    if (config.provider !== "local") {
      return failure(
        createQiError("INVALID_PROVIDER", "Expected local provider", "VALIDATION", {
          provider: config.provider,
        })
      );
    }

    if (!config.authentication.endpoint) {
      return failure(
        createQiError(
          "MISSING_ENDPOINT",
          "Local models require endpoint configuration",
          "VALIDATION",
          { configPath: "authentication.endpoint" }
        )
      );
    }

    try {
      new URL(config.authentication.endpoint);
    } catch {
      return failure(
        createQiError("INVALID_ENDPOINT", "Invalid endpoint URL", "VALIDATION", {
          endpoint: config.authentication.endpoint,
        })
      );
    }

    return success(undefined);
  };

  const generate = async (_request: GenerationRequest): Promise<Result<AgentResponse>> => {
    // TODO: Implement Ollama/local model integration
    // This would typically use fetch() to call local model endpoints
    const startTime = Date.now();

    metrics.recordRequest(false, Date.now() - startTime);

    return failure(
      createQiError("NOT_IMPLEMENTED", "Local agent not yet implemented", "SYSTEM", {
        provider: "local",
        endpoint: config.authentication.endpoint,
      })
    );
  };

  return {
    generate,
    validateConfiguration: validateConfig,
    getProviderInfo: (): ProviderInfo => ({
      provider: "local",
      version: "1.0.0",
      capabilities: ["text-generation", "code-generation"],
      modelSupport: ["llama2", "codellama", "mistral", "custom"],
    }),
    getMetrics: () => metrics.getMetrics(),
    getHealthStatus: () => ({ status: "healthy" }),
  };
};

// ============================================================================
// AGENT FACTORY
// ============================================================================

/**
 * Create an agent based on the provider configuration
 * @pure - Factory function
 */
export const createAgent = (config: AgentConfig): Result<Agent> => {
  switch (config.provider) {
    case "claude-code":
      return success(createClaudeCodeAgent(config));

    case "openai":
      return success(createOpenAIAgent(config));

    case "local":
      return success(createLocalAgent(config));

    case "anthropic":
    case "bedrock":
    case "vertex":
      return failure(
        createQiError(
          "NOT_IMPLEMENTED",
          `Provider ${config.provider} not yet implemented`,
          "SYSTEM",
          { provider: config.provider }
        )
      );

    default:
      return failure(
        createQiError("UNKNOWN_PROVIDER", `Unknown provider: ${config.provider}`, "VALIDATION", {
          provider: config.provider,
        })
      );
  }
};

// ============================================================================
// AGENT CONFIGURATION
// ============================================================================

export const createDefaultAgentConfig = (provider: AgentProvider): AgentConfig => ({
  provider,
  timeout: 30000,
  maxRetries: 3,
  authentication: {},
});

export const createClaudeCodeConfig = (apiKey?: string): AgentConfig => ({
  provider: "claude-code",
  timeout: 30000,
  maxRetries: 3,
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

export const createOpenAIConfig = (apiKey?: string): AgentConfig => ({
  provider: "openai",
  timeout: 30000,
  maxRetries: 3,
  authentication: {
    apiKey: apiKey || process.env.OPENAI_API_KEY,
  },
});

export const createLocalConfig = (endpoint: string): AgentConfig => ({
  provider: "local",
  timeout: 60000,
  maxRetries: 2,
  authentication: {
    endpoint,
  },
});

// ============================================================================
// VALIDATION FUNCTIONS
// ============================================================================

/**
 * Validate generation request structure and content
 */
export const validateGenerationRequest = (
  request: GenerationRequest
): Result<GenerationRequest> => {
  if (!request.userPrompt || request.userPrompt.trim() === "") {
    return failure(
      createQiError(
        "INVALID_REQUEST",
        "User prompt is required and cannot be empty",
        "VALIDATION",
        { userPrompt: request.userPrompt }
      )
    );
  }

  if (!request.model) {
    return failure(
      createQiError("INVALID_MODEL", "Model configuration is required", "VALIDATION", {
        model: request.model,
      })
    );
  }

  if (
    request.model.temperature !== undefined &&
    (request.model.temperature < 0 || request.model.temperature > 2)
  ) {
    return failure(
      createQiError("INVALID_TEMPERATURE", "Temperature must be between 0 and 2", "VALIDATION", {
        temperature: request.model.temperature,
        validRange: "0-2",
      })
    );
  }

  if (
    request.model.maxTokens !== undefined &&
    (request.model.maxTokens < 1 || request.model.maxTokens > 10000)
  ) {
    return failure(
      createQiError("INVALID_MAX_TOKENS", "Max tokens must be between 1 and 10000", "VALIDATION", {
        maxTokens: request.model.maxTokens,
        validRange: "1-10000",
      })
    );
  }

  return success(request);
};

// ============================================================================
// EXPORTS
// ============================================================================

export const QiAgent = {
  // Factory
  createAgent,

  // Specific agents
  createClaudeCodeAgent,
  createOpenAIAgent,
  createLocalAgent,

  // Configuration
  createDefaultAgentConfig,
  createClaudeCodeConfig,
  createOpenAIConfig,
  createLocalConfig,

  // Validation
  validateGenerationRequest,
} as const;
