# QiAgent v4.0 TypeScript API Reference

## Agent Factory Operations

### createAgent
```typescript
createAgent(config: AgentConfig): Result<Agent>
```
**Purpose**: Create an agent instance based on provider configuration  
**Parameters**:
- `config: AgentConfig` - Provider configuration including authentication and reliability settings
**Returns**: `Result<Agent, QiError>` - Agent instance or configuration error  
**Performance**: ~5-10ms (configuration validation)  
**Errors**: `UNKNOWN_PROVIDER`, `NOT_IMPLEMENTED`, `MISSING_AUTH`

### createClaudeCodeAgent
```typescript
createClaudeCodeAgent(config: AgentConfig): Agent
```
**Purpose**: Create Claude Code agent with Anthropic SDK integration  
**Parameters**:
- `config: AgentConfig` - Must have provider='claude-code' and valid API key
**Returns**: `Agent` - Configured Claude Code agent instance  
**Performance**: ~5ms (instance creation)  
**Dependencies**: @anthropic-ai/sdk, valid ANTHROPIC_API_KEY

### createOpenAIAgent
```typescript
createOpenAIAgent(config: AgentConfig): Agent
```
**Purpose**: Create OpenAI agent with OpenAI SDK integration  
**Parameters**:
- `config: AgentConfig` - Must have provider='openai' and valid API key
**Returns**: `Agent` - Configured OpenAI agent instance  
**Performance**: ~5ms (instance creation)  
**Dependencies**: openai, valid OPENAI_API_KEY

### createLocalAgent
```typescript
createLocalAgent(config: AgentConfig): Agent
```
**Purpose**: Create local model agent for Ollama/custom endpoints  
**Parameters**:
- `config: AgentConfig` - Must have provider='local' and valid endpoint URL
**Returns**: `Agent` - Configured local agent instance  
**Performance**: ~3ms (instance creation)  
**Status**: 🚧 Planned (basic structure implemented)

## Agent Interface Operations

### generate
```typescript
generate(request: GenerationRequest): Promise<Result<AgentResponse>>
```
**Purpose**: Generate AI response with comprehensive reliability patterns  
**Parameters**:
- `request: GenerationRequest` - Generation request with model config and prompts
**Returns**: `Promise<Result<AgentResponse, QiError>>` - Generated response or error  
**Performance**: 
- Claude Code: ~1-3s (network dependent)
- OpenAI: ~1-2s (network dependent)  
- Local: Variable (model dependent)
**Reliability**: Circuit breaker, rate limiting, retry with exponential backoff  
**Errors**: `RATE_LIMITED`, `SERVICE_UNAVAILABLE`, `TIMEOUT_ERROR`, `VALIDATION_ERROR`

### validateConfiguration
```typescript
validateConfiguration(): Result<void>
```
**Purpose**: Validate agent configuration and credentials  
**Returns**: `Result<void, QiError>` - Success or validation error  
**Performance**: ~1ms (validation checks)  
**Errors**: `INVALID_PROVIDER`, `MISSING_AUTH`, `INVALID_TIMEOUT`

### getProviderInfo
```typescript
getProviderInfo(): ProviderInfo
```
**Purpose**: Get provider capabilities and model support information  
**Returns**: `ProviderInfo` - Provider metadata including capabilities and models  
**Performance**: ~0.1ms (cached information)  
**Pure Function**: No side effects, same input always returns same output

### getMetrics
```typescript
getMetrics(): AgentMetrics
```
**Purpose**: Get performance and reliability metrics  
**Returns**: `AgentMetrics` - Request counts, success rates, response times, reliability stats  
**Performance**: ~1ms (metric calculation)  
**Real-time**: Reflects current agent performance

### getHealthStatus
```typescript
getHealthStatus(): HealthStatus
```
**Purpose**: Get current agent health status  
**Returns**: `HealthStatus` - Health determination based on success rates and circuit breaker state  
**Performance**: ~1ms (health calculation)  
**Levels**: 'healthy' | 'degraded' | 'critical'

## Configuration Operations

### createDefaultAgentConfig
```typescript
createDefaultAgentConfig(provider: AgentProvider): AgentConfig
```
**Purpose**: Create default configuration for specified provider  
**Parameters**:
- `provider: AgentProvider` - Provider type ('claude-code', 'openai', 'local', etc.)
**Returns**: `AgentConfig` - Default configuration with production-ready values  
**Performance**: ~0.1ms (object creation)

### createClaudeCodeConfig
```typescript
createClaudeCodeConfig(apiKey?: string): AgentConfig
```
**Purpose**: Create Claude Code specific configuration  
**Parameters**:
- `apiKey?: string` - Optional API key (falls back to ANTHROPIC_API_KEY env var)
**Returns**: `AgentConfig` - Claude Code optimized configuration  
**Defaults**:
- Timeout: 30s
- Max retries: 3
- Rate limit: 10 requests/minute
- Circuit breaker: 5 failure threshold, 30s timeout

### createOpenAIConfig
```typescript
createOpenAIConfig(apiKey?: string): AgentConfig
```
**Purpose**: Create OpenAI specific configuration  
**Parameters**:
- `apiKey?: string` - Optional API key (falls back to OPENAI_API_KEY env var)
**Returns**: `AgentConfig` - OpenAI optimized configuration  
**Defaults**: Same as Claude Code with OpenAI-specific adjustments

### createLocalConfig
```typescript
createLocalConfig(endpoint: string): AgentConfig
```
**Purpose**: Create local model configuration  
**Parameters**:
- `endpoint: string` - Local model endpoint URL (e.g., 'http://localhost:11434')
**Returns**: `AgentConfig` - Local model optimized configuration  
**Defaults**: Higher timeout (60s), fewer retries (2)

## Validation Operations

### validateGenerationRequest
```typescript
validateGenerationRequest(request: GenerationRequest): Result<GenerationRequest>
```
**Purpose**: Validate generation request structure and parameters  
**Parameters**:
- `request: GenerationRequest` - Request to validate
**Returns**: `Result<GenerationRequest, QiError>` - Validated request or validation error  
**Performance**: ~1ms (validation checks)  
**Checks**:
- User prompt presence and non-empty
- Model configuration validity
- Temperature range (0-2)
- Max tokens range (1-10000)
**Errors**: `INVALID_REQUEST`, `INVALID_MODEL`, `INVALID_TEMPERATURE`, `INVALID_MAX_TOKENS`

## Type Definitions

### AgentConfig
```typescript
interface AgentConfig {
  readonly provider: AgentProvider;
  readonly timeout: number;                    // Request timeout in milliseconds
  readonly maxRetries: number;                 // Maximum retry attempts
  readonly rateLimit?: number;                 // Requests per minute limit
  readonly authentication: AgentAuthentication;
  readonly circuitBreaker?: CircuitBreakerConfig;
  readonly retryBackoff?: RetryConfig;
}
```

### GenerationRequest
```typescript
interface GenerationRequest {
  readonly model: ModelConfig;
  readonly systemPrompt?: string;              // Optional system prompt
  readonly userPrompt: string;                 // Required user prompt
  readonly context?: Record<string, unknown>;  // Optional request context
  readonly timeout?: number;                   // Optional request-specific timeout
  readonly stream?: boolean;                   // Optional streaming mode
}
```

### AgentResponse
```typescript
interface AgentResponse {
  readonly content: string;                    // Generated content
  readonly model: string;                      // Model used for generation
  readonly usage?: TokenUsage;                 // Token usage statistics
  readonly finishReason: string;               // Completion reason
  readonly id?: string;                        // Response ID from provider
  readonly metadata?: Record<string, unknown>; // Provider-specific metadata
}
```

### ModelConfig
```typescript
interface ModelConfig {
  readonly id: string;                         // Model identifier
  readonly name: string;                       // Human-readable model name
  readonly provider: string;                   // Provider name
  readonly modelName: string;                  // Provider-specific model name
  readonly temperature?: number;               // Sampling temperature (0-2)
  readonly maxTokens?: number;                 // Maximum tokens (1-10000)
  readonly topP?: number;                      // Nucleus sampling parameter
  readonly frequencyPenalty?: number;          // Frequency penalty (-2 to 2)
  readonly presencePenalty?: number;           // Presence penalty (-2 to 2)
  readonly stopSequences?: string[];           // Custom stop sequences
}
```

### AgentMetrics
```typescript
interface AgentMetrics {
  requestCount: number;                        // Total requests processed
  successCount: number;                        // Successful requests
  failureCount: number;                        // Failed requests
  successRate: number;                         // Success rate (0-1)
  averageResponseTime: number;                 // Average response time (ms)
  rateLimitHits: number;                       // Rate limit violations
  circuitBreakerTrips: number;                 // Circuit breaker activations
}
```

### ProviderInfo
```typescript
interface ProviderInfo {
  readonly provider: AgentProvider;            // Provider identifier
  readonly version: string;                    // Provider implementation version
  readonly capabilities: string[];             // Supported capabilities
  readonly modelSupport: string[];             // Supported models
}
```

### CircuitBreakerConfig
```typescript
interface CircuitBreakerConfig {
  readonly failureThreshold: number;           // Failures before opening (default: 5)
  readonly timeout: number;                    // Open state duration (default: 30000ms)
  readonly monitoringPeriod: number;           // Monitoring window (default: 60000ms)
}
```

### RetryConfig
```typescript
interface RetryConfig {
  readonly initialDelay: number;               // Initial delay (default: 1000ms)
  readonly maxDelay: number;                   // Maximum delay (default: 30000ms)
  readonly multiplier: number;                 // Backoff multiplier (default: 2)
  readonly jitter: boolean;                    // Add randomization (default: true)
}
```

## Usage Examples

### Basic Agent Creation and Usage
```typescript
import { QiAgent } from './src/qiagent/index.js';

// Create and configure agent
const config = QiAgent.createClaudeCodeConfig(process.env.ANTHROPIC_API_KEY);
const agentResult = QiAgent.createAgent(config);

if (agentResult._tag === "Right") {
  const agent = agentResult.right;
  
  // Validate configuration
  const validation = agent.validateConfiguration();
  if (validation._tag === "Left") {
    console.error('Configuration invalid:', validation.left);
    return;
  }
  
  // Generate with comprehensive error handling
  const request: GenerationRequest = {
    model: {
      id: 'claude-3.5',
      name: 'Claude 3.5 Sonnet',
      provider: 'anthropic',
      modelName: 'claude-3-5-sonnet-20241022',
      temperature: 0.7,
      maxTokens: 4000
    },
    systemPrompt: 'You are an expert TypeScript programmer.',
    userPrompt: 'Implement a Result<T> monad with error handling.'
  };
  
  const result = await agent.generate(request);
  
  if (result._tag === "Right") {
    const response = result.right;
    console.log('Generated:', response.content);
    console.log('Model:', response.model);
    console.log('Usage:', response.usage);
    console.log('Finish Reason:', response.finishReason);
  } else {
    const error = result.left;
    console.error(`[${error.category}] ${error.code}: ${error.message}`);
    console.error('Context:', Object.fromEntries(error.context));
  }
}
```

### Advanced Configuration with Reliability Patterns
```typescript
// Custom configuration with reliability patterns
const advancedConfig = QiAgent.createClaudeCodeConfig();
advancedConfig.rateLimit = 20;              // 20 requests per minute
advancedConfig.circuitBreaker = {
  failureThreshold: 3,                      // More sensitive
  timeout: 60000,                           // Longer recovery
  monitoringPeriod: 120000                  // Longer monitoring
};
advancedConfig.retryBackoff = {
  initialDelay: 2000,                       // Start with 2s
  maxDelay: 60000,                          // Max 60s
  multiplier: 1.5,                          // Gentler progression
  jitter: true                              // Add randomization
};

const agentResult = QiAgent.createAgent(advancedConfig);
```

### Multi-Provider Support
```typescript
// Create multiple agents for different providers
const providers: AgentProvider[] = ['claude-code', 'openai'];
const agents = await Promise.all(
  providers.map(async provider => {
    const config = provider === 'claude-code' 
      ? QiAgent.createClaudeCodeConfig()
      : QiAgent.createOpenAIConfig();
    
    return QiAgent.createAgent(config);
  })
);

// Use first available agent
const availableAgent = agents.find(result => result._tag === "Right");
if (availableAgent && availableAgent._tag === "Right") {
  const agent = availableAgent.right;
  const providerInfo = agent.getProviderInfo();
  console.log(`Using ${providerInfo.provider} v${providerInfo.version}`);
}
```

### Health Monitoring and Metrics
```typescript
// Monitor agent health and performance
const agent = agentResult.right;

setInterval(() => {
  const health = agent.getHealthStatus();
  const metrics = agent.getMetrics();
  
  console.log(`Health: ${health.status}${health.reason ? ` (${health.reason})` : ''}`);
  console.log(`Success Rate: ${(metrics.successRate * 100).toFixed(1)}%`);
  console.log(`Avg Response Time: ${metrics.averageResponseTime.toFixed(0)}ms`);
  console.log(`Circuit Breaker Trips: ${metrics.circuitBreakerTrips}`);
  console.log(`Rate Limit Hits: ${metrics.rateLimitHits}`);
}, 30000); // Every 30 seconds
```

### Error Handling and Recovery
```typescript
// Comprehensive error handling with recovery strategies
const generateWithRecovery = async (
  agent: Agent, 
  request: GenerationRequest
): Promise<Result<AgentResponse>> => {
  const result = await agent.generate(request);
  
  if (result._tag === "Left") {
    const error = result.left;
    
    switch (error.code) {
      case 'RATE_LIMITED':
        const resetTime = error.context.get('resetTime') as number;
        const waitTime = resetTime - Date.now();
        console.log(`Rate limited, waiting ${waitTime}ms...`);
        await new Promise(resolve => setTimeout(resolve, waitTime));
        return generateWithRecovery(agent, request);
        
      case 'CIRCUIT_BREAKER_OPEN':
        console.log('Circuit breaker open, trying alternative...');
        // Could try different provider or simplified request
        break;
        
      case 'SERVICE_UNAVAILABLE':
        console.log('Service unavailable, will retry automatically...');
        // Retry is handled automatically by retry pattern
        break;
        
      default:
        console.error(`Unhandled error: ${error.code}`);
    }
  }
  
  return result;
};
```

## Performance Specifications

### Operation Performance Targets
| Operation | Target | Typical | Notes |
|-----------|--------|---------|-------|
| Agent Creation | < 10ms | ~5ms | Configuration validation |
| Request Validation | < 1ms | ~0.5ms | Parameter checks |
| Circuit Breaker Check | < 0.1ms | ~0.05ms | State machine check |
| Rate Limit Check | < 0.5ms | ~0.2ms | Sliding window calculation |
| Health Status | < 1ms | ~0.5ms | Metrics-based calculation |
| Metrics Collection | < 1ms | ~0.3ms | Statistical computation |

### Provider Performance
| Provider | Typical Response | Reliability | Notes |
|----------|------------------|-------------|-------|
| Claude Code | 1-3s | 99.5%+ | Network dependent |
| OpenAI | 1-2s | 99.9%+ | Network dependent |
| Local | Variable | 95%+ | Model dependent |

---

**QiAgent v4.0 provides production-grade AI agent interactions with comprehensive error handling, reliability patterns, and performance monitoring suitable for mission-critical applications.**