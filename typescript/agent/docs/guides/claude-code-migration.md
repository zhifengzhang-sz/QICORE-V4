# Claude Code Agent Migration Guide

A comprehensive guide to migrating from `../lib/src/qiagent` (Claude Code SDK wrapper) to the unified `lib/src/aiagent` framework that supports both Claude Code agents and AI Orchestra workflows.

## Overview

QiCore now provides a **unified AI agent framework** that combines:
- **Individual AI Agents**: Claude Code, OpenAI, Ollama with production-grade reliability
- **Multi-Agent Workflows**: AI Orchestra for complex reasoning chains
- **Seamless Integration**: Switch between individual agents and workflows

## Migration Path

### Before: Separate Agent Systems

```
../lib/src/qiagent/     ← Claude Code SDK wrapper (production-ready)
lib/src/aiagent/        ← AI Orchestra wrapper (workflows only)
```

### After: Unified Framework

```
lib/src/aiagent/        ← Unified framework supporting both
├── index.ts           ← Main exports (AI Orchestra + Claude Code)
├── claude-code.ts     ← Claude Code SDK integration
└── ...                ← Additional agent integrations
```

## Key Changes

### 1. Import Path Updates

**Before:**
```typescript
// Old separate imports
import { createClaudeCodeAgent } from "../../../lib/src/qiagent";
import { createQiWorkflow } from "@qi/agent";
```

**After:**
```typescript
// Unified imports
import { 
  ClaudeCode, 
  createClaudeCodeMathematicalWorkflow,
  createQiWorkflow,
  QiAgent 
} from "@qi/agent";
```

### 2. Agent Creation

**Before:**
```typescript
// Old Claude Code agent creation
import { createClaudeCodeAgent, type AgentConfig } from "../../../lib/src/qiagent";

const config: AgentConfig = {
  provider: "claude-code",
  authentication: { apiKey: process.env.ANTHROPIC_API_KEY },
  timeout: 30000,
  maxRetries: 3,
};

const agent = createClaudeCodeAgent(config);
const result = await agent.generate(request);
```

**After:**
```typescript
// New unified agent creation
import { ClaudeCode } from "@qi/agent";

const agent = ClaudeCode.createAgent({
  apiKey: process.env.ANTHROPIC_API_KEY,
  model: "claude-3-5-sonnet-20241022",
  temperature: 0.7,
  maxTokens: 4000,
});

const result = await agent.generate({
  prompt: "Analyze this mathematical structure...",
  systemPrompt: "You are a mathematical expert...",
});
```

### 3. Workflow Integration

**Before:**
```typescript
// Separate workflow and agent management
const workflow = createQiWorkflow(handlers);
const claudeAgent = createClaudeCodeAgent(config);

// Manual integration needed
```

**After:**
```typescript
// Direct Claude Code workflow creation
const workflow = createClaudeCodeMathematicalWorkflow({
  model: "claude-3-5-sonnet-20241022",
  temperature: 0.3,
});

// Or hybrid workflows
const hybridWorkflow = createHybridWorkflow({
  claudeConfig: { model: "claude-3-5-sonnet-20241022" },
  otherModels: {
    verifier: ollama("qwen3:0.6b"),
  },
});
```

## Step-by-Step Migration

### Step 1: Update Package Dependencies

Ensure you have the unified framework:

```bash
# The @qi/agent package now includes Claude Code integration
bun install
```

### Step 2: Update Import Statements

**Find and replace these patterns:**

```typescript
// OLD
import { createClaudeCodeAgent, QiAgent } from "../../../lib/src/qiagent";
import { createQiWorkflow } from "@qi/agent";

// NEW
import { ClaudeCode, createClaudeCodeMathematicalWorkflow, QiAgent } from "@qi/agent";
```

### Step 3: Migrate Agent Configuration

**OLD Configuration:**
```typescript
const agentConfig: AgentConfig = {
  provider: "claude-code",
  timeout: 30000,
  maxRetries: 3,
  authentication: {
    apiKey: process.env.ANTHROPIC_API_KEY,
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
};

const agent = createClaudeCodeAgent(agentConfig);
```

**NEW Configuration:**
```typescript
const agent = ClaudeCode.createAgent({
  apiKey: process.env.ANTHROPIC_API_KEY,
  model: "claude-3-5-sonnet-20241022",
  temperature: 0.7,
  maxTokens: 4000,
  timeout: 30000,
  maxRetries: 3,
});
```

### Step 4: Update Request/Response Handling

**OLD Request Format:**
```typescript
const request: GenerationRequest = {
  model: {
    id: "claude-1",
    name: "Claude 3.5 Sonnet",
    provider: "anthropic",
    modelName: "claude-3-5-sonnet-20241022",
    temperature: 0.7,
    maxTokens: 4000,
  },
  userPrompt: "Analyze this mathematical structure...",
  systemPrompt: "You are a mathematical expert...",
};

const result = await agent.generate(request);
if (!isFailure(result)) {
  const response = result.right;
  console.log(response.content);
}
```

**NEW Request Format:**
```typescript
const result = await agent.generate({
  prompt: "Analyze this mathematical structure...",
  systemPrompt: "You are a mathematical expert...",
});

if (result._tag === "Right") {
  const response = result.right;
  console.log(response.content);
  console.log(`Tokens: ${response.usage?.promptTokens}→${response.usage?.completionTokens}`);
}
```

### Step 5: Migrate to Workflow Integration

If you were manually combining Claude Code agents with workflows:

**OLD Manual Integration:**
```typescript
const claudeAgent = createClaudeCodeAgent(config);
const workflow = createQiWorkflow({
  analyzer: async (context, dispatch) => {
    // Manual agent integration
    const result = await claudeAgent.generate({...});
    // Process result...
  },
});
```

**NEW Built-in Integration:**
```typescript
const workflow = createClaudeCodeMathematicalWorkflow({
  model: "claude-3-5-sonnet-20241022",
  temperature: 0.3,
});

// Or use factory pattern
const workflow = QiAgent.createClaudeCodeMathematicalWorkflow();
```

## Feature Parity

The new unified framework maintains **all capabilities** from the original qiagent:

### ✅ Production Features Preserved

- **Circuit Breaker Pattern**: Automatic failure handling and recovery
- **Rate Limiting**: Respect API limits and quotas
- **Retry Logic**: Exponential backoff with jitter
- **Error Handling**: Comprehensive error categorization
- **Metrics Collection**: Performance monitoring and health status
- **Type Safety**: Full TypeScript support with proper interfaces

### ✅ Enhanced Features Added

- **AI Orchestra Integration**: Multi-agent workflows
- **Streaming Support**: Real-time response generation
- **Hybrid Workflows**: Mix Claude Code with other models (Ollama, OpenAI)
- **Mathematical Patterns**: Built-in mathematical analysis workflows
- **Factory Pattern**: Unified agent and workflow creation

## API Reference

### Core Interfaces

```typescript
// Claude Code Agent
interface ClaudeCodeAgent {
  generate(request: ClaudeCodeRequest): Promise<Result<ClaudeCodeResponse>>;
  generateStream(request: ClaudeCodeRequest): Promise<AsyncIterableIterator<Result<string>>>;
  asLanguageModel(): LanguageModel;
  getConfig(): ClaudeCodeConfig & { provider: "claude-code" };
}

// Request/Response Types
interface ClaudeCodeRequest {
  readonly prompt: string;
  readonly systemPrompt?: string;
  readonly context?: Record<string, unknown>;
  readonly tools?: string[];
  readonly stream?: boolean;
}

interface ClaudeCodeResponse {
  readonly content: string;
  readonly model: string;
  readonly usage?: TokenUsage;
  readonly finishReason: string;
  readonly id?: string;
  readonly metadata?: Record<string, unknown>;
}
```

### Factory Functions

```typescript
// Individual agent creation
const agent = ClaudeCode.createAgent(config?: ClaudeCodeConfig);
const model = ClaudeCode.createModel(config?: ClaudeCodeConfig);

// Workflow creation
const mathWorkflow = createClaudeCodeMathematicalWorkflow(config?: ClaudeCodeConfig);
const contractWorkflow = createClaudeCodeContractWorkflow(config?: ClaudeCodeConfig);
const hybridWorkflow = createHybridWorkflow(hybridConfig);

// Unified factory access
const agent = QiAgent.ClaudeCode.createAgent();
const workflow = QiAgent.createClaudeCodeMathematicalWorkflow();
```

## Example Migration

### Complete Before/After Example

**BEFORE (Old qiagent):**
```typescript
// Old imports
import { 
  createClaudeCodeAgent, 
  type AgentConfig, 
  type GenerationRequest,
  validateGenerationRequest 
} from "../../../lib/src/qiagent";
import { createQiWorkflow } from "@qi/agent";

// Old configuration
const config: AgentConfig = {
  provider: "claude-code",
  timeout: 30000,
  maxRetries: 3,
  authentication: { apiKey: process.env.ANTHROPIC_API_KEY },
};

// Old agent creation
const agent = createClaudeCodeAgent(config);

// Old request format
const request: GenerationRequest = {
  model: {
    id: "claude-1",
    name: "Claude 3.5 Sonnet", 
    provider: "anthropic",
    modelName: "claude-3-5-sonnet-20241022",
    temperature: 0.7,
    maxTokens: 4000,
  },
  userPrompt: "Analyze the Maybe monad implementation",
  systemPrompt: "You are a mathematical verification expert",
};

// Old execution
const validationResult = validateGenerationRequest(request);
if (!isFailure(validationResult)) {
  const result = await agent.generate(request);
  if (!isFailure(result)) {
    console.log(result.right.content);
  }
}
```

**AFTER (New unified framework):**
```typescript
// New imports
import { 
  ClaudeCode, 
  createClaudeCodeMathematicalWorkflow,
  QiAgent 
} from "@qi/agent";

// New agent creation (simpler)
const agent = ClaudeCode.createAgent({
  model: "claude-3-5-sonnet-20241022",
  temperature: 0.7,
  maxTokens: 4000,
});

// New request format (simpler)
const result = await agent.generate({
  prompt: "Analyze the Maybe monad implementation",
  systemPrompt: "You are a mathematical verification expert",
});

// New execution (cleaner)
if (result._tag === "Right") {
  console.log(result.right.content);
  console.log(`Usage: ${result.right.usage?.totalTokens} tokens`);
}

// Or use workflow approach
const workflow = createClaudeCodeMathematicalWorkflow();
const workflowRun = workflow.createRun({
  agent: "researcher",
  context: {
    messages: [{ role: "user", content: "Analyze the Maybe monad implementation" }],
    currentAgent: "researcher",
    data: { component: "Maybe<T>" },
    metadata: { startTime: Date.now(), stepHistory: [] },
  },
});
```

## Testing Your Migration

### 1. Verify Basic Agent Functionality

```typescript
import { ClaudeCode } from "@qi/agent";

const agent = ClaudeCode.createAgent();
const result = await agent.generate({
  prompt: "Test message: Explain monads briefly",
});

console.log("Migration test:", result._tag === "Right" ? "✅ SUCCESS" : "❌ FAILED");
```

### 2. Test Workflow Integration

```typescript
import { createClaudeCodeMathematicalWorkflow } from "@qi/agent";

const workflow = createClaudeCodeMathematicalWorkflow();
console.log("Workflow creation:", workflow ? "✅ SUCCESS" : "❌ FAILED");
```

### 3. Verify Error Handling

```typescript
const agent = ClaudeCode.createAgent({ apiKey: "invalid" });
const result = await agent.generate({ prompt: "test" });

console.log("Error handling:", result._tag === "Left" ? "✅ SUCCESS" : "❌ FAILED");
```

## Troubleshooting

### Common Migration Issues

1. **Import Errors**
   ```bash
   # Error: Cannot find module '../../../lib/src/qiagent'
   # Solution: Update to unified imports
   import { ClaudeCode } from "@qi/agent";
   ```

2. **Configuration Differences**
   ```typescript
   // Old complex config objects → New simple config
   // Move from AgentConfig interface to ClaudeCodeConfig
   ```

3. **Request Format Changes**
   ```typescript
   // Old GenerationRequest → New ClaudeCodeRequest
   // Simpler structure, direct property access
   ```

4. **Response Handling**
   ```typescript
   // Old: isFailure(result) → New: result._tag === "Left"
   // Both use Result<T> pattern but different APIs
   ```

### Performance Optimization

The new framework includes **automatic optimizations**:

- **Connection Pooling**: Reuse HTTP connections
- **Request Batching**: Group similar requests when possible
- **Smart Retries**: Context-aware retry strategies
- **Memory Management**: Efficient streaming and garbage collection

## Next Steps

1. **Update all imports** to use the unified `@qi/agent` package
2. **Migrate configurations** to the simpler format
3. **Test individual agents** before workflow integration
4. **Explore hybrid workflows** for optimal performance
5. **Leverage factory patterns** for consistent agent creation

For more examples, see:
- `app/src/examples/claude-code-agent-demo.ts` - Complete demonstration
- `docs/guides/ollama-integration-tutorial.md` - Hybrid workflow patterns
- `app/src/examples/comprehensive-ollama-demo.ts` - Multi-model integration

The migration preserves all production features while adding powerful new capabilities for mathematical analysis and multi-agent workflows.