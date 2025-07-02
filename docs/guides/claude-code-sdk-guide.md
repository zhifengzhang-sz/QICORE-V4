# 🤖 Claude Code SDK Integration Guide

**Complete guide to using Claude Code SDK through the @qi/agent unified framework**

## 🎯 **Overview**

The QiCore project provides seamless Claude Code SDK integration through the `@qi/agent` package, offering both individual agent capabilities and sophisticated multi-agent workflows powered by AI Orchestra.

### **Why Choose Claude Code Integration?**

✅ **Production-ready**: Built-in error handling, rate limiting, and circuit breakers  
✅ **Type-safe**: Full TypeScript support with Result<T> patterns  
✅ **Unified API**: Consistent interface with other AI providers  
✅ **Streaming support**: Real-time response generation  
✅ **Mathematical focus**: Specialized workflows for formal analysis  
✅ **Zero breaking changes**: Existing workflows continue to work  

## 🚀 **Quick Start (5 Minutes)**

### **1. Installation**

```bash
# Core dependencies
bun add @qi/agent @anthropic-ai/sdk

# Set your API key
export ANTHROPIC_API_KEY="your-api-key-here"
```

### **2. Basic Usage**

```typescript
import { ClaudeCode } from "@qi/agent";

// Create Claude Code agent
const result = ClaudeCode.createAgent({
  model: "claude-3-5-sonnet-20241022",
  temperature: 0.3,
  maxTokens: 1000,
});

if (result._tag === "Right") {
  const agent = result.right;
  
  // Generate response
  const response = await agent.generate({
    prompt: "Explain TypeScript generics in simple terms",
    systemPrompt: "You are a helpful coding teacher.",
  });
  
  if (response._tag === "Right") {
    console.log(response.right.content);
  }
}
```

### **3. Run the Demo**

```bash
# Run the comprehensive demo
bun app/src/examples/claude-code-agent-demo.ts

# Run the quickstart tutorial
bun app/src/examples/claude-code-quickstart.ts
```

## 📚 **Available Demos**

The project includes comprehensive demos showcasing different Claude Code integration patterns:

| Demo | Description | Best For |
|------|-------------|----------|
| **claude-code-quickstart.ts** | 5-minute introduction to Claude Code basics | New users |
| **claude-code-agent-demo.ts** | Full-featured workflows and advanced patterns | Understanding capabilities |
| **claude-code-tools-demo.ts** | File analysis, documentation, and code tools | Development workflows |
| **claude-code-production-demo.ts** | Production patterns with monitoring | Deployment planning |

## 🏗️ **Architecture Patterns**

### **1. Individual Agent Usage**

Perfect for simple, single-step operations:

```typescript
import { ClaudeCode } from "@qi/agent";

// Create agent with specific configuration
const agent = ClaudeCode.createAgent({
  model: "claude-3-5-sonnet-20241022",
  temperature: 0.2,  // Lower for more focused output
  maxTokens: 2000,
  maxRetries: 3,
  timeout: 30000,
});

// Use for code analysis
const codeReview = await agent.generate({
  prompt: `Review this TypeScript code for potential issues:
  
\`\`\`typescript
${yourCode}
\`\`\``,
  systemPrompt: "You are an expert TypeScript developer focused on best practices.",
});
```

### **2. Mathematical Workflows**

Specialized multi-agent workflows for formal analysis:

```typescript
import { createClaudeCodeMathematicalWorkflow } from "@qi/agent";

// Create mathematical analysis workflow
const workflow = createClaudeCodeMathematicalWorkflow({
  model: "claude-3-5-sonnet-20241022",
  temperature: 0.1,  // Very low for deterministic mathematical output
  maxTokens: 4000,
});

// Execute mathematical analysis
const result = workflow.createRun({
  agent: "researcher",
  context: {
    messages: [{
      role: "user",
      content: "Verify that the Maybe<T> type satisfies monad laws"
    }],
    currentAgent: "researcher",
    data: { component: "Maybe<T>", analysisType: "monad_verification" },
    metadata: { startTime: Date.now() }
  },
  onFinish: async (finalState) => {
    console.log("Analysis complete:", finalState.context.data);
  }
});
```

### **3. Hybrid Workflows**

Combine Claude Code with other models for optimal performance:

```typescript
import { createHybridWorkflow } from "@qi/agent";
import { ollama } from "ollama-ai-provider";

// Use Claude for analysis, Ollama for verification
const hybrid = createHybridWorkflow({
  claudeConfig: {
    model: "claude-3-5-sonnet-20241022",
    temperature: 0.3,
  },
  otherModels: {
    verifier: ollama("qwen3:0.6b"),  // Fast local verification
  }
});
```

### **4. Streaming Responses**

Real-time response generation:

```typescript
// Get streaming iterator
const streamIterator = await agent.generateStream({
  prompt: "Explain the SOLID principles step by step",
  systemPrompt: "Provide detailed explanations with examples.",
});

// Process chunks in real-time
for await (const result of streamIterator) {
  if (result._tag === "Right") {
    process.stdout.write(result.right);  // Live output
  } else {
    console.error("Stream error:", result.left.message);
    break;
  }
}
```

## 🔧 **Configuration Options**

### **Agent Configuration**

```typescript
interface ClaudeCodeConfig {
  readonly apiKey?: string;           // API key (or use env var)
  readonly timeout?: number;          // Request timeout (default: 30000ms)
  readonly maxRetries?: number;       // Retry attempts (default: 3)
  readonly temperature?: number;      // Creativity (0-1, default: 0.7)
  readonly maxTokens?: number;        // Response length (default: 4000)
  readonly model?: string;            // Model ID (default: claude-3-5-sonnet-20241022)
}
```

### **Specialized Configurations**

```typescript
// Mathematical analysis (deterministic)
const mathAgent = ClaudeCode.createAgent({
  temperature: 0.1,     // Low creativity for consistency
  maxTokens: 8000,      // Long explanations
  maxRetries: 5,        // Extra reliability
  timeout: 60000,       // Complex analysis needs time
});

// Creative writing (flexible)
const creativeAgent = ClaudeCode.createAgent({
  temperature: 0.8,     // High creativity
  maxTokens: 2000,      // Moderate length
  maxRetries: 2,        // Fast iteration
});

// Code review (balanced)
const reviewAgent = ClaudeCode.createAgent({
  temperature: 0.3,     // Focused but flexible
  maxTokens: 1500,      // Concise feedback
  maxRetries: 3,        // Standard reliability
});
```

## 🛡️ **Production Patterns**

### **Error Handling**

All Claude Code operations return `Result<T>` for robust error handling:

```typescript
const result = await agent.generate({ prompt: "Hello" });

// Pattern matching for type-safe error handling
if (result._tag === "Right") {
  const response = result.right;
  console.log("Success:", response.content);
  console.log("Tokens:", response.usage?.totalTokens);
} else {
  const error = result.left;
  console.error("Error:", error.message);
  console.error("Category:", error.category);  // NETWORK, BUSINESS, SYSTEM, etc.
}
```

### **Rate Limiting**

```typescript
class RateLimiter {
  constructor(private maxRequests: number, private windowMs: number) {}
  
  async checkLimit(): Promise<boolean> {
    // Implementation tracks requests per time window
    return this.requests.length < this.maxRequests;
  }
}

// Usage
const rateLimiter = new RateLimiter(10, 60000);  // 10 requests per minute

if (await rateLimiter.checkLimit()) {
  const result = await agent.generate({ prompt: "..." });
} else {
  console.log("Rate limit exceeded, please wait");
}
```

### **Circuit Breaker**

```typescript
class CircuitBreaker {
  private state: 'closed' | 'open' | 'half-open' = 'closed';
  
  async execute<T>(operation: () => Promise<T>): Promise<T> {
    if (this.state === 'open') {
      throw new Error('Circuit breaker is OPEN');
    }
    
    try {
      const result = await operation();
      this.onSuccess();
      return result;
    } catch (error) {
      this.onFailure();
      throw error;
    }
  }
}
```

### **Health Monitoring**

```typescript
class ClaudeCodeService {
  getHealthStatus() {
    return {
      initialized: this.initialized,
      circuitBreaker: { state: 'closed', failures: 0 },
      rateLimiter: { current: 5, limit: 10, resetTime: Date.now() + 30000 },
      lastSuccess: Date.now() - 1000,
      errorRate: 0.02,  // 2% error rate
    };
  }
}
```

## 🔗 **Integration with Existing Code**

### **Factory Pattern Access**

Use the unified QiAgent factory for consistent API:

```typescript
import { QiAgent } from "@qi/agent";

// Create Claude Code agent through factory
const claudeAgent = QiAgent.ClaudeCode.createAgent({
  model: "claude-3-5-sonnet-20241022",
});

// Create Claude Code model for workflows
const claudeModel = QiAgent.ClaudeCode.createModel({
  temperature: 0.4,
});

// Create mathematical workflow
const mathWorkflow = QiAgent.createMathematicalWorkflow({
  researcherModel: claudeModel,
  verifierModel: claudeModel,
  reporterModel: claudeModel,
});
```

### **Backward Compatibility**

Existing workflows continue to work unchanged:

```typescript
// Your existing code works as-is
import { QiPrompt } from "@qi/agent";

const qiPrompt = new QiPrompt();
const result = await qiPrompt.generateText("Hello world");

// Now enhanced with Claude Code capabilities
const claudeResult = await qiPrompt.generateText("Hello world", {
  model: claudeModel,  // Use Claude Code model
});
```

## 📊 **Performance Characteristics**

### **Response Times**

| Operation | Typical Latency | Notes |
|-----------|----------------|-------|
| Agent Creation | < 100μs | Cached after first creation |
| Simple Generation | 1-3 seconds | Network dependent |
| Streaming Start | < 500ms | First chunk arrival |
| Mathematical Analysis | 5-15 seconds | Complex reasoning |
| Workflow Execution | 10-30 seconds | Multi-step processes |

### **Token Usage Optimization**

```typescript
// Optimize for cost and speed
const costOptimized = ClaudeCode.createAgent({
  model: "claude-3-haiku-20240307",  // Faster, cheaper model
  maxTokens: 1000,                   // Limit response length
  temperature: 0.2,                  // Focused responses
});

// Optimize for quality
const qualityOptimized = ClaudeCode.createAgent({
  model: "claude-3-5-sonnet-20241022",  // Best reasoning model
  maxTokens: 4000,                      // Allow detailed responses
  temperature: 0.7,                     // Balanced creativity
});
```

## 🧪 **Testing Strategies**

### **Unit Testing**

```typescript
import { describe, it, expect } from "vitest";
import { ClaudeCode } from "@qi/agent";

describe("Claude Code Agent", () => {
  it("should create agent successfully", () => {
    const result = ClaudeCode.createAgent();
    expect(result._tag).toBe("Right");
  });

  it("should handle generation errors gracefully", async () => {
    const agent = ClaudeCode.createAgent({ apiKey: "invalid" });
    const result = await agent.generate({ prompt: "test" });
    expect(result._tag).toBe("Left");
    expect(result.left.category).toBe("NETWORK");
  });
});
```

### **Integration Testing**

```typescript
describe("Claude Code Integration", () => {
  it("should complete mathematical workflow", async () => {
    const workflow = createClaudeCodeMathematicalWorkflow();
    
    const result = await workflow.createRun({
      agent: "researcher",
      context: { /* test context */ },
      onFinish: async (state) => {
        expect(state.context.data.analysis).toBeDefined();
      }
    });
    
    expect(result).toBeDefined();
  });
});
```

## 🚀 **Deployment Guide**

### **Environment Setup**

```bash
# Production environment variables
export ANTHROPIC_API_KEY="your-production-key"
export CLAUDE_MODEL="claude-3-5-sonnet-20241022"
export CLAUDE_TIMEOUT="30000"
export CLAUDE_MAX_RETRIES="3"
export CLAUDE_RATE_LIMIT="50"  # requests per minute
```

### **Docker Configuration**

```dockerfile
FROM oven/bun:1.0

WORKDIR /app

# Install dependencies
COPY package.json bun.lockb ./
RUN bun install --frozen-lockfile

# Copy source
COPY . .

# Environment
ENV NODE_ENV=production
ENV ANTHROPIC_API_KEY=${ANTHROPIC_API_KEY}

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD bun run health-check.ts

# Start application
CMD ["bun", "run", "start"]
```

### **Monitoring Setup**

```typescript
// Application metrics
const metrics = {
  claude_requests_total: 0,
  claude_requests_failed: 0,
  claude_latency_avg: 0,
  claude_tokens_used: 0,
  claude_cost_usd: 0,
};

// Track metrics for each request
await trackMetrics(async () => {
  return await agent.generate({ prompt });
});
```

## 🎯 **Best Practices**

### **DO's**

✅ **Use Result<T> patterns** for all error handling  
✅ **Set appropriate timeouts** for your use case  
✅ **Implement rate limiting** to respect API limits  
✅ **Monitor token usage** for cost optimization  
✅ **Use streaming** for better user experience  
✅ **Cache agents** rather than recreating them  
✅ **Implement circuit breakers** for resilience  

### **DON'Ts**

❌ **Don't hardcode API keys** in source code  
❌ **Don't ignore error handling** - always check Result<T>  
❌ **Don't create new agents** for every request  
❌ **Don't use blocking calls** in UI threads  
❌ **Don't exceed rate limits** without backoff  
❌ **Don't skip monitoring** in production  

## 🔗 **Next Steps**

1. **Start with quickstart**: Run `claude-code-quickstart.ts`
2. **Explore workflows**: Try `claude-code-agent-demo.ts`
3. **Build tools**: Adapt `claude-code-tools-demo.ts` patterns
4. **Plan production**: Review `claude-code-production-demo.ts`
5. **Integrate**: Add to your existing project with the factory pattern

## 📚 **Additional Resources**

- **[Anthropic API Documentation](https://docs.anthropic.com/)**
- **[QiCore Architecture Guide](./architecture.md)**
- **[AI Orchestra Workflows](../api/orchestra.md)**
- **[Error Handling Patterns](./error-handling.md)**
- **[Production Deployment](./deployment.md)**

---

**Ready to build powerful AI applications with Claude Code? Start with the quickstart demo and explore the comprehensive examples!** 🚀