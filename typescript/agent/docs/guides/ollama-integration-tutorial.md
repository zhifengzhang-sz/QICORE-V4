# QiCore + Ollama Integration Tutorial

A comprehensive guide to using QiCore's AI wrappers (@qi/prompt, @qi/mcp, @qi/agent) with Ollama for local LLM-powered mathematical analysis workflows.

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Quick Start](#quick-start)
4. [Wrapper Deep Dive](#wrapper-deep-dive)
5. [Complete Examples](#complete-examples)
6. [Best Practices](#best-practices)
7. [Troubleshooting](#troubleshooting)

## Overview

QiCore provides three powerful wrappers that integrate seamlessly with Ollama for local LLM processing:

- **@qi/prompt**: Vercel AI SDK wrapper for mathematical prompting and text generation
- **@qi/mcp**: Model Context Protocol wrapper for external tool integration
- **@qi/agent**: AI Orchestra wrapper for multi-agent workflow orchestration

## Prerequisites

### 1. Install Ollama

```bash
# macOS
brew install ollama

# Linux
curl -fsSL https://ollama.ai/install.sh | sh

# Windows
# Download from https://ollama.ai/download
```

### 2. Start Ollama Service

```bash
ollama serve
```

### 3. Pull Required Models

```bash
# Fast, lightweight model for development
ollama pull qwen3:0.6b

# More capable model for production
ollama pull llama3.1:8b
```

### 4. Install QiCore Dependencies

```bash
bun install
```

## Quick Start

### Verify Ollama Connection

```typescript
import { ollama } from "ollama-ai-provider";
import { generateText } from "@qi/prompt";

// Test connection
const response = await fetch("http://localhost:11434/api/tags");
if (response.ok) {
    console.log("✅ Ollama is running");
} else {
    console.log("❌ Ollama not available");
}

// Test text generation
const { text } = await generateText({
    model: ollama("qwen3:0.6b"),
    prompt: "Explain what a monad is in category theory",
    temperature: 0.7,
    maxTokens: 200,
});

console.log("Generated text:", text);
```

### Run Demo Examples

```bash
# Run individual wrapper demos
bun run app/src/examples/ollama-qiprompt-demo.ts
bun run app/src/examples/ollama-qimcp-demo.ts
bun run app/src/examples/ollama-qiagent-demo.ts

# Run comprehensive integration demo
bun run app/src/examples/comprehensive-ollama-demo.ts
```

## Wrapper Deep Dive

### @qi/prompt: Mathematical Text Generation

The `@qi/prompt` wrapper enhances Vercel AI SDK with mathematical analysis capabilities.

#### Basic Text Generation

```typescript
import { generateText } from "@qi/prompt";
import { ollama } from "ollama-ai-provider";

const { text } = await generateText({
    model: ollama("qwen3:0.6b"),
    system: "You are a mathematical analysis expert specializing in category theory.",
    prompt: `Analyze the mathematical properties of this Result<T> type:

interface Result<T> {
    readonly isSuccess: boolean;
    readonly value: T | undefined;
    readonly error: Error | undefined;
    map<U>(fn: (value: T) => U): Result<U>;
    flatMap<U>(fn: (value: T) => Result<U>): Result<U>;
}

Focus on its categorical structure and algebraic properties.`,
    temperature: 0.7,
    maxTokens: 800,
});

console.log("Analysis:", text);
```

#### Streaming Text Generation

```typescript
import { streamText } from "@qi/prompt";

const { textStream } = await streamText({
    model: ollama("qwen3:0.6b"),
    system: "You are a formal verification expert.",
    prompt: `Verify if this TypeScript implementation satisfies the Monad laws:

class Maybe<T> {
    constructor(private value: T | null) {}
    
    static of<T>(value: T): Maybe<T> { return new Maybe(value); }
    
    flatMap<U>(fn: (value: T) => Maybe<U>): Maybe<U> {
        return this.value === null ? new Maybe<U>(null) : fn(this.value);
    }
}

Check: Left Identity, Right Identity, and Associativity laws.`,
    temperature: 0.3,
    maxTokens: 1000,
});

// Process stream with real-time updates
for await (const chunk of textStream) {
    process.stdout.write(chunk);
}
```

#### Structured Object Generation

```typescript
import { generateObject } from "@qi/prompt";
import { z } from "zod";

// Define schema for mathematical analysis
const analysisSchema = z.object({
    component: z.string().describe("The mathematical component being analyzed"),
    category: z.enum(["functor", "applicative", "monad", "monoid", "semigroup"])
        .describe("Primary categorical structure"),
    properties: z.array(z.object({
        name: z.string(),
        satisfied: z.boolean(),
        explanation: z.string(),
    })).describe("Mathematical properties and their verification"),
    completenessScore: z.number().min(0).max(100).describe("Completeness score (0-100)"),
    recommendations: z.array(z.string()).describe("Suggestions for improvement"),
    mathematicalNotes: z.string().describe("Additional mathematical insights"),
});

const { object } = await generateObject({
    model: ollama("qwen3:0.6b"),
    system: "You are a category theory expert. Analyze mathematical structures with precision.",
    prompt: `Analyze the mathematical structure of this Either type:

type Either<L, R> = Left<L> | Right<R>;

interface Left<L> { readonly _tag: 'Left'; readonly left: L; }
interface Right<R> { readonly _tag: 'Right'; readonly right: R; }

const map = <L, R, B>(fa: Either<L, R>, f: (a: R) => B): Either<L, B> =>
    fa._tag === 'Left' ? fa : { _tag: 'Right', right: f(fa.right) };

const flatMap = <L, R, B>(fa: Either<L, R>, f: (a: R) => Either<L, B>): Either<L, B> =>
    fa._tag === 'Left' ? fa : f(fa.right);`,
    schema: analysisSchema,
    temperature: 0.4,
});

console.log("Structured analysis:", object);
```

#### Tool Integration

```typescript
import { generateText } from "@qi/prompt";
import { z } from "zod";

const { text, toolCalls } = await generateText({
    model: ollama("qwen3:0.6b"),
    system: "You are a mathematical verification assistant. Use the provided tools to analyze and verify mathematical structures.",
    prompt: "I need to verify if the function composition operation forms a monoid. Please check the identity and associativity properties.",
    tools: {
        verifyIdentityLaw: {
            description: "Verify identity law for a mathematical structure",
            parameters: z.object({
                structure: z.string().describe("The mathematical structure to verify"),
                identityElement: z.string().describe("The proposed identity element"),
                operation: z.string().describe("The binary operation"),
            }),
            execute: async ({ structure, identityElement, operation }) => {
                // Simulate mathematical verification
                const isValid = structure === "function composition" && 
                               identityElement === "identity function" && 
                               operation === "compose";
                
                return {
                    verified: isValid,
                    explanation: isValid 
                        ? "Identity law satisfied: compose(f, id) = compose(id, f) = f"
                        : "Identity law verification failed",
                    confidence: isValid ? 0.95 : 0.3,
                };
            },
        },
        verifyAssociativityLaw: {
            description: "Verify associativity law for a binary operation",
            parameters: z.object({
                operation: z.string().describe("The binary operation to verify"),
                example: z.string().describe("Example demonstrating associativity"),
            }),
            execute: async ({ operation }) => {
                return {
                    verified: true,
                    explanation: `Associativity verified for ${operation}: (f ∘ g) ∘ h = f ∘ (g ∘ h)`,
                    mathematicalProof: "Function composition is inherently associative by definition",
                    confidence: 0.99,
                };
            },
        },
    },
    maxSteps: 3,
});

console.log("Tool-assisted verification result:", text);
console.log("Tools called:", toolCalls.length);
```

### @qi/mcp: External Tool Integration

The `@qi/mcp` wrapper provides seamless integration with Model Context Protocol servers for external tool access.

#### Memory Server Integration

```typescript
import { MCPClient } from "@qi/mcp";
import { generateText } from "@qi/prompt";
import { ollama } from "ollama-ai-provider";

const mcpLogger = {
    info: (msg: string) => console.log(`[MCP] ℹ️  ${msg}`),
    warn: (msg: string) => console.log(`[MCP] ⚠️  ${msg}`),
    error: (msg: string) => console.log(`[MCP] ❌ ${msg}`),
};

const mcpClient = new MCPClient(mcpLogger);

// Connect to memory server
const connected = await mcpClient.connectToServer({
    name: "memory-math",
    command: "bunx",
    args: ["--bun", "@modelcontextprotocol/server-memory"],
    env: { NODE_ENV: "development" },
});

if (connected) {
    console.log("✅ Connected to memory server");
    
    // Store mathematical concepts with AI-generated descriptions
    const mathConcepts = [
        { entity: "Monad", relations: ["satisfies", "Identity Law", "Associativity Law"] },
        { entity: "Functor", relations: ["maps", "preserves structure", "composition"] },
        { entity: "Result<T>", relations: ["implements", "Monad", "Functor"] },
    ];
    
    for (const concept of mathConcepts) {
        // Generate rich descriptions using Ollama
        const { text: description } = await generateText({
            model: ollama("qwen3:0.6b"),
            system: "You are a mathematical expert. Provide precise, technical descriptions.",
            prompt: `Provide a concise but comprehensive description of ${concept.entity} in category theory and functional programming.`,
            temperature: 0.3,
            maxTokens: 200,
        });
        
        console.log(`Generated description for ${concept.entity}:`, description.substring(0, 150) + "...");
        // Store in memory server (would use actual MCP calls)
    }
    
    await mcpClient.disconnect();
}
```

#### Filesystem Server Integration

```typescript
import { AnalysisFileManager } from "@qi/mcp/tools/file";

const fileManager = new AnalysisFileManager();

// Connect to filesystem server
const connected = await mcpClient.connectToServer({
    name: "filesystem-analysis",
    command: "bunx",
    args: ["--bun", "@modelcontextprotocol/server-filesystem", process.cwd()],
    env: { NODE_ENV: "development" },
});

if (connected) {
    // Save mathematical analysis results
    fileManager.saveAnalysisResult({
        component: "Option<T>",
        timestamp: new Date().toISOString(),
        algebraicStructures: ["Functor", "Monad"],
        completenessScore: 85,
        inevitablePatterns: ["Sequential computation", "Error handling"],
        gaps: ["Edge case handling"],
        claudeAnalysis: "Mathematical contract for Option type",
        ollamaVerification: "/* TypeScript implementation */",
    });
    
    // Analyze code using Ollama
    const { text: analysis } = await generateText({
        model: ollama("qwen3:0.6b"),
        system: "You are a formal verification expert.",
        prompt: `Analyze this Option<T> implementation for mathematical correctness:

type Option<T> = Some<T> | None;
interface Some<T> { readonly _tag: 'Some'; readonly value: T; }
interface None { readonly _tag: 'None'; }

Check:
1. Functor laws (identity, composition)
2. Monad laws (left identity, right identity, associativity)
3. Implementation correctness
4. Type safety`,
        temperature: 0.3,
        maxTokens: 800,
    });
    
    console.log("Mathematical analysis:", analysis.substring(0, 300) + "...");
    
    await mcpClient.disconnect();
}
```

### @qi/agent: Multi-Agent Workflows

The `@qi/agent` wrapper orchestrates complex multi-agent workflows using AI Orchestra.

#### Basic Multi-Agent Workflow

```typescript
import { 
    createQiWorkflow, 
    type QiAgentHandler, 
    type QiWorkflowContext,
    type Dispatch,
    type HandlerResult,
    processStream 
} from "@qi/agent";
import { streamText } from "@qi/prompt";
import { ollama } from "ollama-ai-provider";

// Define agent handlers
const handlers: Record<string, QiAgentHandler> = {
    analyzer: async (
        context: QiWorkflowContext, 
        dispatch: Dispatch
    ): Promise<HandlerResult<"analyzer" | "reviewer" | "reporter", QiWorkflowContext>> => {
        console.log("🔍 Analyzer: Starting mathematical analysis...");
        
        await dispatch("analysis_started", {
            agent: "analyzer",
            component: context.data.component || "unknown",
        });
        
        // Use Ollama for analysis
        const stream = streamText({
            model: ollama("qwen3:0.6b"),
            system: "You are a mathematical analysis expert. Provide concise, technical analysis.",
            prompt: `Analyze the mathematical properties of: ${context.data.component || "mathematical structure"}

Focus on:
1. Categorical structure (Functor, Monad, etc.)
2. Algebraic properties
3. Verification of mathematical laws`,
            temperature: 0.3,
            maxTokens: 400,
        });
        
        // Process the stream
        const result = await processStream(stream, dispatch);
        
        return {
            nextState: "reviewer",
            context: {
                ...context,
                data: {
                    ...context.data,
                    analysis: result.text,
                    analysisComplete: true,
                },
            },
        };
    },
    
    reviewer: async (
        context: QiWorkflowContext, 
        dispatch: Dispatch
    ): Promise<HandlerResult<"analyzer" | "reviewer" | "reporter", QiWorkflowContext>> => {
        console.log("🔎 Reviewer: Validating analysis...");
        
        const stream = streamText({
            model: ollama("qwen3:0.6b"),
            system: "You are a mathematical verification expert. Review analysis for correctness.",
            prompt: `Review this mathematical analysis for accuracy:

${context.data.analysis || "No analysis provided"}

Provide:
1. Verification of mathematical claims
2. Identification of any errors or gaps
3. Suggestions for improvement
4. Confidence score (0-100)`,
            temperature: 0.2,
            maxTokens: 300,
        });
        
        const result = await processStream(stream, dispatch);
        
        return {
            nextState: "reporter",
            context: {
                ...context,
                data: {
                    ...context.data,
                    review: result.text,
                    reviewComplete: true,
                },
            },
        };
    },
    
    reporter: async (
        context: QiWorkflowContext, 
        dispatch: Dispatch
    ): Promise<HandlerResult<"analyzer" | "reviewer" | "reporter", QiWorkflowContext>> => {
        console.log("📊 Reporter: Generating final report...");
        
        const stream = streamText({
            model: ollama("qwen3:0.6b"),
            system: "You are a technical report writer. Create clear, structured reports.",
            prompt: `Create a comprehensive report based on:

ANALYSIS: ${context.data.analysis || "No analysis"}

REVIEW: ${context.data.review || "No review"}

Structure the report with:
1. Executive Summary
2. Mathematical Findings
3. Verification Results
4. Recommendations
5. Conclusion`,
            temperature: 0.4,
            maxTokens: 500,
        });
        
        const result = await processStream(stream, dispatch);
        
        return {
            context: {
                ...context,
                data: {
                    ...context.data,
                    finalReport: result.text,
                    workflowComplete: true,
                },
            },
        };
    },
};

// Create and run the workflow
const workflow = createQiWorkflow(handlers);

const initialContext: QiWorkflowContext = {
    messages: [{ role: "user", content: "Analyze the Result<T> monad implementation" }],
    currentAgent: "analyzer",
    data: {
        component: "Result<T>",
        timestamp: new Date().toISOString(),
    },
    metadata: {
        startTime: Date.now(),
        stepHistory: [],
    },
};

const run = workflow.createRun({
    agent: "analyzer",
    context: initialContext,
    onFinish: async (finalState) => {
        console.log("🎯 Workflow completed!");
        console.log(`📊 Final agent: ${finalState.agent}`);
        console.log(`📈 Steps executed: ${finalState.context.metadata?.stepHistory?.length || 0}`);
        
        if (finalState.context.data.finalReport) {
            console.log(`📋 Report preview: ${finalState.context.data.finalReport.substring(0, 200)}...`);
        }
    },
});

console.log("✅ Workflow created and initiated");
```

#### Advanced Mathematical Workflow

```typescript
import { createMathematicalWorkflow } from "@qi/agent";

// Create specialized mathematical analysis workflow
const mathematicalWorkflow = createMathematicalWorkflow({
    researcherModel: ollama("qwen3:0.6b"),
    verifierModel: ollama("qwen3:0.6b"),
    reporterModel: ollama("qwen3:0.6b"),
});

const workflowContext: QiWorkflowContext = {
    messages: [{
        role: "user",
        content: "Analyze the mathematical properties of the Either<L,R> type and verify it satisfies the Monad laws"
    }],
    currentAgent: "researcher",
    workflowStep: "initial_analysis",
    data: {
        component: "Either<L,R>",
        analysisType: "category_theory",
        complexity: "advanced",
        laws: ["left_identity", "right_identity", "associativity"],
    },
    metadata: {
        startTime: Date.now(),
        stepHistory: [],
        requiresVerification: true,
    },
};

console.log("✅ Mathematical workflow created and ready");
```

#### Streaming Multi-Agent Workflow

```typescript
const streamingHandlers: Record<string, QiAgentHandler> = {
    researcher: async (context, dispatch) => {
        console.log("🔬 Researcher: Beginning investigation...");
        
        const researchStream = streamText({
            model: ollama("qwen3:0.6b"),
            system: "You are a mathematical researcher. Investigate the given topic thoroughly.",
            prompt: `Research the mathematical concept: ${context.data.topic || "Category Theory"}

Provide streaming insights as you research:
1. Historical context
2. Mathematical foundations  
3. Key theorems and properties
4. Modern applications
5. Open problems`,
            temperature: 0.5,
            maxTokens: 600,
        });
        
        console.log("📡 Starting streaming research...");
        
        let chunkCount = 0;
        let researchContent = "";
        
        // Process stream with real-time dispatch
        for await (const chunk of researchStream) {
            researchContent += chunk;
            chunkCount++;
            
            // Dispatch progress every 5 chunks
            if (chunkCount % 5 === 0) {
                await dispatch("research_progress", {
                    agent: "researcher",
                    chunks: chunkCount,
                    contentLength: researchContent.length,
                    preview: `${researchContent.substring(0, 100)}...`,
                });
                process.stdout.write(".");
            }
        }
        
        console.log(`\n✅ Research completed (${chunkCount} chunks)`);
        
        return {
            nextState: "synthesizer",
            context: {
                ...context,
                data: {
                    ...context.data,
                    research: researchContent,
                    researchChunks: chunkCount,
                },
            },
        };
    },
    
    synthesizer: async (context, dispatch) => {
        console.log("🧬 Synthesizer: Creating synthesis...");
        
        const synthesisStream = streamText({
            model: ollama("qwen3:0.6b"),
            system: "You are a mathematical synthesizer. Create clear, actionable insights from research.",
            prompt: `Synthesize this mathematical research into key insights:

${context.data.research || "No research provided"}

Create:
1. Key mathematical insights (3-5 points)
2. Practical applications
3. Learning recommendations
4. Further research directions`,
            temperature: 0.3,
            maxTokens: 400,
        });
        
        const synthesisResult = await processStream(synthesisStream, dispatch);
        
        return {
            context: {
                ...context,
                data: {
                    ...context.data,
                    synthesis: synthesisResult.text,
                    workflowComplete: true,
                },
            },
        };
    },
};

// Create streaming workflow
const streamingWorkflow = createQiWorkflow(streamingHandlers);

const streamingContext: QiWorkflowContext = {
    messages: [
        { role: "user", content: "Research and synthesize insights about Monad Transformers" },
    ],
    currentAgent: "researcher",
    data: {
        topic: "Monad Transformers",
        researchDepth: "comprehensive",
    },
    metadata: {
        startTime: Date.now(),
        stepHistory: [],
        streamingEnabled: true,
    },
};

const streamingRun = streamingWorkflow.createRun({
    agent: "researcher",
    context: streamingContext,
    onFinish: async (finalState) => {
        console.log("\n🎊 Streaming workflow completed!");
        console.log(`📊 Research chunks: ${finalState.context.data.researchChunks || 0}`);
        console.log(`📝 Synthesis preview: ${finalState.context.data.synthesis?.substring(0, 150) || "None"}...`);
    },
});
```

## Complete Examples

### Comprehensive Integration Example

The following example demonstrates all three wrappers working together in a complete mathematical analysis workflow:

```bash
# Run the comprehensive demo
bun run app/src/examples/comprehensive-ollama-demo.ts
```

This demo shows:
1. **Research Phase** (@qi/prompt): Generate mathematical content and analysis
2. **Storage Phase** (@qi/mcp): Store findings in memory and filesystem servers
3. **Verification Phase** (@qi/agent): Multi-agent verification workflow
4. **Synthesis Phase**: Combine results into final comprehensive report

### Mathematical Reasoning Chain

```typescript
// Multi-step mathematical reasoning
const steps = [
    "Define the mathematical structure",
    "Identify categorical properties", 
    "Verify algebraic laws",
    "Assess completeness and suggest improvements",
];

const context = {
    structure: "Option<T> type",
    definition: `type Option<T> = Some<T> | None;`,
    findings: [] as string[],
};

for (let i = 0; i < steps.length; i++) {
    console.log(`\n📋 Step ${i + 1}: ${steps[i]}`);
    
    const { text } = await generateText({
        model: ollama("qwen3:0.6b"),
        system: `You are conducting step ${i + 1} of ${steps.length} in a mathematical analysis.
            Previous findings: ${context.findings.join("; ")}
            Be concise and focus on this specific step.`,
        prompt: `${steps[i]} for: ${context.structure}

Definition: ${context.definition}

Provide a focused analysis for this step only.`,
        temperature: 0.3,
        maxTokens: 300,
    });
    
    context.findings.push(`Step ${i + 1}: ${text.substring(0, 100)}...`);
    console.log(`✅ Result: ${text.substring(0, 200)}...`);
}
```

## Best Practices

### 1. Model Selection

- **qwen3:0.6b**: Fast, suitable for development and simple tasks
- **llama3.1:8b**: More capable, better for complex mathematical reasoning
- **llama3.2**: Latest features, balanced performance

### 2. Temperature Settings

- **0.1-0.3**: Formal verification, mathematical proofs
- **0.4-0.6**: General analysis, balanced creativity
- **0.7-0.9**: Creative exploration, hypothesis generation

### 3. Token Limits

- **Short responses (200-400 tokens)**: Quick analysis, step-by-step reasoning
- **Medium responses (500-800 tokens)**: Comprehensive analysis
- **Long responses (1000+ tokens)**: Detailed reports, complex proofs

### 4. Error Handling

```typescript
async function safeOllamaCall<T>(operation: () => Promise<T>, fallback: T): Promise<T> {
    try {
        return await operation();
    } catch (error) {
        console.warn("Ollama operation failed, using fallback:", error);
        return fallback;
    }
}

// Usage
const analysis = await safeOllamaCall(
    () => generateText({
        model: ollama("qwen3:0.6b"),
        prompt: "Analyze mathematical structure",
    }),
    { text: "Analysis unavailable - Ollama connection failed" }
);
```

### 5. Performance Optimization

```typescript
// Batch operations for efficiency
const concepts = ["Monad", "Functor", "Applicative"];
const analyses = await Promise.all(
    concepts.map(concept => 
        generateText({
            model: ollama("qwen3:0.6b"),
            prompt: `Briefly explain ${concept} in category theory`,
            maxTokens: 200,
        })
    )
);
```

### 6. Structured Workflows

```typescript
// Use clear state management
interface MathematicalWorkflowState {
    phase: "analysis" | "verification" | "synthesis";
    component: string;
    findings: Record<string, any>;
    confidence: number;
    nextActions: string[];
}

// Maintain workflow context
const updateWorkflowState = (
    state: MathematicalWorkflowState, 
    updates: Partial<MathematicalWorkflowState>
): MathematicalWorkflowState => ({
    ...state,
    ...updates,
});
```

## Troubleshooting

### Common Issues

#### 1. Ollama Connection Failed

**Problem**: `fetch failed` or connection refused errors

**Solutions**:
```bash
# Check if Ollama is running
ollama ps

# Start Ollama service
ollama serve

# Check available models
ollama list

# Pull required model
ollama pull qwen3:0.6b
```

#### 2. Model Not Found

**Problem**: `model "qwen3:0.6b" not found`

**Solutions**:
```bash
# Pull the specific model
ollama pull qwen3:0.6b

# List available models
ollama list

# Use alternative model
# Replace qwen3:0.6b with available model in your code
```

#### 3. Memory Issues

**Problem**: Out of memory or slow performance

**Solutions**:
```typescript
// Reduce token limits
const { text } = await generateText({
    model: ollama("qwen3:0.6b"),
    prompt: "Your prompt here",
    maxTokens: 200, // Reduced from 800
    temperature: 0.3,
});

// Use streaming for large responses
const { textStream } = await streamText({
    model: ollama("qwen3:0.6b"),
    prompt: "Your prompt here",
    maxTokens: 1000,
});
```

#### 4. MCP Server Connection Issues

**Problem**: Cannot connect to MCP servers

**Solutions**:
```typescript
// Add connection timeout and retry logic
const connectWithRetry = async (client: MCPClient, config: any, maxRetries = 3) => {
    for (let i = 0; i < maxRetries; i++) {
        try {
            const connected = await client.connectToServer(config);
            if (connected) return true;
        } catch (error) {
            console.warn(`Connection attempt ${i + 1} failed:`, error);
            await new Promise(resolve => setTimeout(resolve, 1000));
        }
    }
    return false;
};
```

#### 5. Workflow Hangs

**Problem**: Multi-agent workflows get stuck

**Solutions**:
```typescript
// Add timeouts to workflow steps
const timeoutPromise = <T>(promise: Promise<T>, timeoutMs: number): Promise<T> =>
    Promise.race([
        promise,
        new Promise<T>((_, reject) =>
            setTimeout(() => reject(new Error(`Operation timed out after ${timeoutMs}ms`)), timeoutMs)
        ),
    ]);

// Usage in workflow
const result = await timeoutPromise(
    processStream(stream, dispatch),
    30000 // 30 second timeout
);
```

### Debug Mode

Enable detailed logging for troubleshooting:

```typescript
// Set debug environment variables
process.env.MCP_DEBUG = "true";
process.env.OLLAMA_DEBUG = "true";

// Use detailed logging
const debugLogger = {
    info: (msg: string) => console.log(`[DEBUG] ${new Date().toISOString()} ${msg}`),
    warn: (msg: string) => console.warn(`[WARN] ${new Date().toISOString()} ${msg}`),
    error: (msg: string) => console.error(`[ERROR] ${new Date().toISOString()} ${msg}`),
};
```

### Performance Monitoring

```typescript
// Monitor operation performance
const performanceMetrics = {
    connections: 0,
    operations: 0,
    errors: 0,
    totalTime: 0,
};

const monitoredOperation = async <T>(operation: () => Promise<T>, name: string): Promise<T> => {
    const startTime = Date.now();
    try {
        const result = await operation();
        performanceMetrics.operations++;
        performanceMetrics.totalTime += Date.now() - startTime;
        console.log(`✅ ${name}: ${Date.now() - startTime}ms`);
        return result;
    } catch (error) {
        performanceMetrics.errors++;
        console.log(`❌ ${name}: failed`);
        throw error;
    }
};

// Usage
const text = await monitoredOperation(
    () => generateText({
        model: ollama("qwen3:0.6b"),
        prompt: "Analyze mathematical structure",
    }),
    "Mathematical Analysis"
);
```

## Next Steps

1. **Explore Advanced Patterns**: Check out the examples in `app/src/examples/`
2. **Customize Workflows**: Modify the handlers in the agent examples
3. **Integrate with Your Domain**: Adapt the mathematical analysis patterns to your specific use case
4. **Scale Performance**: Implement batching and caching for production use
5. **Contribute**: Submit improvements and new patterns to the QiCore project

For more detailed API documentation, see the individual wrapper documentation in the `docs/api/` directory.