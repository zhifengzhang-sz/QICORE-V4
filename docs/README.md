# 🎭 QiCore Agent Documentation

**4-Layer AI Agent Architecture: QiPrompt + QiAgent + AI Orchestra + Vercel AI SDK**

## 🔥 **What This Project Provides**

This project gives you **production-ready workflow agents** using a sophisticated 4-layer architecture:

- **🎯 QiAgent**: Lightweight wrapper around AI Orchestra for simplified workflow creation
- **🎯 QiPrompt**: Lightweight wrapper around Vercel AI SDK for consistent LLM interactions
- **🎭 AI Orchestra**: Multi-agent workflow orchestration with handoffs and state management
- **🌊 Vercel AI SDK**: Modern LLM interface with streaming, tools, and multi-provider support
- **🧠 QiCore Integration**: Mathematical analysis tools and MCP (Model Context Protocol) capabilities

## 🏗️ **4-Layer Architecture**

```
Your App → QiAgent → AI Orchestra → QiPrompt → Vercel AI SDK → LLM
```

**Perfect separation of concerns with lightweight wrappers at each level**

### **🎯 QiAgent Layer**
- **Purpose**: Simplified workflow creation and agent orchestration
- **Features**: Declarative agent definitions, workflow patterns, built-in error handling
- **API**: `createAgent()`, `registerWorkflow()`, `executeWorkflow()`

### **🎭 AI Orchestra Layer**  
- **Purpose**: Multi-agent coordination with handoffs and state management
- **Features**: Agent handoffs, context preservation, streaming workflow execution
- **API**: `createWorkflowOrchestra()`, `processStream()`, handoff tools

### **🎯 QiPrompt Layer**
- **Purpose**: Consistent, reliable LLM interactions with retry logic
- **Features**: Text generation, structured output, streaming, multi-provider support
- **API**: `generateText()`, `generateStructured()`, `streamText()`

### **🌊 Vercel AI SDK Layer**
- **Purpose**: Direct LLM provider integration with streaming and tools
- **Features**: Model providers, streaming, tool calling, type safety
- **API**: `streamText()`, `generateObject()`, provider clients

## 📚 **Documentation Structure**

### **📖 Guides**
- **[🏗️ Architecture Overview](./guides/architecture.md)** - Complete 4-layer technical deep dive
- **[🚀 Getting Started](./guides/getting-started.md)** - Build your first workflow agent
- **[🛠️ Creating Workflows](./guides/creating-workflows.md)** - Design custom multi-agent workflows
- **[🔧 Integration Guide](./guides/integration.md)** - Add to existing projects

### **💡 Examples**
- **[🎯 QiPrompt + QiAgent Demo](./examples/qiprompt-qiagent-demo.md)** - 4-layer architecture in action
- **[🧮 Mathematical Analysis](./examples/mathematical-workflow.md)** - Research → Verification → Reporting
- **[👁️ Code Review](./examples/code-review-workflow.md)** - Analysis → Security → Final Review
- **[🎯 Custom Workflows](./examples/custom-patterns.md)** - Various workflow patterns

### **📋 API Reference**
- **[🎯 QiAgent API](./api/qiagent.md)** - Workflow creation and agent management
- **[🎯 QiPrompt API](./api/qiprompt.md)** - LLM interaction patterns
- **[🎭 AI Orchestra API](./api/orchestra.md)** - Low-level workflow orchestration
- **[🛠️ Tool Integration](./api/tools.md)** - MCP tools and custom functions
- **[📊 Types & Interfaces](./api/types.md)** - TypeScript definitions

## 🌟 **Key Features**

### **🎯 Simplified Workflow Creation**
Build sophisticated multi-agent workflows with minimal code:

```typescript
import { createAgent, AgentRoles, WorkflowPatterns } from '@qi/mcp'

// Create agent with QiPrompt integration
const agent = createAgent()

// Use pre-built workflow patterns
const workflow = WorkflowPatterns.contentCreation('article-workflow')
agent.registerWorkflow(workflow)

// Execute with simple API
const result = await agent.executeWorkflow(
  'article-workflow',
  'Write about quantum computing'
)
```

### **🎯 Consistent LLM Interactions**
Reliable, type-safe LLM interactions with built-in retry logic:

```typescript
import { QiPrompt, CommonSchemas } from '@qi/mcp'

const qiPrompt = new QiPrompt()

// Simple text generation
const text = await qiPrompt.generateText("Explain monads")

// Structured generation with validation
const analysis = await qiPrompt.generateStructured(
  "Analyze this code", 
  { schema: CommonSchemas.analysis }
)

// Real-time streaming
await qiPrompt.streamText("Write a story", {
  onChunk: (chunk) => console.log(chunk)
})
```

### **🌊 Real-Time Streaming**
Built on Vercel AI SDK's streaming foundation with workflow progress:

```typescript
// Frontend gets live updates as workflow progresses
const { messages, data } = useChat()
// data.currentStep: 'verification', progress: 75%
// data.currentAgent: 'verifier'
```

### **🔧 Zero Vendor Lock-in**
Works with any Vercel AI SDK provider:

```typescript
import { QiPrompt } from '@qi/mcp'

// Easy provider switching
const qiPrompt = new QiPrompt({
  defaultModel: ollama('llama3.2')    // or
  defaultModel: openai('gpt-4')       // or  
  defaultModel: anthropic('claude-3-sonnet')
})
```

### **⚡ 4-Layer Benefits**
Each layer has a single, clear responsibility:

```
✅ Your App: Business logic and UI
✅ QiAgent: Workflow patterns and orchestration  
✅ AI Orchestra: Agent handoffs and state management
✅ QiPrompt: LLM interaction patterns and reliability
✅ Vercel AI SDK: Provider integration and streaming
✅ LLM: Actual language model execution
```

## 🚀 **Quick Start**

```bash
# Install dependencies
bun add ai zod

# Run architecture demo
bun app/src/examples/qiprompt-qiagent-demo.ts
```

```typescript
// Create a simple workflow
import { createAgent, AgentRoles, QiPrompt } from '@qi/mcp'

// Set up QiPrompt with your model
const qiPrompt = new QiPrompt({
  defaultModel: yourModel // ollama('llama3.2'), openai('gpt-4'), etc.
})

// Create QiAgent with QiPrompt integration  
const agent = createAgent(qiPrompt)

// Create a simple research → write workflow
agent.registerWorkflow({
  name: "research-and-write",
  description: "Research a topic and write content",
  agents: {
    researcher: AgentRoles.researcher(),
    writer: AgentRoles.writer()
  },
  steps: {
    research: { agent: 'researcher', nextSteps: ['write'] },
    write: { agent: 'writer', nextSteps: [] }
  },
  startStep: 'research'
})

// Execute the workflow
const result = await agent.executeWorkflow(
  'research-and-write',
  'Research and write about TypeScript best practices'
)

console.log('Final result:', result.finalResult)
```

## 🎯 **Why This 4-Layer Architecture?**

### **✅ What You Get**
- **Simplified APIs** at every level for common patterns
- **Maximum flexibility** with easy component swapping
- **Built-in reliability** with retry logic and error handling  
- **Type safety** throughout the entire stack
- **Real-time streaming** with workflow progress tracking
- **Production ready** architecture with proper separation of concerns

### **❌ What You Avoid**
- Heavy framework dependencies and complexity
- Manual error handling and retry logic
- Complex multi-agent state management
- Vendor lock-in to specific LLM providers
- Boilerplate code for common patterns

## 🔗 **Layer Integration Flow**

```
User Request
    ↓
🎯 QiAgent.executeWorkflow()
    ↓ 
🎭 AI Orchestra processes agent handoffs
    ↓
🎯 QiPrompt.generateText() with retry logic
    ↓
🌊 Vercel AI SDK streamText() 
    ↓
🤖 LLM Provider (Ollama/OpenAI/Anthropic)
    ↓
📤 Streaming Response back through all layers
```

**Each layer adds value without breaking the flow**

## 🛠️ **Project Structure**

```
agent/                          # Project root
├── docs/                       # 📚 This documentation
│   ├── guides/                 # Architecture & tutorials
│   ├── examples/               # Working examples
│   └── api/                    # API reference
├── lib/                        # 📦 Reusable library
│   └── src/
│       ├── qiagent/            # QiAgent: AI Orchestra wrapper
│       ├── qiprompt/           # QiPrompt: Vercel AI SDK wrapper
│       ├── agent/              # AI Orchestra integration (lower level)
│       ├── mcp/                # Model Context Protocol tools
│       └── prompt/             # Prompt engineering utilities
└── app/                        # 🚀 Application examples
    └── src/
        ├── examples/           # Demo workflows and patterns
        └── agents/             # Specific agent implementations
```

## 🎉 **What You've Achieved**

✅ **4-layer architecture** with perfect separation of concerns  
✅ **Simplified APIs** that hide complexity while preserving power  
✅ **Production-ready reliability** with built-in error handling  
✅ **Real-time streaming** throughout multi-agent workflows  
✅ **Type safety** across the entire stack  
✅ **Zero vendor lock-in** with easy provider switching  
✅ **Highly scalable** from simple workflows to complex multi-agent systems  

**Ready to build sophisticated AI workflows? Start with the [Architecture Guide](./guides/architecture.md)!** 🚀 