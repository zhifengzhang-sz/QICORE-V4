# 🎭 QiCore Agent Documentation

**Workflow-driven AI agents using AI Orchestra + Vercel AI SDK**

## 🔥 **What This Project Provides**

This project gives you **production-ready workflow agents** that combine:

- **🌊 Vercel AI SDK**: Modern LLM interface with streaming, tools, and multi-provider support
- **🎭 AI Orchestra**: Lightweight workflow orchestration for agent handoffs and state management
- **🧠 QiCore Integration**: Mathematical analysis tools and MCP (Model Context Protocol) capabilities

## 📚 **Documentation Structure**

### **📖 Guides**
- **[🏗️ Architecture Overview](./guides/architecture.md)** - How AI Orchestra + Vercel AI SDK work together
- **[🚀 Getting Started](./guides/getting-started.md)** - Build your first workflow agent
- **[🛠️ Creating Workflows](./guides/creating-workflows.md)** - Design custom multi-agent workflows
- **[🔧 Integration Guide](./guides/integration.md)** - Add to existing projects

### **💡 Examples**
- **[🧮 Mathematical Analysis](./examples/mathematical-workflow.md)** - Research → Verification → Reporting
- **[👁️ Code Review](./examples/code-review-workflow.md)** - Analysis → Security → Final Review
- **[🎯 Custom Workflows](./examples/custom-patterns.md)** - Various workflow patterns

### **📋 API Reference**
- **[🎭 AI Orchestra API](./api/orchestra.md)** - Workflow creation and execution
- **[🛠️ Tool Integration](./api/tools.md)** - MCP tools and custom functions
- **[📊 Types & Interfaces](./api/types.md)** - TypeScript definitions

## 🌟 **Key Features**

### **🎯 Workflow-Driven Agents**
Instead of simple chatbots, build **structured multi-step processes**:

```typescript
// Define workflow through system prompts
Research Agent → Verification Agent → Reporter Agent
```

### **🌊 Real-Time Streaming**
Built on Vercel AI SDK's streaming foundation:

```typescript
// Frontend gets live updates as workflow progresses
const { messages, data } = useChat()
// data.currentStep: 'verification', progress: 75%
```

### **🔧 Zero Vendor Lock-in**
Works with any Vercel AI SDK provider:

```typescript
import { openai } from '@ai-sdk/openai'
import { anthropic } from '@ai-sdk/anthropic' 
import { ollama } from '@ai-sdk/ollama'

// All work the same in workflows
```

### **⚡ Lightweight Architecture**
**Not LangChain** - uses proven, lightweight components:

```
Your App → Vercel AI SDK → AI Orchestra → LLM
```

## 🚀 **Quick Start**

```bash
# Install dependencies
bun add ai ai-orchestra zod

# Run mathematical analysis workflow
bun run app/src/examples/workflow-demo.ts
```

```typescript
// Create a workflow
import { createWorkflowOrchestra } from '@qi/mcp/agent/orchestra'

const workflow = createWorkflowOrchestra({
  step1: {
    name: "Analyzer",
    model: yourModel,
    systemPrompt: "Analyze the input, then hand off to step2",
    handoffs: ['step2'],
    tools: { /* your tools */ }
  },
  step2: {
    name: "Reporter", 
    model: yourModel,
    systemPrompt: "Generate final report from analysis",
    tools: { /* your tools */ }
  }
})

// Execute the workflow
const result = await executeWorkflow(
  workflow, 
  'step1', 
  [{ role: 'user', content: 'Analyze this data...' }]
)
```

## 🎯 **Why This Architecture?**

### **✅ What You Get**
- **Multi-agent workflows** without complex orchestration frameworks
- **Real-time streaming** of workflow progress  
- **Type-safe** agent definitions and tool integration
- **Production-ready** error handling and state management
- **Easy customization** for any domain (math, code, content, etc.)

### **❌ What You Avoid**
- Heavy LangChain dependencies and complexity
- Manual state management between agents
- Complex graph definitions and learning curves
- Vendor lock-in to specific LLM providers

## 🔗 **How the Integration Works**

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Your App      │    │  Vercel AI SDK │    │  AI Orchestra   │
│                 │    │                 │    │                 │
│ • useChat()     │───▶│ • streamText()  │───▶│ • Agent Logic   │
│ • API Routes    │    │ • Tool Calling  │    │ • Handoffs      │
│ • UI Components │    │ • Streaming     │    │ • State Mgmt    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

**Vercel AI SDK** provides the **LLM foundation** (models, streaming, tools)
**AI Orchestra** adds **workflow orchestration** (agent coordination, handoffs)
**Your App** gets **powerful multi-agent capabilities** with **simple APIs**

## 🛠️ **Project Structure**

```
agent/                          # Project root
├── docs/                       # 📚 This documentation
│   ├── guides/                 # Architecture & tutorials
│   ├── examples/               # Working examples
│   └── api/                    # API reference
├── lib/                        # 📦 Reusable library
│   └── src/
│       ├── agent/              # AI Orchestra integration
│       ├── mcp/                # Model Context Protocol tools
│       └── prompt/             # Prompt engineering utilities
└── app/                        # 🚀 Application examples
    └── src/
        ├── examples/           # Demo workflows
        └── agents/             # Specific agent implementations
```

## 🎉 **What You've Achieved**

✅ **Modern AI architecture** using proven, lightweight tools  
✅ **Multi-agent workflows** without complex frameworks  
✅ **Real-time streaming** with excellent developer experience  
✅ **Type safety** throughout the entire stack  
✅ **Production ready** with error handling and state management  
✅ **Highly customizable** for any domain or use case  

**Ready to build sophisticated AI workflows? Start with the [Architecture Guide](./guides/architecture.md)!** 🚀 