# 🏗️ Architecture Overview: AI Orchestra + Vercel AI SDK

**How the integration works under the hood**

## 🎯 **The Big Picture**

```
┌─────────────────────────────────────────────────────────────────┐
│               Your Application                                  │
│                                                                 │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐        │
│  │   Frontend  │    │ API Routes  │    │  Workflows  │        │
│  │             │    │             │    │             │        │
│  │ • useChat() │───▶│ • POST/chat │───▶│ • Agents    │        │
│  │ • Streaming │    │ • Streaming │    │ • Handoffs  │        │
│  └─────────────┘    └─────────────┘    └─────────────┘        │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                    AI Orchestra                                 │
│                                                                 │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐        │
│  │  Workflow   │    │   Agent     │    │   Handoff   │        │
│  │ Orchestrator│───▶│ Coordinator │───▶│  Manager    │        │
│  │             │    │             │    │             │        │
│  └─────────────┘    └─────────────┘    └─────────────┘        │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                   Vercel AI SDK                                 │
│                                                                 │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐        │
│  │ streamText()│    │   Models    │    │    Tools    │        │
│  │             │───▶│             │───▶│             │        │
│  │ • Streaming │    │ • Ollama    │    │ • Function  │        │
│  │ • Messages  │    │ • OpenAI    │    │   Calling   │        │
│  └─────────────┘    └─────────────┘    └─────────────┘        │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
                    ┌─────────────────┐
                    │      LLMs       │
                    │                 │
                    │ • Ollama        │
                    │ • OpenAI        │
                    │ • Anthropic     │
                    └─────────────────┘
```

## 🔑 **Key Insight: AI Orchestra is a Vercel AI SDK Extension**

**AI Orchestra doesn't replace Vercel AI SDK - it builds ON TOP of it.**

### **What Vercel AI SDK Provides (Foundation)**
- **Model Connections**: `@ai-sdk/ollama`, `@ai-sdk/openai`, `@ai-sdk/anthropic`
- **Core Functions**: `streamText()`, `generateText()`, `generateObject()`
- **Streaming**: Real-time response streaming to frontend
- **Tool Calling**: Function execution during LLM conversations
- **Type Safety**: Structured generation with Zod schemas

### **What AI Orchestra Adds (Orchestration Layer)**
- **Multi-Agent Workflows**: Coordinate multiple agents in sequence/parallel
- **Agent Handoffs**: Pass context from one agent to another
- **State Management**: Preserve conversation state across workflow steps
- **Workflow Definition**: Declarative YAML/JSON workflow configuration
- **Progress Tracking**: Monitor workflow execution in real-time

## 🔥 **How They Work Together: The Technical Deep Dive**

### **1. AI Orchestra Wraps `streamText()`**

```typescript
// Inside AI Orchestra's core execution engine
export async function executeAgentStep(agent: Agent, context: WorkflowContext) {
  // AI Orchestra prepares the context and tools
  const allTools = {
    ...agent.tools,
    ...createHandoffTools(agent.handoffs) // Add handoff capabilities
  }

  // But the actual LLM call is 100% Vercel AI SDK
  const chunks = streamText({
    model: agent.model,           // Your Vercel AI SDK model
    system: agent.systemPrompt,   // Agent's role definition
    messages: context.messages,   // Conversation history
    tools: allTools,             // Tools + handoff functions
    maxSteps: 5                  // Tool calling rounds
  })

  // AI Orchestra processes the streaming response
  const result = await processStream(chunks, (toolCall) => {
    if (toolCall.toolName.startsWith('handoff_')) {
      // Agent wants to transfer to another agent
      return handleAgentHandoff(toolCall, context)
    }
    // Regular tool execution
    return executeRegularTool(toolCall)
  })

  return result
}
```

### **2. Streaming Flows Through Both Layers**

```typescript
// Your API route (Next.js example)
export async function POST(req: Request) {
  const { messages } = await req.json()

  // Create AI Orchestra workflow
  const workflow = createWorkflowOrchestra({
    researcher: { /* agent config */ },
    verifier: { /* agent config */ },
    reporter: { /* agent config */ }
  })

  // Execute workflow - returns Vercel AI SDK stream
  const stream = await executeWorkflow(workflow, 'researcher', messages)

  // Return the raw Vercel AI SDK stream to your frontend
  return stream.toResponse()
}
```

```typescript
// Your frontend gets the same streaming experience
export default function Chat() {
  const { messages, input, handleInputChange, handleSubmit } = useChat({
    api: '/api/chat' // Your workflow endpoint
  })

  return (
    <div>
      {messages.map(message => (
        <div key={message.id}>
          <strong>{message.role}:</strong> {message.content}
          
          {/* AI Orchestra adds workflow metadata */}
          {message.annotations?.currentAgent && (
            <span>🤖 Agent: {message.annotations.currentAgent}</span>
          )}
          {message.annotations?.workflowStep && (
            <span>📍 Step: {message.annotations.workflowStep}</span>
          )}
        </div>
      ))}
    </div>
  )
}
```

### **3. Tool Integration is Seamlessly Enhanced**

```typescript
// Your regular Vercel AI SDK tools work unchanged
const regularTools = {
  searchWeb: tool({
    description: 'Search the web for information',
    parameters: z.object({ query: z.string() }),
    execute: async ({ query }) => {
      return await webSearch(query)
    }
  }),
  
  analyzeCode: tool({
    description: 'Analyze code quality',
    parameters: z.object({ code: z.string() }),
    execute: async ({ code }) => {
      return await codeAnalysis(code)
    }
  })
}

// AI Orchestra automatically adds handoff tools
const workflowTools = {
  ...regularTools,
  
  // These are added automatically by AI Orchestra
  handoff_to_verifier: tool({
    description: 'Hand off to verification agent',
    parameters: z.object({ 
      analysis: z.string(),
      confidence: z.number() 
    }),
    execute: async (params) => {
      // AI Orchestra handles the handoff
      return await transferToAgent('verifier', params)
    }
  })
}
```

## 🧠 **Agent Definition: Where the Magic Happens**

### **The System Prompt is the Workflow Definition**

```typescript
const researchAgent = {
  name: "Mathematical Researcher",
  model: ollama('llama3.2'), // Any Vercel AI SDK model
  
  // This is where you define the workflow step
  systemPrompt: `
    You are a mathematical research agent. Your job is to:
    
    1. Analyze the given mathematical concept thoroughly
    2. Research relevant theorems and proofs
    3. Identify potential verification points
    4. When your analysis is complete, use handoff_to_verifier() 
       to transfer your findings to the verification agent
    
    Always be thorough but concise. The verifier needs clear,
    structured information to do their job effectively.
  `,
  
  handoffs: ['verifier'], // Who you can transfer to
  
  tools: {
    searchMathDatabase: tool({ /* ... */ }),
    analyzeConcept: tool({ /* ... */ })
  }
}
```

### **Handoffs Are Just Function Calls**

```typescript
// During execution, the LLM calls:
{
  "name": "handoff_to_verifier",
  "arguments": {
    "analysis": "The Result<T> monad follows category theory laws...",
    "confidence": 0.85,
    "keyPoints": ["Functor laws", "Applicative laws", "Monad laws"]
  }
}

// AI Orchestra intercepts this and:
// 1. Saves the context and results
// 2. Switches to the verifier agent
// 3. Passes the analysis data as context
// 4. Continues the workflow seamlessly
```

## 🔄 **Complete Workflow Execution Flow**

### **Phase 1: Initialization**
```typescript
// 1. Your app creates workflow definition
const workflow = createWorkflowOrchestra({
  researcher: { /* config */ },
  verifier: { /* config */ },
  reporter: { /* config */ }
})

// 2. AI Orchestra sets up agent routing
// 3. Each agent gets wrapped with handoff capabilities
```

### **Phase 2: Execution**
```typescript
// 1. User message arrives: "Analyze the Result<T> monad"
const messages = [{ role: 'user', content: 'Analyze the Result<T> monad' }]

// 2. Start with researcher agent
const stream = await executeWorkflow(workflow, 'researcher', messages)

// 3. Behind the scenes:
//    a. AI Orchestra calls Vercel AI SDK streamText() with researcher config
//    b. LLM generates analysis using researcher's tools
//    c. LLM calls handoff_to_verifier() when done
//    d. AI Orchestra intercepts, switches to verifier agent
//    e. Verifier agent continues the workflow
//    f. Process repeats until final agent completes
```

### **Phase 3: Streaming Response**
```typescript
// Throughout the entire workflow:
// - Each agent's responses stream to your frontend in real-time
// - Workflow metadata (current agent, step progress) is included
// - Tool calls and results are visible
// - The user sees a seamless conversation despite multiple agents
```

## 🎯 **Why This Architecture Works**

### **✅ Strengths**

1. **Vercel AI SDK Compatibility**: Every Vercel AI SDK feature works unchanged
2. **Minimal Overhead**: AI Orchestra is just a coordination layer
3. **Streaming Preserved**: Real-time responses throughout workflows
4. **Tool Integration**: Existing tools work, handoffs are added automatically
5. **Type Safety**: Full TypeScript support throughout the stack
6. **Model Flexibility**: Any Vercel AI SDK provider works (OpenAI, Anthropic, Ollama)

### **🔧 Implementation Benefits**

1. **No Vendor Lock-in**: Switch LLM providers without changing workflow logic
2. **Incremental Adoption**: Add workflows to existing Vercel AI SDK apps
3. **Familiar APIs**: If you know Vercel AI SDK, you know this
4. **Production Ready**: Built on proven, stable foundations
5. **Easy Debugging**: Clear separation between orchestration and LLM calls

## 🚀 **Getting Started Recommendations**

### **Step 1: Master Vercel AI SDK Basics**
```bash
# Learn the foundation first
bun add ai @ai-sdk/ollama zod
```

### **Step 2: Add AI Orchestra**
```bash
# Add workflow capabilities
bun add ai-orchestra
```

### **Step 3: Build Your First Workflow**
- Start with 2 agents (simple handoff)
- Use system prompts to define roles clearly
- Test with streaming to see the handoff in action
- Add tools once the basic flow works

### **Step 4: Scale Up**
- Add more agents to the workflow
- Implement parallel execution paths
- Add sophisticated error handling
- Build domain-specific tool libraries

## 🔗 **Next Steps**

- **[🚀 Getting Started Guide](./getting-started.md)** - Build your first workflow
- **[🛠️ Creating Workflows](./creating-workflows.md)** - Advanced patterns
- **[💡 Examples](../examples/)** - Working code examples
- **[📋 API Reference](../api/)** - Complete technical documentation

**The key insight: AI Orchestra makes Vercel AI SDK workflows possible without changing what makes Vercel AI SDK great.** 🎭 