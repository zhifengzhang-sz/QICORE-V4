# 📋 AI Orchestra API Reference

**Complete technical documentation for workflow agent APIs**

This document provides the complete API reference for creating and executing workflow agents using AI Orchestra + Vercel AI SDK.

## 🎭 **Core Functions**

### **`createWorkflowOrchestra(agents)`**

Creates a workflow orchestra from agent definitions.

**Signature:**
```typescript
function createWorkflowOrchestra(
  agents: Record<string, AgentConfig>
): WorkflowOrchestra
```

**Parameters:**
- `agents`: Object mapping agent names to their configurations

**Returns:** `WorkflowOrchestra` - Configured workflow ready for execution

**Example:**
```typescript
const workflow = createWorkflowOrchestra({
  researcher: {
    name: "Research Specialist",
    model: ollama('llama3.2'),
    systemPrompt: "You are a research specialist...",
    handoffs: ['writer'],
    tools: { webSearch: webSearchTool }
  },
  writer: {
    name: "Content Writer", 
    model: ollama('llama3.2'),
    systemPrompt: "You are a content writer...",
    handoffs: ['editor'],
    tools: {}
  }
})
```

---

### **`executeWorkflow(workflow, startAgent, messages)`**

Executes a workflow starting with a specific agent.

**Signature:**
```typescript
async function executeWorkflow(
  workflow: WorkflowOrchestra,
  startAgent: string,
  messages: CoreMessage[]
): Promise<WorkflowResult>
```

**Parameters:**
- `workflow`: The workflow orchestra to execute
- `startAgent`: Name of the agent to start with
- `messages`: Initial conversation messages

**Returns:** `Promise<WorkflowResult>` - Final workflow results with streaming

**Example:**
```typescript
const result = await executeWorkflow(
  workflow,
  'researcher',
  [{ role: 'user', content: 'Analyze AI trends' }]
)
```

---

### **`processStream(stream, dispatcher)`**

Processes an AI SDK stream with workflow-aware handling.

**Signature:**
```typescript
async function processStream(
  stream: AISDKStream,
  dispatcher: (toolCall: ToolCall) => Promise<ToolResult>
): Promise<StreamResult>
```

**Parameters:**
- `stream`: Vercel AI SDK stream object
- `dispatcher`: Function to handle tool calls and handoffs

**Returns:** `Promise<StreamResult>` - Processed stream with metadata

---

## 🤖 **Agent Configuration**

### **`AgentConfig` Interface**

Defines the structure for agent configuration objects.

```typescript
interface AgentConfig {
  name: string                              // Human-readable agent name
  model: LanguageModel                      // Vercel AI SDK model instance
  systemPrompt: string | PromptFunction    // Agent role and instructions
  handoffs: string[]                        // List of agents this can transfer to
  tools: Record<string, Tool>               // Vercel AI SDK tools
  maxSteps?: number                         // Max tool calling rounds (default: 5)
  temperature?: number                      // Model temperature (default: 0.7)
}
```

**Field Details:**

#### **`name: string`**
Human-readable name for the agent, used in logging and debugging.

#### **`model: LanguageModel`**
Any Vercel AI SDK model instance:
```typescript
import { ollama } from '@ai-sdk/ollama'
import { openai } from '@ai-sdk/openai'
import { anthropic } from '@ai-sdk/anthropic'

// Any of these work:
model: ollama('llama3.2')
model: openai('gpt-4')
model: anthropic('claude-3-sonnet')
```

#### **`systemPrompt: string | PromptFunction`**
Defines the agent's role and workflow instructions:

```typescript
// Simple string prompt
systemPrompt: "You are a research agent. Use tools to research topics..."

// Dynamic prompt function
systemPrompt: (context: WorkflowContext) => `
  You are a ${context.agentRole} working on ${context.currentTask}.
  Current workflow step: ${context.step}
  Previous results: ${context.previousResults}
`
```

#### **`handoffs: string[]`**
Array of agent names this agent can transfer execution to:

```typescript
// Can transfer to writer or reviewer
handoffs: ['writer', 'reviewer']

// Final agent - no handoffs
handoffs: []
```

#### **`tools: Record<string, Tool>`**
Standard Vercel AI SDK tools - handoff tools are added automatically:

```typescript
tools: {
  searchWeb: tool({
    description: 'Search for information',
    parameters: z.object({ query: z.string() }),
    execute: async ({ query }) => await webSearch(query)
  }),
  
  analyzeData: tool({
    description: 'Analyze data patterns',
    parameters: z.object({ data: z.array(z.unknown()) }),
    execute: async ({ data }) => await analyze(data)
  })
}
```

---

## 🔄 **Workflow Context**

### **`WorkflowContext` Interface**

Context object passed between agents during handoffs.

```typescript
interface WorkflowContext {
  messages: CoreMessage[]           // Full conversation history
  currentStep?: string             // Current workflow step
  previousResults?: unknown        // Results from previous agents
  metadata: WorkflowMetadata       // Execution metadata
  data: Record<string, unknown>    // Custom workflow data
}
```

### **`WorkflowMetadata` Interface**

Metadata about workflow execution.

```typescript
interface WorkflowMetadata {
  workflowId: string              // Unique workflow execution ID
  startTime: Date                 // When workflow started
  currentAgent: string            // Currently executing agent
  previousAgent?: string          // Previous agent (if any)
  handoffCount: number           // Number of handoffs so far
  totalSteps: number             // Total workflow steps
  executionPath: string[]        // Path of agent execution
}
```

---

## 🔗 **Handoff System**

### **Automatic Handoff Tools**

AI Orchestra automatically generates handoff tools based on agent configuration:

```typescript
// For agent with handoffs: ['writer', 'reviewer']
// These tools are automatically created:

handoff_to_writer: tool({
  description: 'Transfer execution to the writer agent',
  parameters: z.object({
    results: z.unknown().describe('Results to pass to writer'),
    reason: z.string().describe('Reason for handoff'),
    confidence: z.number().min(0).max(1).optional()
  }),
  execute: async (params) => {
    // AI Orchestra handles the handoff
    return transferToAgent('writer', params)
  }
})

handoff_to_reviewer: tool({
  description: 'Transfer execution to the reviewer agent', 
  parameters: z.object({
    results: z.unknown().describe('Results to pass to reviewer'),
    reason: z.string().describe('Reason for handoff'),
    confidence: z.number().min(0).max(1).optional()
  }),
  execute: async (params) => {
    return transferToAgent('reviewer', params)
  }
})
```

### **Custom Handoff Logic**

You can customize handoff behavior with middleware:

```typescript
const workflow = createWorkflowOrchestra(agents, {
  handoffMiddleware: async (from, to, context, params) => {
    // Custom validation
    if (params.confidence < 0.8) {
      throw new Error('Confidence too low for handoff')
    }
    
    // Custom context transformation
    const enhancedContext = {
      ...context,
      handoffReason: params.reason,
      qualityScore: params.confidence
    }
    
    return enhancedContext
  }
})
```

---

## 📊 **Return Types**

### **`WorkflowResult` Interface**

Final result of workflow execution.

```typescript
interface WorkflowResult {
  content: string                    // Final response content
  metadata: WorkflowMetadata        // Execution metadata
  messages: CoreMessage[]           // Full conversation history
  toolCalls: ToolCall[]            // All tool calls made
  handoffs: HandoffRecord[]        // Record of all handoffs
  executionTime: number            // Total execution time (ms)
  success: boolean                 // Whether workflow completed successfully
  error?: Error                    // Error if workflow failed
}
```

### **`HandoffRecord` Interface**

Record of agent handoffs during execution.

```typescript
interface HandoffRecord {
  from: string                     // Source agent name
  to: string                       // Destination agent name  
  timestamp: Date                  // When handoff occurred
  reason: string                   // Reason provided for handoff
  confidence?: number              // Confidence score (0-1)
  data: unknown                    // Data passed during handoff
}
```

---

## ⚡ **Streaming Support**

### **Real-time Workflow Streaming**

All workflows support real-time streaming with progress updates:

```typescript
// Stream with progress tracking
const stream = await executeWorkflow(workflow, 'researcher', messages)

// Process stream events
for await (const chunk of stream) {
  switch (chunk.type) {
    case 'agent-start':
      console.log(`🤖 Starting agent: ${chunk.agentName}`)
      break
      
    case 'tool-call':
      console.log(`🔧 Tool called: ${chunk.toolName}`)
      break
      
    case 'handoff':
      console.log(`🔄 Handoff: ${chunk.from} → ${chunk.to}`)
      break
      
    case 'content':
      process.stdout.write(chunk.content)
      break
      
    case 'workflow-complete':
      console.log(`✅ Workflow complete: ${chunk.result}`)
      break
  }
}
```

### **Frontend Integration**

Seamlessly integrate with React using Vercel AI SDK hooks:

```typescript
// API Route (Next.js)
export async function POST(req: Request) {
  const { messages } = await req.json()
  
  const stream = await executeWorkflow(workflow, 'researcher', messages)
  
  return stream.toResponse()
}

// React Component
export default function WorkflowChat() {
  const { messages, input, handleInputChange, handleSubmit, isLoading } = useChat({
    api: '/api/workflow'
  })

  return (
    <div>
      {messages.map(message => (
        <div key={message.id}>
          <strong>{message.role}:</strong> {message.content}
          
          {/* AI Orchestra adds metadata */}
          {message.annotations?.currentAgent && (
            <span className="agent-badge">
              🤖 {message.annotations.currentAgent}
            </span>
          )}
          
          {message.annotations?.handoff && (
            <span className="handoff-badge">
              🔄 {message.annotations.handoff.from} → {message.annotations.handoff.to}
            </span>
          )}
        </div>
      ))}
      
      <form onSubmit={handleSubmit}>
        <input
          value={input}
          onChange={handleInputChange}
          placeholder="Start a workflow..."
          disabled={isLoading}
        />
        <button type="submit" disabled={isLoading}>
          {isLoading ? 'Processing...' : 'Send'}
        </button>
      </form>
    </div>
  )
}
```

---

## 🔧 **Advanced Configuration**

### **Error Handling**

Configure workflow error handling:

```typescript
const workflow = createWorkflowOrchestra(agents, {
  errorHandling: {
    maxRetries: 3,
    retryDelay: 1000,
    fallbackAgent: 'error-handler',
    onError: async (error, context) => {
      console.error('Workflow error:', error)
      // Custom error handling logic
    }
  }
})
```

### **Performance Tuning**

Optimize workflow performance:

```typescript
const workflow = createWorkflowOrchestra(agents, {
  performance: {
    parallelExecution: true,      // Enable parallel agent execution
    caching: true,               // Cache tool results
    streamingBuffer: 1024,       // Streaming buffer size
    timeoutMs: 30000            // Workflow timeout
  }
})
```

### **Debugging**

Enable detailed debugging:

```typescript
const workflow = createWorkflowOrchestra(agents, {
  debug: {
    logLevel: 'verbose',
    traceExecution: true,
    saveContext: true,
    outputDir: './workflow-logs'
  }
})
```

---

## 🔗 **Related APIs**

- **[Tool Integration API](./tools.md)** - MCP tools and custom functions
- **[Types & Interfaces](./types.md)** - Complete TypeScript definitions
- **[Architecture Guide](../guides/architecture.md)** - How it all works together
- **[Getting Started](../guides/getting-started.md)** - Build your first workflow

**Need help with a specific use case? Check the [examples](../examples/) for working code!** 🚀 