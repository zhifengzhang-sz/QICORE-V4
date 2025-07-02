# 🚀 Getting Started Guide

**Build your first workflow agent in 15 minutes**

This guide walks you through creating a simple but powerful workflow agent using AI Orchestra + Vercel AI SDK.

## 🎯 **What We'll Build**

A **content creation workflow** that automatically:
1. **Research Agent** - Researches a topic using web search
2. **Writer Agent** - Creates content based on research
3. **Editor Agent** - Reviews and improves the content

```
User Input → Research Agent → Writer Agent → Editor Agent → Final Content
```

## 📋 **Prerequisites**

```bash
# 1. Node.js 18+ and Bun installed
bun --version  # Should show 1.0.0+

# 2. Ollama running locally
ollama serve
ollama pull llama3.2

# 3. A new project directory
mkdir my-workflow-agent
cd my-workflow-agent
```

## 🏗️ **Step 1: Project Setup**

### **Initialize the Project**
```bash
# Create package.json
bun init -y

# Install dependencies
bun add ai @ai-sdk/ollama ai-orchestra zod

# Create directory structure
mkdir -p src/{agents,workflows,tools}
```

### **Basic TypeScript Config**
```json
// tsconfig.json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "allowSyntheticDefaultImports": true,
    "esModuleInterop": true,
    "allowJs": true,
    "strict": true,
    "skipLibCheck": true,
    "types": ["bun-types"]
  }
}
```

## 🔧 **Step 2: Create Your First Agent**

### **Define the Research Agent**
```typescript
// src/agents/research.ts
import { ollama } from '@ai-sdk/ollama'
import { tool } from 'ai'
import { z } from 'zod'

// Mock web search tool (replace with real API)
const webSearchTool = tool({
  description: 'Search the web for information about a topic',
  parameters: z.object({
    query: z.string().describe('Search query')
  }),
  execute: async ({ query }) => {
    // Mock implementation - replace with real web search
    return {
      results: [
        {
          title: `Information about ${query}`,
          content: `Detailed information about ${query}...`,
          url: `https://example.com/search?q=${query}`
        }
      ]
    }
  }
})

export const researchAgent = {
  name: "Research Specialist",
  model: ollama('llama3.2'),
  
  systemPrompt: `
    You are a research specialist. Your job is to:
    
    1. Use the webSearch tool to find information about the given topic
    2. Analyze and synthesize the search results
    3. Create a comprehensive research summary
    4. When your research is complete, use handoff_to_writer() to pass 
       your findings to the writing agent
    
    Be thorough but concise. Focus on facts and key insights.
  `,
  
  handoffs: ['writer'],
  
  tools: {
    webSearch: webSearchTool
  }
}
```

### **Define the Writer Agent**
```typescript
// src/agents/writer.ts
import { ollama } from '@ai-sdk/ollama'

export const writerAgent = {
  name: "Content Writer",
  model: ollama('llama3.2'),
  
  systemPrompt: `
    You are a skilled content writer. Your job is to:
    
    1. Take the research provided by the research agent
    2. Create engaging, well-structured content based on the research
    3. Write in a clear, professional tone
    4. When your draft is complete, use handoff_to_editor() to pass 
       your content to the editing agent
    
    Focus on creating valuable, readable content that serves the user's needs.
  `,
  
  handoffs: ['editor'],
  
  tools: {} // No special tools needed
}
```

### **Define the Editor Agent**
```typescript
// src/agents/editor.ts
import { ollama } from '@ai-sdk/ollama'

export const editorAgent = {
  name: "Content Editor",
  model: ollama('llama3.2'),
  
  systemPrompt: `
    You are a professional content editor. Your job is to:
    
    1. Review the content provided by the writer
    2. Check for clarity, flow, and engagement
    3. Fix any grammatical or structural issues
    4. Enhance the content while preserving the writer's voice
    5. Provide the final, polished version
    
    This is the final step - produce publication-ready content.
  `,
  
  handoffs: [], // Final agent - no handoffs
  
  tools: {} // No special tools needed
}
```

## 🎭 **Step 3: Create the Workflow**

```typescript
// src/workflows/content-creation.ts
import { createWorkflowOrchestra, executeWorkflow } from 'ai-orchestra'
import { researchAgent } from '../agents/research.js'
import { writerAgent } from '../agents/writer.js'
import { editorAgent } from '../agents/editor.js'

export const contentCreationWorkflow = createWorkflowOrchestra({
  researcher: researchAgent,
  writer: writerAgent,
  editor: editorAgent
})

export async function createContent(topic: string) {
  const initialMessage = {
    role: 'user' as const,
    content: `Create comprehensive content about: ${topic}`
  }

  console.log(`🚀 Starting content creation workflow for: ${topic}`)
  
  const result = await executeWorkflow(
    contentCreationWorkflow,
    'researcher', // Start with research agent
    [initialMessage]
  )

  return result
}
```

## 🏃 **Step 4: Create the Main Application**

```typescript
// src/index.ts
import { createContent } from './workflows/content-creation.js'

async function main() {
  const topic = process.argv[2] || "artificial intelligence in healthcare"
  
  console.log('🎭 AI Content Creation Workflow')
  console.log('================================')
  console.log(`Topic: ${topic}\n`)
  
  try {
    const result = await createContent(topic)
    
    console.log('\n✅ Workflow Complete!')
    console.log('📝 Final Content:')
    console.log('=================')
    console.log(result.content)
    
  } catch (error) {
    console.error('❌ Workflow failed:', error)
  }
}

main()
```

### **Add Run Script to package.json**
```json
{
  "scripts": {
    "start": "bun src/index.ts",
    "dev": "bun --watch src/index.ts"
  }
}
```

## 🎯 **Step 5: Run Your Workflow**

```bash
# Run with default topic
bun start

# Run with custom topic
bun start "machine learning in finance"

# Expected output:
# 🎭 AI Content Creation Workflow
# ================================
# Topic: machine learning in finance
# 
# 🚀 Starting content creation workflow for: machine learning in finance
# [Research Specialist] Searching for information...
# [Research Specialist → Writer] Transferring research findings...
# [Content Writer] Creating content based on research...
# [Content Writer → Editor] Passing draft for review...
# [Content Editor] Reviewing and polishing content...
# 
# ✅ Workflow Complete!
# 📝 Final Content:
# =================
# [Your generated content here]
```

## 🔍 **What Just Happened?**

### **Behind the Scenes**
1. **Research Agent** received your topic and used the web search tool
2. **Agent called** `handoff_to_writer()` with research findings
3. **Writer Agent** received the research and created content
4. **Writer called** `handoff_to_editor()` with the draft
5. **Editor Agent** reviewed and polished the final content

### **The Magic of AI Orchestra + Vercel AI SDK**
- **Vercel AI SDK** handled all LLM interactions, streaming, and tool calling
- **AI Orchestra** managed the agent coordination and handoffs
- **Your workflow** remained simple and declarative
- **Real-time streaming** showed progress throughout

## 🚀 **Next Steps: Enhance Your Workflow**

### **Add Real Web Search**
```typescript
// Replace mock web search with real API
import { searchWeb } from '@your-search-provider'

const webSearchTool = tool({
  description: 'Search the web for information',
  parameters: z.object({ query: z.string() }),
  execute: async ({ query }) => {
    return await searchWeb(query) // Real implementation
  }
})
```

### **Add More Agents** 
```typescript
// Add fact-checker agent
const factCheckerAgent = {
  name: "Fact Checker",
  systemPrompt: "Verify facts and add citations...",
  handoffs: ['editor'],
  tools: { checkFacts: factCheckTool }
}

// Update workflow
const enhancedWorkflow = createWorkflowOrchestra({
  researcher: researchAgent,
  writer: writerAgent,
  factChecker: factCheckerAgent, // New agent
  editor: editorAgent
})
```

### **Add Streaming UI**
```typescript
// Create a Next.js API route
export async function POST(req: Request) {
  const { topic } = await req.json()
  
  const stream = await createContent(topic)
  
  // Return streaming response to frontend
  return stream.toResponse()
}
```

### **Add Error Handling**
```typescript
export async function createContent(topic: string) {
  let retries = 0
  const maxRetries = 3
  
  while (retries < maxRetries) {
    try {
      return await executeWorkflow(/* ... */)
    } catch (error) {
      retries++
      if (retries === maxRetries) throw error
      console.log(`Retrying... (${retries}/${maxRetries})`)
    }
  }
}
```

## 🎉 **Congratulations!**

You've built your first workflow agent! You now understand:

✅ **How to define agents** with system prompts and tools  
✅ **How handoffs work** through function calls  
✅ **How AI Orchestra coordinates** multiple agents  
✅ **How Vercel AI SDK provides** the LLM foundation  
✅ **How to create workflows** that solve real problems  

## 📚 **What's Next?**

- **[🏗️ Architecture Guide](./architecture.md)** - Understand the technical details
- **[🛠️ Creating Workflows](./creating-workflows.md)** - Advanced patterns and techniques
- **[💡 Examples](../examples/)** - More complex workflow examples
- **[📋 API Reference](../api/)** - Complete technical documentation

**Ready to build more sophisticated workflows? Explore the advanced guides!** 🚀 