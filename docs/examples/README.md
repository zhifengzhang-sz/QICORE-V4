# 💡 Workflow Examples

**Working code examples showing AI Orchestra + Vercel AI SDK integration with Claude Code SDK**

This directory contains **executable examples** that demonstrate workflow-driven agents and individual AI capabilities. All examples are located in `app/src/examples/` but documented here for clarity.

## 🔥 **What are Workflow-Driven Agents?**

Unlike simple chatbots, workflow-driven agents follow **structured, multi-step processes** where:

- **Workflows are defined through system prompts** (step-by-step instructions)
- **Agents hand off to each other** using function calls  
- **Context is preserved** across all workflow steps
- **Tools enable concrete actions** at each step

## 📁 **Examples Available**

### 🤖 **Claude Code SDK Integration**

#### 1. `claude-code-quickstart.ts` - 5-Minute Getting Started
**Perfect for developers new to Claude Code SDK**
```bash
bun app/src/examples/claude-code-quickstart.ts
```
- Create your first Claude Code agent
- Generate responses with error handling
- Get help with code review
- Try streaming responses
- Learn what's next

#### 2. `claude-code-agent-demo.ts` - Comprehensive Integration
**Full-featured demonstration of Claude Code capabilities**
```bash
bun app/src/examples/claude-code-agent-demo.ts
```
- Individual Claude Code agents with streaming
- Mathematical workflows (Research → Verification → Reporting)
- Contract verification workflows
- Hybrid workflows (Claude + Ollama)
- Factory pattern usage through QiAgent

#### 3. `claude-code-tools-demo.ts` - Development Tools Integration
**How Claude Code integrates with specific development tools**
```bash
bun app/src/examples/claude-code-tools-demo.ts
```
- File analysis and code review
- Project structure analysis
- Test generation with Vitest
- API documentation generation
- Code refactoring suggestions

#### 4. `claude-code-production-demo.ts` - Production Patterns
**Production-ready patterns with monitoring and resilience**
```bash
bun app/src/examples/claude-code-production-demo.ts
```
- Rate limiting and circuit breakers
- Comprehensive error handling
- Performance monitoring and metrics
- Health checks and deployment guidance
- Production service wrapper patterns

### 🎭 **Multi-Agent Workflows**

#### 5. `workflow-demo.ts` - Complete Working Examples

**Mathematical Analysis Workflow:**
```
Research Agent → Verification Agent → Reporter Agent
```
- Research Agent analyzes mathematical concepts
- Verification Agent proves correctness using formal methods
- Reporter Agent generates comprehensive documentation

**Custom Code Review Workflow:**
```
Security Agent → Performance Agent → Style Agent → Reviewer Agent
```
- Security Agent identifies vulnerabilities
- Performance Agent optimizes bottlenecks  
- Style Agent enforces coding standards
- Reviewer Agent synthesizes final assessment

## 🚀 **Running the Examples**

### Prerequisites
```bash
# Install dependencies (from project root)
bun install

# Make sure Ollama is running
ollama pull llama3.2
```

### Execute Workflows
```bash
# Run the mathematical analysis demo
bun run app/src/examples/workflow-demo.ts

# Expected output:
# 🔬 Mathematical Analysis Workflow
# 📋 Starting analysis with Research Agent...
# 🔄 Handoff to Verification Agent...
# 📝 Handoff to Reporter Agent...
# ✅ Workflow Complete!
```

## 🔍 **What You'll See**

### **Real Agent Handoffs**
```
[Research Agent] Analyzing the Result<T> monad...
The Result<T> type represents computations that may fail...

[Research Agent → Verification Agent] 
Transferring analysis: {
  concept: "Result<T> monad",
  findings: "...",
  confidence: 0.87
}

[Verification Agent] Received analysis, proceeding with verification...
Checking functor laws: ✓
Checking applicative laws: ✓
Checking monad laws: ✓

[Verification Agent → Reporter Agent]
Transferring verification results...

[Reporter Agent] Generating final documentation...
```

### **Streaming in Action**
- Each agent's responses stream in real-time
- Handoff metadata is visible
- Tool calls and results are logged
- Workflow progress is tracked

## 🛠️ **Code Structure Explained**

### **Agent Definition Pattern**
```typescript
const researchAgent = {
  name: "Mathematical Researcher",
  model: ollama('llama3.2'),
  
  // Workflow instructions via system prompt
  systemPrompt: `
    You are a mathematical research agent.
    When analysis is complete, use handoff_to_verifier()
    to transfer your findings.
  `,
  
  handoffs: ['verifier'], // Who you can transfer to
  tools: { /* domain-specific tools */ }
}
```

### **Workflow Creation Pattern**
```typescript
const workflow = createWorkflowOrchestra({
  researcher: researchAgent,
  verifier: verificationAgent,
  reporter: reporterAgent
})

// Execute starting with researcher
const result = await executeWorkflow(
  workflow, 
  'researcher', 
  [{ role: 'user', content: 'Analyze Result<T>' }]
)
```

## 🎯 **Key Learning Points**

1. **System Prompts Define Workflow Steps** - Clear instructions about what each agent should do and when to hand off

2. **Handoffs Are Function Calls** - Agents call `handoff_to_nextAgent()` with structured data

3. **Context Preservation** - All conversation history and handoff data flows through the workflow

4. **Streaming Works Throughout** - Users see real-time progress across all agents

5. **Tools Are Shared and Enhanced** - Regular tools work normally, handoff tools are added automatically

## 🔧 **Customization Tips**

### **Add Your Own Agents**
```typescript
const yourAgent = {
  name: "Your Custom Agent",
  model: yourModel,
  systemPrompt: "Your specific instructions...",
  handoffs: ['next_agent'],
  tools: {
    yourCustomTool: tool({
      description: "What your tool does",
      parameters: z.object({ /* params */ }),
      execute: async (params) => {
        // Your tool implementation
      }
    })
  }
}
```

### **Create Domain-Specific Workflows**
- **Content Creation**: Research → Writing → Editing → Publishing
- **Data Analysis**: Collection → Processing → Validation → Reporting  
- **Code Development**: Planning → Implementation → Testing → Documentation
- **Customer Support**: Triage → Investigation → Resolution → Follow-up

## 🔄 **Next Steps**

1. **Modify the examples** - Change the system prompts and see how behavior changes
2. **Add new agents** - Create your own workflow steps
3. **Integrate tools** - Add MCP tools, web search, databases, etc.
4. **Build frontends** - Use the streaming APIs with React/Next.js
5. **Deploy to production** - These patterns work in any Node.js environment

## 📚 **Further Reading**

- **[Architecture Guide](../guides/architecture.md)** - How AI Orchestra + Vercel AI SDK work together
- **[Creating Workflows Guide](../guides/creating-workflows.md)** - Advanced workflow patterns
- **[API Reference](../api/)** - Complete technical documentation

**Ready to build your own workflow agents? Start by modifying the examples!** 🚀 