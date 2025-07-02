# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Core Commands
- `bun install` - Install all workspace dependencies
- `bun run dev` - Start development mode (runs app)
- `bun run build` - Build all workspace packages
- `bun run test` - Run all tests in workspace
- `bun run typecheck` - Type check all TypeScript files
- `bun run lint` - Check code formatting with Biome
- `bun run lint:fix` - Fix code formatting issues
- `bun run format` - Format code with Biome

### Workspace-Specific Commands
- `bun run test:lib` - Test library package only
- `bun run test:app` - Test app package only
- `bun run check` - Run typecheck + test for all packages
- `bun run clean` - Clean build artifacts

### Application Commands
- `bun run mcp:verify` - Run MCP verification agent
- `bun run start` - Start the application

### Test Commands
- `vitest` - Interactive test runner
- `vitest run` - Run tests once
- `vitest --ui` - Test UI interface
- `vitest --coverage` - Run with coverage

## Project Architecture

This is a **Bun workspace** with **library/application separation**:

### High-Level Structure
```
agent/
├── lib/          # @qicore/agent-lib - Reusable MCP tools and utilities
├── app/          # @qicore/agent-app - Application-specific agent implementations  
├── tests/        # Cross-package integration tests
└── docs/         # AI Orchestra + Vercel AI SDK documentation
```

### Key Architectural Concepts

**MCP (Model Context Protocol) Architecture:**
- **External Server Model**: MCP servers run as separate processes
- **Client Connection**: Agents connect to existing MCP servers (filesystem, memory)
- **No Runtime Spawning**: Avoid spawning servers within the application
- **Bun Compatibility**: Use `bunx` instead of `npx` for external packages

**AI Orchestra + Vercel AI SDK Integration (2025 Enhanced):**
- **Vercel AI SDK 4.2**: LLM foundation with reasoning models, PDF/image generation, computer use tools
- **AI Orchestra**: Specialized vertical agents, adaptive workflow automation, microservices architecture
- **Workflow-Driven**: Multi-agent workflows with self-healing capabilities and real-time adaptation
- **Streaming-First**: Enhanced real-time progress with workflow metadata and agent coordination

### Library Package (`lib/`)
Contains reusable components:
- `src/qimcp/` - MCP client and tools (file operations, memory)
- `src/qiagent/` - QiCore agent framework integration (AI Orchestra + Claude Code wrapper)
- `src/qiprompt/` - QiCore prompt engineering tools (Vercel AI SDK wrapper)

### Application Package (`app/`)
Contains specific implementations:
- `src/agents/` - Concrete agent implementations (ollama.ts)
- `src/utils/` - Application-specific utilities
- `src/mcp-verification-agent.ts` - Main orchestrator
- `src/index.ts` - Application entry point

## Technology Stack

**Runtime & Package Manager:**
- **Bun**: Latest v1.2.17+ (all-in-one JavaScript runtime with 25x faster package manager)
- **Workspaces**: Monorepo management with automatic workspace detection
- **TypeScript**: ES modules with first-class support, direct .ts/.tsx execution

**Testing & Quality:**
- **Vitest 3.0**: Next-generation testing with enhanced performance, rich plugin ecosystem, improved mocking
- **Biome 2.0**: Type-aware linting without TypeScript compiler, multi-file analysis, plugin support
- **TypeScript**: Strict type checking with ES module compatibility

**AI & MCP (2025 Features):**
- **@modelcontextprotocol/sdk**: MCP v2025-06-18 with OAuth security, elicitation support, SSE transport
- **ai**: Vercel AI SDK 4.2 with reasoning models (Claude 3.7 Sonnet), PDF support, image generation
- **ai-orchestra**: Workflow orchestration with specialized vertical agents, adaptive automation
- **ollama-ai-provider**: Local model support with Vercel AI SDK integration
- **@ai-sdk/openai**: OpenAI provider with context-aware completions, prompt caching
- **@ai-sdk/anthropic**: Anthropic provider with computer use integration

## MCP Server Usage (2025 Enhanced)

**Start External MCP Servers (v2025-06-18):**
```bash
# Terminal 1: Memory Server with OAuth security
bunx --bun @modelcontextprotocol/server-memory

# Terminal 2: Filesystem Server with SSE transport
bunx --bun @modelcontextprotocol/server-filesystem /path/to/workspace

# Terminal 3: Your Agent
bun run mcp:verify
```

**Enhanced Connection Pattern:**
```typescript
const client = new MCPClient(logger);
await client.connectToServer({
  name: 'filesystem',
  command: 'bunx',
  args: ['--bun', '@modelcontextprotocol/server-filesystem', process.cwd()],
  transport: 'sse', // SSE transport for server-to-client streaming
  security: 'oauth', // OAuth Resource Server with protected metadata
});
```

**MCP 2025 Features:**
- **Enhanced Security**: OAuth Resource Server classification with protected resource metadata
- **Elicitation Support**: Servers can request additional information from clients
- **Multiple Transports**: stdio, WebSockets, HTTP SSE, UNIX sockets
- **Industry Adoption**: OpenAI, Google DeepMind, Cursor, Windsurf integration

## Code Style (Biome 2.0 Configuration)

- **Indentation**: Tabs (width 2)
- **Line Width**: 100 characters
- **Quotes**: Double quotes
- **Semicolons**: Always
- **Trailing Commas**: ES5 style
- **Arrow Parentheses**: Always

**Biome 2.0 Enhanced Features:**
- **Type-Aware Linting**: No TypeScript compiler dependency, 75% floating promise detection
- **Linter Plugins**: GritQL-based custom rule creation
- **Multi-File Analysis**: Cross-file dependency checking
- **Enhanced Suppressions**: // biome-ignore-all and start/end comments
- **Domain System**: React, Next.js, testing framework rule groups

## Testing Strategy (Vitest 3.0)

**Test Organization:**
- Unit tests alongside source files (`*.test.ts`)
- Integration tests in `/tests/` directory
- Coverage targets all `/src/` TypeScript files
- Exclude index.ts files (exports only)

**Running Tests:**
- Use `bun test` for all tests
- Use `bun run test:lib` or `bun run test:app` for specific packages
- Use `vitest --ui` for interactive testing
- Coverage reports include text, JSON, and HTML formats

**Vitest 3.0 Enhanced Features:**
- **Improved Performance**: Outpaces Jest with Vite's optimized build pipeline
- **Enhanced Mocking**: Dynamic module mocking with fine-grained control
- **Rich Plugin Ecosystem**: Custom matchers and integration plugins
- **Enhanced Snapshot Testing**: Better diffing capabilities and update commands
- **Built-in CI/CD Support**: Optimized for cloud-based environments

## Workflow Development (2025 Enhanced)

**Agent Definition Pattern:**
```typescript
const agent = {
  name: "Reasoning Agent",
  model: anthropic('claude-3.7-sonnet'), // 2025: Reasoning models supported
  systemPrompt: `Define role and handoff criteria here`,
  handoffs: ['next-agent'], // Who to transfer to
  tools: { 
    // 2025: Enhanced tools with PDF, image generation, computer use
    analyzePDF: tool({ /* PDF support */ }),
    generateImage: tool({ /* Image generation */ }),
    /* Regular Vercel AI SDK tools */ 
  }
}
```

**Workflow Execution (Enhanced):**
```typescript
const workflow = createWorkflowOrchestra({
  step1: agent1, // Specialized vertical agent
  step2: agent2  // Adaptive workflow automation
});

// 2025: Self-healing workflows with real-time adaptation
const stream = await executeWorkflow(workflow, 'step1', messages, {
  selfHealing: true,
  adaptiveWorkflow: true,
  microservicesArchitecture: true
});
```

**2025 Workflow Features:**
- **Reasoning Models**: Claude 3.7 Sonnet, DeepSeek R1 with chain-of-thought
- **Specialized Agents**: Vertical AI agents for domain-specific tasks
- **Self-Healing**: Automatic error detection and workflow adaptation
- **Multi-Modal**: PDF processing, image generation, computer use integration

## Import Patterns

**Library Components**:
- `@qi/mcp` - MCP client and filesystem/memory tools
- `@qi/agent` - AI Orchestra wrapper for workflow orchestration  
- `@qi/prompt` - Vercel AI SDK wrapper for mathematical prompting

**Usage Examples**:
```typescript
import { MCPClient } from "@qi/mcp";
import { createMathWorkflow } from "@qi/agent";
import { createMathematicalPromptManager } from "@qi/prompt";
```

## Common Patterns

**File Operations**: Use MCP filesystem tools rather than direct Node.js fs operations
**Analysis Output**: Generate timestamped analysis directories with JSON + Markdown reports
**Error Handling**: Graceful fallbacks when MCP servers unavailable
**Streaming**: Preserve real-time updates throughout multi-agent workflows
**Mathematical Analysis**: Focus on algebraic structures, law verification, completeness scoring

## Development Workflow

1. **Setup**: `bun install` in workspace root
2. **Development**: `bun run dev` for application development
3. **Testing**: `bun run test` during development
4. **Quality**: `bun run lint:fix` before committing
5. **Type Safety**: `bun run typecheck` to validate TypeScript
6. **Clean Build**: `bun run clean` to reset state