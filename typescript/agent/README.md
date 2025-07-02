# QiCore Agent Workspace

**MCP-Powered Mathematical Verification Agent with Library/Application Architecture**

## 🏗️ **Architecture Overview**

This project follows a **library/application pattern** for maximum reusability:

### **Key Improvements:**
1. ✅ **Library/App separation** - Reusable components in `lib/`, specific logic in `app/`
2. ✅ **No runtime MCP server spawning** - Connects to external servers instead  
3. ✅ **Proper Bun/Biome/Vitest setup** - Following project standards
4. ✅ **Clean separation of concerns** - Each functionality in its own module
5. ✅ **ES Modules with .js extensions** - Proper import paths for Bun

## 📁 **Project Structure**

```
typescript/agent/
├── lib/                           # 📚 Reusable Library
│   ├── src/
│   │   ├── mcp/
│   │   │   └── client.ts          # MCP Client (reusable)
│   │   ├── analysis/
│   │   │   └── file-output.ts     # Analysis utilities (reusable)
│   │   └── index.ts               # Library exports
│   ├── package.json               # Library dependencies
│   ├── tsconfig.json             # Library TypeScript config
│   └── vitest.config.ts          # Library test config
├── app/                           # 🚀 Application
│   ├── src/
│   │   ├── agents/
│   │   │   └── ollama-only-agent.ts # App-specific agent
│   │   ├── utils/
│   │   │   ├── compare-specifications.ts
│   │   │   └── test-mathematical-contracts.ts
│   │   ├── mcp-verification-agent.ts # Main orchestrator
│   │   └── index.ts               # App entry point
│   ├── package.json               # App dependencies (includes lib)
│   ├── tsconfig.json             # App TypeScript config
│   └── vitest.config.ts          # App test config
├── package.json                   # Workspace configuration
├── biome.json                     # Shared linting/formatting
└── README.md                     # This file
```

## 🚀 **Getting Started**

### **Installation**
```bash
cd typescript/agent

# Install all workspace dependencies
bun run install:all

# Or install individually
bun install              # Workspace root
cd lib && bun install    # Library
cd ../app && bun install # Application
```

### **Development**
```bash
# From workspace root:
bun run mcp:verify       # Run the main agent
bun run dev              # Watch mode
bun run test             # Run all tests
bun run lint:fix         # Lint and format
bun run typecheck        # Check types

# Individual components:
bun run test:lib         # Test library only
bun run test:app         # Test app only
bun run build:lib        # Build library only
bun run build:app        # Build app only
```

## 🔧 **Key Components**

### **1. MCP Client (`src/mcp/client.ts`)**
- **Purpose**: Connect to external MCP servers (memory, filesystem)
- **Key Point**: Does NOT spawn servers - connects to existing ones
- **Usage**: 
  ```typescript
  const client = new MCPClient(logger);
  await client.connectToServer({
    name: 'filesystem',
    command: 'bunx',
    args: ['--bun', '@modelcontextprotocol/server-filesystem', process.cwd()]
  });
  ```

### **2. Analysis File Manager (`src/analysis/file-output.ts`)**
- **Purpose**: Handle analysis result formatting and file output
- **Features**: Markdown reports, JSON data, summary generation
- **Usage**:
  ```typescript
  const fileManager = new AnalysisFileManager();
  const { reportPath } = fileManager.saveAnalysisResult(result);
  ```

### **3. Main Agent (`src/mcp-verification-agent.ts`)**
- **Purpose**: Orchestrates the entire verification workflow
- **Features**: 
  - Connects to MCP servers
  - Analyzes mathematical contracts
  - Saves results via both MCP and direct filesystem
  - Generates comprehensive summaries

### **4. Ollama Agent (`src/agents/ollama-only-agent.ts`)**
- **Purpose**: Pure mathematical analysis using Ollama
- **Moved from**: Original `mathforge/agent/ollama-only-agent.ts`

## 🎯 **MCP Server Architecture**

### **How MCP Servers Should Work:**

**❌ WRONG (Previous Approach):**
- Runtime spawning servers within the application
- Mixing server lifecycle with client logic
- Using `npx` in a Bun project

**✅ CORRECT (Current Approach):**
- MCP servers run as separate processes
- Client connects to existing servers
- Clean separation between client and server
- Using `bunx` for Bun compatibility

### **Starting MCP Servers Externally:**
```bash
# Terminal 1: Memory Server
bunx --bun @modelcontextprotocol/server-memory

# Terminal 2: Filesystem Server  
bunx --bun @modelcontextprotocol/server-filesystem /path/to/workspace

# Terminal 3: Your Agent
bun run mcp:verify
```

## 🧪 **Testing**

```bash
# Run all tests
bun test

# Watch mode
bun test --watch

# Coverage
bun test --coverage

# UI mode
bun test --ui
```

## 📊 **Scripts Available**

| Script | Description |
|--------|-------------|
| `bun run mcp:verify` | Run MCP verification agent |
| `bun run agent:analysis` | Run analysis agent |
| `bun run agent:ollama` | Run Ollama agent |
| `bun run dev` | Development with watch mode |
| `bun test` | Run tests |
| `bun run lint` | Check linting |
| `bun run lint:fix` | Fix linting issues |
| `bun run format` | Format code |
| `bun run typecheck` | TypeScript type checking |

## 🎯 **Key Differences from Previous Implementation**

### **1. MCP Architecture**
- **Before**: Runtime server spawning with complex lifecycle management
- **After**: Simple client connections to external servers

### **2. Project Setup**
- **Before**: Mixed npm/npx usage in Bun project
- **After**: Pure Bun with `bunx` for external packages

### **3. Code Organization**
- **Before**: Single large files with mixed concerns
- **After**: Modular architecture with single responsibilities

### **4. Error Handling**
- **Before**: Complex try/catch with server cleanup
- **After**: Simple connection management with graceful fallbacks

## 🔬 **Mathematical Analysis Features**

- **Algebraic Structure Detection**: Semi-groups, Monoids, Monads
- **Law Verification**: Mathematical property checking
- **Completeness Scoring**: Quantitative analysis
- **Gap Identification**: Missing implementation detection
- **Pattern Recognition**: Inevitable mathematical patterns

## 📈 **Output Examples**

The agent generates:
- Individual component analysis reports (Markdown)
- Raw data files (JSON)
- Comprehensive summary reports
- MCP-stored analysis data
- Memory server summaries

---

**Built with:** Bun + TypeScript + Biome + Vitest + MCP SDK 