# MathForge - Universal Mathematical Code Generator

A TypeScript-based Universal Mathematical Code Generator with Formal Verification, built for the AI era.

## Architecture Overview

### **Phase 1: TypeScript + Claude Code SDK + MCP Foundation**
- **Runtime**: Bun (TypeScript-native)
- **AI Integration**: Claude Code SDK + Model Context Protocol (MCP)
- **Local LLM**: Ollama integration via MCP servers
- **Core Framework**: QiCore patterns (Result<T>, logging, performance)

### **Phase 2: Agent Integration**
- **Option A**: TypeScript-native agent framework (CrewAI-TS, LangGraph.js)
- **Option B**: Agno integration via Python bridge
- **Option C**: Custom agent orchestration using MCP

### **Phase 3: Advanced Features**
- Multi-agent workflows
- QiPrompt integration
- Advanced mathematical reasoning

## Technology Stack

### **Core**
- **Language**: TypeScript 5.8.3
- **Runtime**: Bun 1.2+
- **Testing**: Vitest 3.2.4
- **Linting**: Biome 2.0.6

### **AI & Agents**
- **Claude Code SDK**: Primary AI interface
- **Model Context Protocol (MCP)**: Tool and server integration
- **Ollama**: Local LLM runtime
- **QiCore**: Logging, error handling, performance monitoring

### **Mathematical Processing**
- **YAML Parsing**: Secure mathematical specification parsing
- **Code Generation**: Multi-language output (Python, TypeScript, Rust, etc.)
- **Formal Verification**: Mathematical proof validation

## Quick Start

```bash
# Install dependencies
bun install

# Start development
bun run dev

# Run tests
bun test

# Run demo
bun run demo:enterprise
```

## Project Structure

```
mathforge/
├── core/                 # Core mathematical processing
├── agent/               # AI agent implementations
├── mcp/                 # Model Context Protocol servers
├── claude/              # Claude Code SDK integration
├── types/               # TypeScript type definitions
└── tests/               # Comprehensive test suite
```

## Why TypeScript?

1. **Claude Code SDK**: First-class TypeScript support
2. **MCP Ecosystem**: Official TypeScript SDK
3. **Bun Integration**: Native TypeScript runtime
4. **Type Safety**: Mathematical operations benefit from strong typing
5. **Tooling**: Superior development experience
6. **QiCore**: Better integration with existing TypeScript patterns

## Development Philosophy

- **Type Safety First**: Leverage TypeScript's type system for mathematical correctness
- **Performance**: Bun + native TypeScript for maximum speed
- **AI-Native**: Built for Claude Code SDK and MCP integration
- **Local-First**: Ollama support for offline development
- **Enterprise-Ready**: QiCore patterns for production reliability
