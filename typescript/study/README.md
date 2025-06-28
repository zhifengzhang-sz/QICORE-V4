# AI Code Generation Consistency Study (Bun + TypeScript)

🚀 **Modern AI Research Platform** built with 2024-2025 best practices

## Project Overview

This project implements a one-day study to measure AI code generation consistency across different models and instruction approaches using the **2024-2025 AI stack winner: Bun + TypeScript**.

## Key Features

- ⚡ **Ultra-fast Bun runtime** (3x faster than Node.js)
- 🏗️ **Built-in SQLite database** (no external dependencies)  
- 🔧 **Modern TypeScript** with path aliases and strict typing
- 📊 **Statistical analysis** with consistency metrics
- 🤖 **Multi-model support** (OpenAI GPT-4, Anthropic Claude)
- 🔄 **Haskell subproject** for code compilation

## Quick Start

### Prerequisites
- Bun v1.2+ (Install: `curl -fsSL https://bun.sh/install | bash`)
- API Keys: OpenAI and/or Anthropic

### Installation
```bash
cd typescript/study
bun install

# Set environment variables
export OPENAI_API_KEY="your-key-here"
export ANTHROPIC_API_KEY="your-key-here"
```

### Run Study
```bash
# Quick study (3 runs per model)
bun run start --quick

# Full study (10 runs per model)  
bun run start

# Analyze existing study
bun run start --analyze-only --study-id=<study-id>
```

## Study Design

### Models Tested
- **GPT-4 Turbo** (OpenAI)
- **Claude 3.5 Sonnet** (Anthropic)

### Instructions Compared
- **Modern YAML** - Knowledge-first approach
- **Simple YAML** - Traditional approach

### Quality Metrics (0-100 scale)
- **Syntactic**: Compilation success, syntax correctness
- **Semantic**: Logic correctness, type safety
- **Modern**: Modern language features usage
- **Completeness**: Implementation thoroughness
- **Documentation**: Code documentation quality
- **Performance**: Efficiency characteristics

### Consistency Metrics
- **Mean Score**: Average quality across runs
- **Standard Deviation**: Variability measure
- **Coefficient of Variation**: Relative consistency
- **Consistency Rank**: High/Medium/Low classification

## Why This Stack Won (2024-2025)

### Bun Performance Advantages
- **3x faster runtime** than Node.js (50k+ req/s vs 13k req/s)
- **17x faster package installs** than npm
- **Built-in databases**: SQLite, PostgreSQL, Redis, S3
- **Native TypeScript**: No transpilation needed
- **All-in-one toolkit**: Runtime + package manager + test runner + bundler

### Modern Tooling
- **Biome**: 24x faster than Prettier, 22x faster than ESLint
- **Path aliases**: Clean imports with `@/` prefix
- **Hot reload**: Instant development feedback

## Available Scripts

```bash
# Development
bun run dev              # Hot reload development
bun run start           # Run study
bun run test            # Run test suite

# Code Quality (24x faster than traditional tools)
bun run lint            # Biome linting
bun run format          # Biome formatting

# Study Operations
bun run study:generate  # Generate code samples
bun run study:analyze   # Analyze results
bun run study:dashboard # Start web dashboard

# Haskell Integration
bun run haskell:build   # Compile Haskell evaluator
```

## Expected Results

1. **Model Comparison**: Which AI model produces more consistent code?
2. **Instruction Effectiveness**: Do modern YAML instructions reduce variability?
3. **Quality vs Consistency**: Trade-offs between average quality and consistency
4. **Practical Insights**: Actionable recommendations for AI-assisted development

---

**Built with 💙 using Bun, TypeScript, and modern AI tools** 