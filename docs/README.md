# QiCore v4.0 Technical Documentation

This directory contains the complete technical documentation for the QiCore v4.0 framework, organized by **sources** (that drive the process) and **build outputs** (generated results).

## Directory Structure

```
docs/
├── sources/        # Source files that drive the design process
│   ├── agent/     # AI workflow orchestration and automation
│   ├── guides/    # Core methodology and transformation guides
│   ├── math/      # Mathematical foundations and study materials
│   └── nl/        # Natural language contracts (human input)
└── build/          # Generated outputs from the design process
    ├── design/    # Stage 2 output: Design patterns and analysis
    ├── impl/      # Stage 3 output: Implementation templates
    ├── objective/ # Stage 1 output: Mathematical specifications
    └── verification/  # Verification results and reports
```

## Core Documentation Files

### 🚀 Quick Start
1. **Start Here**: `sources/guides/guide.md` - Complete step-by-step transformation process
2. **Mathematical Foundation**: `sources/guides/common.md` - Pattern library and mathematical definitions
3. **Natural Language Specs**: `sources/nl/` - Input contracts that drive the entire process
4. **Verification**: `../check-list.md` - Ensure all files are properly configured

## Sources Directory (`sources/`)

**Files that drive the QiCore v4.0 design process**

### 📚 Transformation Guides (`sources/guides/`)

The heart of the QiCore v4.0 methodology:

- **`common.md`** - Mathematical patterns library (monads, functors, etc.)
- **`formal.prompt.md`** - Stage 1: Natural Language → Mathematical formalization
- **`design.prompt.md`** - Stage 2: Mathematics → Design patterns  
- **`impl.prompt.md`** - Stage 3: Design patterns → Code implementation
- **`guide.md`** - Complete workflow orchestration

### 🎯 Natural Language Contracts (`sources/nl/`)

Human-written input specifications:

- `qi.v4.class.contracts.md` - 8 behavioral contracts
- `qi.v4.component.contracts.md` - 5 component organization contracts

**These are the only files humans need to write!**

### 🤖 Agent Workflows (`sources/agent/`)

Automated workflow orchestration:

- **`build/`** - YAML workflow definitions for automation tools
  - `inst.formal.yaml` - Automate Stage 1 transformation
  - `inst.design.yaml` - Automate Stage 2 transformation
  - `inst.impl.[lang].yaml` - Automate Stage 3 for each language
- **`verification/`** - Automated verification processes

### 📐 Mathematical Foundations (`sources/math/`)

Study guides and mathematical background:

- **`qi.v4.mathematical.study.guide.md`** - Learn the mathematical concepts
- **`qi.v4.mathematical.models.md`** - Detailed mathematical models

## Build Directory (`build/`)

**Generated outputs from the design process**

### 🎯 Stage 1 Output (`build/objective/formal/`)

Mathematical specifications generated from natural language:

- `qi.v4.formal.spec.md` - Generated mathematical formalization

### 🏗️ Stage 2 Output (`build/design/`)

Design patterns derived from mathematics:

- `qi.v4.design.analysis.md` - Design patterns and implementation strategies

### 💻 Stage 3 Output (`build/impl/`)

Code templates and implementation guides:

- **Templates**: `qi.v4.[lang].template.md` - Actual code implementations
- **Guides**: `qi.v4.[lang].impl.md` - How to use and integrate the code
- **Languages**: TypeScript, Python, Rust, Haskell, Go

### ✅ Verification Results (`build/verification/`)

Quality assurance and verification:

- **`documentation.chain.verification.md`** - Verify documentation completeness
- **`implementation.verification.md`** - Verify implementation correctness

## The QiCore v4.0 Process Flow

```
Stage 0: Human writes natural language contracts
   ↓ (sources/nl/ → sources/guides/formal.prompt.md + common.md)
Stage 1: AI generates mathematical formalization  
   ↓ (build/objective/formal/ → sources/guides/design.prompt.md + common.md)
Stage 2: AI generates design patterns
   ↓ (build/design/ → sources/guides/impl.prompt.md + common.md)
Stage 3: AI generates implementations
   ↓ (build/impl/)
Result: Production-ready code in multiple languages
```

## Usage Patterns

### For New Users
1. Read `sources/guides/guide.md` for complete workflow
2. Examine `sources/nl/` contracts to understand inputs
3. Follow the 3-stage transformation process
4. Use generated code from `build/impl/`

### For Advanced Users
- Customize patterns in `sources/guides/common.md`
- Extend workflow automation in `sources/agent/build/`
- Add new languages following `build/impl/` patterns
- Contribute verification processes in `build/verification/`

### For Researchers
- Study mathematical foundations in `sources/math/`
- Analyze transformation precision in workflow files
- Examine pattern coverage in `sources/guides/common.md`
- Review verification methodologies

## Key Principles

1. **Mathematical Precision**: Every transformation preserves semantic meaning
2. **Cross-Language Consistency**: Same patterns work across all target languages
3. **Verifiable Process**: Each step can be validated mathematically
4. **Human-AI Collaboration**: Humans specify intent, AI handles implementation

## Getting Help

- **Process Questions**: See `sources/guides/guide.md`
- **Pattern Questions**: See `sources/guides/common.md`
- **Implementation Questions**: See `build/impl/qi.v4.[lang].impl.md`
- **Mathematical Questions**: See `sources/math/qi.v4.mathematical.study.guide.md`

---

This documentation represents the complete QiCore v4.0 technical framework for revolutionizing AI-assisted software development through mathematical precision and categorical foundations. 