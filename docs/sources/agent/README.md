# QiCore v4.0 Agent Directory

> **Purpose**: Instructions and automation for AI-assisted QiCore generation and verification  
> Date: June 19, 2025

## Directory Structure

```
agent/
├── build/                    # Generation and build instructions
│   ├── process-instructions.md      # Overall process documentation
│   ├── process-instructions.yaml    # Main build orchestration
│   ├── process-stage1-2.yaml       # Formal spec & design generation
│   ├── process-stage3.yaml         # Implementation generation
│   ├── language-config.yaml        # Language-specific configurations
│   └── generate.md                 # Generation guidelines
│
└── verification/             # Verification and validation
    ├── verification-instructions.md # Verification guidelines
    ├── verification-checklist.md    # Manual checklist
    ├── formal.yaml                  # Formal spec verification
    └── impl.yaml                    # Implementation verification
```

## Workflow

### 1. Generation Workflow (build/)

```
objective/nl + guides → formal spec → design → implementation
```

1. **Stage 1-2**: Generate formal specification and design analysis
   - Input: Natural language contracts + guides
   - Output: Mathematical formalization + design patterns
   - Run: `process-stage1-2.yaml`

2. **Stage 3**: Generate language-specific implementations
   - Input: Design analysis
   - Output: Templates and implementation guides
   - Run: `process-stage3.yaml --language=[ts|py|hs|rs|go]`

### 2. Verification Workflow (verification/)

```
formal spec → verify math → implementation → verify code → compliance
```

1. **Formal Verification**: Verify mathematical correctness
   - Checks: Simplified math, realistic performance, complete patterns
   - Run: `formal.yaml`

2. **Implementation Verification**: Verify code quality
   - Checks: All contracts, concrete examples, error handling, performance
   - Run: `impl.yaml --language=[ts|py|hs|rs|go]`

## Key Updates (June 19, 2025)

Based on the review feedback, the following improvements were made:

### Mathematical Simplification
- Configuration: Sheaf → Monoid with merge operation
- Logging: Free Monad → Simple effect interface

### Performance Realism
- Replaced "< 100ns" with language-tier targets:
  - Native: < 1μs (Rust, C++)
  - VM: < 10μs (Go, Java)
  - Interpreted: < 100μs (Python, JavaScript)
  - Functional: < 50μs (Haskell)

### Enhanced Requirements
- Concrete, runnable examples (not just interfaces)
- Error recovery and circuit breaker patterns
- Streaming data handling
- Clear 95% compliance metrics (property-based tests)

### Bidirectional Workflow
- Implementation insights flow back to improve specifications
- Iterative refinement supported

## Usage

### Generate QiCore Implementation

```bash
# Generate formal spec and design
./run-agent.sh build/process-stage1-2.yaml

# Generate TypeScript implementation
./run-agent.sh build/process-stage3.yaml --language=ts

# Generate all languages
for lang in ts py hs rs go; do
  ./run-agent.sh build/process-stage3.yaml --language=$lang
done
```

### Verify Implementation

```bash
# Verify formal specification
./run-agent.sh verification/formal.yaml

# Verify TypeScript implementation
./run-agent.sh verification/impl.yaml --language=ts

# Run all verifications
./run-agent.sh verification/all
```

## Next Steps

1. Run formal specification verification
2. Regenerate formal spec if needed
3. Regenerate design analysis
4. Generate implementations for each language
5. Verify all implementations meet requirements
6. Achieve 95% compliance across all languages 