# MathForge AI Code Generation Experiment

## Overview
This experiment compares three different approaches to AI-assisted code generation for consistency and quality.

## Quick Start

### Verify Setup First
```bash
# Verify all required files are present
./verify-files.sh
```

### Option 1: Using the Helper Script
```bash
# Set up and run a specific approach
./run-experiment.sh approach1  # Natural Language approach
./run-experiment.sh approach2  # Structured Guidance approach  
./run-experiment.sh approach3  # Formal Verification approach

# Set up all approaches
./run-experiment.sh all
```

### Option 2: Manual Setup
```bash
# For Approach 1 - Multiple experiments
cd approach1
# Run Claude with instruction.md (creates ./output)
mv output e1
# Run Claude again (creates ./output)
mv output e2
# Run Claude again (creates ./output)
mv output e3

# For Approach 2 - Multiple experiments
cd approach2
# Run Claude with instruction.yaml (creates ./output)
mv output e1
# Run Claude again (creates ./output)
mv output e2
# And so on...

# For Approach 3 - Multiple experiments  
cd approach3
# Run Claude with instruction.yaml (creates ./output)
mv output e1
# Run Claude again (creates ./output)
mv output e2
# And so on...
```

## Output Structure

Each approach can have multiple experiment runs for consistency testing:

```
docs/experiment/
├── inputs/             # Shared documentation (ALL approaches read from here)
│   ├── qi.v4.component.contracts.md
│   ├── patterns.md
│   ├── component-implementation.md
│   ├── testing-strategy.md
│   ├── integration-patterns.md
│   ├── function-signatures.md
│   ├── usage-examples.md
│   └── performance-specifications.md
├── approach1/          # Natural Language approach
│   ├── e1/             # First experiment run
│   ├── e2/             # Second experiment run
│   ├── e3/             # Third experiment run
│   └── instruction.md  # Reads: ../inputs/qi.v4.component.contracts.md
├── approach2/          # Structured Guidance approach
│   ├── e1/             # First experiment run
│   ├── e2/             # Second experiment run
│   ├── e3/             # Third experiment run
│   └── instruction.yaml # Reads: ALL ../inputs/*.md files
└── approach3/          # Formal Verification approach
    ├── e1/             # First experiment run
    ├── e2/             # Second experiment run
    ├── e3/             # Third experiment run
    ├── instruction.yaml # Reads: ALL ../inputs/*.md + ./property-specifications.md
    └── property-specifications.md
```

## Three Approaches

### Approach 1: Natural Language Specification
- **Input**: Simple natural language specification (`nl-spec.md`)
- **Goal**: Test basic AI code generation without guidance
- **Output**: `./results/approach1/`

### Approach 2: Structured Guidance + MAX-MIN Principle  
- **Input**: Comprehensive guidance with package recommendations
- **Goal**: Test structured AI guidance with explicit completeness requirements
- **Output**: `./results/approach2/`

### Approach 3: Formal Verification Enhanced
- **Input**: Structured guidance + formal verification requirements
- **Goal**: Test verification-enhanced AI code generation
- **Output**: `./results/approach3/`

## Environment Variables

- **`EXPERIMENT_OUTPUT`**: Directory where generated code will be placed
  - If not set, defaults to `./output` in each approach directory
  - After each experiment: `mv output e1`, then `mv output e2`, etc.
  - Multiple experiments per approach test consistency across runs

## Success Criteria

All approaches target:
- ✅ **5 Components**: Result, QiError, Configuration, Logger, Cache
- ✅ **85%+ Test Coverage** with 300+ tests
- ✅ **Zero Linting Errors**
- ✅ **Integration Tests** between components
- ✅ **Performance Benchmarks** meeting TypeScript tier requirements

Additional for Approach 3:
- ✅ **Property-Based Testing** with 1000+ cases per mathematical law
- ✅ **Formal Verification** of monad and monoid laws

## Research Question

**How do we make AI consistently follow the guidance we give it?**

This experiment tests whether:
1. **Approach 1**: Basic AI generation produces inconsistent results across multiple runs
2. **Approach 2**: Structured guidance improves consistency across multiple runs
3. **Approach 3**: Formal verification ensures mathematical correctness and consistency

## Experimental Design

### Multiple Runs Per Approach
- **3-5 experiments** per approach to test consistency
- **Same instructions** for each run within an approach
- **Fresh Claude sessions** for each experiment to avoid bias

### Consistency Metrics
- **Code Structure**: Similar file organization across runs
- **Implementation Patterns**: Consistent use of design patterns
- **Package Usage**: Similar dependency choices (MAX-MIN compliance)
- **Test Coverage**: Consistent testing strategies
- **Error Handling**: Similar error handling patterns

## Analysis

After running multiple experiments per approach, compare:

### Within-Approach Consistency
- **Approach 1**: How much do e1, e2, e3 differ from each other?
- **Approach 2**: Does structured guidance reduce variation between runs?
- **Approach 3**: Does verification eliminate inconsistencies?

### Cross-Approach Quality
- **Code Quality**: Architecture, patterns, best practices
- **Completeness**: All 5 components implemented and tested
- **Mathematical Correctness**: Monad/monoid laws (Approach 3)
- **Test Coverage**: Comprehensive testing strategies

### Key Research Insights
- **Consistency Impact**: Which approach produces most consistent results?
- **Quality vs. Consistency**: Is there a trade-off between quality and consistency?
- **Guidance Effectiveness**: What types of guidance most improve consistency? 