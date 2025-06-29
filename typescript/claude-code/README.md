# Claude Code Request Manager

A comprehensive TypeScript library for managing Claude Code CLI and SDK interactions with structured instruction building, execution, and validation.

## Features

- 🔧 **Dual Method Support**: Both CLI and SDK execution methods
- 📋 **Structured Instructions**: Type-safe instruction building with validation
- 🧪 **Comprehensive Testing**: Built-in validation and quality assessment
- 🎯 **QiCore Integration**: Pre-configured for QiCore development patterns
- 📊 **Detailed Reporting**: Execution results with quality scoring
- 🔒 **Type Safety**: Full TypeScript support with Zod validation

## Installation

```bash
cd typescript/claude-code
npm install
```

## Quick Start

### Basic Usage

```typescript
import { ClaudeRequestManager, createHaskellInstruction } from './src';

// Create a manager
const manager = new ClaudeRequestManager({
  method: 'cli',
  workingDir: '/path/to/project',
  model: 'sonnet',
  verbose: true
});

// Create instruction for Haskell generation
const instruction = createHaskellInstruction('src/MyModule.hs');

// Execute instruction
const result = await manager.executeInstruction(instruction);

console.log('Success:', result.response.success);
console.log('Generated content length:', result.response.content?.length);
```

### QiCore Development

```typescript
import { createQiCoreManager, createHaskellInstruction } from './src';

// Pre-configured for QiCore development
const manager = createQiCoreManager('cli', {
  verbose: true,
  model: 'sonnet'
});

// Generate Cache module
const instruction = createHaskellInstruction('haskell/QiCore/Core/Cache.hs');
const result = await manager.executeInstruction(instruction);
```

## Test the Haskell Generation

Run the test to compare with existing QiCore quality:

```bash
# Test single scenario
npm run test:haskell

# Test multiple scenarios
tsx src/test-haskell-generation.ts --multiple
```

## Available Scripts

```bash
npm run build          # Build TypeScript
npm run dev            # Run development version
npm run test:haskell   # Test Haskell generation
npm run lint           # Lint code
npm run format         # Format code
```

## Instruction Types

### Haskell Instructions

```typescript
const instruction = createHaskellInstruction(
  'haskell/QiCore/Core/Cache.hs',
  '/path/to/project'
);

// Automatically includes:
// - Modern Haskell 2024 knowledge updates
// - QiCore quality standards
// - Mathematical law compliance requirements
// - Performance optimization guidance
```

### TypeScript Instructions

```typescript
const instruction = createTypeScriptInstruction(
  'src/components/MyComponent.ts',
  '/path/to/project'
);
```

### Analysis Instructions

```typescript
const instruction = createAnalysisInstruction(
  'src/modules/authentication',
  '/path/to/project'
);
```

## Configuration Options

### CLI Method

```typescript
const manager = new ClaudeRequestManager({
  method: 'cli',
  workingDir: '/path/to/project',
  outputFormat: 'json',       // 'text' | 'json' | 'stream-json'
  model: 'sonnet',           // 'sonnet' | 'opus' | 'haiku'
  maxTurns: 10,
  verbose: true,
  addDirs: ['../shared']     // Additional directories
});
```

### SDK Method

```typescript
const manager = new ClaudeRequestManager({
  method: 'sdk',
  workingDir: '/path/to/project',
  maxTurns: 10,
  systemPrompt: 'You are a Haskell expert...',
  allowedTools: ['Read', 'Write', 'Edit', 'Glob', 'Grep'],
  verbose: true
});
```

## Validation and Quality Assessment

The library includes comprehensive validation:

```typescript
import { ResultValidator } from './src';

const validator = new ResultValidator('/path/to/project');
const testResult = await validator.validateExecution(
  executionResult,
  ['expected/file1.hs', 'expected/file2.hs']
);

console.log('Overall Score:', testResult.overallScore);
console.log('Success:', testResult.success);

// Generate detailed report
const report = validator.generateReport(testResult);
console.log(report);
```

### Validation Criteria

- **Basic Execution**: Success/failure status
- **Content Quality**: Code patterns, documentation, error handling
- **File Generation**: Expected files created
- **Haskell Quality**: Modern patterns, QiCore integration, type safety
- **Performance**: Execution time and efficiency

## Architecture

```
src/
├── types/              # TypeScript types and schemas
├── instructions/       # Instruction builders
├── managers/          # CLI and SDK managers
├── tests/             # Validation and testing utilities
└── test-haskell-generation.ts  # Main test file
```

### Key Components

- **RequestManager**: Main orchestration class
- **CliManager**: Claude Code CLI wrapper
- **SdkManager**: Claude Code SDK wrapper (when available)
- **InstructionBuilder**: Type-safe instruction construction
- **ResultValidator**: Quality assessment and scoring

## Quality Standards

The library enforces QiCore quality standards:

### Haskell
- GHC2024 language edition with modern extensions
- Strict performance annotations (!fields)
- Rich Haddock documentation with examples
- Mathematical law compliance (Monad/Functor/Applicative)
- Integration with Result<T> and QiError
- Modern deriving strategies

### TypeScript
- Strict TypeScript configuration
- Runtime validation with Zod
- Comprehensive JSDoc documentation
- Functional programming patterns
- Integration with existing QiCore patterns

## Error Handling

```typescript
import { ClaudeCodeError, ValidationError, ExecutionError } from './src';

try {
  const result = await manager.executeInstruction(instruction);
  if (!result.response.success) {
    console.error('Execution failed:', result.response.error);
  }
} catch (error) {
  if (error instanceof ValidationError) {
    console.error('Validation failed:', error.message);
  } else if (error instanceof ExecutionError) {
    console.error('Execution error:', error.message);
  }
}
```

## Testing

The test suite includes:

1. **Single Module Generation**: Generate and validate one module
2. **Multiple Scenarios**: Test different generation patterns
3. **Quality Comparison**: Compare against existing QiCore implementations
4. **Performance Validation**: Execution time and efficiency metrics

## Example Output

```
🧪 Testing Claude Code Haskell Generation
==========================================
Base directory: /home/zzhang/dev/qi/github/mcp-server/qicore-v4
Target file: haskell/QiCore/Core/Cache.hs

🔧 Testing CLI Method
=====================
CLI Status: ✅ Available
Executing CLI instruction...
CLI Result:
- Success: true
- Execution time: 45000ms
- Content length: 12547 chars

📊 Validation Results:
- Overall Score: 87.3%
- Success: ✅ PASS
- Validations: 8

📄 Detailed Report:
# Validation Report: cli_execution_2024-01-15T10:30

**Overall Score:** 87.3%
**Success:** ✅ PASS
**Execution Time:** 45000ms

## Validation Results

✅ **Basic execution success** (100.0%)
   Execution completed successfully

✅ **Content quality** (85.0%)
   Passed 4/4 quality checks

✅ **File existence: haskell/QiCore/Core/Cache.hs** (100.0%)
   File was generated successfully

✅ **Haskell quality: haskell/QiCore/Core/Cache.hs** (80.0%)
   Found 4/6 modern patterns

✅ **QiCore integration: haskell/QiCore/Core/Cache.hs** (100.0%)
   Proper QiCore type integration found
```

## Contributing

1. Follow existing code patterns and TypeScript strict mode
2. Add comprehensive tests for new features
3. Update documentation for API changes
4. Ensure all validations pass

## License

MIT - See LICENSE file for details