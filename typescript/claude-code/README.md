# Claude Code Manager with QiCore Integration

A comprehensive TypeScript library for managing Claude Code CLI and SDK interactions with integrated QiCore base components and QiAgent capabilities for production-grade AI workflows.

## 🚀 Features

### Core Claude Code Management
- **Request Management**: Execute Claude Code instructions with reliability patterns
- **CLI Integration**: Direct Claude CLI command execution and response handling
- **SDK Integration**: Claude SDK API integration with proper authentication
- **Instruction Building**: Specialized builders for different code generation tasks

### QiCore Integration
- **Result<T> Pattern**: Monadic error handling throughout all operations
- **Structured Errors**: QiError with context chaining and categorization
- **Performance Monitoring**: Microsecond-precision performance measurement
- **Caching**: LRU + TTL memory caching for repeated requests
- **Configuration**: Monoid-based configuration merging
- **Logging**: Production-grade structured logging

### QiAgent Capabilities
- **Multi-Provider Support**: Claude Code, OpenAI, and local model integration
- **Reliability Patterns**: Circuit breaker, rate limiting, retry with backoff
- **Metrics Collection**: Request/response metrics and health monitoring
- **Context Management**: Multi-turn conversation handling

## 📦 Installation

```bash
cd typescript/claude-code
bun install
```

## 🎯 Quick Start

### Basic Claude Code Request with QiCore

```typescript
import { createQiCoreClaudeManager, createQiCoreImplementationInstruction } from './src';

// Create enhanced request manager
const manager = createQiCoreClaudeManager({
  method: 'cli',
  workingDir: '/path/to/project',
  model: 'sonnet',
  enableCaching: true,
  enablePerformanceMonitoring: true
});

// Create instruction for QiCore component
const instruction = createQiCoreImplementationInstruction('result', {
  performanceTarget: '< 100μs',
  mathematicalPatterns: ['Either monad', 'Functor laws', 'Monad laws']
});

// Execute with reliability patterns
const result = await manager.executeInstruction(instruction);

if (result._tag === 'Right') {
  console.log('Success!', result.right.originalResponse.response.content);
  console.log('Performance:', result.right.performance);
} else {
  console.error('Error:', result.left.message);
  console.error('Context:', result.left.context);
}
```

### Complete QiCore System Implementation

```typescript
import { createQiSystemInstruction } from './src';

// Generate instruction for complete QiCore + QiAgent system
const systemInstruction = createQiSystemInstruction({
  includeDocumentation: true,
  includeExamples: true,
  performanceTargets: {
    'result_operations': '< 100μs',
    'config_merge': '< 1ms',
    'cache_operations': '< 50μs',
    'logger_level_check': '< 1μs'
  }
});

const result = await manager.executeInstruction(systemInstruction);
// This will implement all 6 QiCore components + QiAgent with comprehensive tests
```

### QiAgent Integration

```typescript
// Create QiAgent for advanced AI interactions
const agentResult = manager.createQiAgent({
  provider: 'claude-code',
  authentication: { apiKey: 'your-api-key' },
  rateLimit: 5,
  circuitBreaker: {
    failureThreshold: 3,
    timeout: 30000
  }
});

if (agentResult._tag === 'Right') {
  const agent = agentResult.right;
  
  const response = await agent.generate({
    model: { provider: 'claude-code', modelName: 'sonnet' },
    systemPrompt: 'You are a helpful coding assistant.',
    userPrompt: 'Implement a binary search function in TypeScript.'
  });
  
  console.log('Agent response:', response);
  console.log('Metrics:', agent.getMetrics());
  console.log('Health:', agent.getHealthStatus());
}
```

### Traditional Haskell Generation (Legacy)

```typescript
import { ClaudeRequestManager, createHaskellInstruction } from './src';

// Create traditional manager for Haskell generation
const manager = new ClaudeRequestManager({
  method: 'cli',
  workingDir: '/path/to/project',
  model: 'sonnet',
  verbose: true
});

// Create instruction for Haskell generation
const instruction = createHaskellInstruction('haskell/QiCore/Core/Cache.hs');
const result = await manager.executeInstruction(instruction);

console.log('Success:', result.response.success);
console.log('Content length:', result.response.content?.length);
```

## 🏗️ Architecture

### QiCore Components

#### Base Components
- **Error**: Structured error handling with context chaining
- **Result**: Either monad for composable error handling

#### Core Components  
- **Config**: Configuration management with monoid merge
- **Logger**: Production logging with structured output
- **Cache**: Memory caching with LRU + TTL eviction
- **Performance**: Microsecond-precision measurement

### Integration Patterns

```typescript
// Component composition through Result<T>
const createService = async (): Promise<Result<Service>> => {
  const configResult = loadConfig();
  if (configResult._tag === 'Left') return configResult;
  
  const loggerResult = createLogger(configResult.right);
  if (loggerResult._tag === 'Left') return loggerResult;
  
  const cacheResult = createCache(configResult.right);
  if (cacheResult._tag === 'Left') return cacheResult;
  
  return success(new Service(configResult.right, loggerResult.right, cacheResult.right));
};
```

## 📊 Performance

All components meet TypeScript tier performance requirements:

- **Result operations**: < 100μs
- **Config merge**: < 1ms  
- **Logger level check**: < 1μs
- **Cache operations**: < 50μs

## 🧪 Testing

```bash
# Run all tests
bun test

# Run with coverage
bun run test:coverage

# Test QiCore integration
bun test tests/integration/qicore-integration.test.ts

# Test Haskell generation (legacy)
bun run test:haskell
```

## 📋 Quality Metrics

- **Test Coverage**: 85%+ with 300+ comprehensive tests
- **Linting**: Zero errors with Biome + ESLint
- **Mathematical Correctness**: Property tests verify monad/monoid laws
- **Production Ready**: Comprehensive error handling and edge cases

## 🔧 Configuration

### QiCore Enhanced Configuration

```typescript
interface QiCoreRequestConfig {
  method: 'cli' | 'sdk';
  workingDir: string;
  model?: 'sonnet' | 'opus' | 'haiku';
  maxTurns?: number;
  verbose?: boolean;
  allowedTools?: string[];
  
  // QiCore enhancements
  enableCaching?: boolean;
  enablePerformanceMonitoring?: boolean;
  cacheSize?: number;
  cacheTTL?: number;
}
```

### Traditional Configuration (Legacy)

```typescript
interface ClaudeRequestConfig {
  method: 'cli' | 'sdk';
  workingDir: string;
  outputFormat?: 'text' | 'json' | 'stream-json';
  model?: 'sonnet' | 'opus' | 'haiku';
  maxTurns?: number;
  verbose?: boolean;
  allowedTools?: string[];
  addDirs?: string[];
}
```

## 🎨 Instruction Builders

### QiCore Instructions

```typescript
// Individual component implementation
const resultInstruction = createQiCoreImplementationInstruction('result', {
  outputPath: 'src/qicore/base/result.ts',
  includeTests: true,
  performanceTarget: '< 100μs',
  mathematicalPatterns: ['Either monad', 'Functor laws', 'Monad laws']
});

// Integration testing
const integrationInstruction = createQiCoreIntegrationInstruction(
  ['config', 'logger'],
  'config-logger-integration',
  {
    performanceRequirements: {
      'config_load_and_log': '< 5ms'
    }
  }
);

// Complete system
const systemInstruction = createQiSystemInstruction({
  includeDocumentation: true,
  performanceTargets: {
    'result_operations': '< 100μs',
    'config_merge': '< 1ms'
  }
});
```

### Traditional Instructions (Legacy)

```typescript
// Haskell generation
const haskellInstruction = createHaskellInstruction('haskell/QiCore/Core/Cache.hs');

// TypeScript generation  
const tsInstruction = createTypeScriptInstruction('src/components/MyComponent.ts');

// Analysis
const analysisInstruction = createAnalysisInstruction('src/modules/authentication');
```

## 🚀 Migration from Legacy

### Upgrading to QiCore Integration

```typescript
// Before: Traditional manager
const oldManager = new ClaudeRequestManager({
  method: 'cli',
  workingDir: '/path/to/project'
});

// After: QiCore enhanced manager
const newManager = createQiCoreClaudeManager({
  method: 'cli',
  workingDir: '/path/to/project',
  enableCaching: true,
  enablePerformanceMonitoring: true
});

// Before: Basic instruction execution
const result = await oldManager.executeInstruction(instruction);
console.log('Success:', result.response.success);

// After: Result<T> pattern with error handling
const result = await newManager.executeInstruction(instruction);
if (result._tag === 'Right') {
  console.log('Success:', result.right.originalResponse.response.success);
  console.log('Performance:', result.right.performance);
} else {
  console.error('Error:', result.left.message);
}
```

## 🤝 Integration Examples

### Adding to Existing Projects

```typescript
import { QiCoreRequestManager, Result, isSuccess } from './src';

// Enhance existing functions with Result patterns
async function existingFunction(): Promise<Result<string>> {
  try {
    const result = await someOperation();
    return success(result);
  } catch (error) {
    return failure(createQiError('OPERATION_FAILED', String(error), 'SYSTEM'));
  }
}

// Compose with other Result-returning functions
const pipeline = pipe(
  existingFunction(),
  flatMap(result => processResult(result)),
  flatMap(processed => saveResult(processed))
);
```

## 🛠️ Development

```bash
# Install dependencies
bun install

# Run tests
bun test

# Type checking
bun run typecheck

# Linting
bun run lint

# Build
bun run build

# Development mode
bun run dev
```

## 📚 API Reference

### Enhanced Managers (QiCore)
- `QiCoreRequestManager` - Enhanced request manager with QiCore patterns
- `createQiCoreClaudeManager()` - Factory for pre-configured QiCore manager

### Traditional Managers (Legacy)
- `ClaudeRequestManager` - Base Claude Code request management
- `ClaudeCliManager` - CLI-specific operations
- `ClaudeSdkManager` - SDK-specific operations

### QiCore Instruction Builders
- `createQiCoreImplementationInstruction()` - Component implementation
- `createQiCoreIntegrationInstruction()` - Integration testing  
- `createQiAgentInstruction()` - Agent implementation
- `createQiSystemInstruction()` - Complete system implementation

### Traditional Instruction Builders (Legacy)
- `createHaskellInstruction()` - Haskell code generation
- `createTypeScriptInstruction()` - TypeScript code generation
- `createAnalysisInstruction()` - Code analysis

### QiCore Components
- `Result<T>` - Either monad for error handling
- `QiError` - Structured error with context
- `Config` - Configuration with monoid merge
- `Logger` - Production logging
- `Cache` - Memory cache with LRU + TTL
- `Performance` - Measurement and benchmarking

### QiAgent Components
- `QiAgent.createClaudeCodeAgent()` - Claude Code agent factory
- `QiAgent.createOpenAIAgent()` - OpenAI agent factory
- `Agent.generate()` - Text generation with reliability
- `Agent.getMetrics()` - Performance metrics
- `Agent.getHealthStatus()` - Health monitoring

## 📄 License

MIT - see LICENSE file for details.

## 🤖 Generated with QiCore

This library demonstrates the power of QiCore's mathematical foundations applied to practical AI workflow management. Every component follows category theory principles while maintaining production quality and performance.

---

**Perfect for**: AI-assisted development, automated code generation, reliable Claude Code integration, production AI workflows