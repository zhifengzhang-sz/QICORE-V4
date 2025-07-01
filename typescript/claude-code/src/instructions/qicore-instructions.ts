/**
 * QiCore-specific instruction builders for Claude Code
 * Provides specialized instructions for QiCore development workflows
 */

import type { ClaudeInstruction } from '../types';

/**
 * Create instruction for implementing QiCore components
 */
export function createQiCoreImplementationInstruction(
  component: 'error' | 'result' | 'config' | 'logger' | 'cache' | 'performance',
  options: {
    outputPath?: string;
    includeTests?: boolean;
    performanceTarget?: string;
    mathematicalPatterns?: string[];
  } = {}
): ClaudeInstruction {
  const componentSpecs = {
    error: {
      description: 'QiError with context chaining and cause management',
      patterns: ['Product type', 'Context accumulation'],
      requirements: 'Structured error data, context chaining, serialization'
    },
    result: {
      description: 'Result<T> Either monad for error handling',
      patterns: ['Either monad', 'Functor', 'Monad laws'],
      requirements: 'Monad laws verified, composition patterns, type safety'
    },
    config: {
      description: 'Configuration with monoid merge operations',
      patterns: ['Monoid', 'Associative merge'],
      requirements: 'Monoid laws, environment loading, validation'
    },
    logger: {
      description: 'Production logging with structured output',
      patterns: ['Simple effect', 'Transport abstraction'],
      requirements: 'Level filtering, structured logging, transport failures'
    },
    cache: {
      description: 'Memory cache with LRU and TTL eviction',
      patterns: ['State monad', 'LRU algorithm'],
      requirements: 'LRU eviction, TTL expiry, concurrent access safety'
    },
    performance: {
      description: 'Performance measurement and benchmarking',
      patterns: ['Function composition', 'Statistical analysis'],
      requirements: 'Microsecond precision, benchmarking, metrics collection'
    }
  };

  const spec = componentSpecs[component];
  const outputPath = options.outputPath ?? `src/qicore/${component === 'error' || component === 'result' ? 'base' : 'core'}/${component}.ts`;
  const testPath = `tests/qicore/${component === 'error' || component === 'result' ? 'base' : 'core'}/${component}.test.ts`;

  let instruction = `Implement QiCore v4.0 ${component} component:

## Component: ${spec.description}

### Mathematical Patterns
${spec.patterns.map(p => `- ${p}`).join('\n')}

### Requirements
- ${spec.requirements}
- TypeScript tier performance: ${options.performanceTarget ?? '< 100μs operations'}
- Production-ready error handling
- Comprehensive edge case coverage

### File Structure
- Implementation: \`${outputPath}\`
${options.includeTests !== false ? `- Tests: \`${testPath}\`` : ''}

### Implementation Guidelines
1. Use fp-ts for Either monad (Result component)
2. Use winston for logging (Logger component)  
3. Apply category theory thinking at implementation level
4. Verify mathematical laws through property tests
5. Include comprehensive edge cases and error scenarios
6. Meet TypeScript performance tier requirements

### Documentation Requirements
- JSDoc comments for all public APIs
- Usage examples in comments
- Performance characteristics documented
- Mathematical properties explained

Please implement with production quality and comprehensive test coverage.`;

  if (options.mathematicalPatterns) {
    instruction += `\n\n### Additional Mathematical Patterns\n${options.mathematicalPatterns.map(p => `- ${p}`).join('\n')}`;
  }

  return {
    workingDir: '/home/zzhang/dev/qi/github/mcp-server/qicore-v4/typescript',
    knowledgeUpdate: [
      'QiCore v4.0 architecture patterns',
      `Modern TypeScript ${component} implementation techniques`,
      'Mathematical correctness verification patterns',
      'Performance optimization for TypeScript tier'
    ],
    actionInstruction: instruction,
    qualityStandards: [
      'Production-ready TypeScript with strict mode',
      'Comprehensive test coverage and edge cases',
      'Mathematical law compliance where applicable',
      'Performance meets TypeScript tier requirements',
      'Integration with existing QiCore patterns'
    ],
    validationCriteria: [
      'Code passes all lint and type checks',
      'Tests achieve comprehensive coverage',
      'Performance benchmarks meet targets',
      'Documentation includes usage examples',
      'Integration with Result<T> and QiError works seamlessly'
    ],
    context: {
      component,
      outputPath,
      includeTests: options.includeTests !== false,
      performanceTarget: options.performanceTarget ?? '< 100μs operations'
    }
  };
}

/**
 * Create instruction for QiCore integration testing
 */
export function createQiCoreIntegrationInstruction(
  components: string[],
  scenario: string,
  options: {
    testPath?: string;
    performanceRequirements?: Record<string, string>;
  } = {}
): ClaudeInstruction {
  const testPath = options.testPath ?? `tests/qicore/integration/${scenario}.test.ts`;

  const instruction = `Create QiCore integration test for ${scenario}:

## Integration Scenario
Test interaction between: ${components.join(', ')}

## Test Requirements
1. **Component Integration**
   - Test how components work together
   - Verify data flow between components
   - Test error propagation through component chain

2. **Performance Integration**
   - Measure end-to-end performance
   - Verify no performance degradation in composition
   - Test under concurrent load

3. **Error Scenarios**
   - Test graceful failure handling
   - Verify error context preservation
   - Test recovery patterns

4. **Edge Cases**
   - Test boundary conditions
   - Test resource exhaustion scenarios
   - Test concurrent access patterns

## Test Structure
- File: \`${testPath}\`
- Use vitest testing framework
- Include performance benchmarks
- Cover success and failure paths
- Test mathematical properties end-to-end

${options.performanceRequirements ? 
  `## Performance Requirements\n${Object.entries(options.performanceRequirements).map(([op, req]) => `- ${op}: ${req}`).join('\n')}` 
  : ''}

Please create comprehensive integration tests with realistic scenarios.`;

  return {
    workingDir: '/home/zzhang/dev/qi/github/mcp-server/qicore-v4/typescript',
    knowledgeUpdate: [
      'QiCore integration testing patterns',
      'Vitest advanced testing techniques',
      'Performance testing and benchmarking',
      'Component composition testing strategies'
    ],
    actionInstruction: instruction,
    qualityStandards: [
      'Comprehensive integration test coverage',
      'Performance benchmarks and validation',
      'Realistic test scenarios and edge cases',
      'Clear test documentation and reporting'
    ],
    validationCriteria: [
      'All integration tests pass successfully',
      'Performance requirements are met',
      'Test coverage includes error scenarios',
      'Tests demonstrate component interoperability'
    ],
    context: {
      components,
      scenario,
      testPath,
      performanceRequirements: options.performanceRequirements
    }
  };
}

/**
 * Create instruction for QiAgent implementation
 */
export function createQiAgentInstruction(
  agentType: 'claude-code' | 'openai' | 'local',
  options: {
    reliabilityPatterns?: string[];
    outputPath?: string;
    includeMetrics?: boolean;
  } = {}
): ClaudeInstruction {
  const outputPath = options.outputPath ?? 'src/qiagent/index.ts';
  const reliabilityPatterns = options.reliabilityPatterns ?? [
    'Circuit Breaker',
    'Rate Limiting', 
    'Retry with Exponential Backoff',
    'Request/Response Metrics'
  ];

  const instruction = `Implement QiAgent for ${agentType} provider:

## Agent Requirements
1. **Provider Integration**
   - ${agentType === 'claude-code' ? 'Claude Code CLI and SDK integration' : ''}
   - ${agentType === 'openai' ? 'OpenAI API integration with function calling' : ''}
   - ${agentType === 'local' ? 'Local model integration with ollama/llamacpp' : ''}

2. **Reliability Patterns**
${reliabilityPatterns.map(pattern => `   - ${pattern}`).join('\n')}

3. **QiCore Integration**
   - Use Result<T> for all operations
   - Structured error handling with QiError
   - Performance monitoring for all requests
   - Configuration management with QiCore Config

4. **Agent Capabilities**
   - Text generation with model parameters
   - Multi-turn conversations with context
   - Function calling (where supported)
   - Streaming responses
   - Request validation and sanitization

## Implementation Structure
- File: \`${outputPath}\`
- Factory functions for agent creation
- Configuration validation
- Comprehensive error handling
- Performance metrics collection
${options.includeMetrics !== false ? '- Built-in metrics and health monitoring' : ''}

## Testing Requirements
- Unit tests for each reliability pattern
- Integration tests with mock providers
- Performance benchmarks
- Error scenario testing
- Concurrent request handling

Please implement with production-grade reliability and comprehensive observability.`;

  return {
    workingDir: '/home/zzhang/dev/qi/github/mcp-server/qicore-v4/typescript',
    knowledgeUpdate: [
      `${agentType} provider integration patterns`,
      'Production reliability patterns and circuit breakers',
      'Agent observability and metrics collection',
      'Multi-provider agent architecture'
    ],
    actionInstruction: instruction,
    qualityStandards: [
      'Production-grade reliability implementation',
      'Comprehensive error handling and recovery',
      'Performance monitoring and metrics',
      'Thread-safe concurrent operations'
    ],
    validationCriteria: [
      'All reliability patterns work under load',
      'Error scenarios are handled gracefully',
      'Performance metrics are collected accurately',
      'Integration with QiCore components is seamless'
    ],
    context: {
      agentType,
      reliabilityPatterns,
      outputPath,
      includeMetrics: options.includeMetrics !== false
    }
  };
}

/**
 * Create instruction for complete QiCore + QiAgent system
 */
export function createQiSystemInstruction(
  options: {
    includeDocumentation?: boolean;
    includeExamples?: boolean;
    performanceTargets?: Record<string, string>;
  } = {}
): ClaudeInstruction {
  const instruction = `Implement complete QiCore v4.0 + QiAgent system:

## System Requirements
1. **QiCore Base Components** (ALL 6 components)
   - Error: Context chaining and structured errors
   - Result: Either monad with composition patterns  
   - Config: Monoid merge with environment loading
   - Logger: Production logging with transports
   - Cache: LRU + TTL memory cache
   - Performance: Measurement and benchmarking

2. **QiAgent Integration**
   - Claude Code agent with reliability patterns
   - OpenAI agent with function calling
   - Multi-provider support with unified interface

3. **Integration Patterns**
   - Component composition through Result<T>
   - Error recovery and circuit breaking
   - Performance monitoring across all operations
   - Configuration-driven behavior

4. **Quality Requirements**
   - 85%+ test coverage with 300+ comprehensive tests
   - Zero linting errors with modern TypeScript
   - Mathematical correctness verified through property tests
   - Performance compliance: ${JSON.stringify(options.performanceTargets ?? {
     'result_operations': '< 100μs',
     'config_merge': '< 1ms', 
     'cache_operations': '< 50μs',
     'logger_level_check': '< 1μs'
   }, null, 2)}

## Implementation Strategy
1. Implement base components first (Error, Result)
2. Build core components (Config, Logger, Cache, Performance)
3. Create QiAgent with reliability patterns
4. Add integration patterns and composition
5. Create comprehensive tests for ALL components
6. Add performance benchmarks and validation

## File Structure
\`\`\`
src/
├── qicore/
│   ├── base/
│   │   ├── error.ts
│   │   └── result.ts
│   └── core/
│       ├── config.ts
│       ├── logger.ts
│       ├── cache.ts
│       └── performance.ts
└── qiagent/
    └── index.ts

tests/
├── qicore/
│   ├── base/
│   ├── core/
│   ├── integration/
│   └── performance/
└── qiagent/
    ├── unit/
    ├── integration/
    └── reliability/
\`\`\`

${options.includeDocumentation !== false ? `
## Documentation Requirements
- API documentation for all components
- Usage examples and tutorials
- Performance characteristics
- Mathematical properties explained
- Integration patterns documented
` : ''}

${options.includeExamples !== false ? `
## Example Usage
- Complete application examples
- Integration with existing codebases
- Performance optimization examples
- Error handling patterns
` : ''}

Please implement the complete system with production quality and comprehensive testing.`;

  return {
    workingDir: '/home/zzhang/dev/qi/github/mcp-server/qicore-v4/typescript',
    knowledgeUpdate: [
      'Complete QiCore v4.0 system architecture',
      'TypeScript monorepo management patterns',
      'End-to-end integration testing strategies',
      'Production deployment and monitoring'
    ],
    actionInstruction: instruction,
    qualityStandards: [
      '85%+ test coverage across all components',
      'Zero linting errors with modern TypeScript',
      'Mathematical correctness verified through property tests',
      'Performance compliance across all operations',
      'Production-ready error handling and observability'
    ],
    validationCriteria: [
      'All 6 QiCore components implement and test successfully',
      'QiAgent integration provides reliable multi-provider support',
      'End-to-end system tests demonstrate full functionality',
      'Performance benchmarks meet all specified targets',
      'Documentation covers all APIs with usage examples'
    ],
    context: {
      includeDocumentation: options.includeDocumentation !== false,
      includeExamples: options.includeExamples !== false,
      performanceTargets: options.performanceTargets
    }
  };
}