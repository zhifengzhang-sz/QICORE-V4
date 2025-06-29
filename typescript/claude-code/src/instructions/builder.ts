import type { ClaudeInstruction } from '@/types';

/**
 * Build a comprehensive instruction for Claude Code
 */
export function buildInstruction(config: ClaudeInstruction): string {
  const instruction = `
# QiCore-Quality Code Generation Task

## Working Directory
${config.workingDir}

## Knowledge Update Requirements (CRITICAL - Execute First)
${config.knowledgeUpdate.map((topic) => `- Research and update knowledge on: ${topic}`).join('\n')}

## Action Instructions
${config.actionInstruction}

## Quality Standards to Match
${config.qualityStandards.map((standard) => `- ${standard}`).join('\n')}

## Validation Criteria
${config.validationCriteria.map((criteria) => `- [ ] ${criteria}`).join('\n')}

${
  config.context
    ? `## Additional Context
${Object.entries(config.context)
  .map(([key, value]) => `- ${key}: ${JSON.stringify(value)}`)
  .join('\n')}`
    : ''
}

## Success Criteria
Generate production-ready code that matches the existing QiCore implementation quality standards.
All validation criteria must be met for successful completion.

## Response Format
Please provide:
1. Summary of knowledge updates performed
2. Implementation approach and decisions
3. Generated code with explanations
4. Validation of quality standards compliance
5. Any additional recommendations or considerations
`;

  return instruction.trim();
}

/**
 * Create instruction specifically for Haskell code generation
 */
export function createHaskellInstruction(
  targetFile: string,
  baseDir = '/home/zzhang/dev/qi/github/mcp-server/qicore-v4'
): ClaudeInstruction {
  return {
    workingDir: baseDir,
    knowledgeUpdate: [
      'Modern Haskell 2024 best practices and GHC2024 extensions',
      'Haskell performance optimization techniques for production code',
      'Modern Nix-Haskell integration patterns with flakes',
      'Property-based testing with QuickCheck for mathematical law verification',
      'Advanced type-level programming and deriving strategies',
    ],
    actionInstruction: `
Examine the following files to understand QiCore quality standards:
1. docs/experiment/sources/nl/base.contracts.md - Component contracts and requirements
2. haskell/QiCore/Base/Result.hs - Result type implementation patterns
3. haskell/QiCore/Base/Error.hs - Error handling patterns
4. haskell/qicore-base.cabal - Build configuration and extensions
5. docs/sources/guides/design.prompt.md - Design patterns and principles

Generate ${targetFile} that exactly matches the quality of existing components:
- Use the same code style, language extensions, and architectural patterns
- Implement comprehensive API with 3x more functions than minimum requirements
- Include rich Haddock documentation with usage examples
- Ensure mathematical law compliance (Monad/Functor/Applicative where applicable)
- Integrate seamlessly with Result<T> and QiError types
- Apply strict performance annotations and optimization patterns
- Follow the exact same module structure and export patterns

For Cache implementation specifically:
- Support both memory and persistent storage backends
- Implement TTL (time-to-live) expiration with lazy checking
- Provide LRU eviction policies with configurable capacity
- Ensure thread-safe operations with appropriate STM usage
- Create atomic operations like getOrSet for race-free updates
- Include comprehensive error handling with QiError integration
`,
    qualityStandards: [
      'GHC2024 language edition with modern extensions (LambdaCase, ViewPatterns, etc.)',
      'Strict performance annotations (!fields) on all data types',
      'Comprehensive warning flags (-Wall -Wcompat -Widentities etc.)',
      'Rich Haddock documentation with examples and usage patterns',
      'Mathematical law compliance verified with property-based tests',
      'Seamless integration with existing QiCore error handling',
      'Modern deriving strategies (deriving stock, deriving anyclass)',
      'Efficient import patterns with qualified imports',
      'Thread-safe concurrent operations where applicable',
    ],
    validationCriteria: [
      'Code style matches existing QiCore base components exactly',
      'Implements comprehensive API beyond minimum contract requirements',
      'Includes property-based tests for all mathematical laws',
      'Provides rich documentation with practical usage examples',
      'Integrates seamlessly with Result<T> and QiError without friction',
      'Passes all linting, type checking, and warning-free compilation',
      'Demonstrates modern Haskell idioms and performance optimizations',
      'Module exports follow QiCore patterns with proper re-exports',
      'Error handling covers all failure modes with appropriate categories',
      'Concurrent operations are thread-safe and deadlock-free',
    ],
    context: {
      targetFile,
      componentType: 'Cache',
      existingBaseFiles: [
        'haskell/QiCore/Base/Result.hs',
        'haskell/QiCore/Base/Error.hs',
        'haskell/QiCore/Base.hs',
      ],
      requiredIntegrations: ['Result<T>', 'QiError', 'STM', 'Concurrent operations'],
      performanceTargets: 'Functional tier: < 50 microseconds per operation',
    },
  };
}

/**
 * Create instruction for TypeScript code generation
 */
export function createTypeScriptInstruction(
  targetFile: string,
  baseDir = '/home/zzhang/dev/qi/github/mcp-server/qicore-v4'
): ClaudeInstruction {
  return {
    workingDir: baseDir,
    knowledgeUpdate: [
      'Modern TypeScript 5.3+ features and best practices',
      'Advanced type-level programming patterns',
      'Performance optimization for Node.js/Browser environments',
      'Modern testing patterns with Vitest and property-based testing',
    ],
    actionInstruction: `
Examine the existing TypeScript QiCore implementation:
1. typescript/src/qicore/base/ - Base component patterns
2. typescript/src/qicore/core/ - Core component patterns  
3. typescript/package.json - Build configuration
4. typescript/tsconfig.json - TypeScript configuration

Generate ${targetFile} following QiCore TypeScript patterns:
- Use modern TypeScript features and strict type checking
- Implement comprehensive APIs with builder patterns
- Include comprehensive JSDoc documentation
- Ensure type safety and runtime validation with Zod
- Follow functional programming patterns with immutability
- Integrate with existing Result<T> and QiError patterns
`,
    qualityStandards: [
      'Strict TypeScript configuration with no any types',
      'Comprehensive JSDoc documentation with examples',
      'Runtime validation with Zod schemas',
      'Functional programming patterns with immutability',
      'Modern ES2022+ features and clean code practices',
      'Integration with existing QiCore error handling patterns',
    ],
    validationCriteria: [
      'Code passes strict TypeScript compilation without errors',
      'Comprehensive test coverage with Vitest',
      'Runtime validation schemas match TypeScript types',
      'Documentation includes usage examples and API reference',
      'Integration with Result<T> and QiError works seamlessly',
      'Performance meets interpreted tier targets (< 100μs per operation)',
    ],
    context: {
      targetFile,
      language: 'TypeScript',
      existingPatterns: ['Result<T>', 'QiError', 'Functional composition'],
      buildSystem: 'Vitest + TypeScript',
    },
  };
}

/**
 * Create instruction for general code analysis and improvement
 */
export function createAnalysisInstruction(
  analysisTarget: string,
  baseDir = '/home/zzhang/dev/qi/github/mcp-server/qicore-v4'
): ClaudeInstruction {
  return {
    workingDir: baseDir,
    knowledgeUpdate: [
      'Modern software architecture patterns and best practices',
      'Code quality metrics and analysis techniques',
      'Performance optimization strategies',
      'Testing strategies and quality assurance',
    ],
    actionInstruction: `
Analyze ${analysisTarget} and provide comprehensive assessment:
1. Code quality and adherence to best practices
2. Performance characteristics and optimization opportunities
3. Test coverage and testing strategy
4. Documentation quality and completeness
5. Integration patterns and architectural decisions
6. Recommendations for improvements

Compare against QiCore quality standards and provide specific, actionable recommendations.
`,
    qualityStandards: [
      'Comprehensive analysis covering all aspects of code quality',
      'Specific, actionable recommendations with examples',
      'Performance analysis with concrete metrics',
      'Testing strategy evaluation with coverage assessment',
      'Documentation review with improvement suggestions',
    ],
    validationCriteria: [
      'Analysis covers all requested aspects thoroughly',
      'Recommendations are specific and actionable',
      'Performance suggestions include concrete optimizations',
      'Testing recommendations improve quality assurance',
      'Documentation improvements enhance usability',
    ],
    context: {
      analysisTarget,
      comparisonStandard: 'QiCore quality benchmarks',
      focusAreas: ['Quality', 'Performance', 'Testing', 'Documentation'],
    },
  };
}
