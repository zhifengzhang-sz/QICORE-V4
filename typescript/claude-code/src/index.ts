/**
 * Claude Code Request Manager
 *
 * A TypeScript library for managing Claude Code CLI and SDK interactions
 * with comprehensive instruction building, execution, and validation.
 */

// Core managers
export { ClaudeRequestManager } from './managers/request-manager';
export { ClaudeCliManager } from './managers/cli-manager';
export { ClaudeSdkManager } from './managers/sdk-manager';

// Instruction builders
export {
  buildInstruction,
  createHaskellInstruction,
  createTypeScriptInstruction,
  createAnalysisInstruction,
} from './instructions/builder';

// Validation and testing
export { ResultValidator } from './tests/validator';

// Types and schemas
export * from './types';

// Test utilities
export { testHaskellGeneration, testMultipleScenarios } from './test-haskell-generation';

/**
 * Quick start example:
 *
 * ```typescript
 * import { ClaudeRequestManager, createHaskellInstruction } from '@qicore/claude-code-manager';
 *
 * const manager = new ClaudeRequestManager({
 *   method: 'cli',
 *   workingDir: '/path/to/project',
 *   model: 'sonnet',
 *   verbose: true
 * });
 *
 * const instruction = createHaskellInstruction('src/MyModule.hs');
 * const result = await manager.executeInstruction(instruction);
 *
 * console.log('Success:', result.response.success);
 * console.log('Content:', result.response.content);
 * ```
 */

/**
 * Default configuration for QiCore projects
 */
export const QICORE_DEFAULTS = {
  workingDir: '/home/zzhang/dev/qi/github/mcp-server/qicore-v4',
  model: 'sonnet' as const,
  maxTurns: 10,
  verbose: true,
  allowedTools: ['Read', 'Write', 'Edit', 'MultiEdit', 'Glob', 'Grep', 'Bash', 'LS'],
};

/**
 * Create a pre-configured manager for QiCore development
 */
export function createQiCoreManager(
  method: 'cli' | 'sdk' = 'cli',
  overrides: Partial<{
    workingDir: string;
    model: 'sonnet' | 'opus' | 'haiku';
    maxTurns: number;
    verbose: boolean;
  }> = {}
) {
  return new ClaudeRequestManager({
    method,
    ...QICORE_DEFAULTS,
    ...overrides,
  });
}
