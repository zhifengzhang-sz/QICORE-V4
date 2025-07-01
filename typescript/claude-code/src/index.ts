/**
 * Claude Code Request Manager with QiCore Integration
 *
 * A comprehensive TypeScript library for managing Claude Code CLI and SDK interactions
 * with QiCore base components and QiAgent integration for production-grade AI workflows.
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
export { ResultValidator } from './types/tests/validator';

// Types and schemas
export * from './types';

// Test utilities
export { testHaskellGeneration, testMultipleScenarios } from './test-haskell-generation';

// QiCore Integration - Selective exports to avoid conflicts
export type { Result } from '@qi/core/base/result';
export { success, failure, isSuccess, isFailure } from '@qi/core/base/result';
export type { QiError } from '@qi/core/base/error';
export { createQiError } from '@qi/core/base/error';
export type { Logger } from '@qi/core/core/logger';
export { createDefault as createLogger } from '@qi/core/core/logger';

// QiAgent Integration - Main namespace
export { QiAgent } from '@qi/agent/index';

// Enhanced Managers with QiCore Integration
export { QiCoreRequestManager, createQiCoreClaudeManager } from './managers/qicore-request-manager';

// QiCore-specific Instruction Builders
export {
  createQiCoreImplementationInstruction,
  createQiCoreIntegrationInstruction,
  createQiAgentInstruction,
  createQiSystemInstruction
} from './instructions/qicore-instructions';

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
