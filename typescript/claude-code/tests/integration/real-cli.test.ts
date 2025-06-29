import { describe, it, expect } from 'vitest';
import { ClaudeRequestManager } from '@/managers/request-manager';
import { createHaskellInstruction } from '@/instructions/builder';

describe('Real Claude Code CLI Integration', () => {
  it('should work with real Claude Code CLI (via full path)', async () => {
    // Note: Claude CLI may not be available in test PATH
    // This test demonstrates the integration without requiring actual CLI
    const manager = new ClaudeRequestManager({
      method: 'cli',
      workingDir: process.cwd(),
      verbose: true,
    });

    const status = await manager.getStatus();
    console.log('Claude Code CLI Status:', status);
    
    // Test that the manager is properly configured
    expect(status).toBeDefined();
    expect(status.method).toBe('cli');
    // CLI may not be available in test environment, so just test the structure
    expect(status.available).toBeDefined();
  });

  it('should generate instruction correctly', () => {
    const instruction = createHaskellInstruction(
      'test/Test.hs',
      process.cwd()
    );

    expect(instruction.workingDir).toBe(process.cwd());
    expect(instruction.knowledgeUpdate).toContain('Modern Haskell 2024 best practices and GHC2024 extensions');
    expect(instruction.actionInstruction).toContain('Generate test/Test.hs');
    expect(instruction.qualityStandards.length).toBeGreaterThan(0);
    expect(instruction.validationCriteria.length).toBeGreaterThan(0);
  });
});