import { describe, it, expect } from 'vitest';
import { ClaudeCliManager } from '@/managers/cli-manager';
import { ExecutionError } from '@/types';

describe('ClaudeCliManager (Integration)', () => {
  describe('Constructor', () => {
    it('should create manager with valid options', () => {
      const manager = new ClaudeCliManager({
        workingDir: '/test/project',
        outputFormat: 'json',
        model: 'sonnet',
        maxTurns: 5,
        verbose: true,
      });
      
      expect(manager).toBeInstanceOf(ClaudeCliManager);
    });

    it('should throw error without working directory', () => {
      expect(() => {
        new ClaudeCliManager({} as any);
      }).toThrow(ExecutionError);
    });
  });

  describe('Flag Building', () => {
    it('should build minimal flags', () => {
      const manager = new ClaudeCliManager({
        workingDir: '/test/project',
      });

      // Access private method for testing
      const flags = (manager as any).buildFlags();
      expect(flags).toBe('');
    });

    it('should build all flags', () => {
      const manager = new ClaudeCliManager({
        workingDir: '/test/project',
        outputFormat: 'stream-json',
        model: 'haiku',
        maxTurns: 3,
        verbose: true,
        addDirs: ['/path1', '/path2'],
      });

      const flags = (manager as any).buildFlags();
      expect(flags).toContain('--output-format stream-json');
      expect(flags).toContain('--model haiku');
      expect(flags).toContain('--max-turns 3');
      expect(flags).toContain('--verbose');
      expect(flags).toContain('--add-dir /path1 /path2');
    });
  });

  describe('Instruction Escaping', () => {
    it('should escape all special characters', () => {
      const manager = new ClaudeCliManager({
        workingDir: '/test/project',
      });
      
      const instruction = 'Test "quotes" $vars `backticks` \\backslashes\nNewlines\rCarriage returns';
      const escaped = (manager as any).escapeInstruction(instruction);

      expect(escaped).toContain('\\"quotes\\"');
      expect(escaped).toContain('\\$vars');
      expect(escaped).toContain('\\`backticks\\`');
      expect(escaped).toContain('\\\\backslashes');
      expect(escaped).toContain('\\n');
      expect(escaped).toContain('\\r');
    });

    it('should handle empty instruction', () => {
      const manager = new ClaudeCliManager({
        workingDir: '/test/project',
      });
      
      const escaped = (manager as any).escapeInstruction('');
      expect(escaped).toBe('');
    });

    it('should handle instruction with no special characters', () => {
      const manager = new ClaudeCliManager({
        workingDir: '/test/project',
      });
      
      const instruction = 'Simple instruction without special characters';
      const escaped = (manager as any).escapeInstruction(instruction);
      expect(escaped).toBe(instruction);
    });
  });

  describe('Response Parsing', () => {
    it('should parse valid JSON', () => {
      const manager = new ClaudeCliManager({
        workingDir: '/test/project',
        outputFormat: 'json',
      });
      
      const validJson = '{"success": true, "data": "test"}';
      const parsed = (manager as any).parseResponse(validJson);

      expect(parsed).toEqual({ success: true, data: 'test' });
    });

    it('should return text for invalid JSON', () => {
      const manager = new ClaudeCliManager({
        workingDir: '/test/project',
        outputFormat: 'json',
      });
      
      const invalidJson = 'Not valid JSON';
      const parsed = (manager as any).parseResponse(invalidJson);

      expect(parsed).toBe('Not valid JSON');
    });

    it('should handle text format', () => {
      const manager = new ClaudeCliManager({
        workingDir: '/test/project',
        outputFormat: 'text',
      });

      const text = 'Plain text response';
      const parsed = (manager as any).parseResponse(text);

      expect(parsed).toBe('Plain text response');
    });
  });

  describe('Real CLI Integration (if available)', () => {
    it('should attempt to check CLI status', async () => {
      const manager = new ClaudeCliManager({
        workingDir: process.cwd(),
        verbose: false, // Reduce noise in test output
      });

      // This will work if Claude CLI is available, otherwise gracefully fail
      const status = await manager.getStatus();
      
      expect(status).toBeDefined();
      expect(status.workingDir).toBe(process.cwd());
      expect(typeof status.available).toBe('boolean');
    });
  });
});