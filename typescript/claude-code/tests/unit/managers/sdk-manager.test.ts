import { describe, it, expect } from 'vitest';
import { ClaudeSdkManager } from '@/managers/sdk-manager';
import { ExecutionError } from '@/types';

describe('ClaudeSdkManager (Integration)', () => {
  describe('Constructor', () => {
    it('should create manager with valid options', () => {
      const manager = new ClaudeSdkManager({
        workingDir: '/test/project',
        maxTurns: 10,
        systemPrompt: 'You are a helpful assistant',
        allowedTools: ['Read', 'Write', 'Edit'],
        verbose: true,
        permissionMode: 'strict',
      });
      
      expect(manager).toBeInstanceOf(ClaudeSdkManager);
    });

    it('should throw error without working directory', () => {
      expect(() => {
        new ClaudeSdkManager({} as any);
      }).toThrow(ExecutionError);
    });
  });

  describe('SDK Options Building', () => {
    it('should build complete options', () => {
      const manager = new ClaudeSdkManager({
        workingDir: '/test/project',
        maxTurns: 10,
        systemPrompt: 'You are a helpful assistant',
        allowedTools: ['Read', 'Write', 'Edit'],
        verbose: true,
        permissionMode: 'strict',
      });

      const options = (manager as any).buildSdkOptions();

      expect(options).toEqual({
        cwd: '/test/project',
        maxTurns: 10,
        systemPrompt: 'You are a helpful assistant',
        allowedTools: ['Read', 'Write', 'Edit'],
        permissionMode: 'strict',
      });
    });

    it('should use defaults for minimal configuration', () => {
      const manager = new ClaudeSdkManager({
        workingDir: '/test/project',
      });

      const options = (manager as any).buildSdkOptions();

      expect(options.cwd).toBe('/test/project');
      expect(options.maxTurns).toBe(10);
      expect(options.systemPrompt).toBeUndefined();
      expect(options.allowedTools).toEqual(['Read', 'Write', 'Edit', 'MultiEdit', 'Glob', 'Grep', 'Bash', 'LS']);
      expect(options.permissionMode).toBe('permissive');
    });
  });

  describe('SDK Integration (Real)', () => {
    it('should attempt to check SDK status', async () => {
      const manager = new ClaudeSdkManager({
        workingDir: process.cwd(),
        verbose: false, // Reduce noise in test output
      });

      // This will gracefully handle SDK availability
      const status = await manager.getStatus();
      
      expect(status).toBeDefined();
      expect(status.workingDir).toBe(process.cwd());
      expect(typeof status.available).toBe('boolean');
      expect(status.configuration).toBeDefined();
    });

    it('should handle SDK query gracefully', async () => {
      const manager = new ClaudeSdkManager({
        workingDir: process.cwd(),
        verbose: false,
      });

      // This should return an error response when SDK is not available
      const result = await manager.executeQuery('Test instruction');
      
      expect(result).toBeDefined();
      expect(typeof result.success).toBe('boolean');
      expect(result.metadata?.method).toBe('sdk');
      
      // If SDK is not available, should gracefully fail
      if (!result.success) {
        expect(result.error).toContain('SDK not yet available');
      }
    });

    it('should handle batch operations gracefully', async () => {
      const manager = new ClaudeSdkManager({
        workingDir: process.cwd(),
        verbose: false,
      });

      // Test empty batch
      const results = await manager.executeQueryBatch([]);
      expect(results).toEqual([]);
    });
  });
});