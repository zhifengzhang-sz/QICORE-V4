import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { ClaudeRequestManager } from '@/managers/request-manager';
import { ValidationError, ExecutionError } from '@/types';

// Mock the managers with proper structure
vi.mock('@/managers/cli-manager', () => ({
  ClaudeCliManager: vi.fn(),
}));
vi.mock('@/managers/sdk-manager', () => ({
  ClaudeSdkManager: vi.fn(),
}));

// Import the mocked classes
import { ClaudeCliManager } from '@/managers/cli-manager';
import { ClaudeSdkManager } from '@/managers/sdk-manager';

const MockClaudeCliManager = vi.mocked(ClaudeCliManager);
const MockClaudeSdkManager = vi.mocked(ClaudeSdkManager);

describe('ClaudeRequestManager', () => {
  let manager: ClaudeRequestManager;
  let mockCliManager: any;
  let mockSdkManager: any;

  beforeEach(() => {
    vi.clearAllMocks();
    
    // Setup mock CLI manager
    mockCliManager = {
      executeQuery: vi.fn(),
      getStatus: vi.fn(),
      testConnection: vi.fn(),
    };
    
    // Setup mock SDK manager
    mockSdkManager = {
      executeQuery: vi.fn(),
      getStatus: vi.fn(),
      testConnection: vi.fn(),
    };

    MockClaudeCliManager.mockImplementation(() => mockCliManager);
    MockClaudeSdkManager.mockImplementation(() => mockSdkManager);
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('Constructor and Configuration', () => {
    it('should create CLI manager with valid config', () => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
        outputFormat: 'json',
        model: 'sonnet',
        maxTurns: 5,
        verbose: true,
      });

      expect(MockClaudeCliManager).toHaveBeenCalledWith({
        workingDir: '/test/project',
        outputFormat: 'json',
        model: 'sonnet',
        maxTurns: 5,
        verbose: true,
      });
    });

    it('should create SDK manager with valid config', () => {
      manager = new ClaudeRequestManager({
        method: 'sdk',
        workingDir: '/test/project',
        maxTurns: 10,
        systemPrompt: 'Test prompt',
        allowedTools: ['Read', 'Write'],
        verbose: false,
      });

      expect(MockClaudeSdkManager).toHaveBeenCalledWith({
        workingDir: '/test/project',
        maxTurns: 10,
        systemPrompt: 'Test prompt',
        allowedTools: ['Read', 'Write'],
        verbose: false,
      });
    });

    it('should throw validation error for missing working directory', () => {
      expect(() => {
        new ClaudeRequestManager({
          method: 'cli',
        } as any);
      }).toThrow(ValidationError);
    });

    it('should throw validation error for invalid method', () => {
      expect(() => {
        new ClaudeRequestManager({
          method: 'invalid' as any,
          workingDir: '/test/project',
        });
      }).toThrow(ValidationError);
    });

    it('should throw validation error for negative maxTurns', () => {
      expect(() => {
        new ClaudeRequestManager({
          method: 'cli',
          workingDir: '/test/project',
          maxTurns: -1,
        });
      }).toThrow(ValidationError);
    });
  });

  describe('executeInstruction', () => {
    beforeEach(() => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
        verbose: true,
      });
    });

    it('should execute instruction successfully with CLI', async () => {
      const instruction = global.testUtils.createMockInstruction();
      const mockResponse = global.testUtils.createMockClaudeResponse();
      
      mockCliManager.executeQuery.mockResolvedValue(mockResponse);

      const result = await manager.executeInstruction(instruction);

      expect(result.instruction).toEqual(instruction);
      expect(result.response).toEqual(mockResponse);
      expect(result.method).toBe('cli');
      expect(result.executionTime).toBeGreaterThanOrEqual(0);
      expect(result.timestamp).toBeDefined();
    });

    it('should execute instruction successfully with SDK', async () => {
      manager = new ClaudeRequestManager({
        method: 'sdk',
        workingDir: '/test/project',
      });

      const instruction = global.testUtils.createMockInstruction();
      const mockResponse = global.testUtils.createMockClaudeResponse();
      
      mockSdkManager.executeQuery.mockResolvedValue(mockResponse);

      const result = await manager.executeInstruction(instruction);

      expect(result.method).toBe('sdk');
      expect(result.response).toEqual(mockResponse);
    });

    it('should handle execution errors gracefully', async () => {
      const instruction = global.testUtils.createMockInstruction();
      const executionError = new Error('CLI execution failed');
      
      mockCliManager.executeQuery.mockRejectedValue(executionError);

      const result = await manager.executeInstruction(instruction);

      expect(result.response.success).toBe(false);
      expect(result.response.error).toBe('CLI execution failed');
      expect(result.response.metadata?.failed).toBe(true);
    });

    it('should validate instruction before execution', async () => {
      const invalidInstruction = {
        workingDir: '', // Invalid: empty
        knowledgeUpdate: [],
        actionInstruction: '', // Invalid: empty
        qualityStandards: [],
        validationCriteria: [],
      };

      const result = await manager.executeInstruction(invalidInstruction as any);

      expect(result.response.success).toBe(false);
      expect(result.response.error).toContain('working directory');
    });

    it('should validate knowledge updates are provided', async () => {
      const invalidInstruction = global.testUtils.createMockInstruction({
        knowledgeUpdate: [], // Invalid: empty array
      });

      const result = await manager.executeInstruction(invalidInstruction);

      expect(result.response.success).toBe(false);
      expect(result.response.error).toContain('Knowledge update requirements');
    });

    it('should throw error for invalid method configuration', async () => {
      // Create manager with CLI but then manually break the configuration
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
      });
      
      // Force invalid state by manually setting method without proper manager
      (manager as any).config.method = 'invalid';
      (manager as any).cliManager = undefined;

      const instruction = global.testUtils.createMockInstruction();

      const result = await manager.executeInstruction(instruction);

      expect(result.response.success).toBe(false);
      expect(result.response.error).toContain('Invalid method configuration');
    });
  });

  describe('executeInstructionSequence', () => {
    beforeEach(() => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
      });
    });

    it('should execute all instructions in sequence when all succeed', async () => {
      const instructions = [
        global.testUtils.createMockInstruction({ actionInstruction: 'Instruction 1' }),
        global.testUtils.createMockInstruction({ actionInstruction: 'Instruction 2' }),
        global.testUtils.createMockInstruction({ actionInstruction: 'Instruction 3' }),
      ];

      mockCliManager.executeQuery.mockResolvedValue(global.testUtils.createMockClaudeResponse());

      const results = await manager.executeInstructionSequence(instructions);

      expect(results).toHaveLength(3);
      expect(mockCliManager.executeQuery).toHaveBeenCalledTimes(3);
      expect(results.every(r => r.response.success)).toBe(true);
    });

    it('should stop sequence on first failure', async () => {
      const instructions = [
        global.testUtils.createMockInstruction({ actionInstruction: 'Instruction 1' }),
        global.testUtils.createMockInstruction({ actionInstruction: 'Instruction 2' }),
        global.testUtils.createMockInstruction({ actionInstruction: 'Instruction 3' }),
      ];

      mockCliManager.executeQuery
        .mockResolvedValueOnce(global.testUtils.createMockClaudeResponse())
        .mockResolvedValueOnce(global.testUtils.createMockClaudeResponse({ success: false, error: 'Failed' }))
        .mockResolvedValueOnce(global.testUtils.createMockClaudeResponse());

      const results = await manager.executeInstructionSequence(instructions);

      expect(results).toHaveLength(2); // Should stop after second failure
      expect(mockCliManager.executeQuery).toHaveBeenCalledTimes(2);
      expect(results[0].response.success).toBe(true);
      expect(results[1].response.success).toBe(false);
    });
  });

  describe('executeInstructionBatch', () => {
    beforeEach(() => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
      });
    });

    it('should execute all instructions in parallel', async () => {
      const instructions = [
        global.testUtils.createMockInstruction({ actionInstruction: 'Instruction 1' }),
        global.testUtils.createMockInstruction({ actionInstruction: 'Instruction 2' }),
      ];

      mockCliManager.executeQuery.mockResolvedValue(global.testUtils.createMockClaudeResponse());

      const results = await manager.executeInstructionBatch(instructions);

      expect(results).toHaveLength(2);
      expect(mockCliManager.executeQuery).toHaveBeenCalledTimes(2);
    });

    it('should handle batch execution errors', async () => {
      const instructions = [
        global.testUtils.createMockInstruction(),
      ];

      mockCliManager.executeQuery.mockRejectedValue(new Error('Batch failed'));

      const results = await manager.executeInstructionBatch(instructions);
      
      expect(results).toHaveLength(1);
      expect(results[0].response.success).toBe(false);
      expect(results[0].response.error).toBe('Batch failed');
    });
  });

  describe('getStatus', () => {
    it('should return CLI status when available', async () => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
      });

      const mockStatus = {
        available: true,
        version: 'v1.0.0',
        workingDir: '/test/project',
        configuration: {},
      };

      mockCliManager.getStatus.mockResolvedValue(mockStatus);

      const status = await manager.getStatus();

      expect(status.method).toBe('cli');
      expect(status.available).toBe(true);
      expect(status.managerStatus).toEqual(mockStatus);
    });

    it('should return SDK status when available', async () => {
      manager = new ClaudeRequestManager({
        method: 'sdk',
        workingDir: '/test/project',
      });

      const mockStatus = {
        available: true,
        workingDir: '/test/project',
        configuration: {},
        sdkVersion: 'v2.0.0',
      };

      mockSdkManager.getStatus.mockResolvedValue(mockStatus);

      const status = await manager.getStatus();

      expect(status.method).toBe('sdk');
      expect(status.available).toBe(true);
      expect(status.managerStatus).toEqual(mockStatus);
    });

    it('should handle status check errors', async () => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
      });

      mockCliManager.getStatus.mockRejectedValue(new Error('Status check failed'));

      const status = await manager.getStatus();

      expect(status.available).toBe(false);
      expect(status.managerStatus).toBeUndefined();
    });
  });

  describe('switchMethod', () => {
    it('should switch from CLI to SDK', () => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
      });

      MockClaudeCliManager.mockClear();
      MockClaudeSdkManager.mockClear();

      manager.switchMethod('sdk');

      expect(MockClaudeSdkManager).toHaveBeenCalled();
      expect(manager.getConfig().method).toBe('sdk');
    });

    it('should switch from SDK to CLI', () => {
      manager = new ClaudeRequestManager({
        method: 'sdk',
        workingDir: '/test/project',
      });

      MockClaudeCliManager.mockClear();
      MockClaudeSdkManager.mockClear();

      manager.switchMethod('cli');

      expect(MockClaudeCliManager).toHaveBeenCalled();
      expect(manager.getConfig().method).toBe('cli');
    });
  });

  describe('updateConfig', () => {
    it('should update configuration successfully', () => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
      });

      manager.updateConfig({
        model: 'opus',
        maxTurns: 15,
        verbose: true,
      });

      const config = manager.getConfig();
      expect(config.model).toBe('opus');
      expect(config.maxTurns).toBe(15);
      expect(config.verbose).toBe(true);
    });

    it('should validate updated configuration', () => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
      });

      expect(() => {
        manager.updateConfig({
          maxTurns: -5, // Invalid
        });
      }).toThrow(ValidationError);
    });

    it('should reinitialize managers after config update', () => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
      });

      MockClaudeCliManager.mockClear();

      manager.updateConfig({
        model: 'opus',
      });

      // Should create new CLI manager with updated config
      expect(MockClaudeCliManager).toHaveBeenCalledWith(
        expect.objectContaining({
          model: 'opus',
        })
      );
    });
  });

  describe('getConfig', () => {
    it('should return current configuration', () => {
      const initialConfig = {
        method: 'cli' as const,
        workingDir: '/test/project',
        model: 'sonnet' as const,
        verbose: true,
      };

      manager = new ClaudeRequestManager(initialConfig);

      const config = manager.getConfig();
      expect(config).toEqual(initialConfig);
    });

    it('should return a copy of configuration', () => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
      });

      const config1 = manager.getConfig();
      const config2 = manager.getConfig();

      expect(config1).toEqual(config2);
      expect(config1).not.toBe(config2); // Should be different objects
    });
  });

  describe('Private Methods', () => {
    beforeEach(() => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: '/test/project',
      });
    });

    it('should validate instruction working directory', async () => {
      const invalidInstruction = global.testUtils.createMockInstruction({
        workingDir: '', // Invalid
      });

      const result = await manager.executeInstruction(invalidInstruction);
      expect(result.response.success).toBe(false);
      expect(result.response.error).toContain('working directory');
    });

    it('should validate action instruction', async () => {
      const invalidInstruction = global.testUtils.createMockInstruction({
        actionInstruction: '   ', // Invalid: whitespace only
      });

      const result = await manager.executeInstruction(invalidInstruction);
      expect(result.response.success).toBe(false);
      expect(result.response.error).toContain('Action instruction');
    });

    it('should validate knowledge update array', async () => {
      const invalidInstruction = global.testUtils.createMockInstruction({
        knowledgeUpdate: [], // Invalid: empty
      });

      const result = await manager.executeInstruction(invalidInstruction);
      expect(result.response.success).toBe(false);
      expect(result.response.error).toContain('Knowledge update');
    });
  });
});