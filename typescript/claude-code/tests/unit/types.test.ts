import { describe, it, expect } from 'vitest';
import {
  ClaudeResponseSchema,
  ClaudeInstructionSchema,
  ClaudeCliOptionsSchema,
  ClaudeSdkOptionsSchema,
  RequestManagerConfigSchema,
  ExecutionResultSchema,
  ValidationResultSchema,
  TestResultSchema,
  ClaudeCodeError,
  ValidationError,
  ExecutionError,
  type ClaudeResponse,
  type ClaudeInstruction,
} from '@/types';

describe('Type Schemas', () => {
  describe('ClaudeResponseSchema', () => {
    it('should validate correct response', () => {
      const validResponse = {
        success: true,
        content: 'Test content',
        error: null,
      };
      
      expect(() => ClaudeResponseSchema.parse(validResponse)).not.toThrow();
    });

    it('should accept optional fields', () => {
      const responseWithOptionals = {
        success: false,
        content: null,
        error: 'Test error',
        messages: ['message1', 'message2'],
        metadata: { executionTime: 1000 },
      };
      
      expect(() => ClaudeResponseSchema.parse(responseWithOptionals)).not.toThrow();
    });

    it('should reject invalid response', () => {
      const invalidResponse = {
        success: 'not a boolean',
        content: 123,
      };
      
      expect(() => ClaudeResponseSchema.parse(invalidResponse)).toThrow();
    });
  });

  describe('ClaudeInstructionSchema', () => {
    it('should validate complete instruction', () => {
      const validInstruction = {
        workingDir: '/test/path',
        knowledgeUpdate: ['Update 1', 'Update 2'],
        actionInstruction: 'Do something',
        qualityStandards: ['Standard 1'],
        validationCriteria: ['Criteria 1'],
        context: { key: 'value' },
      };
      
      expect(() => ClaudeInstructionSchema.parse(validInstruction)).not.toThrow();
    });

    it('should require core fields', () => {
      const incompleteInstruction = {
        workingDir: '/test/path',
        // Missing required fields
      };
      
      expect(() => ClaudeInstructionSchema.parse(incompleteInstruction)).toThrow();
    });
  });

  describe('ClaudeCliOptionsSchema', () => {
    it('should validate CLI options with all fields', () => {
      const validOptions = {
        workingDir: '/test/path',
        outputFormat: 'json' as const,
        model: 'sonnet' as const,
        maxTurns: 5,
        verbose: true,
        addDirs: ['/additional/path'],
      };
      
      expect(() => ClaudeCliOptionsSchema.parse(validOptions)).not.toThrow();
    });

    it('should validate minimal CLI options', () => {
      const minimalOptions = {
        workingDir: '/test/path',
      };
      
      expect(() => ClaudeCliOptionsSchema.parse(minimalOptions)).not.toThrow();
    });

    it('should reject invalid model', () => {
      const invalidOptions = {
        workingDir: '/test/path',
        model: 'invalid-model',
      };
      
      expect(() => ClaudeCliOptionsSchema.parse(invalidOptions)).toThrow();
    });
  });

  describe('ClaudeSdkOptionsSchema', () => {
    it('should validate SDK options', () => {
      const validOptions = {
        workingDir: '/test/path',
        maxTurns: 10,
        systemPrompt: 'You are a helpful assistant',
        allowedTools: ['Read', 'Write'],
        verbose: false,
        permissionMode: 'strict' as const,
      };
      
      expect(() => ClaudeSdkOptionsSchema.parse(validOptions)).not.toThrow();
    });
  });

  describe('RequestManagerConfigSchema', () => {
    it('should validate CLI method config', () => {
      const cliConfig = {
        method: 'cli' as const,
        workingDir: '/test/path',
        outputFormat: 'text' as const,
        model: 'sonnet' as const,
        maxTurns: 5,
        verbose: true,
      };
      
      expect(() => RequestManagerConfigSchema.parse(cliConfig)).not.toThrow();
    });

    it('should validate SDK method config', () => {
      const sdkConfig = {
        method: 'sdk' as const,
        workingDir: '/test/path',
        systemPrompt: 'Test prompt',
        allowedTools: ['Read'],
      };
      
      expect(() => RequestManagerConfigSchema.parse(sdkConfig)).not.toThrow();
    });
  });

  describe('ExecutionResultSchema', () => {
    it('should validate execution result', () => {
      const validResult = {
        instruction: {
          workingDir: '/test',
          knowledgeUpdate: ['update'],
          actionInstruction: 'action',
          qualityStandards: ['standard'],
          validationCriteria: ['criteria'],
        },
        response: {
          success: true,
          content: 'content',
          error: null,
        },
        executionTime: 1000,
        timestamp: '2024-01-01T00:00:00.000Z',
        method: 'cli' as const,
      };
      
      expect(() => ExecutionResultSchema.parse(validResult)).not.toThrow();
    });
  });

  describe('ValidationResultSchema', () => {
    it('should validate with all fields', () => {
      const validResult = {
        passed: true,
        criteria: 'Test criteria',
        details: 'Test details',
        score: 0.85,
      };
      
      expect(() => ValidationResultSchema.parse(validResult)).not.toThrow();
    });

    it('should validate with minimal fields', () => {
      const minimalResult = {
        passed: false,
        criteria: 'Test criteria',
      };
      
      expect(() => ValidationResultSchema.parse(minimalResult)).not.toThrow();
    });

    it('should reject invalid score range', () => {
      const invalidResult = {
        passed: true,
        criteria: 'Test criteria',
        score: 1.5, // Invalid: > 1
      };
      
      expect(() => ValidationResultSchema.parse(invalidResult)).toThrow();
    });
  });

  describe('TestResultSchema', () => {
    it('should validate complete test result', () => {
      const validTestResult = {
        testName: 'Test Name',
        instruction: {
          workingDir: '/test',
          knowledgeUpdate: ['update'],
          actionInstruction: 'action',
          qualityStandards: ['standard'],
          validationCriteria: ['criteria'],
        },
        executionResult: {
          instruction: {
            workingDir: '/test',
            knowledgeUpdate: ['update'],
            actionInstruction: 'action',
            qualityStandards: ['standard'],
            validationCriteria: ['criteria'],
          },
          response: {
            success: true,
            content: 'content',
            error: null,
          },
          executionTime: 1000,
          timestamp: '2024-01-01T00:00:00.000Z',
          method: 'cli' as const,
        },
        validationResults: [
          {
            passed: true,
            criteria: 'Test criteria',
            score: 0.9,
          },
        ],
        overallScore: 0.9,
        success: true,
      };
      
      expect(() => TestResultSchema.parse(validTestResult)).not.toThrow();
    });
  });
});

describe('Error Classes', () => {
  describe('ClaudeCodeError', () => {
    it('should create error with code and details', () => {
      const error = new ClaudeCodeError('Test message', 'TEST_CODE', { detail: 'test' });
      
      expect(error.message).toBe('Test message');
      expect(error.code).toBe('TEST_CODE');
      expect(error.details).toEqual({ detail: 'test' });
      expect(error.name).toBe('ClaudeCodeError');
    });
  });

  describe('ValidationError', () => {
    it('should create validation error', () => {
      const error = new ValidationError('Validation failed', { field: 'test' });
      
      expect(error.message).toBe('Validation failed');
      expect(error.code).toBe('VALIDATION_ERROR');
      expect(error.name).toBe('ValidationError');
      expect(error).toBeInstanceOf(ClaudeCodeError);
    });
  });

  describe('ExecutionError', () => {
    it('should create execution error', () => {
      const error = new ExecutionError('Execution failed', { step: 'test' });
      
      expect(error.message).toBe('Execution failed');
      expect(error.code).toBe('EXECUTION_ERROR');
      expect(error.name).toBe('ExecutionError');
      expect(error).toBeInstanceOf(ClaudeCodeError);
    });
  });
});

describe('Type Inference', () => {
  it('should infer correct types from schemas', () => {
    // This test ensures TypeScript compilation works correctly
    const response: ClaudeResponse = {
      success: true,
      content: 'test',
      error: null,
    };
    
    const instruction: ClaudeInstruction = {
      workingDir: '/test',
      knowledgeUpdate: ['update'],
      actionInstruction: 'action',
      qualityStandards: ['standard'],
      validationCriteria: ['criteria'],
    };
    
    expect(response.success).toBe(true);
    expect(instruction.workingDir).toBe('/test');
  });
});