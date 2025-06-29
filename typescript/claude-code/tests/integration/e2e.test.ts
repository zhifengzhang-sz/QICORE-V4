import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { ClaudeRequestManager } from '@/managers/request-manager';
import { createHaskellInstruction, createTypeScriptInstruction } from '@/instructions/builder';
import { ResultValidator } from '@/types/tests/validator';
import { promises as fs } from 'fs';
import { join } from 'path';

/**
 * End-to-End Integration Tests
 * 
 * These tests validate the complete flow of the Claude Code request manager
 * in a realistic environment. They are designed to be run against actual
 * Claude Code CLI/SDK when available.
 * 
 * Note: These tests may be skipped in CI if Claude Code is not available.
 */
describe('E2E Integration Tests', () => {
  const testProjectDir = '/tmp/claude-code-test-project';
  const testTimeout = 60000; // 1 minute timeout for real Claude Code calls

  beforeAll(async () => {
    // Create temporary test project directory
    await fs.mkdir(testProjectDir, { recursive: true });
    await fs.mkdir(join(testProjectDir, 'src'), { recursive: true });
    await fs.mkdir(join(testProjectDir, 'haskell'), { recursive: true });
    
    // Create basic project structure
    await fs.writeFile(
      join(testProjectDir, 'package.json'),
      JSON.stringify({
        name: 'test-project',
        version: '1.0.0',
        type: 'module',
      }, null, 2)
    );
  });

  afterAll(async () => {
    // Cleanup test directory
    try {
      await fs.rm(testProjectDir, { recursive: true, force: true });
    } catch (error) {
      console.warn('Failed to cleanup test directory:', error);
    }
  });

  describe('CLI Integration', () => {
    let cliManager: ClaudeRequestManager;
    let validator: ResultValidator;

    beforeAll(() => {
      cliManager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: testProjectDir,
        outputFormat: 'text',
        model: 'sonnet',
        maxTurns: 3,
        verbose: false, // Reduce noise in tests
      });
      
      validator = new ResultValidator(testProjectDir);
    });

    it('should check CLI availability', async () => {
      const status = await cliManager.getStatus();
      
      if (!status.available) {
        console.warn('Claude Code CLI not available, skipping CLI integration tests');
        return;
      }
      
      expect(status.available).toBe(true);
      expect(status.method).toBe('cli');
    }, testTimeout);

    it('should execute simple instruction successfully', async () => {
      const status = await cliManager.getStatus();
      if (!status.available) {
        console.warn('Skipping: Claude Code CLI not available');
        return;
      }

      const instruction = {
        workingDir: testProjectDir,
        knowledgeUpdate: ['Modern software development practices'],
        actionInstruction: 'Create a simple "Hello World" program explanation',
        qualityStandards: ['Clear explanation', 'Practical examples'],
        validationCriteria: ['Contains code example', 'Explains the concept'],
      };

      const result = await cliManager.executeInstruction(instruction);
      
      expect(result.response.success).toBe(true);
      expect(result.response.content).toBeTruthy();
      expect(result.executionTime).toBeGreaterThan(0);
      expect(result.method).toBe('cli');
    }, testTimeout);

    it('should handle Haskell code generation instruction', async () => {
      const status = await cliManager.getStatus();
      if (!status.available) {
        console.warn('Skipping: Claude Code CLI not available');
        return;
      }

      const instruction = createHaskellInstruction(
        'haskell/SimpleModule.hs',
        testProjectDir
      );

      const result = await cliManager.executeInstruction(instruction);
      
      if (result.response.success) {
        expect(result.response.content).toContain('module');
        expect(result.response.content).toContain('Haskell');
        
        // Validate the result
        const testResult = await validator.validateExecution(result);
        expect(testResult.overallScore).toBeGreaterThan(0.5);
      } else {
        console.warn('Haskell generation failed:', result.response.error);
      }
    }, testTimeout);

    it('should handle TypeScript code generation instruction', async () => {
      const status = await cliManager.getStatus();
      if (!status.available) {
        console.warn('Skipping: Claude Code CLI not available');
        return;
      }

      const instruction = createTypeScriptInstruction(
        'src/TestComponent.ts',
        testProjectDir
      );

      const result = await cliManager.executeInstruction(instruction);
      
      if (result.response.success) {
        expect(result.response.content).toContain('TypeScript');
        
        // Validate the result
        const testResult = await validator.validateExecution(result);
        expect(testResult.overallScore).toBeGreaterThan(0.5);
      } else {
        console.warn('TypeScript generation failed:', result.response.error);
      }
    }, testTimeout);

    it('should execute instruction sequence', async () => {
      const status = await cliManager.getStatus();
      if (!status.available) {
        console.warn('Skipping: Claude Code CLI not available');
        return;
      }

      const instructions = [
        {
          workingDir: testProjectDir,
          knowledgeUpdate: ['Software architecture'],
          actionInstruction: 'Explain the concept of modularity in software',
          qualityStandards: ['Clear explanation'],
          validationCriteria: ['Covers modularity concept'],
        },
        {
          workingDir: testProjectDir,
          knowledgeUpdate: ['Code organization'],
          actionInstruction: 'Explain how to organize code files in a project',
          qualityStandards: ['Practical advice'],
          validationCriteria: ['Includes file structure examples'],
        },
      ];

      const results = await cliManager.executeInstructionSequence(instructions);
      
      expect(results).toHaveLength(2);
      
      if (results[0].response.success) {
        expect(results[0].response.content).toContain('modular');
      }
      
      if (results[1].response.success) {
        expect(results[1].response.content).toBeTruthy();
      }
    }, testTimeout * 2);
  });

  describe('SDK Integration', () => {
    let sdkManager: ClaudeRequestManager;

    beforeAll(() => {
      sdkManager = new ClaudeRequestManager({
        method: 'sdk',
        workingDir: testProjectDir,
        maxTurns: 3,
        verbose: false,
        allowedTools: ['Read', 'Write', 'Edit'],
      });
    });

    it('should check SDK availability', async () => {
      const status = await sdkManager.getStatus();
      
      if (!status.available) {
        console.warn('Claude Code SDK not available, skipping SDK integration tests');
        return;
      }
      
      expect(status.available).toBe(true);
      expect(status.method).toBe('sdk');
    }, testTimeout);

    it('should execute instruction via SDK', async () => {
      const status = await sdkManager.getStatus();
      if (!status.available) {
        console.warn('Skipping: Claude Code SDK not available');
        return;
      }

      const instruction = {
        workingDir: testProjectDir,
        knowledgeUpdate: ['Modern development practices'],
        actionInstruction: 'Provide a brief overview of test-driven development',
        qualityStandards: ['Accurate information', 'Concise explanation'],
        validationCriteria: ['Explains TDD concept', 'Mentions benefits'],
      };

      const result = await sdkManager.executeInstruction(instruction);
      
      expect(result.response.success).toBe(true);
      expect(result.response.content).toBeTruthy();
      expect(result.method).toBe('sdk');
      expect(result.response.messages).toBeDefined();
    }, testTimeout);
  });

  describe('Manager Switching', () => {
    let manager: ClaudeRequestManager;

    beforeAll(() => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: testProjectDir,
        verbose: false,
      });
    });

    it('should switch between CLI and SDK methods', async () => {
      expect(manager.getConfig().method).toBe('cli');
      
      // Switch to SDK
      manager.switchMethod('sdk');
      expect(manager.getConfig().method).toBe('sdk');
      
      // Switch back to CLI
      manager.switchMethod('cli');
      expect(manager.getConfig().method).toBe('cli');
    });

    it('should maintain configuration when switching methods', async () => {
      const originalConfig = manager.getConfig();
      
      manager.switchMethod('sdk');
      const sdkConfig = manager.getConfig();
      
      expect(sdkConfig.workingDir).toBe(originalConfig.workingDir);
      expect(sdkConfig.verbose).toBe(originalConfig.verbose);
      expect(sdkConfig.method).toBe('sdk');
    });
  });

  describe('Error Handling', () => {
    let manager: ClaudeRequestManager;

    beforeAll(() => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: testProjectDir,
        maxTurns: 1, // Low max turns to potentially trigger errors
        verbose: false,
      });
    });

    it('should handle invalid instructions gracefully', async () => {
      const invalidInstruction = {
        workingDir: '', // Invalid
        knowledgeUpdate: [],
        actionInstruction: '',
        qualityStandards: [],
        validationCriteria: [],
      };

      const result = await manager.executeInstruction(invalidInstruction as any);
      
      expect(result.response.success).toBe(false);
      expect(result.response.error).toBeTruthy();
    });

    it('should handle very complex instructions', async () => {
      const complexInstruction = createHaskellInstruction(
        'complex/VeryComplexModule.hs',
        testProjectDir
      );
      
      // Add extremely detailed requirements
      complexInstruction.knowledgeUpdate.push(
        'Advanced category theory',
        'Monad transformers',
        'Type-level programming',
        'Performance optimization at the assembly level'
      );
      
      complexInstruction.actionInstruction += `
        Additionally, implement:
        - A complete web server with websockets
        - Database migrations with rollback support
        - Comprehensive test suite with property testing
        - Complete documentation with examples
        - Performance benchmarks
        - Docker containerization
        - CI/CD pipeline configuration
      `;

      const result = await manager.executeInstruction(complexInstruction);
      
      // Even if it fails, it should fail gracefully
      expect(result.response).toBeDefined();
      expect(result.executionTime).toBeGreaterThan(0);
      expect(result.timestamp).toBeDefined();
    }, testTimeout);
  });

  describe('Validation Integration', () => {
    let validator: ResultValidator;

    beforeAll(() => {
      validator = new ResultValidator(testProjectDir);
    });

    it('should validate execution results comprehensively', async () => {
      const mockResult = {
        instruction: {
          workingDir: testProjectDir,
          knowledgeUpdate: ['Test'],
          actionInstruction: 'Generate TypeScript code',
          qualityStandards: ['Type safety'],
          validationCriteria: ['Compiles without errors'],
        },
        response: {
          success: true,
          content: `
            // TypeScript Example
            interface User {
              id: number;
              name: string;
            }
            
            /**
             * Create a new user
             */
            function createUser(name: string): Result<User> {
              if (!name.trim()) {
                return failure(new Error("Name is required"));
              }
              return success({ id: Date.now(), name });
            }
          `,
          error: null,
        },
        executionTime: 2000,
        timestamp: new Date().toISOString(),
        method: 'cli' as const,
      };

      const testResult = await validator.validateExecution(mockResult);
      
      expect(testResult.success).toBe(true);
      expect(testResult.overallScore).toBeGreaterThan(0.7);
      expect(testResult.validationResults.length).toBeGreaterThan(0);
      
      const report = validator.generateReport(testResult);
      expect(report).toContain('Validation Report');
      expect(report).toContain('Overall Score');
    });

    it('should handle validation of real file generation', async () => {
      // Create a test file
      const testFilePath = join(testProjectDir, 'src', 'TestGenerated.ts');
      const testContent = `
        export interface TestInterface {
          value: string;
        }
        
        export function testFunction(): TestInterface {
          return { value: 'test' };
        }
      `;
      
      await fs.writeFile(testFilePath, testContent);

      const mockResult = {
        instruction: {
          workingDir: testProjectDir,
          knowledgeUpdate: ['TypeScript'],
          actionInstruction: 'Generate TypeScript interface',
          qualityStandards: ['Type safety'],
          validationCriteria: ['Valid TypeScript syntax'],
        },
        response: {
          success: true,
          content: testContent,
          error: null,
        },
        executionTime: 1000,
        timestamp: new Date().toISOString(),
        method: 'cli' as const,
      };

      const testResult = await validator.validateExecution(
        mockResult,
        ['src/TestGenerated.ts']
      );
      
      expect(testResult.success).toBe(true);
      expect(testResult.generatedFiles).toContain('src/TestGenerated.ts');
      
      // Check file existence validation passed
      const fileValidation = testResult.validationResults.find(
        v => v.criteria.includes('File existence')
      );
      expect(fileValidation?.passed).toBe(true);
    });
  });

  describe('Performance Tests', () => {
    let manager: ClaudeRequestManager;

    beforeAll(() => {
      manager = new ClaudeRequestManager({
        method: 'cli',
        workingDir: testProjectDir,
        maxTurns: 1,
        verbose: false,
      });
    });

    it('should complete simple instruction within reasonable time', async () => {
      const status = await manager.getStatus();
      if (!status.available) {
        console.warn('Skipping: Claude Code not available');
        return;
      }

      const startTime = Date.now();
      
      const instruction = {
        workingDir: testProjectDir,
        knowledgeUpdate: ['Basic programming'],
        actionInstruction: 'Explain what a variable is in programming',
        qualityStandards: ['Simple explanation'],
        validationCriteria: ['Mentions variable concept'],
      };

      const result = await manager.executeInstruction(instruction);
      const totalTime = Date.now() - startTime;
      
      expect(result.executionTime).toBeLessThan(testTimeout);
      expect(totalTime).toBeLessThan(testTimeout);
      
      if (result.response.success) {
        expect(result.response.content).toBeTruthy();
      }
    }, testTimeout);

    it('should handle concurrent instructions efficiently', async () => {
      const status = await manager.getStatus();
      if (!status.available) {
        console.warn('Skipping: Claude Code not available');
        return;
      }

      const instructions = Array(3).fill(null).map((_, i) => ({
        workingDir: testProjectDir,
        knowledgeUpdate: ['Programming concepts'],
        actionInstruction: `Explain programming concept #${i + 1}`,
        qualityStandards: ['Clear explanation'],
        validationCriteria: ['Relevant content'],
      }));

      const startTime = Date.now();
      const results = await manager.executeInstructionBatch(instructions);
      const totalTime = Date.now() - startTime;
      
      expect(results).toHaveLength(3);
      expect(totalTime).toBeLessThan(testTimeout * 2); // Should be faster than sequential
      
      // Check that all completed (successfully or not)
      expect(results.every(r => r.response)).toBe(true);
    }, testTimeout * 2);
  });
});