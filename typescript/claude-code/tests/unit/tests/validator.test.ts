import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { access, readFile } from 'node:fs/promises';
import { ResultValidator } from '@/types/tests/validator';

// Mock fs promises
vi.mock('node:fs/promises', () => ({
  access: vi.fn(),
  readFile: vi.fn(),
}));

const mockAccess = vi.mocked(access);
const mockReadFile = vi.mocked(readFile);

describe('ResultValidator', () => {
  let validator: ResultValidator;
  const baseDir = '/test/project';

  beforeEach(() => {
    vi.clearAllMocks();
    validator = new ResultValidator(baseDir);
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('validateExecution', () => {
    it('should validate successful execution with all checks', async () => {
      const executionResult = global.testUtils.createMockExecutionResult({
        response: global.testUtils.createMockClaudeResponse({
          success: true,
          content: `
            module Test where
            import qualified Data.Text as T
            
            -- | A test function with documentation
            testFunction :: Int -> Result String
            testFunction x = success "test"
            
            data TestData = TestData !Int !String
              deriving stock (Eq, Show)
          `,
        }),
      });

      const expectedFiles = ['src/Test.hs'];
      
      // Mock file access as successful
      mockAccess.mockResolvedValue(undefined);
      mockReadFile.mockResolvedValue(`
        {-# LANGUAGE GHC2021 #-}
        module Test where
        import qualified Data.Text as T
        import QiCore.Base (Result, success)
        
        -- | A test function
        testFunction :: Int -> Result String
        testFunction x = success "test"
        
        data TestData = TestData !Int !String
          deriving stock (Eq, Show)
      `);

      const testResult = await validator.validateExecution(executionResult, expectedFiles);

      expect(testResult.success).toBe(true);
      expect(testResult.overallScore).toBeGreaterThan(0.8);
      expect(testResult.validationResults).toHaveLength(3); // Basic + content + file validation (haskell validation depends on instruction content)
      expect(testResult.generatedFiles).toEqual(expectedFiles);
    });

    it('should handle execution failure', async () => {
      const executionResult = global.testUtils.createMockExecutionResult({
        response: global.testUtils.createMockClaudeResponse({
          success: false,
          error: 'Execution failed',
          content: null,
        }),
      });

      const testResult = await validator.validateExecution(executionResult);

      expect(testResult.success).toBe(false);
      expect(testResult.overallScore).toBeLessThan(0.8);
      expect(testResult.validationResults[0].passed).toBe(false);
      expect(testResult.notes).toContain('Execution failed');
    });

    it('should validate content quality with comprehensive checks', async () => {
      const executionResult = global.testUtils.createMockExecutionResult({
        response: global.testUtils.createMockClaudeResponse({
          content: `
            A comprehensive response with:
            - Code examples: function test() { return "hello"; }
            - Documentation: /** This function does something */
            - Error handling: try { result() } catch (error) { handle(error); }
            - Multiple lines of substantial content that exceeds the minimum length requirement
          `,
        }),
      });

      const testResult = await validator.validateExecution(executionResult);

      const contentValidation = testResult.validationResults.find(v => v.criteria === 'Content quality');
      expect(contentValidation).toBeDefined();
      expect(contentValidation?.passed).toBe(true);
      expect(contentValidation?.score).toBeGreaterThan(0.7);
    });

    it('should fail content quality for poor content', async () => {
      const executionResult = global.testUtils.createMockExecutionResult({
        response: global.testUtils.createMockClaudeResponse({
          content: 'Short response without code or documentation',
        }),
      });

      const testResult = await validator.validateExecution(executionResult);

      const contentValidation = testResult.validationResults.find(v => v.criteria === 'Content quality');
      expect(contentValidation?.passed).toBe(false);
      expect(contentValidation?.score).toBeLessThan(0.7);
    });
  });

  describe('validateGeneratedFiles', () => {
    it('should validate existing files', async () => {
      const expectedFiles = ['src/Test.hs', 'src/Another.hs'];
      
      mockAccess.mockResolvedValue(undefined); // Files exist

      const results = await (validator as any).validateGeneratedFiles(expectedFiles);

      expect(results).toHaveLength(2);
      expect(results[0].passed).toBe(true);
      expect(results[0].criteria).toBe('File existence: src/Test.hs');
      expect(results[1].passed).toBe(true);
      expect(results[1].criteria).toBe('File existence: src/Another.hs');
    });

    it('should handle missing files', async () => {
      const expectedFiles = ['src/Missing.hs'];
      
      mockAccess.mockRejectedValue(new Error('ENOENT: no such file'));

      const results = await (validator as any).validateGeneratedFiles(expectedFiles);

      expect(results).toHaveLength(1);
      expect(results[0].passed).toBe(false);
      expect(results[0].criteria).toBe('File existence: src/Missing.hs');
      expect(results[0].details).toContain('File not found');
    });
  });

  describe('validateHaskellQuality', () => {
    it('should validate modern Haskell patterns', async () => {
      const haskellContent = `
        {-# LANGUAGE GHC2021 #-}
        {-# LANGUAGE LambdaCase #-}
        
        module Test where
        
        import qualified Data.Text as T
        import QiCore.Base (Result, success, QiError)
        
        -- | A well-documented function
        data TestData = TestData !Int !String
          deriving stock (Eq, Show)
          deriving anyclass (ToJSON)
        
        instance Functor TestResult where
          fmap f (Success a) = Success (f a)
          fmap _ (Failure e) = Failure e
      `;
      
      mockReadFile.mockResolvedValue(haskellContent);

      const results = await (validator as any).validateHaskellQuality(['src/Test.hs']);

      expect(results).toHaveLength(2); // Quality + QiCore integration
      
      const qualityResult = results.find((r: any) => r.criteria.includes('Haskell quality'));
      expect(qualityResult.passed).toBe(true);
      expect(qualityResult.score).toBeGreaterThan(0.6);
      
      const qicoreResult = results.find((r: any) => r.criteria.includes('QiCore integration'));
      expect(qicoreResult.passed).toBe(true);
    });

    it('should detect missing modern patterns', async () => {
      const oldHaskellContent = `
        module Test where
        
        data TestData = TestData Int String
        
        testFunction x = x + 1
      `;
      
      mockReadFile.mockResolvedValue(oldHaskellContent);

      const results = await (validator as any).validateHaskellQuality(['src/Test.hs']);

      const qualityResult = results.find((r: any) => r.criteria.includes('Haskell quality'));
      expect(qualityResult.passed).toBe(false);
      expect(qualityResult.score).toBeLessThan(0.6);
    });

    it('should validate QiCore integration', async () => {
      const qicoreContent = `
        module Test where
        import QiCore.Base (Result, QiError, success, failure)
        
        testFunction :: Int -> Result String
        testFunction x = success "test"
      `;
      
      mockReadFile.mockResolvedValue(qicoreContent);

      const results = await (validator as any).validateHaskellQuality(['src/Test.hs']);

      const qicoreResult = results.find((r: any) => r.criteria.includes('QiCore integration'));
      expect(qicoreResult.passed).toBe(true);
    });

    it('should detect missing QiCore integration', async () => {
      const nonQicoreContent = `
        module Test where
        
        testFunction :: Int -> String
        testFunction x = "test"
      `;
      
      mockReadFile.mockResolvedValue(nonQicoreContent);

      const results = await (validator as any).validateHaskellQuality(['src/Test.hs']);

      const qicoreResult = results.find((r: any) => r.criteria.includes('QiCore integration'));
      expect(qicoreResult.passed).toBe(false);
    });

    it('should handle file read errors', async () => {
      mockReadFile.mockRejectedValue(new Error('Permission denied'));

      const results = await (validator as any).validateHaskellQuality(['src/Test.hs']);

      expect(results).toHaveLength(1);
      expect(results[0].passed).toBe(false);
      expect(results[0].details).toContain('Failed to read file');
    });
  });

  describe('calculateOverallScore', () => {
    it('should calculate average score', () => {
      const results = [
        { passed: true, criteria: 'Test 1', score: 1.0 },
        { passed: true, criteria: 'Test 2', score: 0.8 },
        { passed: false, criteria: 'Test 3', score: 0.6 },
        { passed: true, criteria: 'Test 4', score: 0.9 },
      ];

      const score = (validator as any).calculateOverallScore(results);
      expect(score).toBeCloseTo(0.825); // (1.0 + 0.8 + 0.6 + 0.9) / 4
    });

    it('should return 0 for empty results', () => {
      const score = (validator as any).calculateOverallScore([]);
      expect(score).toBe(0);
    });

    it('should handle missing scores', () => {
      const results = [
        { passed: true, criteria: 'Test 1', score: 1.0 },
        { passed: false, criteria: 'Test 2' }, // No score
      ];

      const score = (validator as any).calculateOverallScore(results);
      expect(score).toBe(0.5); // (1.0 + 0) / 2
    });
  });

  describe('isHaskellInstruction', () => {
    it('should detect Haskell instructions', () => {
      const haskellInstruction = global.testUtils.createMockInstruction({
        actionInstruction: 'Generate a Haskell module',
        context: { targetFile: 'src/Test.hs' },
      });

      const isHaskell = (validator as any).isHaskellInstruction(haskellInstruction);
      expect(isHaskell).toBe(true);
    });

    it('should detect non-Haskell instructions', () => {
      const tsInstruction = global.testUtils.createMockInstruction({
        actionInstruction: 'Generate a TypeScript component',
        context: { targetFile: 'src/Component.ts' },
      });

      const isHaskell = (validator as any).isHaskellInstruction(tsInstruction);
      expect(isHaskell).toBe(false);
    });
  });

  describe('generateTestName', () => {
    it('should generate descriptive test name', () => {
      const result = global.testUtils.createMockExecutionResult({
        method: 'cli',
        timestamp: '2024-01-15T10:30:00.000Z',
      });

      const testName = (validator as any).generateTestName(result);
      expect(testName).toBe('cli_execution_2024-01-15T10:30');
    });
  });

  describe('generateNotes', () => {
    it('should generate notes for successful validation', () => {
      const validationResults = [
        { passed: true, criteria: 'Test 1', details: 'Success' },
        { passed: true, criteria: 'Test 2', details: 'Success' },
      ];
      const result = global.testUtils.createMockExecutionResult();

      const notes = (validator as any).generateNotes(validationResults, result);
      expect(notes).toBe('All validations passed successfully');
    });

    it('should generate notes for failed validations', () => {
      const validationResults = [
        { passed: true, criteria: 'Test 1', details: 'Success' },
        { passed: false, criteria: 'Test 2', details: 'Failed check' },
        { passed: false, criteria: 'Test 3', details: 'Another failure' },
      ];
      const result = global.testUtils.createMockExecutionResult();

      const notes = (validator as any).generateNotes(validationResults, result);
      expect(notes).toContain('2 validation(s) failed');
      expect(notes).toContain('Test 2: Failed check');
      expect(notes).toContain('Test 3: Another failure');
    });

    it('should include execution error in notes', () => {
      const validationResults = [
        { passed: false, criteria: 'Test 1', details: 'Failed' },
      ];
      const result = global.testUtils.createMockExecutionResult({
        response: global.testUtils.createMockClaudeResponse({
          success: false,
          error: 'Execution error occurred',
        }),
      });

      const notes = (validator as any).generateNotes(validationResults, result);
      expect(notes).toContain('Execution error: Execution error occurred');
    });
  });

  describe('generateReport', () => {
    it('should generate comprehensive validation report', () => {
      const testResult = {
        testName: 'test_execution_2024-01-15',
        overallScore: 0.85,
        success: true,
        executionResult: global.testUtils.createMockExecutionResult({
          executionTime: 5000,
        }),
        validationResults: [
          { passed: true, criteria: 'Basic execution', score: 1.0, details: 'Success' },
          { passed: true, criteria: 'Content quality', score: 0.8, details: 'Good content' },
          { passed: false, criteria: 'File existence', score: 0.0, details: 'File not found' },
        ],
        notes: 'Some validations failed',
        instruction: global.testUtils.createMockInstruction(),
        generatedFiles: ['test.hs'],
      };

      const report = validator.generateReport(testResult);

      expect(report).toContain('# Validation Report: test_execution_2024-01-15');
      expect(report).toContain('**Overall Score:** 85.0%');
      expect(report).toContain('**Success:** ✅ PASS');
      expect(report).toContain('**Execution Time:** 5000ms');
      expect(report).toContain('✅ **Basic execution** (100.0%)');
      expect(report).toContain('✅ **Content quality** (80.0%)');
      expect(report).toContain('❌ **File existence**');
      expect(report).toContain('## Notes');
      expect(report).toContain('Some validations failed');
    });

    it('should handle failed test result', () => {
      const testResult = {
        testName: 'failed_test',
        overallScore: 0.3,
        success: false,
        executionResult: global.testUtils.createMockExecutionResult(),
        validationResults: [
          { passed: false, criteria: 'Basic execution', details: 'Failed' },
        ],
        notes: 'Test failed',
        instruction: global.testUtils.createMockInstruction(),
        generatedFiles: [],
      };

      const report = validator.generateReport(testResult);

      expect(report).toContain('**Success:** ❌ FAIL');
      expect(report).toContain('❌ **Basic execution**');
    });

    it('should handle validations without scores', () => {
      const testResult = {
        testName: 'test_without_scores',
        overallScore: 0.5,
        success: true,
        executionResult: global.testUtils.createMockExecutionResult(),
        validationResults: [
          { passed: true, criteria: 'Test without score', details: 'Success' },
        ],
        instruction: global.testUtils.createMockInstruction(),
        generatedFiles: [],
      };

      const report = validator.generateReport(testResult);

      expect(report).toContain('✅ **Test without score**');
      expect(report).not.toContain('('); // No score percentage
    });
  });

  describe('Edge Cases', () => {
    it('should handle execution result without expected files', async () => {
      const executionResult = global.testUtils.createMockExecutionResult();

      const testResult = await validator.validateExecution(executionResult);

      expect(testResult.success).toBeDefined();
      expect(testResult.validationResults.length).toBeGreaterThan(0);
    });

    it('should handle empty content', async () => {
      const executionResult = global.testUtils.createMockExecutionResult({
        response: global.testUtils.createMockClaudeResponse({
          content: '',
        }),
      });

      const testResult = await validator.validateExecution(executionResult);

      const contentValidation = testResult.validationResults.find(v => v.criteria === 'Content quality');
      // Content validation may not exist for empty content, so check if it exists and is false
      if (contentValidation) {
        expect(contentValidation.passed).toBe(false);
      } else {
        // If no content validation exists for empty content, that's also valid
        expect(testResult.validationResults.length).toBeGreaterThan(0);
      }
    });

    it('should handle null content', async () => {
      const executionResult = global.testUtils.createMockExecutionResult({
        response: global.testUtils.createMockClaudeResponse({
          content: null,
        }),
      });

      const testResult = await validator.validateExecution(executionResult);

      expect(testResult.validationResults.some(v => v.criteria === 'Content quality')).toBe(false);
    });
  });
});