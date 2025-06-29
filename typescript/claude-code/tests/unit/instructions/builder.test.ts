import { describe, it, expect } from 'vitest';
import {
  buildInstruction,
  createHaskellInstruction,
  createTypeScriptInstruction,
  createAnalysisInstruction,
} from '@/instructions/builder';

describe('Instruction Builder', () => {
  describe('buildInstruction', () => {
    it('should build complete instruction with all sections', () => {
      const config = {
        workingDir: '/test/project',
        knowledgeUpdate: ['Modern TypeScript', 'Testing patterns'],
        actionInstruction: 'Create a comprehensive module',
        qualityStandards: ['Type safety', 'Documentation'],
        validationCriteria: ['Passes tests', 'Follows patterns'],
        context: {
          targetFile: 'src/test.ts',
          framework: 'React',
        },
      };

      const instruction = buildInstruction(config);

      expect(instruction).toContain('QiCore-Quality Code Generation Task');
      expect(instruction).toContain('/test/project');
      expect(instruction).toContain('Modern TypeScript');
      expect(instruction).toContain('Testing patterns');
      expect(instruction).toContain('Create a comprehensive module');
      expect(instruction).toContain('Type safety');
      expect(instruction).toContain('Documentation');
      expect(instruction).toContain('Passes tests');
      expect(instruction).toContain('Follows patterns');
      expect(instruction).toContain('targetFile');
      expect(instruction).toContain('src/test.ts');
      expect(instruction).toContain('framework');
      expect(instruction).toContain('React');
    });

    it('should build instruction without context', () => {
      const config = {
        workingDir: '/test/project',
        knowledgeUpdate: ['Modern TypeScript'],
        actionInstruction: 'Create a module',
        qualityStandards: ['Type safety'],
        validationCriteria: ['Passes tests'],
      };

      const instruction = buildInstruction(config);

      expect(instruction).toContain('QiCore-Quality Code Generation Task');
      expect(instruction).not.toContain('Additional Context');
    });

    it('should format knowledge updates as list', () => {
      const config = {
        workingDir: '/test',
        knowledgeUpdate: ['Update 1', 'Update 2', 'Update 3'],
        actionInstruction: 'Test',
        qualityStandards: ['Standard'],
        validationCriteria: ['Criteria'],
      };

      const instruction = buildInstruction(config);

      expect(instruction).toContain('- Research and update knowledge on: Update 1');
      expect(instruction).toContain('- Research and update knowledge on: Update 2');
      expect(instruction).toContain('- Research and update knowledge on: Update 3');
    });

    it('should format quality standards as list', () => {
      const config = {
        workingDir: '/test',
        knowledgeUpdate: ['Update'],
        actionInstruction: 'Test',
        qualityStandards: ['Standard 1', 'Standard 2'],
        validationCriteria: ['Criteria'],
      };

      const instruction = buildInstruction(config);

      expect(instruction).toContain('- Standard 1');
      expect(instruction).toContain('- Standard 2');
    });

    it('should format validation criteria as checklist', () => {
      const config = {
        workingDir: '/test',
        knowledgeUpdate: ['Update'],
        actionInstruction: 'Test',
        qualityStandards: ['Standard'],
        validationCriteria: ['Criteria 1', 'Criteria 2'],
      };

      const instruction = buildInstruction(config);

      expect(instruction).toContain('- [ ] Criteria 1');
      expect(instruction).toContain('- [ ] Criteria 2');
    });
  });

  describe('createHaskellInstruction', () => {
    it('should create Haskell instruction with default base directory', () => {
      const instruction = createHaskellInstruction('test/Module.hs');

      expect(instruction.workingDir).toBe('/home/zzhang/dev/qi/github/mcp-server/qicore-v4');
      expect(instruction.knowledgeUpdate).toContain('Modern Haskell 2024 best practices and GHC2024 extensions');
      expect(instruction.knowledgeUpdate).toContain('Haskell performance optimization techniques for production code');
      expect(instruction.actionInstruction).toContain('test/Module.hs');
      expect(instruction.actionInstruction).toContain('docs/experiment/sources/nl/base.contracts.md');
      expect(instruction.context?.targetFile).toBe('test/Module.hs');
      expect(instruction.context?.componentType).toBe('Cache');
    });

    it('should create Haskell instruction with custom base directory', () => {
      const customDir = '/custom/project';
      const instruction = createHaskellInstruction('src/Test.hs', customDir);

      expect(instruction.workingDir).toBe(customDir);
      expect(instruction.context?.targetFile).toBe('src/Test.hs');
    });

    it('should include comprehensive quality standards', () => {
      const instruction = createHaskellInstruction('test.hs');

      expect(instruction.qualityStandards).toContain('GHC2024 language edition with modern extensions (LambdaCase, ViewPatterns, etc.)');
      expect(instruction.qualityStandards).toContain('Strict performance annotations (!fields) on all data types');
      expect(instruction.qualityStandards).toContain('Rich Haddock documentation with examples and usage patterns');
      expect(instruction.qualityStandards).toContain('Mathematical law compliance verified with property-based tests');
    });

    it('should include validation criteria', () => {
      const instruction = createHaskellInstruction('test.hs');

      expect(instruction.validationCriteria).toContain('Code style matches existing QiCore base components exactly');
      expect(instruction.validationCriteria).toContain('Implements comprehensive API beyond minimum contract requirements');
      expect(instruction.validationCriteria).toContain('Includes property-based tests for all mathematical laws');
    });

    it('should include Cache-specific requirements', () => {
      const instruction = createHaskellInstruction('Cache.hs');

      expect(instruction.actionInstruction).toContain('Support both memory and persistent storage backends');
      expect(instruction.actionInstruction).toContain('Implement TTL (time-to-live) expiration with lazy checking');
      expect(instruction.actionInstruction).toContain('Provide LRU eviction policies with configurable capacity');
      expect(instruction.actionInstruction).toContain('Ensure thread-safe operations with appropriate STM usage');
    });
  });

  describe('createTypeScriptInstruction', () => {
    it('should create TypeScript instruction with modern features', () => {
      const instruction = createTypeScriptInstruction('src/Component.ts');

      expect(instruction.knowledgeUpdate).toContain('Modern TypeScript 5.3+ features and best practices');
      expect(instruction.knowledgeUpdate).toContain('Advanced type-level programming patterns');
      expect(instruction.actionInstruction).toContain('typescript/src/qicore/base/');
      expect(instruction.context?.language).toBe('TypeScript');
      expect(instruction.context?.buildSystem).toBe('Vitest + TypeScript');
    });

    it('should include TypeScript-specific quality standards', () => {
      const instruction = createTypeScriptInstruction('test.ts');

      expect(instruction.qualityStandards).toContain('Strict TypeScript configuration with no any types');
      expect(instruction.qualityStandards).toContain('Runtime validation with Zod schemas');
      expect(instruction.qualityStandards).toContain('Functional programming patterns with immutability');
    });

    it('should include TypeScript validation criteria', () => {
      const instruction = createTypeScriptInstruction('test.ts');

      expect(instruction.validationCriteria).toContain('Code passes strict TypeScript compilation without errors');
      expect(instruction.validationCriteria).toContain('Runtime validation schemas match TypeScript types');
      expect(instruction.validationCriteria).toContain('Performance meets interpreted tier targets (< 100μs per operation)');
    });
  });

  describe('createAnalysisInstruction', () => {
    it('should create analysis instruction', () => {
      const instruction = createAnalysisInstruction('src/modules/auth');

      expect(instruction.knowledgeUpdate).toContain('Modern software architecture patterns and best practices');
      expect(instruction.knowledgeUpdate).toContain('Code quality metrics and analysis techniques');
      expect(instruction.actionInstruction).toContain('Analyze src/modules/auth');
      expect(instruction.actionInstruction).toContain('Code quality and adherence to best practices');
      expect(instruction.context?.analysisTarget).toBe('src/modules/auth');
      expect(instruction.context?.comparisonStandard).toBe('QiCore quality benchmarks');
    });

    it('should include comprehensive analysis areas', () => {
      const instruction = createAnalysisInstruction('test-target');

      expect(instruction.actionInstruction).toContain('Performance characteristics and optimization opportunities');
      expect(instruction.actionInstruction).toContain('Test coverage and testing strategy');
      expect(instruction.actionInstruction).toContain('Documentation quality and completeness');
      expect(instruction.actionInstruction).toContain('Integration patterns and architectural decisions');
    });

    it('should include analysis-specific quality standards', () => {
      const instruction = createAnalysisInstruction('test-target');

      expect(instruction.qualityStandards).toContain('Comprehensive analysis covering all aspects of code quality');
      expect(instruction.qualityStandards).toContain('Specific, actionable recommendations with examples');
      expect(instruction.qualityStandards).toContain('Performance analysis with concrete metrics');
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty arrays gracefully', () => {
      const config = {
        workingDir: '/test',
        knowledgeUpdate: [],
        actionInstruction: 'Test',
        qualityStandards: [],
        validationCriteria: [],
      };

      const instruction = buildInstruction(config);

      expect(instruction).toContain('QiCore-Quality Code Generation Task');
      expect(instruction).toContain('Knowledge Update Requirements');
      expect(instruction).toContain('Quality Standards to Match');
      expect(instruction).toContain('Validation Criteria');
    });

    it('should handle special characters in paths', () => {
      const instruction = createHaskellInstruction('src/Special-Module_Name.hs', '/path/with spaces/project');

      expect(instruction.workingDir).toBe('/path/with spaces/project');
      expect(instruction.context?.targetFile).toBe('src/Special-Module_Name.hs');
    });

    it('should handle complex context objects', () => {
      const config = {
        workingDir: '/test',
        knowledgeUpdate: ['Update'],
        actionInstruction: 'Test',
        qualityStandards: ['Standard'],
        validationCriteria: ['Criteria'],
        context: {
          nested: {
            deep: {
              value: 'test',
            },
          },
          array: [1, 2, 3],
          boolean: true,
          null: null,
        },
      };

      const instruction = buildInstruction(config);

      expect(instruction).toContain('Additional Context');
      expect(instruction).toContain('nested');
      expect(instruction).toContain('array');
      expect(instruction).toContain('boolean');
    });
  });
});