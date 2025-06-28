import {
  aiModelSchema,
  generationResultSchema,
  instructionSetSchema,
  qualityScoresSchema,
  studyConfigSchema,
  studyStatisticsSchema,
} from '@/types/study';
import { describe, expect, it } from 'vitest';

// Top-level constants for performance
const CONSISTENCY_RANK_REGEX = /^(low|medium|high)$/;
const UUID_REGEX = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

describe('Study Type Validation', () => {
  // biome-ignore lint/nursery/noSecrets: This is a test schema name, not a secret
  describe('studyConfigSchema', () => {
    it('should validate a complete study configuration', () => {
      const validConfig = {
        name: 'Haskell Consistency Study',
        description: 'Testing AI consistency in Haskell code generation',
        runsPerCombination: 5,
        models: [
          {
            id: 'claude-sonnet',
            name: 'Claude 3.5 Sonnet',
            provider: 'anthropic' as const,
            modelName: 'claude-3.5-sonnet-20241022',
          },
        ],
        instructions: [
          {
            id: 'basic-module',
            name: 'Basic Module',
            filePath: './instructions/basic.md',
            content: 'Create a basic Haskell module',
            category: 'simple' as const,
          },
        ],
        outputDir: './results',
        timeout: 30000,
      };

      const result = studyConfigSchema.safeParse(validConfig);
      expect(result.success).toBe(true);

      if (result.success) {
        expect(result.data.name).toBe('Haskell Consistency Study');
        expect(result.data.runsPerCombination).toBe(5);
        expect(result.data.models).toHaveLength(1);
        expect(result.data.instructions).toHaveLength(1);
      }
    });

    it('should reject configuration with invalid runs per combination', () => {
      const invalidConfig = {
        name: 'Test Study',
        description: 'Test',
        runsPerCombination: 0, // Invalid: must be >= 1
        models: [
          {
            id: 'test-model',
            name: 'Test Model',
            provider: 'anthropic' as const,
            modelName: 'test',
          },
        ],
        instructions: [
          {
            id: 'test-instruction',
            name: 'Test',
            filePath: './test.md',
            content: 'test',
            category: 'simple' as const,
          },
        ],
      };

      const result = studyConfigSchema.safeParse(invalidConfig);
      expect(result.success).toBe(false);

      if (!result.success) {
        // biome-ignore lint/nursery/noSecrets: This is a validation path, not a secret
        expect(result.error.issues[0].path).toEqual(['runsPerCombination']);
        expect(result.error.issues[0].message).toContain('greater than or equal to 1');
      }
    });

    it('should use default values for optional fields', () => {
      const minimalConfig = {
        name: 'Minimal Study',
        description: 'Minimal test configuration',
        models: [
          {
            id: 'test-model',
            name: 'Test Model',
            provider: 'anthropic' as const,
            modelName: 'test',
          },
        ],
        instructions: [
          {
            id: 'test-instruction',
            name: 'Test',
            filePath: './test.md',
            content: 'test',
            category: 'simple' as const,
          },
        ],
      };

      const result = studyConfigSchema.safeParse(minimalConfig);
      expect(result.success).toBe(true);

      if (result.success) {
        expect(result.data.runsPerCombination).toBe(3); // Default value
        expect(result.data.outputDir).toBe('./results'); // Default value
        expect(result.data.timeout).toBe(30000); // Default value
      }
    });
  });

  describe('aiModelSchema', () => {
    it('should validate standard AI model configurations', () => {
      const models = [
        {
          id: 'claude-3.5-sonnet',
          name: 'Claude 3.5 Sonnet',
          provider: 'anthropic' as const,
          modelName: 'claude-3.5-sonnet-20241022',
        },
        {
          id: 'gpt-4',
          name: 'GPT-4',
          provider: 'openai' as const,
          modelName: 'gpt-4-0125-preview',
        },
      ];

      for (const model of models) {
        const result = aiModelSchema.safeParse(model);
        expect(result.success).toBe(true);

        if (result.success) {
          expect(result.data.id).toBe(model.id);
          expect(result.data.name).toBe(model.name);
          expect(result.data.provider).toBe(model.provider);
        }
      }
    });

    it('should reject model with invalid provider', () => {
      const invalidModel = {
        id: 'custom-model',
        name: 'Custom Model',
        provider: 'unknown-provider', // Not in enum
        modelName: 'test',
      };

      const result = aiModelSchema.safeParse(invalidModel);
      expect(result.success).toBe(false);

      if (!result.success) {
        expect(result.error.issues[0].path).toEqual(['provider']);
        expect(result.error.issues[0].code).toBe('invalid_enum_value');
      }
    });
  });

  // biome-ignore lint/nursery/noSecrets: This is a test schema name, not a secret
  describe('instructionSetSchema', () => {
    it('should validate instruction set with file path', () => {
      const instruction = {
        id: 'haskell-basic',
        name: 'Basic Haskell Module',
        filePath: './instructions/haskell-basic.md',
        content: 'Create a basic Haskell module with common functions',
        category: 'simple' as const,
      };

      const result = instructionSetSchema.safeParse(instruction);
      expect(result.success).toBe(true);

      if (result.success) {
        expect(result.data.category).toBe('simple');
        expect(result.data.filePath).toBe('./instructions/haskell-basic.md');
      }
    });

    it('should validate instruction set with inline content', () => {
      const instruction = {
        id: 'typescript-api',
        name: 'TypeScript API',
        filePath: './instructions/typescript-api.md',
        content: 'Create a TypeScript Express API with proper error handling...',
        category: 'modern' as const,
      };

      const result = instructionSetSchema.safeParse(instruction);
      expect(result.success).toBe(true);

      if (result.success) {
        expect(result.data.content).toContain('TypeScript Express API');
        expect(result.data.category).toBe('modern');
      }
    });
  });

  // biome-ignore lint/nursery/noSecrets: This is a test schema name, not a secret
  describe('qualityScoresSchema', () => {
    it('should validate quality scores within valid range', () => {
      const scores = {
        syntactic: 85.5,
        semantic: 92.0,
        modern: 78.3,
        completeness: 88.7,
        documentation: 95.2,
        performance: 82.1,
        overall: 86.9,
      };

      const result = qualityScoresSchema.safeParse(scores);
      expect(result.success).toBe(true);

      if (result.success) {
        // All scores should be valid
        for (const score of Object.values(result.data)) {
          expect(score).toBeGreaterThanOrEqual(0);
          expect(score).toBeLessThanOrEqual(100);
        }
      }
    });

    it('should reject scores outside valid range', () => {
      const invalidScores = {
        syntactic: 105.0, // Invalid: > 100
        semantic: -5.0, // Invalid: < 0
        modern: 78.3,
        completeness: 88.7,
        documentation: 95.2,
        performance: 82.1,
        overall: 86.9,
      };

      const result = qualityScoresSchema.safeParse(invalidScores);
      expect(result.success).toBe(false);

      if (!result.success) {
        const { issues } = result.error;
        expect(issues).toHaveLength(2); // Two invalid scores
        expect(issues.some((issue) => issue.path.includes('syntactic'))).toBe(true);
        expect(issues.some((issue) => issue.path.includes('semantic'))).toBe(true);
      }
    });
  });

  // biome-ignore lint/nursery/noSecrets: This is a test schema name, not a secret
  describe('generationResultSchema', () => {
    it('should validate complete generation result', () => {
      const result = {
        id: '550e8400-e29b-41d4-a716-446655440000',
        studyId: '6ba7b810-9dad-11d1-80b4-00c04fd430c8',
        modelId: 'claude-3.5-sonnet',
        instructionId: 'haskell-basic',
        runNumber: 1,
        timestamp: new Date('2025-01-25T10:00:00Z'),
        generatedCode: 'module Main where\n\nmain :: IO ()\nmain = putStrLn "Hello, World!"',
        scores: {
          syntactic: 95.0,
          semantic: 92.0,
          modern: 85.0,
          completeness: 90.0,
          documentation: 88.0,
          performance: 87.0,
          overall: 89.5,
        },
        metadata: {
          tokensUsed: 150,
          responseTime: 1250,
          promptTokens: 75,
          completionTokens: 75,
        },
      };

      const parseResult = generationResultSchema.safeParse(result);
      expect(parseResult.success).toBe(true);

      if (parseResult.success) {
        // Validate UUID format
        expect(parseResult.data.id).toMatch(UUID_REGEX);
        expect(parseResult.data.studyId).toMatch(UUID_REGEX);
        expect(parseResult.data.runNumber).toBe(1);
        expect(parseResult.data.generatedCode).toContain('module Main');
        // Validate score range
        expect(parseResult.data.scores.overall).toBeGreaterThanOrEqual(0);
        expect(parseResult.data.scores.overall).toBeLessThanOrEqual(100);
        // Validate timestamp
        expect(parseResult.data.timestamp).toBeInstanceOf(Date);
        expect(parseResult.data.timestamp.getTime()).not.toBeNaN();
      }
    });
  });

  // biome-ignore lint/nursery/noSecrets: This is a test schema name, not a secret
  describe('studyStatisticsSchema', () => {
    it('should validate comprehensive study statistics', () => {
      const stats = {
        studyId: '550e8400-e29b-41d4-a716-446655440000',
        totalGenerations: 50,
        meanScore: 87.5,
        standardDeviation: 12.3,
        coefficientOfVariation: 0.14,
        consistencyRank: 'medium' as const,
        modelComparison: [
          {
            modelId: 'claude-3.5-sonnet',
            meanScore: 89.2,
            consistency: 0.92,
            rank: 1,
          },
          {
            modelId: 'gpt-4',
            meanScore: 85.8,
            consistency: 0.88,
            rank: 2,
          },
        ],
        instructionComparison: [
          {
            instructionId: 'advanced-types',
            meanScore: 91.0,
            consistency: 0.95,
            rank: 1,
          },
          {
            instructionId: 'basic-module',
            meanScore: 84.0,
            consistency: 0.89,
            rank: 2,
          },
        ],
      };

      const result = studyStatisticsSchema.safeParse(stats);
      expect(result.success).toBe(true);

      if (result.success) {
        expect(result.data.totalGenerations).toBeGreaterThan(0);
        expect(result.data.meanScore).toBeGreaterThanOrEqual(0);
        expect(result.data.meanScore).toBeLessThanOrEqual(100);
        expect(result.data.consistencyRank).toMatch(CONSISTENCY_RANK_REGEX);
        expect(result.data.modelComparison).toHaveLength(2);
        expect(result.data.modelComparison[0].rank).toBe(1);
        expect(result.data.instructionComparison).toHaveLength(2);
      }
    });

    it('should reject invalid statistics', () => {
      const invalidStats = {
        studyId: 'invalid-uuid', // Invalid: not UUID format
        totalGenerations: -1, // Invalid: negative
        meanScore: 87.5,
        standardDeviation: 12.3,
        coefficientOfVariation: 0.14,
        consistencyRank: 'medium' as const,
        modelComparison: [],
        instructionComparison: [],
      };

      const result = studyStatisticsSchema.safeParse(invalidStats);
      expect(result.success).toBe(false);

      if (!result.success) {
        // First validation error should be studyId (UUID format)
        expect(result.error.issues[0].path).toEqual(['studyId']);
        expect(result.error.issues[0].code).toBe('invalid_string');
      }
    });
  });
});
