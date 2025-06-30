import { z } from 'zod';

export interface SafetyConfig {
  enableDelays: boolean;
  minDelaySeconds: number;
  maxDelaySeconds: number;
  delayDescription?: string;
}

export interface StudyConfig {
  name: string;
  description: string;
  models: AIModel[];
  instructions: InstructionSet[];
  runsPerCombination: number;
  outputDir: string;
  timeout: number;
  safetyConfig?: SafetyConfig;
}

export interface AIModel {
  id: string;
  name: string;
  provider: 'openai' | 'anthropic' | 'local';
  modelName: string;
  temperature?: number;
  maxTokens?: number;
}

export interface InstructionSet {
  id: string;
  name: string;
  filePath: string;
  content: string;
  category: 'modern' | 'simple' | 'setup';
}

export interface GeneratedCode {
  code: string;
  model: string;
  instruction: string;
  timestamp: string;
  duration: number;
  success: boolean;
  error?: string;
  metadata: {
    provider: string;
    temperature?: number;
    maxTokens?: number;
    reasoning?: string;
    messageCount?: number;
    sessionId?: string;
    thinkingTokens?: number;
    note?: string;
  };
}

export interface GenerationResult {
  id: string;
  studyId: string;
  modelId: string;
  instructionId: string;
  runNumber: number;
  timestamp: Date;
  generatedCode: string;
  compilationResult?: CompilationResult;
  scores: QualityScores;
  metadata: GenerationMetadata;
}

export interface CompilationResult {
  success: boolean;
  errors: string[];
  warnings: string[];
  executionTime: number;
}

export interface QualityScores {
  syntactic: number; // 0-100, syntax correctness
  semantic: number; // 0-100, semantic correctness
  modern: number; // 0-100, modern language features usage
  completeness: number; // 0-100, implementation completeness
  documentation: number; // 0-100, documentation quality
  performance: number; // 0-100, performance characteristics
  overall: number; // 0-100, weighted average
}

export interface GenerationMetadata {
  tokensUsed: number;
  responseTime: number;
  promptTokens: number;
  completionTokens: number;
  embeddingVector?: number[];
}

export interface StudyStatistics {
  studyId: string;
  totalGenerations: number;
  meanScore: number;
  standardDeviation: number;
  coefficientOfVariation: number;
  consistencyRank: 'high' | 'medium' | 'low';
  modelComparison: ModelComparison[];
  instructionComparison: InstructionComparison[];
}

export interface ModelComparison {
  modelId: string;
  meanScore: number;
  consistency: number;
  rank: number;
}

export interface InstructionComparison {
  instructionId: string;
  meanScore: number;
  consistency: number;
  rank: number;
}

// Validation schemas
export const aiModelSchema = z.object({
  id: z.string(),
  name: z.string(),
  provider: z.enum(['openai', 'anthropic', 'local']),
  modelName: z.string(),
  temperature: z.number().min(0).max(2).optional(),
  maxTokens: z.number().min(1).max(128000).optional(),
});

export const instructionSetSchema = z.object({
  id: z.string(),
  name: z.string(),
  filePath: z.string(),
  content: z.string(),
  category: z.enum(['modern', 'simple', 'setup']),
});

export const qualityScoresSchema = z.object({
  syntactic: z.number().min(0).max(100),
  semantic: z.number().min(0).max(100),
  modern: z.number().min(0).max(100),
  completeness: z.number().min(0).max(100),
  documentation: z.number().min(0).max(100),
  performance: z.number().min(0).max(100),
  overall: z.number().min(0).max(100),
});

export const compilationResultSchema = z.object({
  success: z.boolean(),
  errors: z.array(z.string()),
  warnings: z.array(z.string()),
  executionTime: z.number().min(0),
});

export const generationMetadataSchema = z.object({
  tokensUsed: z.number().min(0),
  responseTime: z.number().min(0),
  promptTokens: z.number().min(0),
  completionTokens: z.number().min(0),
  embeddingVector: z.array(z.number()).optional(),
});

export const generationResultSchema = z.object({
  id: z.string().uuid(),
  studyId: z.string().uuid(),
  modelId: z.string(),
  instructionId: z.string(),
  runNumber: z.number().min(1),
  timestamp: z.date(),
  generatedCode: z.string(),
  compilationResult: compilationResultSchema.optional(),
  scores: qualityScoresSchema,
  metadata: generationMetadataSchema,
});

export const modelComparisonSchema = z.object({
  modelId: z.string(),
  meanScore: z.number().min(0).max(100),
  consistency: z.number().min(0).max(1),
  rank: z.number().min(1),
});

export const instructionComparisonSchema = z.object({
  instructionId: z.string(),
  meanScore: z.number().min(0).max(100),
  consistency: z.number().min(0).max(1),
  rank: z.number().min(1),
});

export const studyStatisticsSchema = z.object({
  studyId: z.string().uuid(),
  totalGenerations: z.number().min(0),
  meanScore: z.number().min(0).max(100),
  standardDeviation: z.number().min(0),
  coefficientOfVariation: z.number().min(0),
  consistencyRank: z.enum(['high', 'medium', 'low']),
  modelComparison: z.array(modelComparisonSchema),
  instructionComparison: z.array(instructionComparisonSchema),
});

export const safetyConfigSchema = z.object({
  enableDelays: z.boolean().default(true),
  minDelaySeconds: z.number().min(0).default(60),
  maxDelaySeconds: z.number().min(0).default(180),
  delayDescription: z.string().optional(),
});

export const studyConfigSchema = z.object({
  name: z.string().min(1),
  description: z.string().min(1),
  models: z.array(aiModelSchema).min(1),
  instructions: z.array(instructionSetSchema).min(1),
  runsPerCombination: z.number().min(1).max(100).default(3),
  outputDir: z.string().default('./results'),
  timeout: z.number().min(1000).default(30000),
  safetyConfig: safetyConfigSchema.optional(),
});
