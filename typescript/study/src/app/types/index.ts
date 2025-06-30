/**
 * @fileoverview Study Data Types - Clean consolidation
 * @purpose Study-specific data types for the AI code generation study platform
 * @dependencies QiCore v4 Base and Core AI Types
 */

import { z } from 'zod';

// Import core AI types for local use and re-export
import type {
  AIModel,
  AIProvider,
  AppError,
  AsyncResult,
  GenerationRequest,
  InstructionCategory,
  InstructionSet,
  QualityMetrics,
  QualityScore,
} from '../../modules/types';

// Re-export core types
export type {
  AsyncResult,
  AppError,
  AIProvider,
  AIModel,
  InstructionCategory,
  InstructionSet,
  GenerationRequest,
  QualityMetrics,
  QualityScore,
};

// ============================================================================
// STUDY-SPECIFIC TYPES
// ============================================================================

export interface ModelCapabilities {
  readonly supportsThinking: boolean;
  readonly supportsStreaming: boolean;
  readonly maxContextLength: number;
  readonly supportedLanguages: readonly string[];
}

export interface GeneratedCode {
  readonly id: string;
  readonly code: string;
  readonly model: AIModel;
  readonly instruction: InstructionSet;
  readonly generation: GenerationMetadata;
  readonly quality: QualityMetrics;
}

export interface GenerationMetadata {
  readonly timestamp: Date;
  readonly duration: number;
  readonly tokensUsed: TokenUsage;
  readonly sessionId?: string;
  readonly reasoning?: string;
  readonly messageCount: number;
}

export interface TokenUsage {
  readonly prompt: number;
  readonly completion: number;
  readonly thinking?: number;
  readonly total: number;
}

// Quality types imported from modules/types.ts

// ============================================================================
// COMPILATION TYPES
// ============================================================================

export interface CompilationResult {
  readonly success: boolean;
  readonly errors: readonly CompilationError[];
  readonly warnings: readonly CompilationWarning[];
  readonly executionTime: number;
  readonly metadata: CompilationMetadata;
}

export interface CompilationError {
  readonly type: 'syntax' | 'type' | 'runtime';
  readonly message: string;
  readonly line?: number;
  readonly column?: number;
  readonly severity: 'error' | 'warning';
}

export interface CompilationWarning {
  readonly type: string;
  readonly message: string;
  readonly line?: number;
  readonly column?: number;
  readonly suggestion?: string;
}

export interface CompilationMetadata {
  readonly compiler: string;
  readonly version: string;
  readonly flags: readonly string[];
  readonly environment: Record<string, string>;
}

// ============================================================================
// STUDY RESULT TYPES
// ============================================================================

// Code Generation Configuration
export interface CodeGeneratorConfig {
  readonly maxTurns: number;
  readonly timeout: number;
  readonly outputFormat: 'text' | 'json' | 'stream-json';
  readonly thinking: {
    readonly enabled: boolean;
    readonly budgetTokens: number;
  };
  readonly verbose: boolean;
  readonly maxRetries: number;
  readonly retryDelay: number;
}

export interface StudyConfig {
  readonly id: string;
  readonly name: string;
  readonly description: string;
  readonly models: readonly AIModel[];
  readonly instructions: readonly InstructionSet[];
  readonly runsPerCombination: number;
  readonly timeout: number;
  readonly maxConcurrency: number;
  readonly outputDir: string;
}

export interface GenerationResult {
  readonly id: string;
  readonly studyId: string;
  readonly modelId: string;
  readonly instructionId: string;
  readonly runNumber: number;
  readonly generatedCode: GeneratedCode;
  readonly compilation?: CompilationResult;
  readonly timestamp: Date;
}

export interface StudyResult {
  readonly id: string;
  readonly config: StudyConfig;
  readonly generations: readonly GenerationResult[];
  readonly statistics: StudyStatistics;
  readonly metadata: StudyMetadata;
}

export interface StudyStatistics {
  readonly totalGenerations: number;
  readonly successRate: number;
  readonly meanScore: number;
  readonly standardDeviation: number;
  readonly coefficientOfVariation: number;
  readonly consistencyRank: 'high' | 'medium' | 'low';
  readonly modelComparisons: readonly ModelComparison[];
  readonly instructionComparisons: readonly InstructionComparison[];
}

export interface ModelComparison {
  readonly modelId: string;
  readonly meanScore: number;
  readonly consistency: number;
  readonly rank: number;
  readonly strengths: readonly string[];
  readonly weaknesses: readonly string[];
}

export interface InstructionComparison {
  readonly instructionId: string;
  readonly meanScore: number;
  readonly consistency: number;
  readonly rank: number;
  readonly effectiveFor: readonly string[];
  readonly challengingFor: readonly string[];
}

export interface StudyMetadata {
  readonly startTime: Date;
  readonly endTime: Date;
  readonly duration: number;
  readonly version: string;
  readonly environment: Record<string, string>;
  readonly resources: ResourceUsage;
}

export interface ResourceUsage {
  readonly totalTokens: number;
  readonly totalCost: number;
  readonly apiCalls: number;
  readonly peakMemoryMB: number;
  readonly averageLatency: number;
}

// ============================================================================
// VALIDATION SCHEMAS
// ============================================================================

export const aiModelSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  provider: z.enum(['anthropic', 'openai', 'local']),
  modelName: z.string().min(1),
  temperature: z.number().min(0).max(2).optional(),
  maxTokens: z.number().min(1).max(200000).optional(),
  topP: z.number().min(0).max(1).optional(),
  frequencyPenalty: z.number().min(-2).max(2).optional(),
  presencePenalty: z.number().min(-2).max(2).optional(),
});

export const instructionSetSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  category: z.enum(['modern', 'simple', 'setup', 'advanced']),
  content: z.string().min(1),
  language: z.string(),
  complexity: z.enum(['low', 'medium', 'high']),
  estimatedTokens: z.number().nonnegative(),
  tags: z.array(z.string()),
  version: z.string(),
});

export const qualityScoreSchema = z.object({
  value: z.number().min(0).max(100),
  confidence: z.number().min(0).max(1),
  details: z.string().optional(),
  criteria: z.array(z.string()),
});

export const qualityMetricsSchema = z.object({
  syntactic: qualityScoreSchema,
  semantic: qualityScoreSchema,
  modern: qualityScoreSchema,
  completeness: qualityScoreSchema,
  documentation: qualityScoreSchema,
  performance: qualityScoreSchema,
  overall: qualityScoreSchema,
});
