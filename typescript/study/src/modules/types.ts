/**
 * @fileoverview Core AI Types - Reusable type definitions
 * @purpose Core AI types used across modules (no app dependencies)
 * @dependencies QiCore Base only
 */

import type { QiError } from './qicore/base/error';
import type { Result } from './qicore/base/result';

// ============================================================================
// RESULT TYPES (from QiCore)
// ============================================================================

/**
 * Async operation that can succeed or fail using QiCore Result<T>
 */
export type AsyncResult<T> = Promise<Result<T>>;

/**
 * Use QiError as the standard error type
 */
export type AppError = QiError;

// ============================================================================
// AI PROVIDER TYPES
// ============================================================================

export type AIProvider = 'anthropic' | 'openai' | 'local';

export interface AIModel {
  readonly id: string;
  readonly name: string;
  readonly provider: AIProvider;
  readonly modelName: string;
  readonly temperature?: number;
  readonly maxTokens?: number;
  readonly topP?: number;
  readonly frequencyPenalty?: number;
  readonly presencePenalty?: number;
}

// ============================================================================
// INSTRUCTION TYPES
// ============================================================================

export type InstructionCategory = 'modern' | 'simple' | 'setup' | 'advanced';

export interface InstructionSet {
  readonly id: string;
  readonly name: string;
  readonly category: InstructionCategory;
  readonly content: string;
  readonly language: string;
  readonly complexity: 'low' | 'medium' | 'high';
  readonly estimatedTokens: number;
  readonly tags: readonly string[];
  readonly version: string;
}

// ============================================================================
// GENERATION TYPES
// ============================================================================

export interface GenerationRequest {
  readonly model: AIModel;
  readonly systemPrompt?: string;
  readonly userPrompt: string;
  readonly maxTokens?: number;
  readonly temperature?: number;
}

// ============================================================================
// QUALITY TYPES
// ============================================================================

export interface QualityMetrics {
  readonly syntactic: QualityScore;
  readonly semantic: QualityScore;
  readonly modern: QualityScore;
  readonly completeness: QualityScore;
  readonly documentation: QualityScore;
  readonly performance: QualityScore;
  readonly overall: QualityScore;
}

export interface QualityScore {
  readonly value: number; // 0-100
  readonly confidence: number; // 0-1
  readonly details?: string;
  readonly criteria: readonly string[];
}
