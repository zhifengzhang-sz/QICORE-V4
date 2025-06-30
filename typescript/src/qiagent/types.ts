/**
 * @fileoverview QiAgent Types - Generation Request and Response Types
 * @purpose Shared types for AI generation requests and responses
 * @version 2.0.0
 */

// ============================================================================
// GENERATION REQUEST TYPES
// ============================================================================

export interface ModelConfig {
  readonly id: string;
  readonly name: string;
  readonly provider: string;
  readonly modelName: string;
  readonly temperature?: number;
  readonly maxTokens?: number;
  readonly topP?: number;
  readonly frequencyPenalty?: number;
  readonly presencePenalty?: number;
  readonly stopSequences?: string[];
}

export interface GenerationRequest {
  readonly model: ModelConfig;
  readonly systemPrompt?: string;
  readonly userPrompt: string;
  readonly context?: Record<string, unknown>;
  readonly timeout?: number;
  readonly stream?: boolean;
}

// ============================================================================
// GENERATION RESPONSE TYPES
// ============================================================================

export interface TokenUsage {
  readonly promptTokens: number;
  readonly completionTokens: number;
  readonly totalTokens: number;
  readonly cost?: number;
}

export interface GenerationResponse {
  readonly content: string;
  readonly model: string;
  readonly usage?: TokenUsage;
  readonly finishReason: string;
  readonly id?: string;
  readonly metadata?: Record<string, unknown>;
  readonly timestamp?: number;
}

// ============================================================================
// EXPORTS
// ============================================================================

export type { ModelConfig, GenerationRequest, TokenUsage, GenerationResponse };
