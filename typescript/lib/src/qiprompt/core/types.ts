/**
 * @fileoverview QiPrompt v4.0 - Core Types and Interfaces
 * @purpose Modern type definitions for prompt engineering with LangChain integration
 * @dependencies QiCore Base (Result<T>), LangChain Core
 * @version 4.0
 * @date December 30, 2025
 * @status Production-ready implementation
 */

import type { BaseMessage } from "@langchain/core/messages";
import type { ChatPromptTemplate, PromptTemplate } from "@langchain/core/prompts";
import type { Result } from "../../qicore/base/result.js";

// ============================================================================
// CORE PROMPT ENGINEERING TYPES
// ============================================================================

/**
 * Supported LLM Providers for prompt execution
 */
export type LLMProvider = "openai" | "anthropic" | "google" | "azure" | "local" | "custom";

/**
 * Model configuration for LLM interaction
 */
export interface ModelConfig {
  readonly provider: LLMProvider;
  readonly modelName: string;
  readonly temperature?: number;
  readonly maxTokens?: number;
  readonly topP?: number;
  readonly frequencyPenalty?: number;
  readonly presencePenalty?: number;
  readonly apiKey?: string;
  readonly baseURL?: string;
  readonly timeout?: number;
}

/**
 * Template variable with type information and validation
 */
export interface TemplateVariable {
  readonly name: string;
  readonly type: "string" | "number" | "boolean" | "array" | "object";
  readonly required: boolean;
  readonly description?: string;
  readonly defaultValue?: unknown;
  readonly validation?: RegExp | ((value: unknown) => boolean);
}

/**
 * Prompt template metadata and configuration
 */
export interface PromptMetadata {
  readonly id: string;
  readonly name: string;
  readonly description: string;
  readonly version: string;
  readonly author?: string;
  readonly category: string;
  readonly tags: readonly string[];
  readonly language: string;
  readonly complexity: "simple" | "moderate" | "complex" | "expert";
  readonly createdAt: Date;
  readonly updatedAt: Date;
}

// ============================================================================
// PROMPT PATTERNS AND TECHNIQUES
// ============================================================================

/**
 * Chain-of-thought prompting configuration
 */
export interface ChainOfThoughtConfig {
  readonly enabled: boolean;
  readonly steps: readonly string[];
  readonly reasoning: "explicit" | "implicit" | "guided";
  readonly examples?: readonly string[];
}

/**
 * Few-shot learning configuration
 */
export interface FewShotConfig {
  readonly enabled: boolean;
  readonly examples: readonly {
    readonly input: string;
    readonly output: string;
    readonly explanation?: string;
  }[];
  readonly maxExamples: number;
  readonly selectionStrategy: "random" | "similarity" | "manual";
}

/**
 * Advanced prompt engineering techniques
 */
export interface PromptTechniques {
  readonly chainOfThought?: ChainOfThoughtConfig;
  readonly fewShot?: FewShotConfig;
  readonly rolePlay?: {
    readonly persona: string;
    readonly expertise: string;
    readonly communication: string;
  };
  readonly constraints?: readonly string[];
  readonly outputFormat?: {
    readonly type: "json" | "xml" | "markdown" | "code" | "structured";
    readonly schema?: string;
  };
}

// ============================================================================
// PROMPT COMPOSITION AND EXECUTION
// ============================================================================

/**
 * Input for prompt composition and execution
 */
export interface PromptInput {
  readonly template: string | PromptTemplate | ChatPromptTemplate;
  readonly variables: Record<string, unknown>;
  readonly metadata?: Partial<PromptMetadata>;
  readonly techniques?: PromptTechniques;
  readonly context?: {
    readonly conversation?: readonly BaseMessage[];
    readonly systemContext?: string;
    readonly userContext?: string;
  };
}

/**
 * Result of prompt execution with rich metadata
 */
export interface PromptExecutionResult {
  readonly id: string;
  readonly prompt: {
    readonly formatted: string;
    readonly messages?: readonly BaseMessage[];
    readonly tokens: number;
  };
  readonly response: {
    readonly content: string;
    readonly tokens: number;
    readonly finishReason: string;
    readonly model: string;
  };
  readonly performance: {
    readonly latency: number;
    readonly tokensPerSecond: number;
    readonly cost?: number;
  };
  readonly metadata: {
    readonly timestamp: Date;
    readonly provider: LLMProvider;
    readonly version: string;
    readonly techniques: readonly string[];
  };
}

/**
 * Prompt validation result with detailed feedback
 */
export interface PromptValidationResult {
  readonly isValid: boolean;
  readonly errors: readonly {
    readonly field: string;
    readonly message: string;
    readonly severity: "error" | "warning" | "info";
  }[];
  readonly warnings: readonly string[];
  readonly suggestions: readonly string[];
  readonly metrics: {
    readonly tokenCount: number;
    readonly complexity: number;
    readonly readability: number;
  };
}

// ============================================================================
// TEMPLATE SYSTEM TYPES
// ============================================================================

/**
 * Template engine configuration
 */
export interface TemplateEngineConfig {
  readonly syntax: "mustache" | "handlebars" | "jinja2" | "custom";
  readonly strictMode: boolean;
  readonly autoEscape: boolean;
  readonly customHelpers?: Record<string, (...args: unknown[]) => unknown>;
  readonly partials?: Record<string, string>;
}

/**
 * Compiled template with optimized execution
 */
export interface CompiledTemplate {
  readonly id: string;
  readonly source: string;
  readonly variables: readonly TemplateVariable[];
  readonly render: (variables: Record<string, unknown>) => Promise<Result<string>>;
  readonly validate: (variables: Record<string, unknown>) => Result<void>;
  readonly metadata: PromptMetadata;
}

// ============================================================================
// PROMPT MANAGEMENT TYPES
// ============================================================================

/**
 * Prompt versioning and deployment
 */
export interface PromptVersion {
  readonly version: string;
  readonly template: CompiledTemplate;
  readonly changelog: string;
  readonly status: "draft" | "testing" | "production" | "deprecated";
  readonly metrics?: {
    readonly usage: number;
    readonly successRate: number;
    readonly avgLatency: number;
    readonly feedback: number;
  };
}

/**
 * A/B testing configuration for prompts
 */
export interface PromptExperiment {
  readonly id: string;
  readonly name: string;
  readonly variants: readonly {
    readonly name: string;
    readonly template: CompiledTemplate;
    readonly weight: number;
  }[];
  readonly criteria: {
    readonly metric: string;
    readonly threshold: number;
    readonly confidence: number;
  };
  readonly status: "running" | "completed" | "paused";
}

// ============================================================================
// ADAPTER LAYER TYPES
// ============================================================================

/**
 * LangChain integration adapter interface
 */
export interface LangChainAdapter {
  readonly createPromptTemplate: (input: PromptInput) => Result<PromptTemplate>;
  readonly createChatTemplate: (input: PromptInput) => Result<ChatPromptTemplate>;
  readonly formatMessages: (
    template: ChatPromptTemplate,
    variables: Record<string, unknown>
  ) => Result<BaseMessage[]>;
  readonly validateTemplate: (template: PromptTemplate | ChatPromptTemplate) => Result<void>;
}

/**
 * QiCore integration facade interface
 */
export interface QiPromptFacade {
  readonly compile: (input: PromptInput) => Result<CompiledTemplate>;
  readonly execute: (
    template: CompiledTemplate,
    model: ModelConfig
  ) => Promise<Result<PromptExecutionResult>>;
  readonly validate: (input: PromptInput) => Result<PromptValidationResult>;
  readonly createExperiment: (
    config: Omit<PromptExperiment, "id" | "status">
  ) => Result<PromptExperiment>;
  readonly getVersion: (templateId: string, version?: string) => Result<PromptVersion>;
}

// ============================================================================
// SAFETY AND GUARDRAILS
// ============================================================================

/**
 * Prompt safety configuration
 */
export interface SafetyConfig {
  readonly enableContentFilter: boolean;
  readonly enableInjectionDetection: boolean;
  readonly maxPromptLength: number;
  readonly allowedDomains?: readonly string[];
  readonly blockedTerms?: readonly string[];
  readonly rateLimit?: {
    readonly requests: number;
    readonly window: number;
  };
}

/**
 * Safety check result
 */
export interface SafetyCheckResult {
  readonly safe: boolean;
  readonly violations: readonly {
    readonly type: "content" | "injection" | "length" | "rate" | "domain";
    readonly severity: "low" | "medium" | "high" | "critical";
    readonly message: string;
    readonly suggestion?: string;
  }[];
  readonly confidence: number;
}
