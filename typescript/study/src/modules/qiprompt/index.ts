/**
 * @fileoverview QiPrompt - Prompt Engineering Component
 * @purpose Handles all prompt construction, template processing, and content extraction
 * @dependencies QiCore Base, Study Types
 */

import { createQiError } from '@/qicore/base/error';
import { type Result, failure, getError, isFailure, success } from '@/qicore/base/result';
import type { AIModel, InstructionSet } from '../types';

// ============================================================================
// REGEX PATTERNS (Top-level for performance)
// ============================================================================

const CODE_BLOCK_REGEX = /```(?:\w+)?\n?([\s\S]*?)```/g;
const CODE_BLOCK_START_REGEX = /```\w*\n?/;
const CODE_BLOCK_END_REGEX = /```$/;
const REASONING_PATTERNS = [
  /(?:thinking|reasoning|approach|strategy):\s*(.*?)(?:\n\n|\n(?=[A-Z])|$)/is,
  /(?:my approach|i will|let me|first).*?(?:\n\n|\n(?=[A-Z])|$)/is,
];
const LEADING_NEWLINES_REGEX = /^\s*\n+/;
const TRAILING_NEWLINES_REGEX = /\n+\s*$/;
const TABS_REGEX = /\t/g;

// ============================================================================
// PROMPT BUILDING
// ============================================================================

/**
 * Build system prompt for AI model
 * @pure
 */
export const buildSystemPrompt = (model: AIModel, instruction: InstructionSet): string => {
  const basePrompt = `You are an expert ${instruction.language} programmer. 
Your task is to generate high-quality, well-documented code that follows modern best practices.

Model: ${model.name} (${model.provider})
Language: ${instruction.language}
Complexity: ${instruction.complexity}
Tags: ${instruction.tags.join(', ')}

Requirements:
- Generate clean, readable, and well-documented code
- Follow modern ${instruction.language} conventions
- Include appropriate error handling
- Add meaningful comments and documentation
- Ensure code is production-ready and maintainable`;

  return basePrompt;
};

/**
 * Build full prompt with instruction context
 * @pure
 */
export const buildPromptWithContext = (
  instruction: InstructionSet
): string => `${instruction.content}

Please ensure your response:
1. Contains only the requested code
2. Follows the specified requirements
3. Is production-ready and well-documented
4. Uses modern ${instruction.language} best practices`;

// ============================================================================
// CONTENT EXTRACTION
// ============================================================================

/**
 * Extract code content from AI response messages
 * @pure
 */
export const extractCodeFromResponse = (responseContent: string): string => {
  // Remove markdown code blocks if present
  const matches = responseContent.match(CODE_BLOCK_REGEX);

  if (matches && matches.length > 0) {
    // Take the first code block and clean it
    return matches[0].replace(CODE_BLOCK_START_REGEX, '').replace(CODE_BLOCK_END_REGEX, '').trim();
  }

  // If no code blocks, return the content as-is but cleaned
  return responseContent.trim();
};

/**
 * Extract reasoning/thinking from AI response
 * @pure
 */
export const extractReasoning = (responseContent: string): string => {
  // Look for common reasoning patterns
  for (const pattern of REASONING_PATTERNS) {
    const match = responseContent.match(pattern);
    if (match) {
      return match[1]?.trim() || match[0]?.trim() || '';
    }
  }

  return '';
};

/**
 * Clean and normalize generated code
 * @pure
 */
export const cleanGeneratedCode = (code: string): string =>
  code
    .replace(LEADING_NEWLINES_REGEX, '') // Remove leading newlines
    .replace(TRAILING_NEWLINES_REGEX, '') // Remove trailing newlines
    .replace(TABS_REGEX, '  ') // Convert tabs to spaces
    .trim();

// ============================================================================
// VALIDATION
// ============================================================================

/**
 * Validate AI model for prompt generation
 * @pure
 */
export const validateModelForPrompt = (model: AIModel): Result<AIModel> => {
  if (!model.id?.trim()) {
    return failure(
      createQiError('VALIDATION_ERROR', 'Model ID must be non-empty string', 'VALIDATION', {
        field: 'model.id',
        value: model.id,
      })
    );
  }

  if (!model.modelName?.trim()) {
    return failure(
      createQiError('VALIDATION_ERROR', 'Model name must be non-empty string', 'VALIDATION', {
        field: 'model.modelName',
        value: model.modelName,
      })
    );
  }

  if (model.temperature !== undefined && (model.temperature < 0 || model.temperature > 2)) {
    return failure(
      createQiError('VALIDATION_ERROR', 'Temperature must be between 0 and 2', 'VALIDATION', {
        field: 'model.temperature',
        value: model.temperature,
      })
    );
  }

  return success(model);
};

/**
 * Validate instruction set for prompt generation
 * @pure
 */
export const validateInstructionForPrompt = (
  instruction: InstructionSet
): Result<InstructionSet> => {
  if (!instruction.id?.trim()) {
    return failure(
      createQiError('VALIDATION_ERROR', 'Instruction ID must be non-empty string', 'VALIDATION', {
        field: 'instruction.id',
        value: instruction.id,
      })
    );
  }

  if (!instruction.content?.trim()) {
    return failure(
      createQiError(
        'VALIDATION_ERROR',
        'Instruction content must be non-empty string',
        'VALIDATION',
        { field: 'instruction.content', value: instruction.content }
      )
    );
  }

  if (!instruction.language?.trim()) {
    return failure(
      createQiError(
        'VALIDATION_ERROR',
        'Instruction language must be non-empty string',
        'VALIDATION',
        { field: 'instruction.language', value: instruction.language }
      )
    );
  }

  return success(instruction);
};

// ============================================================================
// PROMPT CONFIGURATION
// ============================================================================

export interface PromptConfig {
  readonly includeSystemPrompt: boolean;
  readonly includeReasoningRequest: boolean;
  readonly maxPromptLength: number;
  readonly templateVariables: Record<string, string>;
}

export const createDefaultPromptConfig = (): PromptConfig =>
  Object.freeze({
    includeSystemPrompt: true,
    includeReasoningRequest: true,
    maxPromptLength: 8000,
    templateVariables: Object.freeze({}),
  }) as PromptConfig;

// ============================================================================
// COMPLETE PROMPT BUILDER
// ============================================================================

/**
 * Build complete prompt for AI model
 * @pure
 */
export const buildCompletePrompt = (
  model: AIModel,
  instruction: InstructionSet,
  config: PromptConfig = createDefaultPromptConfig()
): Result<{ systemPrompt?: string; userPrompt: string }> => {
  const modelValidation = validateModelForPrompt(model);
  if (isFailure(modelValidation)) {
    const error = getError(modelValidation);
    return failure(
      error ?? createQiError('VALIDATION_ERROR', 'Model validation failed', 'VALIDATION')
    );
  }

  const instructionValidation = validateInstructionForPrompt(instruction);
  if (isFailure(instructionValidation)) {
    const error = getError(instructionValidation);
    return failure(
      error ?? createQiError('VALIDATION_ERROR', 'Instruction validation failed', 'VALIDATION')
    );
  }

  const systemPrompt = config.includeSystemPrompt
    ? buildSystemPrompt(model, instruction)
    : undefined;

  const userPrompt = buildPromptWithContext(instruction);

  return success({
    systemPrompt,
    userPrompt,
  });
};

// ============================================================================
// EXPORTS
// ============================================================================

export const QiPrompt = Object.freeze({
  // Building
  buildSystemPrompt,
  buildPromptWithContext,
  buildCompletePrompt,

  // Extraction
  extractCodeFromResponse,
  extractReasoning,
  cleanGeneratedCode,

  // Validation
  validateModelForPrompt,
  validateInstructionForPrompt,

  // Configuration
  createDefaultPromptConfig,
} as const);
