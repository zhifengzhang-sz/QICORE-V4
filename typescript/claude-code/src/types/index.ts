import { z } from 'zod';

// Base response schema
export const ClaudeResponseSchema = z.object({
  success: z.boolean(),
  content: z.string().nullable(),
  error: z.string().nullable(),
  messages: z.array(z.string()).optional(),
  metadata: z.record(z.unknown()).optional(),
});

export type ClaudeResponse = z.infer<typeof ClaudeResponseSchema>;

// Instruction configuration schema
export const ClaudeInstructionSchema = z.object({
  workingDir: z.string(),
  knowledgeUpdate: z.array(z.string()),
  actionInstruction: z.string(),
  qualityStandards: z.array(z.string()),
  validationCriteria: z.array(z.string()),
  context: z.record(z.unknown()).optional(),
});

export type ClaudeInstruction = z.infer<typeof ClaudeInstructionSchema>;

// CLI options schema
export const ClaudeCliOptionsSchema = z.object({
  workingDir: z.string(),
  outputFormat: z.enum(['text', 'json', 'stream-json']).optional(),
  model: z.enum(['sonnet', 'opus', 'haiku']).optional(),
  maxTurns: z.number().positive().optional(),
  verbose: z.boolean().optional(),
  addDirs: z.array(z.string()).optional(),
});

export type ClaudeCliOptions = z.infer<typeof ClaudeCliOptionsSchema>;

// SDK options schema
export const ClaudeSdkOptionsSchema = z.object({
  workingDir: z.string(),
  maxTurns: z.number().positive().optional(),
  systemPrompt: z.string().optional(),
  allowedTools: z.array(z.string()).optional(),
  verbose: z.boolean().optional(),
  permissionMode: z.enum(['strict', 'permissive']).optional(),
});

export type ClaudeSdkOptions = z.infer<typeof ClaudeSdkOptionsSchema>;

// Request manager config schema
export const RequestManagerConfigSchema = z.object({
  method: z.enum(['cli', 'sdk']),
  workingDir: z.string(),
  outputFormat: z.enum(['text', 'json']).optional(),
  model: z.enum(['sonnet', 'opus', 'haiku']).optional(),
  maxTurns: z.number().positive().optional(),
  verbose: z.boolean().optional(),
  systemPrompt: z.string().optional(),
  allowedTools: z.array(z.string()).optional(),
});

export type RequestManagerConfig = z.infer<typeof RequestManagerConfigSchema>;

// Execution result schema
export const ExecutionResultSchema = z.object({
  instruction: ClaudeInstructionSchema,
  response: ClaudeResponseSchema,
  executionTime: z.number(),
  timestamp: z.string(),
  method: z.enum(['cli', 'sdk']),
});

export type ExecutionResult = z.infer<typeof ExecutionResultSchema>;

// Validation result schema
export const ValidationResultSchema = z.object({
  passed: z.boolean(),
  criteria: z.string(),
  details: z.string().optional(),
  score: z.number().min(0).max(1).optional(),
});

export type ValidationResult = z.infer<typeof ValidationResultSchema>;

// Test result schema
export const TestResultSchema = z.object({
  testName: z.string(),
  instruction: ClaudeInstructionSchema,
  executionResult: ExecutionResultSchema,
  validationResults: z.array(ValidationResultSchema),
  overallScore: z.number().min(0).max(1),
  success: z.boolean(),
  generatedFiles: z.array(z.string()).optional(),
  notes: z.string().optional(),
});

export type TestResult = z.infer<typeof TestResultSchema>;

// Error types
export class ClaudeCodeError extends Error {
  readonly code: string;
  readonly details?: unknown;

  constructor(message: string, code: string, details?: unknown) {
    super(message);
    this.name = 'ClaudeCodeError';
    this.code = code;
    this.details = details;
  }
}

export class ValidationError extends ClaudeCodeError {
  constructor(message: string, details?: unknown) {
    super(message, 'VALIDATION_ERROR', details);
    this.name = 'ValidationError';
  }
}

export class ExecutionError extends ClaudeCodeError {
  constructor(message: string, details?: unknown) {
    super(message, 'EXECUTION_ERROR', details);
    this.name = 'ExecutionError';
  }
}
