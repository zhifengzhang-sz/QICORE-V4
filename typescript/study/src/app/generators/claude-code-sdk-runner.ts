import { readFile, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import type { AIModel, GeneratedCode, InstructionSet } from '@/types/study';
import { type SDKMessage, query } from '@anthropic-ai/claude-code';

/**
 * Claude Code SDK Runner - Uses the TypeScript SDK for programmatic access
 *
 * This runner uses the official @anthropic-ai/claude-code TypeScript SDK
 * for better error handling, type safety, and integration compared to CLI.
 *
 * Authentication:
 * - Uses ANTHROPIC_API_KEY environment variable
 * - Supports third-party providers via environment variables:
 *   - CLAUDE_CODE_USE_BEDROCK=1 for Amazon Bedrock
 *   - CLAUDE_CODE_USE_VERTEX=1 for Google Vertex AI
 *
 * Usage:
 *   const runner = new ClaudeCodeSDKRunner();
 *   const result = await runner.generateCode(model, instruction);
 */

// Regex patterns for code extraction
const INTRO_PREFIX_PATTERN = /^Here['']s the.*?:\s*/i;
const CODE_PREFIX_PATTERN = /^The code.*?:\s*/i;
const CODE_SUFFIX_PATTERN = /\n\nThis code.*$/s;
const CODE_BLOCK_PATTERN = /```(?:haskell)?\n?([\s\S]*?)\n?```/;

export interface ClaudeCodeSDKOptions {
  maxTurns?: number;
  verbose?: boolean;
  timeout?: number;
  workingDir?: string;
  systemPrompt?: string;
  allowedTools?: string[];
}

export class ClaudeCodeSDKRunner {
  private readonly defaultOptions: ClaudeCodeSDKOptions = {
    maxTurns: 3,
    verbose: false,
    timeout: 120000, // 2 minutes
    workingDir: process.cwd(),
  };

  async generateCode(
    model: AIModel,
    instruction: InstructionSet,
    options: Partial<ClaudeCodeSDKOptions> = {}
  ): Promise<GeneratedCode> {
    const startTime = Date.now();
    const opts = { ...this.defaultOptions, ...options };

    try {
      // Validate authentication
      if (
        (process.env.ANTHROPIC_API_KEY ?? '') === '' &&
        (process.env.CLAUDE_CODE_USE_BEDROCK ?? '') === '' &&
        (process.env.CLAUDE_CODE_USE_VERTEX ?? '') === ''
      ) {
        throw new Error(
          'No authentication configured. Set ANTHROPIC_API_KEY or configure third-party provider.'
        );
      }

      // Build the comprehensive prompt
      const prompt = await this.buildPrompt(instruction);

      // Create abort controller for timeout
      const abortController = new AbortController();
      const timeoutId = setTimeout(() => abortController.abort(), opts.timeout);

      try {
        // Run Claude Code SDK query
        const result = await this.runClaudeCodeSDK(prompt, opts, abortController);
        clearTimeout(timeoutId);

        const duration = Date.now() - startTime;

        if (result.success) {
          return {
            code: this.extractCode(result.output),
            model: model.id,
            instruction: instruction.id,
            timestamp: new Date().toISOString(),
            duration,
            success: true,
            metadata: {
              provider: 'anthropic',
              temperature: model.temperature,
              maxTokens: model.maxTokens,
              note: `Claude Code SDK (maxTurns: ${opts.maxTurns}, turns: ${result.turns}, cost: $${result.cost?.toFixed(4) ?? 'unknown'})`,
            },
          };
        }

        return {
          code: '',
          model: model.id,
          instruction: instruction.id,
          timestamp: new Date().toISOString(),
          duration,
          success: false,
          error: result.error,
          metadata: {
            provider: 'anthropic',
            temperature: model.temperature,
            maxTokens: model.maxTokens,
            note: `Claude Code SDK (turns: ${result.turns ?? 'unknown'})`,
          },
        };
      } catch (error) {
        clearTimeout(timeoutId);
        throw error;
      }
    } catch (error) {
      const duration = Date.now() - startTime;
      return {
        code: '',
        model: model.id,
        instruction: instruction.id,
        timestamp: new Date().toISOString(),
        duration,
        success: false,
        error: `Claude Code SDK execution failed: ${error instanceof Error ? error.message : String(error)}`,
        metadata: {
          provider: 'anthropic',
          temperature: model.temperature,
          maxTokens: model.maxTokens,
          note: 'Claude Code SDK (error)',
        },
      };
    }
  }

  async testConnection(): Promise<{ available: boolean; version?: string; error?: string }> {
    try {
      // Test with a simple query
      const abortController = new AbortController();
      const timeoutId = setTimeout(() => abortController.abort(), 5000);

      const messages: SDKMessage[] = [];

      try {
        for await (const message of query({
          prompt: 'Hello',
          abortController,
          options: {
            maxTurns: 1,
          },
        })) {
          messages.push(message);
        }
        clearTimeout(timeoutId);

        // Find the result message
        const resultMessage = messages.find((msg) => msg.type === 'result');
        if (resultMessage && 'subtype' in resultMessage && resultMessage.subtype === 'success') {
          return {
            available: true,
            version: 'SDK',
          };
        }

        return {
          available: false,
          error: 'No successful result received',
        };
      } catch (error) {
        clearTimeout(timeoutId);
        throw error;
      }
    } catch (error) {
      return {
        available: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  private async buildPrompt(instruction: InstructionSet): Promise<string> {
    // Load the modern Haskell specification
    const specPath = join(process.cwd(), '../../docs/experiment/sources/impl/base.hs.modern.yaml');
    let modernSpec = '';

    try {
      modernSpec = await readFile(specPath, 'utf-8');
    } catch (error) {
      // eslint-disable-next-line no-console
      console.warn('Could not load modern Haskell spec:', error);
    }

    // Build comprehensive prompt
    const prompt = `
# Task: Implement QiCore Base Components in Modern Haskell

${
  modernSpec && modernSpec.length > 0
    ? `## Modern Haskell Implementation Specification:
\`\`\`yaml
${modernSpec}
\`\`\`

`
    : ''
}

   ## Instruction Set:
   **ID**: ${instruction.id}
   **Name**: ${instruction.name}

${instruction.content}

## Requirements:
1. Follow the modern Haskell specification above
2. Use GHC2021 language edition
3. Include comprehensive Haddock documentation
4. Implement Result<T> and QiError types with all required operations
5. Use modern Haskell patterns (strict fields, deriving stock, etc.)
6. Provide complete, compilable code

## Output Format:
Please provide only the Haskell code implementation. Include:
- Language pragmas
- Module declaration  
- Imports
- Type definitions
- Function implementations
- Documentation

Start your response with the complete Haskell module code.
    `.trim();

    return prompt;
  }

  private async runClaudeCodeSDK(
    prompt: string,
    options: ClaudeCodeSDKOptions,
    abortController: AbortController
  ): Promise<{
    success: boolean;
    output: string;
    error?: string;
    turns?: number;
    cost?: number;
  }> {
    try {
      const messages: SDKMessage[] = [];

      // Execute the query
      for await (const message of query({
        prompt,
        abortController,
        options: {
          maxTurns: options.maxTurns,
          cwd: options.workingDir,
          ...((options.systemPrompt ?? '') !== '' && { systemPrompt: options.systemPrompt }),
          ...(options.allowedTools && { allowedTools: options.allowedTools }),
        },
      })) {
        messages.push(message);

        if (options.verbose === true) {
          // eslint-disable-next-line no-console
          console.log('Claude Code SDK message:', message.type, message);
        }
      }

      // Find the final result message
      const resultMessage = messages.find((msg) => msg.type === 'result');

      if (!resultMessage || !('subtype' in resultMessage)) {
        return {
          success: false,
          output: '',
          error: 'No result message received from Claude Code SDK',
        };
      }

      if (resultMessage.subtype === 'success') {
        return {
          success: true,
          output: resultMessage.result,
          turns: resultMessage.num_turns,
          cost: resultMessage.total_cost_usd,
        };
      }

      // Handle error cases
      const errorType = resultMessage.subtype;
      let errorMessage = `Claude Code SDK failed with: ${errorType}`;

      if (errorType === 'error_max_turns') {
        errorMessage = `Maximum turns (${options.maxTurns}) reached without completion`;
      }

      return {
        success: false,
        output: '',
        error: errorMessage,
        turns: resultMessage.num_turns,
      };
    } catch (error) {
      if (error instanceof Error && error.name === 'AbortError') {
        return {
          success: false,
          output: '',
          error: `Claude Code SDK timed out after ${options.timeout}ms`,
        };
      }

      return {
        success: false,
        output: '',
        error: `Claude Code SDK error: ${error instanceof Error ? error.message : String(error)}`,
      };
    }
  }

  private extractCode(output: string): string {
    // Clean up common prefixes and suffixes
    let cleaned = output.trim();

    // Remove intro prefixes
    cleaned = cleaned.replace(INTRO_PREFIX_PATTERN, '');
    cleaned = cleaned.replace(CODE_PREFIX_PATTERN, '');

    // Remove trailing explanations
    cleaned = cleaned.replace(CODE_SUFFIX_PATTERN, '');

    // Try to extract from code blocks first
    const codeBlockMatch = cleaned.match(CODE_BLOCK_PATTERN);
    const extractedCode = codeBlockMatch?.[1];
    if (extractedCode != null && extractedCode.length > 0) {
      return this.cleanHaskellCode(extractedCode);
    }

    // If no code block, return the cleaned text
    return this.cleanHaskellCode(cleaned);
  }

  private cleanHaskellCode(text: string): string {
    return text
      .split('\n')
      .map((line) => line.trimEnd()) // Remove trailing whitespace
      .join('\n')
      .replace(/\n{3,}/g, '\n\n') // Normalize multiple newlines
      .trim();
  }

  async savePromptForDebugging(instruction: InstructionSet, filename?: string): Promise<string> {
    const prompt = await this.buildPrompt(instruction);
    const debugFilename = filename ?? `debug-prompt-sdk-${instruction.id}-${Date.now()}.txt`;
    const debugPath = join(process.cwd(), 'debug', debugFilename);

    try {
      await writeFile(debugPath, prompt, 'utf-8');
      return debugPath;
    } catch (error) {
      throw new Error(
        `Failed to save debug prompt: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }
}
