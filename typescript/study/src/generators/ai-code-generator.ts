import type { AIModel, GeneratedCode, InstructionSet } from '@/types/study';
import { type SDKMessage, query } from '@anthropic-ai/claude-code';

// Top-level regex patterns for performance
const HERES_PREFIX_REGEX = /^Here's the.*?:\s*/i;
const CODE_PREFIX_REGEX = /^The code.*?:\s*/i;
const CODE_SUFFIX_REGEX = /\n\nThis code.*$/s;
const CODE_BLOCK_REGEX = /```(?:haskell)?\n?([\s\S]*?)\n?```/g;

interface ClaudeCodeOptions {
  maxTurns?: number;
  systemPrompt?: string;
  outputFormat?: 'text' | 'json' | 'stream-json';
  thinking?: {
    enabled: boolean;
    budgetTokens?: number;
  };
  sessionId?: string;
  verbose?: boolean;
}

// Anthropic message content types
interface AnthropicTextBlock {
  type: 'text';
  text: string;
}

interface AnthropicMessage {
  content: string | AnthropicTextBlock[];
}

export class AICodeGenerator {
  private readonly apiKeys: Map<string, string> = new Map();

  constructor() {
    // Load API keys from environment
    const anthropicKey = process.env.ANTHROPIC_API_KEY;
    const openaiKey = process.env.OPENAI_API_KEY;

    if (anthropicKey != null && anthropicKey !== '') {
      this.apiKeys.set('anthropic', anthropicKey);
    }
    if (openaiKey != null && openaiKey !== '') {
      this.apiKeys.set('openai', openaiKey);
    }
  }

  async generateCode(
    model: AIModel,
    instruction: InstructionSet,
    options: Partial<ClaudeCodeOptions> = {}
  ): Promise<GeneratedCode> {
    const startTime = Date.now();

    try {
      if (model.provider === 'anthropic') {
        return await this.generateWithClaudeCode(model, instruction, options);
      }
      if (model.provider === 'openai') {
        return await this.generateWithOpenAI(model, instruction, options);
      }
      throw new Error(`Unsupported provider: ${model.provider}`);
    } catch (error) {
      const duration = Date.now() - startTime;
      return {
        code: '',
        model: model.id,
        instruction: instruction.id,
        timestamp: new Date().toISOString(),
        duration,
        success: false,
        error: error instanceof Error ? error.message : String(error),
        metadata: {
          provider: model.provider,
          temperature: model.temperature,
          maxTokens: model.maxTokens,
          sessionId: options.sessionId,
        },
      };
    }
  }

  private async generateWithClaudeCode(
    model: AIModel,
    instruction: InstructionSet,
    options: Partial<ClaudeCodeOptions> = {}
  ): Promise<GeneratedCode> {
    const startTime = Date.now();

    if (!this.apiKeys.has('anthropic')) {
      throw new Error('Anthropic API key not found. Set ANTHROPIC_API_KEY environment variable.');
    }

    // Build the comprehensive prompt with context
    const fullPrompt = this.buildPromptWithContext(instruction);

    const claudeOptions: ClaudeCodeOptions = {
      maxTurns: options.maxTurns ?? 3,
      outputFormat: 'json',
      thinking: {
        enabled: true,
        budgetTokens: options.thinking?.budgetTokens ?? 8000,
      },
      systemPrompt: options.systemPrompt ?? this.buildSystemPrompt(model, instruction),
      verbose: options.verbose ?? false,
      ...options,
    };

    const messages: SDKMessage[] = [];
    let reasoning = '';
    let generatedCode = '';
    let success = false;

    try {
      // Use Claude Code SDK for generation
      // eslint-disable-next-line no-undef
      const abortController = new AbortController();

      // Set timeout
      // eslint-disable-next-line no-undef
      const timeoutId = setTimeout(() => {
        abortController.abort();
      }, 60000); // 60 second timeout

      for await (const message of query({
        prompt: fullPrompt,
        abortController,
        options: claudeOptions,
      })) {
        messages.push(message);

        if (message.type === 'assistant') {
          // Extract code from assistant message
          const content = this.extractContent(message.message);
          generatedCode += content;
        } else if (message.type === 'result') {
          if (message.subtype === 'success') {
            success = true;
            generatedCode = message.result;
            break;
          }
          throw new Error(`Claude Code generation failed: ${message.subtype}`);
        }
      }

      // eslint-disable-next-line no-undef
      clearTimeout(timeoutId);

      // Extract reasoning if available
      reasoning = this.extractReasoning(messages);

      const duration = Date.now() - startTime;

      return {
        code: this.cleanGeneratedCode(generatedCode),
        model: model.id,
        instruction: instruction.id,
        timestamp: new Date().toISOString(),
        duration,
        success,
        metadata: {
          provider: model.provider,
          temperature: model.temperature,
          maxTokens: model.maxTokens,
          reasoning,
          messageCount: messages.length,
          sessionId: options.sessionId,
          thinkingTokens: claudeOptions.thinking?.budgetTokens,
        },
      };
    } catch (error) {
      const duration = Date.now() - startTime;
      return {
        code: '',
        model: model.id,
        instruction: instruction.id,
        timestamp: new Date().toISOString(),
        duration,
        success: false,
        error: `Claude Code generation failed: ${error instanceof Error ? error.message : String(error)}`,
        metadata: {
          provider: model.provider,
          temperature: model.temperature,
          maxTokens: model.maxTokens,
          messageCount: messages.length,
          sessionId: options.sessionId,
        },
      };
    }
  }

  private async generateWithOpenAI(
    model: AIModel,
    instruction: InstructionSet,
    _options: Partial<ClaudeCodeOptions> = {}
  ): Promise<GeneratedCode> {
    const startTime = Date.now();

    if (!this.apiKeys.has('openai')) {
      throw new Error('OpenAI API key not found. Set OPENAI_API_KEY environment variable.');
    }

    // For now, implement a basic OpenAI integration
    // In production, you might want to use OpenAI SDK directly
    const _prompt = this.buildPromptWithContext(instruction);

    // Mock implementation - replace with actual OpenAI SDK calls
    const generatedCode = `// Generated with ${model.name}\n// Instruction: ${instruction.name}\n\nmodule MockGenerated where\n\n-- This is a mock implementation\nmockFunction :: String -> String\nmockFunction input = "Mock result for: " ++ input`;

    const duration = Date.now() - startTime;

    return {
      code: generatedCode,
      model: model.id,
      instruction: instruction.id,
      timestamp: new Date().toISOString(),
      duration,
      success: true,
      metadata: {
        provider: model.provider,
        temperature: model.temperature,
        maxTokens: model.maxTokens,
        note: 'Mock implementation - replace with actual OpenAI SDK',
      },
    };
  }

  private buildSystemPrompt(model: AIModel, instruction: InstructionSet): string {
    return `You are an expert Haskell developer using modern 2024-2025 best practices.

Key Requirements:
- Use GHC2021 language edition
- Apply modern Haskell extensions where appropriate
- Write clean, well-documented code with Haddock comments
- Include proper type signatures
- Follow modern error handling patterns (Maybe, Either)
- Use strict fields for performance
- Apply deriving strategies appropriately

Context: You are generating code for an AI consistency study.
Model: ${model.name}
Provider: ${model.provider}
Instruction Category: ${instruction.category}

Generate production-quality Haskell code that demonstrates best practices.
Focus on correctness, clarity, and modern idioms.`;
  }

  private buildPromptWithContext(instruction: InstructionSet): string {
    const timestamp = new Date().toISOString();

    return `${instruction.content}

Additional Context:
- Target: Modern Haskell (GHC 9.6+)
- Language Edition: GHC2021
- Focus: Production-quality code with comprehensive documentation
- Timestamp: ${timestamp}
- Study: AI Code Generation Consistency Research

Please generate the requested Haskell code following the instructions exactly.
Include proper module declarations, imports, type signatures, and documentation.`;
  }

  private extractContent(message: AnthropicMessage): string {
    // Extract content from Anthropic message format
    if (typeof message.content === 'string') {
      return message.content;
    }

    if (Array.isArray(message.content)) {
      return message.content
        .filter((block: AnthropicTextBlock) => block.type === 'text')
        .map((block: AnthropicTextBlock) => block.text)
        .join('\n');
    }

    return '';
  }

  private extractReasoning(messages: SDKMessage[]): string {
    const reasoningMessages = messages.filter(
      (msg) => msg.type === 'assistant' && Object.prototype.hasOwnProperty.call(msg, 'reasoning')
    );

    return reasoningMessages
      .map((msg) => {
        // Cast to unknown first, then check type safely
        const unknownMsg = msg as unknown;
        if (typeof unknownMsg === 'object' && unknownMsg !== null && 'reasoning' in unknownMsg) {
          const { reasoning } = unknownMsg as { reasoning: unknown };
          return typeof reasoning === 'string' ? reasoning : '';
        }
        return '';
      })
      .join('\n');
  }

  private cleanGeneratedCode(code: string): string {
    // Remove markdown code blocks if present
    const match = CODE_BLOCK_REGEX.exec(code);

    if (match) {
      return match[1].trim();
    }

    // Remove common prefixes/suffixes that AI models sometimes add
    return code
      .replace(HERES_PREFIX_REGEX, '')
      .replace(CODE_PREFIX_REGEX, '')
      .replace(CODE_SUFFIX_REGEX, '')
      .trim();
  }

  // Test connection to verify API keys and model availability
  async testConnection(model: AIModel): Promise<boolean> {
    try {
      const testInstruction: InstructionSet = {
        id: 'test',
        name: 'Connection Test',
        filePath: '',
        content: 'Write a simple Haskell function that returns "Hello, World!"',
        category: 'simple',
      };

      const result = await this.generateCode(model, testInstruction, {
        maxTurns: 1,
        thinking: { enabled: false },
      });

      return result.success && result.code.length > 0;
    } catch (error) {
      // eslint-disable-next-line no-console
      console.error(`Connection test failed for ${model.id}:`, error);
      return false;
    }
  }

  // Get available models based on configured API keys
  getAvailableModels(): string[] {
    const models: string[] = [];

    if (this.apiKeys.has('anthropic')) {
      models.push('claude-3-5-sonnet', 'claude-3-7-sonnet', 'claude-4-sonnet');
    }

    if (this.apiKeys.has('openai')) {
      models.push('gpt-4-turbo', 'gpt-4o', 'gpt-4');
    }

    return models;
  }
}
