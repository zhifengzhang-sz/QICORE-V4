import type { ClaudeSdkOptions, ClaudeResponse } from '@/types';
import { ExecutionError } from '@/types';

// Import types for Claude Code SDK (will be installed via npm)
interface ClaudeCodeOptions {
  cwd: string | undefined;
  maxTurns: number | undefined;
  systemPrompt: string | undefined;
  allowedTools: string[] | undefined;
  permissionMode: 'strict' | 'permissive' | undefined;
}

interface QueryOptions {
  prompt: string;
  options?: ClaudeCodeOptions;
}

// Mock the SDK import for now - will be replaced with actual import
// import { query } from '@anthropic-ai/claude-code';
const query = async function* (config: QueryOptions): AsyncGenerator<string> {
  // This is a placeholder - replace with actual SDK when available
  yield `Mock response for: ${config.prompt.substring(0, 100)}...`;
  throw new Error('Claude Code SDK not yet available - please use CLI method');
};

/**
 * Manager for Claude Code SDK interactions
 */
export class ClaudeSdkManager {
  constructor(private options: ClaudeSdkOptions) {
    // Validate options
    if (!options.workingDir) {
      throw new ExecutionError('Working directory is required');
    }
  }

  /**
   * Execute a query using Claude Code SDK
   */
  async executeQuery(instruction: string): Promise<ClaudeResponse> {
    const startTime = Date.now();

    try {
      const options = this.buildSdkOptions();
      const messages: string[] = [];

      if (this.options.verbose) {
        // eslint-disable-next-line no-console
        console.log('Executing SDK query with options:', options);
        // eslint-disable-next-line no-console
        console.log('Instruction preview:', `${instruction.substring(0, 200)}...`);
      }

      // Execute the query and collect all messages
      for await (const message of query({
        prompt: instruction,
        options,
      })) {
        messages.push(message);

        if (this.options.verbose) {
          // eslint-disable-next-line no-console
          console.log('Received message chunk:', `${message.substring(0, 100)}...`);
        }
      }

      const executionTime = Date.now() - startTime;
      const content = messages.join('\n');

      return {
        success: true,
        content,
        error: null,
        messages,
        metadata: {
          executionTime,
          method: 'sdk',
          messageCount: messages.length,
          options: this.options,
        },
      };
    } catch (error) {
      const executionTime = Date.now() - startTime;

      return {
        success: false,
        content: null,
        error: error instanceof Error ? error.message : String(error),
        messages: [],
        metadata: {
          executionTime,
          method: 'sdk',
          failed: true,
          error: error instanceof Error ? error.name : 'UnknownError',
        },
      };
    }
  }

  /**
   * Build SDK options from manager configuration
   */
  private buildSdkOptions(): ClaudeCodeOptions {
    return {
      cwd: this.options.workingDir,
      maxTurns: this.options.maxTurns ?? 10,
      systemPrompt: this.options.systemPrompt,
              allowedTools: this.options.allowedTools ?? [
        'Read',
        'Write',
        'Edit',
        'MultiEdit',
        'Glob',
        'Grep',
        'Bash',
        'LS',
      ],
              permissionMode: this.options.permissionMode ?? 'permissive',
    };
  }

  /**
   * Test SDK availability and configuration
   */
  async testConnection(): Promise<boolean> {
    try {
      // Simple test query to verify SDK is working
      const testQuery = 'Test SDK connection - please respond with "OK"';
      const result = await this.executeQuery(testQuery);

      return result.success && (result.content?.includes('OK') ?? false);
    } catch (error) {
      if (this.options.verbose) {
        console.error('Claude Code SDK not available:', error);
      }
      return false;
    }
  }

  /**
   * Get SDK status and configuration info
   */
  async getStatus(): Promise<{
    available: boolean;
    workingDir: string;
    configuration: ClaudeSdkOptions;
    sdkVersion: string | undefined;
  }> {
    const available = await this.testConnection();

    // Try to get SDK version info
    let sdkVersion: string | undefined;
    try {
      // This would need to be implemented based on actual SDK
      sdkVersion = 'SDK version detection not implemented';
    } catch {
      // Version detection failed
    }

    return {
      available,
      workingDir: this.options.workingDir,
      configuration: this.options,
      sdkVersion,
    };
  }

  /**
   * Execute query with streaming response handling
   */
  async *executeQueryStreaming(instruction: string): AsyncGenerator<{
    chunk: string;
    accumulated: string;
    metadata?: Record<string, unknown>;
  }> {
    const options = this.buildSdkOptions();
    let accumulated = '';

    try {
      for await (const message of query({
        prompt: instruction,
        options,
      })) {
        accumulated += `${message}\n`;

        yield {
          chunk: message,
          accumulated,
          metadata: {
            method: 'sdk',
            streaming: true,
            timestamp: new Date().toISOString(),
          },
        };
      }
    } catch (error) {
      yield {
        chunk: '',
        accumulated,
        metadata: {
          method: 'sdk',
          streaming: true,
          error: error instanceof Error ? error.message : String(error),
          failed: true,
        },
      };
    }
  }

  /**
   * Execute multiple queries in parallel (for batch operations)
   */
  async executeQueryBatch(instructions: string[]): Promise<ClaudeResponse[]> {
    const promises = instructions.map((instruction) => this.executeQuery(instruction));

    try {
      return await Promise.all(promises);
    } catch (error) {
      throw new ExecutionError(
        'Batch execution failed',
        error instanceof Error ? error.message : String(error)
      );
    }
  }
}
