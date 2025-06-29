import { exec } from 'node:child_process';
import { promisify } from 'node:util';
import type { ClaudeCliOptions, ClaudeResponse } from '@/types';
import { ExecutionError } from '@/types';

const execAsync = promisify(exec);

/**
 * Manager for Claude Code CLI interactions
 */
export class ClaudeCliManager {
  constructor(private options: ClaudeCliOptions) {
    // Validate options
    if (!options.workingDir) {
      throw new ExecutionError('Working directory is required');
    }
  }

  /**
   * Execute a query using Claude Code CLI
   */
  async executeQuery(instruction: string): Promise<ClaudeResponse> {
    const startTime = Date.now();

    try {
      const flags = this.buildFlags();
      const escapedInstruction = this.escapeInstruction(instruction);

      // Build the complete command
      const command = this.buildCommand(escapedInstruction, flags);

      if (this.options.verbose) {
        // Verbose logging when explicitly enabled
        // eslint-disable-next-line no-console
        console.log(`Executing CLI command: ${command}`);
      }

      const { stdout, stderr } = await execAsync(command, {
        cwd: this.options.workingDir,
        maxBuffer: 10 * 1024 * 1024, // 10MB buffer for large responses
        timeout: 300000, // 5 minute timeout
      });

      const executionTime = Date.now() - startTime;

      // Parse response based on output format
      const content = this.parseResponse(stdout);

      return {
        success: true,
        content: typeof content === 'string' ? content : JSON.stringify(content),
        error: stderr || null,
        metadata: {
          executionTime,
          method: 'cli',
          outputFormat: this.options.outputFormat ?? 'text',
        },
      };
    } catch (error) {
      const executionTime = Date.now() - startTime;

      return {
        success: false,
        content: null,
        error: error instanceof Error ? error.message : String(error),
        metadata: {
          executionTime,
          method: 'cli',
          failed: true,
        },
      };
    }
  }

  /**
   * Build CLI command with proper working directory and flags
   */
  private buildCommand(instruction: string, flags: string): string {
    const baseCommand = `claude -p "${instruction}"`;
    return flags ? `${baseCommand} ${flags}` : baseCommand;
  }

  /**
   * Build CLI flags from options
   */
  private buildFlags(): string {
    const flags: string[] = [];

    if (this.options.outputFormat) {
      flags.push(`--output-format ${this.options.outputFormat}`);
    }

    if (this.options.model) {
      flags.push(`--model ${this.options.model}`);
    }

    if (this.options.maxTurns) {
      flags.push(`--max-turns ${this.options.maxTurns}`);
    }

    if (this.options.verbose) {
      flags.push('--verbose');
    }

    if (this.options.addDirs && this.options.addDirs.length > 0) {
      const dirs = this.options.addDirs.join(' ');
      flags.push(`--add-dir ${dirs}`);
    }

    return flags.join(' ');
  }

  /**
   * Escape instruction for shell execution
   */
  private escapeInstruction(instruction: string): string {
    return instruction
      .replace(/\\/g, '\\\\') // Escape backslashes
      .replace(/"/g, '\\"') // Escape quotes
      .replace(/\$/g, '\\$') // Escape dollar signs
      .replace(/`/g, '\\`') // Escape backticks
      .replace(/\n/g, '\\n') // Escape newlines
      .replace(/\r/g, '\\r'); // Escape carriage returns
  }

  /**
   * Parse response based on output format
   */
  private parseResponse(stdout: string): string | object {
    if (this.options.outputFormat === 'json' || this.options.outputFormat === 'stream-json') {
      try {
        return JSON.parse(stdout);
              } catch {
        // If JSON parsing fails, return as text with warning
        console.warn('Failed to parse JSON response, returning as text');
        return stdout;
      }
    }

    return stdout;
  }

  /**
   * Test CLI connection and availability
   */
  async testConnection(): Promise<boolean> {
    try {
      const { stdout } = await execAsync('claude --version', {
        cwd: this.options.workingDir,
        timeout: 10000, // 10 second timeout for version check
      });

      if (this.options.verbose) {
        // eslint-disable-next-line no-console
        console.log(`Claude Code CLI version: ${stdout.trim()}`);
      }

      return true;
    } catch (error) {
      if (this.options.verbose) {
        console.error('Claude Code CLI not available:', error);
      }
      return false;
    }
  }

  /**
   * Get CLI status and configuration info
   */
  async getStatus(): Promise<{
    available: boolean;
    version: string | undefined;
    workingDir: string;
    configuration: ClaudeCliOptions;
  }> {
    const available = await this.testConnection();
    let version: string | undefined;

    if (available) {
      try {
        const { stdout } = await execAsync('claude --version', {
          cwd: this.options.workingDir,
          timeout: 5000,
        });
        version = stdout.trim();
      } catch {
        // Version retrieval failed, but CLI is available
      }
    }

    return {
      available,
      version,
      workingDir: this.options.workingDir,
      configuration: this.options,
    };
  }
}
