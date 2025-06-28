import { spawn } from 'node:child_process';
import { existsSync } from 'node:fs';
import { readFile, writeFile } from 'node:fs/promises';
import { homedir } from 'node:os';
import { join } from 'node:path';
import type { AIModel, GeneratedCode, InstructionSet } from '@/types/study';

// Global types for Node.js timer functions
declare const setTimeout: (callback: () => void, ms: number) => NodeJS.Timeout;
declare const clearTimeout: (timeoutId: NodeJS.Timeout) => void;

/**
 * Claude Code Runner - executes Claude Code CLI to generate Haskell implementations
 *
 * Binary Discovery:
 * 1. Uses CLAUDE_CODE_PATH environment variable if set
 * 2. Searches common installation locations:
 *    - ~/.claude/local/claude (default local install)
 *    - ~/.local/bin/claude
 *    - /usr/local/bin/claude
 *    - /usr/bin/claude
 *    - ~/.npm-global/bin/claude
 *    - ~/.yarn/bin/claude
 * 3. Falls back to 'claude' command (if in PATH)
 *
 * Usage:
 *   // Use default discovery
 *   const runner = new ClaudeCodeRunner();
 *
 *   // Or set custom path via environment
 *   process.env.CLAUDE_CODE_PATH = '/custom/path/to/claude';
 */

// Regex patterns for code extraction (moved to top level for performance)
const INTRO_PREFIX_PATTERN = /^Here['']s the.*?:\s*/i;
const CODE_PREFIX_PATTERN = /^The code.*?:\s*/i;
const CODE_SUFFIX_PATTERN = /\n\nThis code.*$/s;
const CODE_BLOCK_PATTERN = /```(?:haskell)?\n?([\s\S]*?)\n?```/;
const VERSION_PATTERN = /version\s*[:\s]*([0-9]+\.[0-9]+\.[0-9]+)/i;

export interface ClaudeCodeOptions {
  outputFormat?: 'text' | 'json' | 'stream-json';
  verbose?: boolean;
  timeout?: number;
  workingDir?: string;
}

export class ClaudeCodeRunner {
  private readonly defaultOptions: ClaudeCodeOptions = {
    outputFormat: 'json',
    verbose: false,
    timeout: 120000, // 2 minutes
    workingDir: process.cwd(),
  };

  private findClaudeCodeBinary(): string {
    // Check environment variable first
    const envPath = process.env.CLAUDE_CODE_PATH;
    if (envPath != null && envPath.length > 0 && existsSync(envPath)) {
      return envPath;
    }

    // Common installation locations to check
    const possiblePaths = [
      // User-specific installations
      join(homedir(), '.claude', 'local', 'claude'),
      join(homedir(), '.local', 'bin', 'claude'),
      // System-wide installations
      '/usr/local/bin/claude',
      '/usr/bin/claude',
      // npm global installation
      join(homedir(), '.npm-global', 'bin', 'claude'),
      // yarn global installation
      join(homedir(), '.yarn', 'bin', 'claude'),
    ];

    for (const path of possiblePaths) {
      if (existsSync(path)) {
        return path;
      }
    }

    // If all else fails, try 'claude' directly (might work if it's in PATH)
    return 'claude';
  }

  async generateCode(
    model: AIModel,
    instruction: InstructionSet,
    options: Partial<ClaudeCodeOptions> = {}
  ): Promise<GeneratedCode> {
    const startTime = Date.now();
    const opts = { ...this.defaultOptions, ...options };

    try {
      // Build the comprehensive prompt
      const prompt = await this.buildPrompt(instruction);

      // Run Claude Code
      const result = await this.runClaudeCode(prompt, opts);

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
            provider: 'anthropic', // Claude Code always uses Anthropic
            temperature: model.temperature,
            maxTokens: model.maxTokens,
            note: `Claude Code v${result.version} (${opts.outputFormat})`,
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
          note:
            typeof result.version === 'string' && result.version.length > 0
              ? `Claude Code v${result.version}`
              : 'Claude Code (version unknown)',
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
        error: `Claude Code execution failed: ${error instanceof Error ? error.message : String(error)}`,
        metadata: {
          provider: 'anthropic',
          temperature: model.temperature,
          maxTokens: model.maxTokens,
        },
      };
    }
  }

  async testConnection(): Promise<{ available: boolean; version?: string; error?: string }> {
    try {
      const result = await this.runCommand(['--version'], { timeout: 5000 });

      if (result.success) {
        return {
          available: true,
          version: result.output.trim(),
        };
      }
      return {
        available: false,
        error: result.error,
      };
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
  modernSpec.length > 0
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

  private async runClaudeCode(
    prompt: string,
    options: ClaudeCodeOptions
  ): Promise<{ success: boolean; output: string; error?: string; version?: string }> {
    // Build command arguments
    const args = [
      '--print', // Non-interactive mode
      '--output-format',
      options.outputFormat ?? 'json',
    ];

    if (options.verbose === true) {
      args.push('--verbose');
    }

    // Add the prompt
    args.push(prompt);

    return this.runCommand(args, {
      timeout: options.timeout,
      cwd: options.workingDir,
    });
  }

  private async runCommand(
    args: string[],
    options: { timeout?: number; cwd?: string } = {}
  ): Promise<{ success: boolean; output: string; error?: string; version?: string }> {
    return new Promise((resolve) => {
      const claudePath = this.findClaudeCodeBinary();
      const child = spawn(claudePath, args, {
        cwd: options.cwd ?? process.cwd(),
        stdio: ['pipe', 'pipe', 'pipe'],
      });

      let stdout = '';
      let stderr = '';

      child.stdout?.on('data', (data: unknown) => {
        stdout += String(data);
      });

      child.stderr?.on('data', (data: unknown) => {
        stderr += String(data);
      });

      // Set timeout
      const timeout = options.timeout ?? 30000;
      const timeoutId = setTimeout(() => {
        child.kill('SIGTERM');
        resolve({
          success: false,
          output: '',
          error: `Command timed out after ${timeout}ms`,
        });
      }, timeout);

      child.on('close', (code) => {
        clearTimeout(timeoutId);

        if (code === 0) {
          resolve({
            success: true,
            output: stdout,
            version: this.extractVersion(stdout),
          });
        } else {
          resolve({
            success: false,
            output: stdout,
            error: stderr || `Command failed with exit code ${code}`,
          });
        }
      });

      child.on('error', (error) => {
        clearTimeout(timeoutId);
        resolve({
          success: false,
          output: '',
          error: error.message,
        });
      });
    });
  }

  private extractCode(output: string): string {
    try {
      // Try to parse as JSON first (if using JSON output format)
      const jsonData = JSON.parse(output) as unknown;
      if (
        typeof jsonData === 'object' &&
        jsonData !== null &&
        'result' in jsonData &&
        typeof (jsonData as { result: unknown }).result === 'string'
      ) {
        return this.cleanHaskellCode((jsonData as { result: string }).result);
      }
    } catch {
      // If not JSON, treat as raw text
    }

    // Extract Haskell code from markdown code blocks or raw text
    return this.cleanHaskellCode(output);
  }

  private cleanHaskellCode(text: string): string {
    // Remove common prefixes/suffixes
    let cleaned = text
      .replace(INTRO_PREFIX_PATTERN, '')
      .replace(CODE_PREFIX_PATTERN, '')
      .replace(CODE_SUFFIX_PATTERN, '');

    // Extract from code blocks if present
    const codeBlockMatch = cleaned.match(CODE_BLOCK_PATTERN);
    if (codeBlockMatch) {
      const [, extractedCode] = codeBlockMatch;
      cleaned = extractedCode;
    }

    // Clean up extra whitespace
    return cleaned.trim();
  }

  private extractVersion(output: string): string | undefined {
    // Try to extract version from various output formats
    const versionMatch = output.match(VERSION_PATTERN);
    return versionMatch ? versionMatch[1] : undefined;
  }

  async savePromptForDebugging(instruction: InstructionSet, filename?: string): Promise<string> {
    const prompt = await this.buildPrompt(instruction);
    const debugFile = filename ?? `debug-prompt-${instruction.id}-${Date.now()}.txt`;
    const debugPath = join(process.cwd(), 'debug', debugFile);

    await writeFile(debugPath, prompt);
    // eslint-disable-next-line no-console
    console.log(`🐛 Debug prompt saved to: ${debugPath}`);
    return debugPath;
  }
}
