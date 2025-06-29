import type {
  RequestManagerConfig,
  ClaudeInstruction,
  ClaudeResponse,
  ExecutionResult,
} from '@/types';
import { ValidationError, ExecutionError } from '@/types';
import { ClaudeCliManager } from './cli-manager';
import { ClaudeSdkManager } from './sdk-manager';
import { buildInstruction } from '@/instructions/builder';

/**
 * Main request manager that coordinates between CLI and SDK methods
 */
export class ClaudeRequestManager {
  private cliManager: ClaudeCliManager | undefined;
  private sdkManager: ClaudeSdkManager | undefined;

  constructor(private config: RequestManagerConfig) {
    this.validateConfig(config);
    this.initializeManagers();
  }

  /**
   * Execute a Claude instruction using the configured method
   */
  async executeInstruction(instruction: ClaudeInstruction): Promise<ExecutionResult> {
    const startTime = Date.now();

    try {
      // Validate instruction
      this.validateInstruction(instruction);

      // Build the formatted instruction
      const builtInstruction = buildInstruction(instruction);

      if (this.config.verbose) {
        // eslint-disable-next-line no-console
        console.log(`Executing instruction via ${this.config.method}`);
        // eslint-disable-next-line no-console
        console.log('Working directory:', instruction.workingDir);
        // eslint-disable-next-line no-console
        console.log('Instruction length:', builtInstruction.length);
      }

      // Execute based on method
      let response: ClaudeResponse;

      if (this.config.method === 'cli' && this.cliManager) {
        response = await this.cliManager.executeQuery(builtInstruction);
      } else if (this.config.method === 'sdk' && this.sdkManager) {
        response = await this.sdkManager.executeQuery(builtInstruction);
      } else {
        throw new ExecutionError(`Invalid method configuration: ${this.config.method}`);
      }

      const executionTime = Date.now() - startTime;

      return {
        instruction,
        response,
        executionTime,
        timestamp: new Date().toISOString(),
        method: this.config.method,
      };
    } catch (error) {
      const executionTime = Date.now() - startTime;

      // Create error response
      const errorResponse: ClaudeResponse = {
        success: false,
        content: null,
        error: error instanceof Error ? error.message : String(error),
        metadata: {
          executionTime,
          method: this.config.method,
          failed: true,
        },
      };

      return {
        instruction,
        response: errorResponse,
        executionTime,
        timestamp: new Date().toISOString(),
        method: this.config.method,
      };
    }
  }

  /**
   * Execute multiple instructions in sequence
   */
  async executeInstructionSequence(instructions: ClaudeInstruction[]): Promise<ExecutionResult[]> {
    const results: ExecutionResult[] = [];

    for (const instruction of instructions) {
      const result = await this.executeInstruction(instruction);
      results.push(result);

      // Stop on first failure unless configured otherwise
      if (!result.response.success) {
        console.warn('Instruction failed, stopping sequence:', result.response.error);
        break;
      }
    }

    return results;
  }

  /**
   * Execute multiple instructions in parallel
   */
  async executeInstructionBatch(instructions: ClaudeInstruction[]): Promise<ExecutionResult[]> {
    const promises = instructions.map((instruction) => this.executeInstruction(instruction));

    try {
      return await Promise.all(promises);
    } catch (error) {
      throw new ExecutionError(
        'Batch execution failed',
        error instanceof Error ? error.message : String(error)
      );
    }
  }

  /**
   * Test connection and get status
   */
  async getStatus(): Promise<{
    method: string;
    available: boolean;
    configuration: RequestManagerConfig;
    managerStatus?: unknown;
  }> {
    let available = false;
    let managerStatus: unknown;

    try {
      if (this.config.method === 'cli' && this.cliManager) {
        const status = await this.cliManager.getStatus();
        available = status.available;
        managerStatus = status;
      } else if (this.config.method === 'sdk' && this.sdkManager) {
        const status = await this.sdkManager.getStatus();
        available = status.available;
        managerStatus = status;
      }
    } catch (error) {
      console.warn('Status check failed:', error);
    }

    return {
      method: this.config.method,
      available,
      configuration: this.config,
      managerStatus,
    };
  }

  /**
   * Switch between CLI and SDK methods
   */
  switchMethod(newMethod: 'cli' | 'sdk'): void {
    this.config.method = newMethod;
    this.initializeManagers();
  }

  /**
   * Update configuration
   */
  updateConfig(updates: Partial<RequestManagerConfig>): void {
    this.config = { ...this.config, ...updates };
    this.validateConfig(this.config);
    this.initializeManagers();
  }

  /**
   * Get current configuration
   */
  getConfig(): RequestManagerConfig {
    return { ...this.config };
  }

  /**
   * Validate configuration
   */
  private validateConfig(config: RequestManagerConfig): void {
    if (!config.workingDir) {
      throw new ValidationError('Working directory is required');
    }

    if (!['cli', 'sdk'].includes(config.method)) {
      throw new ValidationError(`Invalid method: ${config.method}`);
    }

    if (config.maxTurns && config.maxTurns <= 0) {
      throw new ValidationError('maxTurns must be positive');
    }
  }

  /**
   * Validate instruction
   */
  private validateInstruction(instruction: ClaudeInstruction): void {
    if (!instruction.workingDir) {
      throw new ValidationError('Instruction working directory is required');
    }

    if (!instruction.actionInstruction?.trim()) {
      throw new ValidationError('Action instruction is required');
    }

    if (!instruction.knowledgeUpdate || instruction.knowledgeUpdate.length === 0) {
      throw new ValidationError('Knowledge update requirements are required');
    }
  }

  /**
   * Initialize managers based on configuration
   */
  private initializeManagers(): void {
    if (this.config.method === 'cli') {
      this.cliManager = new ClaudeCliManager({
        workingDir: this.config.workingDir,
        outputFormat: this.config.outputFormat,
        model: this.config.model,
        maxTurns: this.config.maxTurns,
        verbose: this.config.verbose,
      });
      this.sdkManager = undefined;
    } else {
      this.sdkManager = new ClaudeSdkManager({
        workingDir: this.config.workingDir,
        maxTurns: this.config.maxTurns,
        systemPrompt: this.config.systemPrompt,
        allowedTools: this.config.allowedTools,
        verbose: this.config.verbose,
      });
      this.cliManager = undefined;
    }
  }
}
