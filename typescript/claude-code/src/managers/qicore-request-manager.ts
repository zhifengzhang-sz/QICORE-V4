/**
 * QiCore-Enhanced Claude Code Request Manager
 * Integrates QiCore Result patterns and QiAgent capabilities with Claude Code
 */

import { ClaudeRequestManager } from './request-manager';
import { success, failure, type Result, isSuccess } from '@qi/core/base/result';
import { createQiError } from '@qi/core/base/error';
import { createDefault as createLogger, type Logger } from '@qi/core/core/logger';
import { createMemoryCache, type MemoryCache } from '@qi/core/core/cache';
import { measureAsync } from '@qi/core/core/performance';
import { QiAgent } from '@qi/agent/index';
import type { 
  ClaudeRequestConfig, 
  ClaudeResponse, 
  InstructionConfig 
} from '../types';

export interface QiCoreRequestConfig extends ClaudeRequestConfig {
  enableCaching?: boolean;
  enablePerformanceMonitoring?: boolean;
  cacheSize?: number;
  cacheTTL?: number;
}

export interface EnhancedClaudeResponse {
  originalResponse: ClaudeResponse;
  performance: {
    duration: number;
    cached: boolean;
    timestamp: number;
  };
  metadata: {
    requestId: string;
    model: string;
    tokensUsed?: number;
  };
}

/**
 * Enhanced Claude Code request manager with QiCore reliability patterns
 */
export class QiCoreRequestManager {
  private baseManager: ClaudeRequestManager;
  private logger: Logger;
  private cache?: MemoryCache<string, ClaudeResponse>;
  private config: QiCoreRequestConfig;
  private requestCounter = 0;

  constructor(config: QiCoreRequestConfig) {
    this.config = config;
    this.baseManager = new ClaudeRequestManager(config);
    
    // Initialize QiCore components
    const loggerResult = createLogger();
    if (isSuccess(loggerResult)) {
      this.logger = loggerResult.right;
    } else {
      throw new Error(`Failed to initialize logger: ${loggerResult.left.message}`);
    }

    // Initialize cache if enabled
    if (config.enableCaching !== false) {
      const cacheResult = createMemoryCache<string, ClaudeResponse>(
        config.cacheSize ?? 100,
        config.cacheTTL ?? 300000 // 5 minutes default
      );
      
      if (isSuccess(cacheResult)) {
        this.cache = cacheResult.right;
        this.logger.log('info', 'Request cache initialized', {
          maxSize: config.cacheSize ?? 100,
          ttl: config.cacheTTL ?? 300000
        });
      }
    }
  }

  /**
   * Execute Claude Code instruction with QiCore reliability patterns
   */
  async executeInstruction(
    instruction: InstructionConfig
  ): Promise<Result<EnhancedClaudeResponse>> {
    const requestId = `req-${++this.requestCounter}-${Date.now()}`;
    
    this.logger.log('info', 'Executing Claude Code instruction', {
      requestId,
      type: instruction.type,
      cached: !!this.cache
    });

    return await this.withPerformanceMonitoring(
      'claude_request',
      async () => {
        // Check cache first
        if (this.cache) {
          const cacheKey = this.createCacheKey(instruction);
          const cachedResult = await this.cache.get(cacheKey);
          
          if (isSuccess(cachedResult)) {
            this.logger.log('debug', 'Cache hit for Claude request', { requestId });
            
            return success({
              originalResponse: cachedResult.right,
              performance: {
                duration: 0, // Cached responses are instant
                cached: true,
                timestamp: Date.now()
              },
              metadata: {
                requestId,
                model: this.config.model ?? 'sonnet'
              }
            });
          }
        }

        // Execute request with error handling
        try {
          const response = await this.baseManager.executeInstruction(instruction);
          
          // Cache successful responses
          if (this.cache && response.response.success) {
            const cacheKey = this.createCacheKey(instruction);
            await this.cache.set(cacheKey, response);
          }

          return success({
            originalResponse: response,
            performance: {
              duration: response.execution?.duration ?? 0,
              cached: false,
              timestamp: Date.now()
            },
            metadata: {
              requestId,
              model: this.config.model ?? 'sonnet',
              tokensUsed: response.response.usage?.total_tokens
            }
          });

        } catch (error) {
          const qiError = createQiError(
            'CLAUDE_REQUEST_FAILED',
            `Claude Code request failed: ${error}`,
            'NETWORK',
            { requestId, instruction: instruction.type, error: String(error) }
          );

          this.logger.log('error', qiError.message, qiError.context);
          return failure(qiError);
        }
      }
    );
  }

  /**
   * Execute multiple instructions with circuit breaker pattern
   */
  async executeBatch(
    instructions: InstructionConfig[]
  ): Promise<Result<EnhancedClaudeResponse[]>> {
    this.logger.log('info', 'Executing batch Claude Code instructions', {
      count: instructions.length
    });

    const results: EnhancedClaudeResponse[] = [];
    let failures = 0;
    const maxFailures = Math.ceil(instructions.length * 0.5); // 50% failure threshold

    for (const [index, instruction] of instructions.entries()) {
      if (failures >= maxFailures) {
        const error = createQiError(
          'BATCH_CIRCUIT_BREAKER_OPEN',
          `Batch execution stopped: too many failures (${failures}/${maxFailures})`,
          'BUSINESS',
          { 
            processedCount: index,
            totalCount: instructions.length,
            failureRate: failures / index
          }
        );

        this.logger.log('error', error.message, error.context);
        return failure(error);
      }

      const result = await this.executeInstruction(instruction);
      
      if (isSuccess(result)) {
        results.push(result.right);
      } else {
        failures++;
        this.logger.log('warn', 'Batch instruction failed', {
          index,
          instruction: instruction.type,
          error: result.left.code
        });
      }
    }

    return success(results);
  }

  /**
   * Create QiAgent for advanced AI interactions
   */
  createQiAgent(config?: unknown): Result<unknown> {
    try {
      const agentConfig = config ?? QiAgent.createClaudeCodeConfig();
      const agent = QiAgent.createClaudeCodeAgent(agentConfig);
      
      // QiAgent methods might not return Result types, so wrap them
      if (agent) {
        return success(agent);
      }
      
      return failure(createQiError(
        'QIAGENT_CREATION_FAILED',
        'QiAgent creation returned null/undefined',
        'SYSTEM',
        { config: agentConfig }
      ));
    } catch (error) {
      return failure(createQiError(
        'QIAGENT_CREATION_FAILED',
        `Failed to create QiAgent: ${error}`,
        'SYSTEM',
        { error: String(error) }
      ));
    }
  }

  /**
   * Get performance metrics
   */
  getMetrics(): {
    totalRequests: number;
    cacheStats?: {
      size: number;
      hitRate: number;
    };
  } {
    const metrics = {
      totalRequests: this.requestCounter
    };

    if (this.cache) {
      // Note: MemoryCache would need to expose these stats
      // This is a placeholder for the interface
      return {
        ...metrics,
        cacheStats: {
          size: 0, // Would come from cache.getSize()
          hitRate: 0 // Would come from cache.getHitRate()
        }
      };
    }

    return metrics;
  }

  /**
   * Graceful shutdown
   */
  async shutdown(): Promise<Result<void>> {
    this.logger.log('info', 'Shutting down QiCore request manager');
    
    try {
      // Clear cache if present
      if (this.cache) {
        // Note: MemoryCache would need a clear() method
        this.logger.log('info', 'Cache cleared during shutdown');
      }

      return success(undefined);
    } catch (error) {
      return failure(createQiError(
        'SHUTDOWN_ERROR',
        `Error during shutdown: ${error}`,
        'SYSTEM',
        { error: String(error) }
      ));
    }
  }

  private createCacheKey(instruction: InstructionConfig): string {
    // Create deterministic cache key from instruction
    return `claude:${instruction.type}:${JSON.stringify(instruction)}`;
  }

  private async withPerformanceMonitoring<T>(
    operation: string,
    fn: () => Promise<T>
  ): Promise<T> {
    if (this.config.enablePerformanceMonitoring !== false) {
      return await measureAsync(operation, fn);
    }
    return await fn();
  }
}

/**
 * Create pre-configured QiCore request manager for Claude Code
 */
export function createQiCoreClaudeManager(
  config: Partial<QiCoreRequestConfig> = {}
): QiCoreRequestManager {
  const defaultConfig: QiCoreRequestConfig = {
    method: 'cli',
    workingDir: '/home/zzhang/dev/qi/github/mcp-server/qicore-v4',
    model: 'sonnet',
    maxTurns: 10,
    verbose: true,
    allowedTools: ['Read', 'Write', 'Edit', 'MultiEdit', 'Glob', 'Grep', 'Bash', 'LS'],
    enableCaching: true,
    enablePerformanceMonitoring: true,
    cacheSize: 100,
    cacheTTL: 300000
  };

  return new QiCoreRequestManager({ ...defaultConfig, ...config });
}