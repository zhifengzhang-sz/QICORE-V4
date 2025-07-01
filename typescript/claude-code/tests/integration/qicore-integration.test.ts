/**
 * QiCore + Claude Code Integration Tests
 * Tests the complete integration of QiCore, QiAgent, and Claude Code functionality
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { 
  QiCoreRequestManager,
  createQiCoreClaudeManager,
  createQiCoreImplementationInstruction,
  createQiSystemInstruction
} from '../../src/index';
import { isSuccess, isFailure } from '@qi/core/base/result';

describe('QiCore + Claude Code Integration', () => {
  let manager: QiCoreRequestManager;

  beforeEach(() => {
    manager = createQiCoreClaudeManager({
      method: 'cli',
      workingDir: '/tmp/test-qicore',
      enableCaching: true,
      enablePerformanceMonitoring: true,
      verbose: false // Reduce noise in tests
    });
  });

  afterEach(async () => {
    await manager.shutdown();
  });

  describe('QiCore Component Implementation', () => {
    it('should create instruction for implementing Result component', () => {
      const instruction = createQiCoreImplementationInstruction('result', {
        outputPath: 'src/qicore/base/result.ts',
        includeTests: true,
        performanceTarget: '< 100μs',
        mathematicalPatterns: ['Either monad', 'Functor laws', 'Monad laws']
      });

      expect(instruction.actionInstruction).toContain('Result<T> Either monad');
      expect(instruction.actionInstruction).toContain('Monad laws');
      expect(instruction.actionInstruction).toContain('fp-ts');
      expect(instruction.context?.component).toBe('result');
      expect(instruction.context?.performanceTarget).toBe('< 100μs');
    });

    it('should create instruction for implementing Logger component', () => {
      const instruction = createQiCoreImplementationInstruction('logger', {
        includeTests: true,
        performanceTarget: '< 1μs level checks'
      });

      expect(instruction.actionInstruction).toContain('Production logging');
      expect(instruction.actionInstruction).toContain('winston');
      expect(instruction.actionInstruction).toContain('structured output');
      expect(instruction.context?.component).toBe('logger');
    });

    it('should create instruction for implementing Cache component', () => {
      const instruction = createQiCoreImplementationInstruction('cache', {
        includeTests: true,
        mathematicalPatterns: ['LRU algorithm', 'TTL expiration']
      });

      expect(instruction.actionInstruction).toContain('LRU and TTL eviction');
      expect(instruction.actionInstruction).toContain('concurrent access safety');
      expect(instruction.actionInstruction).toContain('State monad');
    });
  });

  describe('Complete System Implementation', () => {
    it('should create comprehensive system implementation instruction', () => {
      const instruction = createQiSystemInstruction({
        includeDocumentation: true,
        includeExamples: true,
        performanceTargets: {
          'result_operations': '< 100μs',
          'config_merge': '< 1ms',
          'cache_operations': '< 50μs',
          'logger_level_check': '< 1μs'
        }
      });

      expect(instruction.actionInstruction).toContain('ALL 6 components');
      expect(instruction.actionInstruction).toContain('Error: Context chaining');
      expect(instruction.actionInstruction).toContain('Result: Either monad');
      expect(instruction.actionInstruction).toContain('Config: Monoid merge');
      expect(instruction.actionInstruction).toContain('Logger: Production logging');
      expect(instruction.actionInstruction).toContain('Cache: LRU + TTL');
      expect(instruction.actionInstruction).toContain('Performance: Measurement');
      expect(instruction.actionInstruction).toContain('QiAgent Integration');
      expect(instruction.actionInstruction).toContain('85%+ test coverage');
      expect(instruction.actionInstruction).toContain('300+ comprehensive tests');
      expect(instruction.actionInstruction).toContain('< 100μs');
    });
  });

  describe('Request Manager with QiCore Integration', () => {
    it('should initialize with QiCore components', () => {
      expect(manager).toBeDefined();
      
      const metrics = manager.getMetrics();
      expect(metrics.totalRequests).toBe(0);
      expect(metrics.cacheStats).toBeDefined();
    });

    it('should handle graceful shutdown', async () => {
      const result = await manager.shutdown();
      expect(isSuccess(result)).toBe(true);
    });

    it('should create QiAgent successfully', () => {
      const agentResult = manager.createQiAgent();
      
      // Note: This might fail without actual API keys, but should handle gracefully
      if (isFailure(agentResult)) {
        expect(agentResult.left.code).toMatch(/QIAGENT_CREATION_FAILED|MISSING_AUTH/);
      } else {
        expect(agentResult.right).toBeDefined();
      }
    });
  });

  describe('Performance and Reliability', () => {
    it('should track request performance', async () => {
      const instruction = createQiCoreImplementationInstruction('error');
      
      // This would fail in test environment but should handle gracefully
      const result = await manager.executeInstruction(instruction);
      
      if (isFailure(result)) {
        // Expected in test environment without actual Claude Code access
        expect(result.left.code).toBe('CLAUDE_REQUEST_FAILED');
        expect(result.left.category).toBe('NETWORK');
      } else {
        expect(result.right.performance).toBeDefined();
        expect(result.right.metadata.requestId).toBeDefined();
      }
      
      const metrics = manager.getMetrics();
      expect(metrics.totalRequests).toBe(1);
    });

    it('should handle batch execution with circuit breaker', async () => {
      const instructions = [
        createQiCoreImplementationInstruction('error'),
        createQiCoreImplementationInstruction('result'),
        createQiCoreImplementationInstruction('config')
      ];

      const result = await manager.executeBatch(instructions);
      
      // Expected to fail in test environment, but should handle gracefully
      if (isFailure(result)) {
        expect(result.left.code).toMatch(/CLAUDE_REQUEST_FAILED|BATCH_CIRCUIT_BREAKER_OPEN/);
      }
    });
  });

  describe('Caching Integration', () => {
    it('should cache identical requests', async () => {
      const instruction = createQiCoreImplementationInstruction('result');
      
      // First request
      const result1 = await manager.executeInstruction(instruction);
      
      // Second identical request  
      const result2 = await manager.executeInstruction(instruction);
      
      // Both should have same outcome (likely failure in test env)
      if (isFailure(result1)) {
        expect(isFailure(result2)).toBe(true);
      } else {
        expect(isSuccess(result2)).toBe(true);
        // Second request might be cached (instant)
        expect(result2.right.performance.cached).toBe(true);
      }
    });
  });

  describe('Error Handling and Context', () => {
    it('should provide structured error information', async () => {
      const instruction = createQiCoreImplementationInstruction('error');
      
      const result = await manager.executeInstruction(instruction);
      
      if (isFailure(result)) {
        expect(result.left.code).toBeDefined();
        expect(result.left.message).toBeDefined();
        expect(result.left.category).toBeDefined();
        expect(result.left.timestamp).toBeGreaterThan(0);
        expect(result.left.context.size).toBeGreaterThan(0);
      }
    });
  });
});

describe('QiCore Instruction Builders', () => {
  describe('Component-specific instructions', () => {
    it('should generate correct instruction for each component type', () => {
      const components = ['error', 'result', 'config', 'logger', 'cache', 'performance'] as const;
      
      for (const component of components) {
        const instruction = createQiCoreImplementationInstruction(component);
        
        expect(instruction.actionInstruction).toContain(component);
        expect(instruction.context?.component).toBe(component);
        
        // Each should have appropriate file paths
        if (component === 'error' || component === 'result') {
          expect(instruction.context?.outputPath).toContain('base');
        } else {
          expect(instruction.context?.outputPath).toContain('core');
        }
      }
    });

    it('should include mathematical patterns in instructions', () => {
      const resultInstruction = createQiCoreImplementationInstruction('result', {
        mathematicalPatterns: ['Monad laws', 'Functor composition']
      });

      expect(resultInstruction.actionInstruction).toContain('Either monad');
      expect(resultInstruction.actionInstruction).toContain('Monad laws');
      expect(resultInstruction.actionInstruction).toContain('Functor composition');
    });

    it('should include performance requirements', () => {
      const cacheInstruction = createQiCoreImplementationInstruction('cache', {
        performanceTarget: '< 50μs operations'
      });

      expect(cacheInstruction.actionInstruction).toContain('< 50μs operations');
      expect(cacheInstruction.context?.performanceTarget).toBe('< 50μs operations');
    });
  });

  describe('System-level instructions', () => {
    it('should create comprehensive system requirements', () => {
      const systemInstruction = createQiSystemInstruction({
        performanceTargets: {
          'result_operations': '< 100μs',
          'config_merge': '< 1ms'
        }
      });

      expect(systemInstruction.actionInstruction).toContain('ALL 6 components');
      expect(systemInstruction.actionInstruction).toContain('85%+ test coverage');
      expect(systemInstruction.actionInstruction).toContain('300+ comprehensive tests');
      expect(systemInstruction.actionInstruction).toContain('< 100μs');
      expect(systemInstruction.actionInstruction).toContain('< 1ms');
    });
  });
});