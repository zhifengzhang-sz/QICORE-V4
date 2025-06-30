/* eslint-disable @typescript-eslint/no-unsafe-assignment */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-return */
/* eslint-disable no-console */

import { createQiError } from '@/qicore/base/error';
import { failure, success } from '@/qicore/base/result';
import type { AppError, AsyncResult } from '../modules/types';
import { createCodeGenerator } from './generators/ai-code-generator';
import type {
  AIModel,
  CodeGeneratorConfig,
  GenerationResult,
  InstructionSet,
  StudyConfig,
  StudyExecutor,
  StudyResult,
} from './types/index';

// ============================================================================
// APPLICATION CONFIGURATION
// ============================================================================

/**
 * Application-level configuration
 */
export interface AppConfig {
  readonly codeGeneration: CodeGeneratorConfig;
  readonly concurrency: ConcurrencyConfig;
  readonly persistence: PersistenceConfig;
  readonly monitoring: MonitoringConfig;
}

/**
 * Concurrency management configuration
 */
interface ConcurrencyConfig {
  readonly maxConcurrentGenerations: number;
  readonly batchSize: number;
  readonly delayBetweenBatches: number;
}

/**
 * Data persistence configuration
 */
interface PersistenceConfig {
  readonly outputDirectory: string;
  readonly saveRawResults: boolean;
  readonly saveStatistics: boolean;
  readonly compressionEnabled: boolean;
}

/**
 * Monitoring and observability configuration
 */
interface MonitoringConfig {
  readonly enableMetrics: boolean;
  readonly enableLogging: boolean;
  readonly logLevel: 'debug' | 'info' | 'warn' | 'error';
  readonly metricsEndpoint?: string;
}

// ============================================================================
// APPLICATION STATE MANAGEMENT
// ============================================================================

/**
 * Study execution context
 */
interface StudyExecutionContext {
  readonly studyId: string;
  readonly config: StudyConfig;
  readonly appConfig: AppConfig;
  readonly startTime: Date;
  readonly generations: GenerationResult[];
  readonly errors: AppError[];
}

/**
 * Progress tracking
 */
interface StudyProgress {
  readonly totalCombinations: number;
  readonly completedCombinations: number;
  readonly successfulGenerations: number;
  readonly failedGenerations: number;
  readonly estimatedTimeRemaining: number;
}

// ============================================================================
// PURE ORCHESTRATION FUNCTIONS
// ============================================================================

/**
 * Calculate total number of generation combinations
 * @pure
 * @time O(1)
 * @space O(1)
 */
const calculateTotalCombinations = (config: StudyConfig): number =>
  config.models.length * config.instructions.length * config.runsPerCombination;

/**
 * Generate all model-instruction combinations
 * @pure
 * @time O(m * i) where m = models, i = instructions
 * @space O(m * i * r) where r = runs per combination
 */
const generateCombinations = (
  config: StudyConfig
): Array<{
  model: AIModel;
  instruction: InstructionSet;
  runNumber: number;
}> => {
  const combinations: Array<{
    model: AIModel;
    instruction: InstructionSet;
    runNumber: number;
  }> = [];

  for (const model of config.models) {
    for (const instruction of config.instructions) {
      for (let run = 1; run <= config.runsPerCombination; run++) {
        combinations.push({ model, instruction, runNumber: run });
      }
    }
  }

  return combinations;
};

/**
 * Create study execution context
 * @pure (except for Date.now())
 * @time O(1)
 * @space O(1)
 */
const createExecutionContext = (
  studyId: string,
  config: StudyConfig,
  appConfig: AppConfig
): StudyExecutionContext => ({
  studyId,
  config,
  appConfig,
  startTime: new Date(),
  generations: [],
  errors: [],
});

/**
 * Calculate progress from execution context
 * @pure
 * @time O(1)
 * @space O(1)
 */
const calculateProgress = (context: StudyExecutionContext): StudyProgress => {
  const totalCombinations = calculateTotalCombinations(context.config);
  const completedCombinations = context.generations.length;
  const successfulGenerations = context.generations.filter(
    (g) => g.generatedCode.generation.duration > 0
  ).length;
  const failedGenerations = context.errors.length;

  const progressRatio = completedCombinations / totalCombinations;
  const elapsedTime = Date.now() - context.startTime.getTime();
  const estimatedTimeRemaining = progressRatio > 0 ? elapsedTime / progressRatio - elapsedTime : 0;

  return {
    totalCombinations,
    completedCombinations,
    successfulGenerations,
    failedGenerations,
    estimatedTimeRemaining,
  };
};

// ============================================================================
// EFFECTFUL OPERATIONS
// ============================================================================

/**
 * Execute single generation with error handling
 * @impure (network calls, side effects)
 * @time O(1) + generation time
 * @space O(response size)
 */
const executeGeneration = async (
  generator: ReturnType<typeof createCodeGenerator>,
  studyId: string,
  model: AIModel,
  instruction: InstructionSet,
  runNumber: number
): AsyncResult<GenerationResult, AppError> => {
  try {
    const generationResult = await generator(model, instruction);

    if (generationResult.kind === 'failure') {
      return generationResult;
    }

    const result: GenerationResult = {
      id: `${studyId}-${model.id}-${instruction.id}-${runNumber}`,
      studyId,
      modelId: model.id,
      instructionId: instruction.id,
      runNumber,
      generatedCode: generationResult.data,
      timestamp: new Date(),
    };

    return success(result);
  } catch (error) {
    return failure(
      createQiError(
        'EXTERNAL_SERVICE_ERROR',
        `Code generation failed: ${error instanceof Error ? error.message : String(error)}`,
        'NETWORK',
        {
          service: 'code-generator',
          operation: 'generation',
          originalError: error instanceof Error ? error.name : 'Unknown',
        }
      )
    );
  }
};

/**
 * Execute batch of generations with concurrency control
 * @impure (network calls, concurrency)
 * @time O(batch_size / concurrency) + generation time
 * @space O(batch_size * response_size)
 */
const executeBatch = async (
  generator: ReturnType<typeof createCodeGenerator>,
  context: StudyExecutionContext,
  batch: Array<{ model: AIModel; instruction: InstructionSet; runNumber: number }>
): AsyncResult<GenerationResult[], AppError[]> => {
  const { maxConcurrentGenerations } = context.appConfig.concurrency;

  // Split batch into chunks for concurrency control
  const chunks: Array<typeof batch> = [];
  for (let i = 0; i < batch.length; i += maxConcurrentGenerations) {
    chunks.push(batch.slice(i, i + maxConcurrentGenerations));
  }

  const allResults: GenerationResult[] = [];
  const allErrors: AppError[] = [];

  for (const chunk of chunks) {
    const promises = chunk.map(({ model, instruction, runNumber }) =>
      executeGeneration(generator, context.studyId, model, instruction, runNumber)
    );

    const results = await Promise.all(promises);

    for (const result of results) {
      if (result.kind === 'success') {
        allResults.push(result.data);
      } else {
        allErrors.push(result.error);
      }
    }

    // Delay between chunks to avoid overwhelming APIs
    if (chunks.indexOf(chunk) < chunks.length - 1) {
      await new Promise((resolve) =>
        setTimeout(resolve, context.appConfig.concurrency.delayBetweenBatches)
      );
    }
  }

  return allErrors.length === 0 ? success(allResults) : failure(allErrors);
};

/**
 * Save study results to persistence layer
 * @impure (file system, database)
 * @time O(result_size)
 * @space O(result_size)
 */
const saveStudyResults = async (
  context: StudyExecutionContext,
  results: StudyResult
): AsyncResult<void, AppError> => {
  try {
    const { outputDirectory, saveRawResults, saveStatistics } = context.appConfig.persistence;

    // Create output directory structure
    const studyDir = `${outputDirectory}/${context.studyId}`;

    // Save main study result
    const studyPath = `${studyDir}/study-result.json`;
    await writeJsonFile(studyPath, results);

    // Save raw generations if enabled
    if (saveRawResults) {
      const rawPath = `${studyDir}/raw-generations.json`;
      await writeJsonFile(rawPath, results.generations);
    }

    // Save statistics if enabled
    if (saveStatistics) {
      const statsPath = `${studyDir}/statistics.json`;
      await writeJsonFile(statsPath, results.statistics);
    }

    return success(undefined);
  } catch (error) {
    return failure(
      createQiError(
        'FILE_SYSTEM_ERROR',
        `Failed to save results: ${error instanceof Error ? error.message : String(error)}`,
        'SYSTEM',
        {
          service: 'file-system',
          operation: 'save-results',
          originalError: error instanceof Error ? error.name : 'Unknown',
        }
      )
    );
  }
};

// Mock file writing function (would be replaced with actual implementation)
// @impure (file system)
const writeJsonFile = async (path: string, data: unknown): Promise<void> => {
  // Development mode: just log what would be written
  console.log(`Would write to ${path}:`, `${JSON.stringify(data, null, 2).substring(0, 100)}...`);
};

// ============================================================================
// STUDY ANALYSIS FUNCTIONS
// ============================================================================

/**
 * Analyze study results and generate insights
 * @pure (except for Date operations)
 * @time O(n) where n is number of generations
 * @space O(n)
 */
const analyzeStudyResults = (config: StudyConfig, generations: GenerationResult[]): StudyResult => {
  const studyId = `study-${Date.now()}`;

  // Calculate basic statistics
  const totalGenerations = generations.length;
  const successRate =
    totalGenerations > 0
      ? generations.filter((g) => g.generatedCode.generation.duration > 0).length / totalGenerations
      : 0;

  // Mock quality scores (would be calculated by actual quality evaluator)
  const scores = generations.map(() => Math.random() * 100);
  const meanScore = scores.reduce((sum, score) => sum + score, 0) / scores.length;
  const standardDeviation = Math.sqrt(
    scores.reduce((sum, score) => sum + (score - meanScore) ** 2, 0) / scores.length
  );
  const coefficientOfVariation = standardDeviation / meanScore;

  const consistencyRank =
    coefficientOfVariation < 0.1 ? 'high' : coefficientOfVariation < 0.3 ? 'medium' : 'low';

  // Mock model and instruction comparisons
  const modelComparisons = config.models.map((model, index) => ({
    modelId: model.id,
    meanScore: meanScore + (Math.random() - 0.5) * 20,
    consistency: Math.random(),
    rank: index + 1,
    strengths: ['consistent output', 'good documentation'],
    weaknesses: ['slower generation', 'verbose code'],
  }));

  const instructionComparisons = config.instructions.map((instruction, index) => ({
    instructionId: instruction.id,
    meanScore: meanScore + (Math.random() - 0.5) * 15,
    consistency: Math.random(),
    rank: index + 1,
    effectiveFor: [instruction.metadata.language],
    challengingFor: ['complex algorithms'],
  }));

  return {
    id: studyId,
    config,
    generations,
    statistics: {
      totalGenerations,
      successRate,
      meanScore: meanScore || 0,
      standardDeviation: standardDeviation || 0,
      coefficientOfVariation: coefficientOfVariation || 0,
      consistencyRank,
      modelComparisons,
      instructionComparisons,
    },
    analysis: {
      insights: [
        {
          type: 'performance',
          description: 'Model performance varies significantly across instruction types',
          confidence: 0.85,
          evidence: ['statistical analysis', 'variance measurements'],
        },
      ],
      recommendations: [
        {
          category: 'model',
          action: 'Use higher temperature for creative tasks',
          rationale: 'Improved diversity without sacrificing quality',
          priority: 'medium',
          estimatedImpact: '10-15% improvement in creative outputs',
        },
      ],
      correlations: [],
      anomalies: [],
    },
    metadata: {
      startTime: new Date(),
      endTime: new Date(),
      duration: 0,
      version: '1.0.0',
      environment: {},
      resources: {
        totalTokens: 0,
        totalCost: 0,
        apiCalls: 0,
        peakMemoryMB: 0,
        averageLatency: 0,
      },
    },
  };
};

// ============================================================================
// MAIN ORCHESTRATOR
// ============================================================================

/**
 * Create study executor with dependency injection
 * @pure (constructor)
 * @time O(1)
 * @space O(1)
 */
export const createStudyOrchestrator =
  (appConfig: AppConfig): StudyExecutor =>
  async (config: StudyConfig): AsyncResult<StudyResult, AppError> => {
    const studyId = `study-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    const context = createExecutionContext(studyId, config, appConfig);

    try {
      // Create code generator with app configuration
      const generator = createCodeGenerator(appConfig.codeGeneration);

      // Generate all combinations
      const combinations = generateCombinations(config);
      const { batchSize } = appConfig.concurrency;

      // Execute in batches
      const batches: Array<typeof combinations> = [];
      for (let i = 0; i < combinations.length; i += batchSize) {
        batches.push(combinations.slice(i, i + batchSize));
      }

      const allGenerations: GenerationResult[] = [];
      const allErrors: AppError[] = [];

      for (const [batchIndex, batch] of batches.entries()) {
        console.log(`Executing batch ${batchIndex + 1}/${batches.length}...`);

        const batchResult = await executeBatch(generator, context, batch);

        if (batchResult.kind === 'success') {
          allGenerations.push(...batchResult.data);
        } else {
          allErrors.push(...batchResult.error);
        }

        // Log progress
        const progress = calculateProgress({
          ...context,
          generations: allGenerations,
          errors: allErrors,
        });

        console.log(
          `Progress: ${progress.completedCombinations}/${progress.totalCombinations} ` +
            `(${Math.round((progress.completedCombinations / progress.totalCombinations) * 100)}%)`
        );
      }

      // Analyze results
      const studyResult = analyzeStudyResults(config, allGenerations);

      // Save results
      const saveResult = await saveStudyResults(context, studyResult);
      if (saveResult.kind === 'failure') {
        console.warn('Failed to save results:', saveResult.error.message);
      }

      // Log final statistics
      console.log(
        `Study completed with ${allGenerations.length} successful generations and ${allErrors.length} errors`
      );

      return success(studyResult);
    } catch (error) {
      return failure(
        createQiError(
          'STUDY_EXECUTION_ERROR',
          `Study execution failed: ${error instanceof Error ? error.message : String(error)}`,
          'BUSINESS',
          {
            studyId,
            configName: config.name,
            operation: 'study-execution',
            originalError: error instanceof Error ? error.name : 'Unknown',
          }
        )
      );
    }
  };

// ============================================================================
// CONFIGURATION FACTORIES
// ============================================================================

/**
 * Create default application configuration
 * @pure
 * @time O(1)
 * @space O(1)
 */
export const createDefaultAppConfig = (): AppConfig => ({
  codeGeneration: {
    timeout: 60000,
    maxRetries: 3,
    enableThinking: true,
    thinkingTokenBudget: 8000,
    debug: false,
  },
  concurrency: {
    maxConcurrentGenerations: 3,
    batchSize: 10,
    delayBetweenBatches: 1000,
  },
  persistence: {
    outputDirectory: './study-results',
    saveRawResults: true,
    saveStatistics: true,
    compressionEnabled: false,
  },
  monitoring: {
    enableMetrics: true,
    enableLogging: true,
    logLevel: 'info',
  },
});

/**
 * Create development application configuration
 * @pure
 * @time O(1)
 * @space O(1)
 */
export const createDevAppConfig = (): AppConfig => ({
  ...createDefaultAppConfig(),
  codeGeneration: {
    ...createDefaultAppConfig().codeGeneration,
    timeout: 120000,
    debug: true,
  },
  concurrency: {
    maxConcurrentGenerations: 1, // Sequential for debugging
    batchSize: 5,
    delayBetweenBatches: 2000,
  },
  monitoring: {
    enableMetrics: true,
    enableLogging: true,
    logLevel: 'debug',
  },
});

/**
 * Create production application configuration
 * @pure
 * @time O(1)
 * @space O(1)
 */
export const createProdAppConfig = (): AppConfig => ({
  ...createDefaultAppConfig(),
  codeGeneration: {
    ...createDefaultAppConfig().codeGeneration,
    timeout: 30000,
    enableThinking: false, // Save tokens
    debug: false,
  },
  concurrency: {
    maxConcurrentGenerations: 5,
    batchSize: 20,
    delayBetweenBatches: 500,
  },
  persistence: {
    ...createDefaultAppConfig().persistence,
    compressionEnabled: true,
  },
  monitoring: {
    enableMetrics: true,
    enableLogging: true,
    logLevel: 'warn',
  },
});

// ============================================================================
// UTILITY EXPORTS
// ============================================================================

export const StudyOrchestratorUtils = {
  create: createStudyOrchestrator,
  configs: {
    default: createDefaultAppConfig,
    dev: createDevAppConfig,
    prod: createProdAppConfig,
  },
  analysis: {
    calculateProgress,
    calculateTotalCombinations,
  },
} as const;
