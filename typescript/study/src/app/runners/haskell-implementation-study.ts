/* eslint-disable no-console */

import { readFile } from 'node:fs/promises';
import { StudyResultsStore } from '@/database/study-results-store';
import { ImplementationScorer } from '@/evaluation/implementation-scorer';
import { ClaudeCodeRunner } from '@/generators/claude-code-runner';
import type { AIModel, InstructionSet, StudyConfig } from '@/types/study';

export interface StudyProgress {
  totalRuns: number;
  completedRuns: number;
  currentModel: string;
  currentInstruction: string;
  currentRun: number;
  bestScore: number;
  worstScore: number;
  averageScore: number;
  estimatedTimeRemaining: number;
}

export interface StudyResult {
  studyId: string;
  totalImplementations: number;
  bestImplementation: {
    code: string;
    score: number;
    runId: string;
  };
  worstImplementation: {
    code: string;
    score: number;
    runId: string;
  };
  statistics: {
    averageScore: number;
    scoreRange: number;
    standardDeviation: number;
  };
}

interface ScoringAnomalyDetector {
  scores: number[];
  generationAttempts: number;
  successfulGenerations: number;
}

export class HaskellImplementationStudy {
  private readonly claudeCodeRunner: ClaudeCodeRunner;
  private readonly scorer: ImplementationScorer;
  private readonly resultsStore: StudyResultsStore;
  private progressCallback?: (progress: StudyProgress) => void;
  private anomalyDetector: ScoringAnomalyDetector = {
    scores: [],
    generationAttempts: 0,
    successfulGenerations: 0,
  };

  constructor(resultsDir = 'results/haskell-study') {
    this.claudeCodeRunner = new ClaudeCodeRunner();
    this.scorer = new ImplementationScorer();
    this.resultsStore = new StudyResultsStore(resultsDir);
  }

  /**
   * Circuit breaker: Detect if scoring behavior is anomalous and should stop the study
   * This prevents wasting money on meaningless results
   */
  private checkForScoringAnomalies(): void {
    const { scores, generationAttempts, successfulGenerations } = this.anomalyDetector;

    // Need at least 3 data points to detect patterns
    if (scores.length < 3) {
      return;
    }

    // Anomaly 1: Too many generation failures (>50% failure rate after 10 attempts)
    if (generationAttempts >= 10 && successfulGenerations / generationAttempts < 0.5) {
      throw new Error(
        `💰 CIRCUIT BREAKER: High failure rate (${Math.round((1 - successfulGenerations / generationAttempts) * 100)}%). Stopping to prevent money waste.`
      );
    }

    // Anomaly 2: All scores are zero (scorer is broken)
    if (scores.every((score) => score === 0)) {
      throw new Error(
        '💰 CIRCUIT BREAKER: All scores are 0. Scorer appears broken. Stopping to prevent money waste.'
      );
    }

    // Anomaly 3: All scores are identical (no differentiation)
    const uniqueScores = new Set(scores);
    if (uniqueScores.size === 1 && scores.length >= 5) {
      throw new Error(
        `💰 CIRCUIT BREAKER: All scores identical (${scores[0]}). No differentiation detected. Stopping to prevent money waste.`
      );
    }

    // Anomaly 4: All scores negative (impossible - should be 0-100)
    if (scores.every((score) => score < 0)) {
      throw new Error(
        '💰 CIRCUIT BREAKER: All scores negative. Scoring system malfunction. Stopping to prevent money waste.'
      );
    }

    // Anomaly 5: Scores outside valid range (should be 0-100)
    const invalidScores = scores.filter((score) => score < 0 || score > 100);
    if (invalidScores.length > 0) {
      throw new Error(
        `💰 CIRCUIT BREAKER: Invalid scores detected: ${invalidScores.join(', ')}. Should be 0-100. Stopping to prevent money waste.`
      );
    }

    // Anomaly 6: Very low variance suggests scoring isn't working properly
    if (scores.length >= 8) {
      const mean = scores.reduce((sum, score) => sum + score, 0) / scores.length;
      const variance = scores.reduce((sum, score) => sum + (score - mean) ** 2, 0) / scores.length;
      const standardDeviation = Math.sqrt(variance);

      // If standard deviation is very low, all scores are clustered together
      if (standardDeviation < 2 && mean < 20) {
        throw new Error(
          `💰 CIRCUIT BREAKER: Suspiciously low variance (σ=${standardDeviation.toFixed(1)}) with low scores. Likely scoring malfunction. Stopping to prevent money waste.`
        );
      }
    }

    console.log(
      `✅ Scoring health check passed (${scores.length} samples, σ=${this.calculateStandardDeviation().toFixed(1)})`
    );
  }

  private calculateStandardDeviation(): number {
    const { scores } = this.anomalyDetector;
    if (scores.length < 2) {
      return 0;
    }

    const mean = scores.reduce((sum, score) => sum + score, 0) / scores.length;
    const variance = scores.reduce((sum, score) => sum + (score - mean) ** 2, 0) / scores.length;
    return Math.sqrt(variance);
  }

  async runStudy(
    config: StudyConfig,
    progressCallback?: (progress: StudyProgress) => void
  ): Promise<StudyResult> {
    this.progressCallback = progressCallback;

    console.log('🚀 Starting Haskell Implementation Study...');
    console.log(`📋 Study: ${config.name}`);
    console.log(`🤖 Models: ${config.models.length}`);
    console.log(`📝 Instructions: ${config.instructions.length}`);
    console.log(`🔄 Runs per combination: ${config.runsPerCombination}`);

    // Test Claude Code availability
    const connectionTest = await this.claudeCodeRunner.testConnection();
    if (!connectionTest.available) {
      throw new Error(`Claude Code not available: ${connectionTest.error}`);
    }
    console.log(`✅ Claude Code connected (${connectionTest.version})`);

    // Initialize study
    const studyId = await this.resultsStore.initializeNewStudy();
    console.log(`📊 Study ID: ${studyId}`);

    // Reset anomaly detector for new study
    this.anomalyDetector = {
      scores: [],
      generationAttempts: 0,
      successfulGenerations: 0,
    };

    const totalRuns = config.models.length * config.instructions.length * config.runsPerCombination;
    let completedRuns = 0;
    const startTime = Date.now();

    // Run all combinations
    for (const model of config.models) {
      for (const instruction of config.instructions) {
        for (let run = 1; run <= config.runsPerCombination; run++) {
          console.log(
            `\n🔄 Run ${completedRuns + 1}/${totalRuns}: ${model.name} + ${instruction.name} (Run ${run})`
          );

          try {
            // Track generation attempt
            this.anomalyDetector.generationAttempts++;

            // Generate code
            const generatedCode = await this.claudeCodeRunner.generateCode(model, instruction);

            if (!generatedCode.success) {
              console.warn(`❌ Generation failed: ${generatedCode.error}`);
              // Check for anomalies even on failures (high failure rate detection)
              this.checkForScoringAnomalies();
              continue;
            }

            // Track successful generation
            this.anomalyDetector.successfulGenerations++;

            // Score implementation
            const score = await this.scorer.scoreImplementation(generatedCode);

            // Track score for anomaly detection
            this.anomalyDetector.scores.push(score.overallScore);

            // 💰 MONEY-SAVING CHECK: Stop if scoring behavior is anomalous
            this.checkForScoringAnomalies();

            // Store result (keeps best/worst, discards middle)
            await this.resultsStore.addImplementation(
              generatedCode,
              score,
              `${model.id}-${instruction.id}-run${run}`
            );

            console.log(`✅ Implementation scored: ${score.overallScore}/100`);
            console.log(
              `   Contract: ${score.contractCompliance}% | Modern: ${score.modernityScore}% | Complete: ${score.completenessScore}% | Quality: ${score.qualityScore}%`
            );
          } catch (error) {
            console.error(`❌ Error in run ${completedRuns + 1}:`, error);

            // If it's our circuit breaker, re-throw to stop the study
            if (error instanceof Error && error.message.includes('CIRCUIT BREAKER')) {
              throw error;
            }
          }

          completedRuns++;

          // Update progress
          if (this.progressCallback) {
            const progress = await this.calculateProgress(
              completedRuns,
              totalRuns,
              model.name,
              instruction.name,
              run,
              startTime
            );
            this.progressCallback(progress);
          }

          // 🛡️ SAFETY DELAY: Random wait to avoid automated usage detection
          if (completedRuns < totalRuns && config.safetyConfig?.enableDelays === true) {
            const minDelay = config.safetyConfig.minDelaySeconds;
            const maxDelay = config.safetyConfig.maxDelaySeconds;
            const delayRange = maxDelay - minDelay;
            const delaySeconds = minDelay + Math.random() * delayRange;
            console.log(
              `⏳ Waiting ${Math.round(delaySeconds)}s before next generation (safety delay)...`
            );
            await new Promise((resolve) => setTimeout(resolve, delaySeconds * 1000));
          }
        }
      }
    }

    // Generate final results
    const finalResults = await this.generateFinalResults(studyId);

    // Export results
    await this.resultsStore.exportResults();

    console.log('\n🎉 Study completed successfully!');
    console.log(`📊 Total implementations: ${finalResults.totalImplementations}`);
    console.log(`🏆 Best score: ${finalResults.bestImplementation.score}/100`);
    console.log(`📉 Worst score: ${finalResults.worstImplementation.score}/100`);
    console.log(`📈 Average score: ${finalResults.statistics.averageScore}/100`);

    return finalResults;
  }

  async runSingleGeneration(
    model: AIModel,
    instruction: InstructionSet,
    runId?: string
  ): Promise<{ code: string; score: number; runId: string }> {
    console.log(`🔄 Single generation: ${model.name} + ${instruction.name}`);

    // Generate code
    const generatedCode = await this.claudeCodeRunner.generateCode(model, instruction);

    if (!generatedCode.success) {
      throw new Error(`Generation failed: ${generatedCode.error}`);
    }

    // Score implementation
    const score = await this.scorer.scoreImplementation(generatedCode);

    // Store result
    const finalRunId = runId ?? `single-${Date.now()}`;
    await this.resultsStore.addImplementation(generatedCode, score, finalRunId);

    console.log(`✅ Generated and scored: ${score.overallScore}/100`);

    return {
      code: generatedCode.code,
      score: score.overallScore,
      runId: finalRunId,
    };
  }

  async loadConfigFromFile(configPath: string): Promise<StudyConfig> {
    try {
      const configContent = await readFile(configPath, 'utf-8');
      return JSON.parse(configContent) as StudyConfig;
    } catch (error) {
      throw new Error(
        `Failed to load config from ${configPath}: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }

  async loadInstructionFromFile(filePath: string): Promise<string> {
    try {
      return await readFile(filePath, 'utf-8');
    } catch (error) {
      throw new Error(
        `Failed to load instruction from ${filePath}: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }

  private async calculateProgress(
    completedRuns: number,
    totalRuns: number,
    currentModel: string,
    currentInstruction: string,
    currentRun: number,
    startTime: number
  ): Promise<StudyProgress> {
    const elapsedTime = Date.now() - startTime;
    const averageTimePerRun = elapsedTime / completedRuns;
    const remainingRuns = totalRuns - completedRuns;
    const estimatedTimeRemaining = remainingRuns * averageTimePerRun;

    const summary = await this.resultsStore.getScoreSummary();

    return {
      totalRuns,
      completedRuns,
      currentModel,
      currentInstruction,
      currentRun,
      bestScore: summary.bestScore,
      worstScore: summary.worstScore,
      averageScore: summary.averageScore,
      estimatedTimeRemaining: Math.round(estimatedTimeRemaining / 1000), // seconds
    };
  }

  private async generateFinalResults(studyId: string): Promise<StudyResult> {
    const studyResults = await this.resultsStore.getStudyResults();
    const summary = await this.resultsStore.getScoreSummary();

    if (!studyResults.bestImplementation || !studyResults.worstImplementation) {
      throw new Error('No implementations found in study results');
    }

    return {
      studyId,
      totalImplementations: summary.totalRuns,
      bestImplementation: {
        code: studyResults.bestImplementation.code.code,
        score: studyResults.bestImplementation.score.overallScore,
        runId: studyResults.bestImplementation.runId,
      },
      worstImplementation: {
        code: studyResults.worstImplementation.code.code,
        score: studyResults.worstImplementation.score.overallScore,
        runId: studyResults.worstImplementation.runId,
      },
      statistics: {
        averageScore: summary.averageScore,
        scoreRange: summary.scoreRange,
        standardDeviation: studyResults.scoreDistribution.stdDev,
      },
    };
  }

  // Quick test methods
  async testClaudeCodeConnection(): Promise<boolean> {
    const result = await this.claudeCodeRunner.testConnection();
    console.log(
      result.available
        ? '✅ Claude Code is available'
        : `❌ Claude Code unavailable: ${result.error}`
    );
    return result.available;
  }

  async debugPrompt(instruction: InstructionSet): Promise<string> {
    return this.claudeCodeRunner.savePromptForDebugging(instruction);
  }
}
