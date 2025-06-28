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

export class HaskellImplementationStudy {
  private readonly claudeCodeRunner: ClaudeCodeRunner;
  private readonly scorer: ImplementationScorer;
  private readonly resultsStore: StudyResultsStore;
  private progressCallback?: (progress: StudyProgress) => void;

  constructor(resultsDir = 'results/haskell-study') {
    this.claudeCodeRunner = new ClaudeCodeRunner();
    this.scorer = new ImplementationScorer();
    this.resultsStore = new StudyResultsStore(resultsDir);
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
            // Generate code
            const generatedCode = await this.claudeCodeRunner.generateCode(model, instruction);

            if (!generatedCode.success) {
              console.warn(`❌ Generation failed: ${generatedCode.error}`);
              continue;
            }

            // Score implementation
            const score = await this.scorer.scoreImplementation(generatedCode);

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
