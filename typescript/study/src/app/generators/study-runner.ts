import { randomUUID } from 'node:crypto';
import type { StudyDatabase } from '@/database/study-db';
import { createDefault as createDefaultLogger } from '@/qicore/core/logger';
import type {
  AIModel,
  GenerationResult,
  InstructionSet,
  StudyConfig,
  StudyStatistics,
} from '@/types/study';
import { StatisticalAnalyzer } from '../analysis/statistical-analyzer';
import { CodeEvaluator } from '../evaluation/code-evaluator';
import { AICodeGenerator } from './ai-code-generator';

export class StudyRunner {
  private readonly generator: AICodeGenerator;
  private readonly evaluator: CodeEvaluator;
  private readonly analyzer: StatisticalAnalyzer;
  private readonly logger;

  constructor(
    private readonly config: StudyConfig,
    private readonly db: StudyDatabase
  ) {
    this.generator = new AICodeGenerator();
    this.evaluator = new CodeEvaluator();
    this.analyzer = new StatisticalAnalyzer();

    // Initialize logger
    const loggerResult = createDefaultLogger();
    if (loggerResult._tag === 'Right') {
      this.logger = loggerResult.right;
    } else {
      // Fallback to console if logger creation fails
      this.logger = console;
    }
  }

  async runCompleteStudy(): Promise<string> {
    const studyId = randomUUID();

    this.logger.log(`🏗️  Creating study: ${studyId}`);
    await this.db.createStudy(studyId, this.config.name, this.config.description);

    try {
      // Load instruction content
      await this.loadInstructionContent();

      const totalCombinations = this.config.models.length * this.config.instructions.length;
      let completedGenerations = 0;
      const totalGenerations = totalCombinations * this.config.runsPerCombination;

      this.logger.log(`📝 Starting ${totalGenerations} code generations...`);

      // Generate code for each model x instruction x run combination
      for (const model of this.config.models) {
        for (const instruction of this.config.instructions) {
          this.logger.log(`\\n🤖 Model: ${model.name} | 📋 Instruction: ${instruction.name}`);

          for (let runNumber = 1; runNumber <= this.config.runsPerCombination; runNumber++) {
            const progress = (((completedGenerations + 1) / totalGenerations) * 100).toFixed(1);
            this.logger.log(`   Run ${runNumber}/${this.config.runsPerCombination} (${progress}%)`);

            try {
              // Generate code
              const generationResult = await this.generateAndEvaluateCode(
                studyId,
                model,
                instruction,
                runNumber
              );

              // Save to database
              await this.db.insertGenerationResult(generationResult);
              completedGenerations++;
            } catch (error) {
              this.logger.error('   ❌ Generation failed:', error);
            }
          }
        }
      }

      await this.db.updateStudyStatus(studyId, 'completed');
      this.logger.log(
        `\\n✅ Study completed: ${completedGenerations}/${totalGenerations} generations successful`
      );

      return studyId;
    } catch (error) {
      await this.db.updateStudyStatus(studyId, 'failed');
      throw error;
    }
  }

  async analyzeStudy(studyId: string): Promise<StudyStatistics> {
    this.logger.log(`📊 Analyzing study: ${studyId}`);

    const results = await this.db.getGenerationResults(studyId);

    if (results.length === 0) {
      throw new Error(`No results found for study: ${studyId}`);
    }

    this.logger.log(`📈 Processing ${results.length} generation results...`);

    const statistics = this.analyzer.calculateStatistics(studyId, results);

    await this.db.saveStatistics(statistics);

    // Print summary
    this.printAnalysisSummary(statistics);

    return statistics;
  }

  private async loadInstructionContent(): Promise<void> {
    this.logger.log('📁 Loading instruction files...');

    for (const instruction of this.config.instructions) {
      try {
        const file = Bun.file(instruction.filePath);
        instruction.content = await file.text();
        this.logger.log(`   ✅ Loaded: ${instruction.name}`);
      } catch (error) {
        this.logger.error(`   ❌ Failed to load: ${instruction.filePath}`, error);
        throw new Error(`Cannot load instruction file: ${instruction.filePath}`);
      }
    }
  }

  private async generateAndEvaluateCode(
    studyId: string,
    model: AIModel,
    instruction: InstructionSet,
    runNumber: number
  ): Promise<GenerationResult> {
    const startTime = Date.now();

    // Generate code
    const generationResponse = await this.generator.generateCode(model, instruction);

    // Evaluate the generated code
    const compilationResult = await this.evaluator.compileCode(generationResponse.code);
    const scores = await this.evaluator.scoreCode(generationResponse.code, instruction.category);

    const endTime = Date.now();
    const responseTime = endTime - startTime;

    const result: GenerationResult = {
      id: randomUUID(),
      studyId,
      modelId: model.id,
      instructionId: instruction.id,
      runNumber,
      timestamp: new Date(),
      generatedCode: generationResponse.code,
      compilationResult,
      scores,
      metadata: {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
        tokensUsed:
          typeof generationResponse.metadata?.tokensUsed === 'number'
            ? generationResponse.metadata.tokensUsed
            : 0,
        responseTime,
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
        promptTokens:
          typeof generationResponse.metadata?.promptTokens === 'number'
            ? generationResponse.metadata.promptTokens
            : 0,
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
        completionTokens:
          typeof generationResponse.metadata?.completionTokens === 'number'
            ? generationResponse.metadata.completionTokens
            : 0,
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
        embeddingVector: Array.isArray(generationResponse.metadata?.embeddingVector)
          ? generationResponse.metadata.embeddingVector
          : undefined,
      },
    };

    return result;
  }

  private printAnalysisSummary(stats: StudyStatistics): void {
    this.logger.log('\\n📊 Study Analysis Summary');
    this.logger.log(`   Total Generations: ${stats.totalGenerations}`);
    this.logger.log(`   Mean Score: ${stats.meanScore.toFixed(2)}`);
    this.logger.log(`   Standard Deviation: ${stats.standardDeviation.toFixed(2)}`);
    this.logger.log(
      `   Coefficient of Variation: ${(stats.coefficientOfVariation * 100).toFixed(1)}%`
    );
    this.logger.log(`   Consistency Rank: ${stats.consistencyRank.toUpperCase()}`);

    this.logger.log('\\n🤖 Model Comparison:');
    stats.modelComparison
      .sort((a, b) => b.meanScore - a.meanScore)
      .forEach((model, index) => {
        this.logger.log(
          `   ${index + 1}. ${model.modelId}: ${model.meanScore.toFixed(2)} (CV: ${(model.consistency * 100).toFixed(1)}%)`
        );
      });

    this.logger.log('\\n📋 Instruction Comparison:');
    stats.instructionComparison
      .sort((a, b) => b.meanScore - a.meanScore)
      .forEach((instruction, index) => {
        this.logger.log(
          `   ${index + 1}. ${instruction.instructionId}: ${instruction.meanScore.toFixed(2)} (CV: ${(instruction.consistency * 100).toFixed(1)}%)`
        );
      });
  }
}
