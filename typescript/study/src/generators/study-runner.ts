import { randomUUID } from 'node:crypto';
import type { StudyDatabase } from '@/database/study-db';
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

  constructor(
    private readonly config: StudyConfig,
    private readonly db: StudyDatabase
  ) {
    this.generator = new AICodeGenerator();
    this.evaluator = new CodeEvaluator();
    this.analyzer = new StatisticalAnalyzer();
  }

  async runCompleteStudy(): Promise<string> {
    const studyId = randomUUID();

    console.log(`🏗️  Creating study: ${studyId}`);
    await this.db.createStudy(studyId, this.config.name, this.config.description);

    try {
      // Load instruction content
      await this.loadInstructionContent();

      const totalCombinations = this.config.models.length * this.config.instructions.length;
      let completedGenerations = 0;
      const totalGenerations = totalCombinations * this.config.runsPerCombination;

      console.log(`📝 Starting ${totalGenerations} code generations...`);

      // Generate code for each model x instruction x run combination
      for (const model of this.config.models) {
        for (const instruction of this.config.instructions) {
          console.log(`\\n🤖 Model: ${model.name} | 📋 Instruction: ${instruction.name}`);

          for (let runNumber = 1; runNumber <= this.config.runsPerCombination; runNumber++) {
            const progress = (((completedGenerations + 1) / totalGenerations) * 100).toFixed(1);
            console.log(`   Run ${runNumber}/${this.config.runsPerCombination} (${progress}%)`);

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
              console.error('   ❌ Generation failed:', error);
            }
          }
        }
      }

      await this.db.updateStudyStatus(studyId, 'completed');
      console.log(
        `\\n✅ Study completed: ${completedGenerations}/${totalGenerations} generations successful`
      );

      return studyId;
    } catch (error) {
      await this.db.updateStudyStatus(studyId, 'failed');
      throw error;
    }
  }

  async analyzeStudy(studyId: string): Promise<StudyStatistics> {
    console.log(`📊 Analyzing study: ${studyId}`);

    const results = await this.db.getGenerationResults(studyId);

    if (results.length === 0) {
      throw new Error(`No results found for study: ${studyId}`);
    }

    console.log(`📈 Processing ${results.length} generation results...`);

    const statistics = this.analyzer.calculateStatistics(studyId, results);

    await this.db.saveStatistics(statistics);

    // Print summary
    this.printAnalysisSummary(statistics);

    return statistics;
  }

  private async loadInstructionContent(): Promise<void> {
    console.log('📁 Loading instruction files...');

    for (const instruction of this.config.instructions) {
      try {
        const file = Bun.file(instruction.filePath);
        instruction.content = await file.text();
        console.log(`   ✅ Loaded: ${instruction.name}`);
      } catch (error) {
        console.error(`   ❌ Failed to load: ${instruction.filePath}`, error);
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
    console.log('\\n📊 Study Analysis Summary');
    console.log(`   Total Generations: ${stats.totalGenerations}`);
    console.log(`   Mean Score: ${stats.meanScore.toFixed(2)}`);
    console.log(`   Standard Deviation: ${stats.standardDeviation.toFixed(2)}`);
    console.log(`   Coefficient of Variation: ${(stats.coefficientOfVariation * 100).toFixed(1)}%`);
    console.log(`   Consistency Rank: ${stats.consistencyRank.toUpperCase()}`);

    console.log('\\n🤖 Model Comparison:');
    stats.modelComparison
      .sort((a, b) => b.meanScore - a.meanScore)
      .forEach((model, index) => {
        console.log(
          `   ${index + 1}. ${model.modelId}: ${model.meanScore.toFixed(2)} (CV: ${(model.consistency * 100).toFixed(1)}%)`
        );
      });

    console.log('\\n📋 Instruction Comparison:');
    stats.instructionComparison
      .sort((a, b) => b.meanScore - a.meanScore)
      .forEach((instruction, index) => {
        console.log(
          `   ${index + 1}. ${instruction.instructionId}: ${instruction.meanScore.toFixed(2)} (CV: ${(instruction.consistency * 100).toFixed(1)}%)`
        );
      });
  }
}
