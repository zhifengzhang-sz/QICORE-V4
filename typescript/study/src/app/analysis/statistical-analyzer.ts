import type {
  GenerationResult,
  InstructionComparison,
  ModelComparison,
  StudyStatistics,
} from '../types/study';

export class StatisticalAnalyzer {
  calculateStatistics(studyId: string, results: GenerationResult[]): StudyStatistics {
    const totalGenerations = results.length;
    const overallScores = results.map((r) => r.scores.overall);

    // Calculate basic statistics
    const meanScore = this.calculateMean(overallScores);
    const standardDeviation = this.calculateStandardDeviation(overallScores, meanScore);
    const coefficientOfVariation = standardDeviation / meanScore;

    // Determine consistency rank
    const consistencyRank = this.getConsistencyRank(coefficientOfVariation);

    // Group by model for comparison
    const modelComparison = this.calculateModelComparison(results);

    // Group by instruction for comparison
    const instructionComparison = this.calculateInstructionComparison(results);

    return {
      studyId,
      totalGenerations,
      meanScore,
      standardDeviation,
      coefficientOfVariation,
      consistencyRank,
      modelComparison,
      instructionComparison,
    };
  }

  private calculateMean(scores: number[]): number {
    return scores.reduce((sum, score) => sum + score, 0) / scores.length;
  }

  private calculateStandardDeviation(scores: number[], mean: number): number {
    const variance = scores.reduce((sum, score) => sum + (score - mean) ** 2, 0) / scores.length;
    return Math.sqrt(variance);
  }

  private getConsistencyRank(coefficientOfVariation: number): 'high' | 'medium' | 'low' {
    if (coefficientOfVariation < 0.1) {
      return 'high';
    }
    if (coefficientOfVariation < 0.2) {
      return 'medium';
    }
    return 'low';
  }

  private calculateModelComparison(results: GenerationResult[]): ModelComparison[] {
    const modelGroups = new Map<string, number[]>();

    // Group scores by model
    for (const result of results) {
      const scores = modelGroups.get(result.modelId) ?? [];
      scores.push(result.scores.overall);
      modelGroups.set(result.modelId, scores);
    }

    // Calculate statistics for each model
    const comparisons: ModelComparison[] = [];
    for (const [modelId, scores] of modelGroups) {
      const meanScore = this.calculateMean(scores);
      const standardDeviation = this.calculateStandardDeviation(scores, meanScore);
      const consistency = standardDeviation / meanScore;

      comparisons.push({
        modelId,
        meanScore,
        consistency,
        rank: 0, // Will be set after sorting
      });
    }

    // Sort by mean score and assign ranks
    comparisons.sort((a, b) => b.meanScore - a.meanScore);
    comparisons.forEach((comparison, index) => {
      comparison.rank = index + 1;
    });

    return comparisons;
  }

  private calculateInstructionComparison(results: GenerationResult[]): InstructionComparison[] {
    const instructionGroups = new Map<string, number[]>();

    // Group scores by instruction
    for (const result of results) {
      const scores = instructionGroups.get(result.instructionId) ?? [];
      scores.push(result.scores.overall);
      instructionGroups.set(result.instructionId, scores);
    }

    // Calculate statistics for each instruction
    const comparisons: InstructionComparison[] = [];
    for (const [instructionId, scores] of instructionGroups) {
      const meanScore = this.calculateMean(scores);
      const standardDeviation = this.calculateStandardDeviation(scores, meanScore);
      const consistency = standardDeviation / meanScore;

      comparisons.push({
        instructionId,
        meanScore,
        consistency,
        rank: 0, // Will be set after sorting
      });
    }

    // Sort by mean score and assign ranks
    comparisons.sort((a, b) => b.meanScore - a.meanScore);
    comparisons.forEach((comparison, index) => {
      comparison.rank = index + 1;
    });

    return comparisons;
  }
}
