import type { CompilationResult, QualityScores } from '@/types/study';

export class CodeEvaluator {
  async compileCode(code: string): Promise<CompilationResult> {
    // TODO: Implement actual compilation using GHC or similar
    // For now, return mock compilation result
    const hasBasicSyntax = code.includes('module') && code.includes('where');

    return {
      success: hasBasicSyntax,
      errors: hasBasicSyntax ? [] : ['Missing module declaration'],
      warnings: code.includes('TODO') ? ['Contains TODO comments'] : [],
      executionTime: Math.random() * 1000, // Mock execution time
    };
  }

  async scoreCode(code: string, category: 'modern' | 'simple' | 'setup'): Promise<QualityScores> {
    // TODO: Implement actual code quality scoring
    // For now, return mock scores based on basic heuristics
    const baseScore = Math.random() * 40 + 60; // Random score between 60-100

    const scores: QualityScores = {
      syntactic: baseScore + (Math.random() * 10 - 5),
      semantic: baseScore + (Math.random() * 10 - 5),
      modern: category === 'modern' ? baseScore + 10 : baseScore - 5,
      completeness: baseScore + (Math.random() * 10 - 5),
      documentation: code.includes('--') ? baseScore + 5 : baseScore - 10,
      performance: baseScore + (Math.random() * 10 - 5),
      overall: 0, // Will be calculated
    };

    // Calculate overall score as weighted average
    scores.overall =
      scores.syntactic * 0.2 +
      scores.semantic * 0.25 +
      scores.modern * 0.15 +
      scores.completeness * 0.2 +
      scores.documentation * 0.1 +
      scores.performance * 0.1;

    // Ensure all scores are within 0-100 range
    for (const key of Object.keys(scores)) {
      const scoreKey = key as keyof QualityScores;
      scores[scoreKey] = Math.max(0, Math.min(100, scores[scoreKey]));
    }

    return scores;
  }
}
