import { existsSync } from 'node:fs';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import type { ImplementationScore } from '@/evaluation/implementation-scorer';
import type { GeneratedCode } from '@/types/study';

export interface ScoredImplementation {
  code: GeneratedCode;
  score: ImplementationScore;
  runId: string;
  timestamp: string;
}

export interface StudyResults {
  studyId: string;
  bestImplementation: ScoredImplementation | null;
  worstImplementation: ScoredImplementation | null;
  allScores: Array<{ runId: string; overallScore: number; timestamp: string }>;
  totalRuns: number;
  averageScore: number;
  scoreDistribution: {
    min: number;
    max: number;
    mean: number;
    median: number;
    stdDev: number;
  };
}

export class StudyResultsStore {
  private readonly resultsDir: string;
  private currentStudy: StudyResults;

  constructor(resultsDir = 'results') {
    this.resultsDir = resultsDir;
    this.currentStudy = this.initializeStudy();
  }

  async initializeNewStudy(studyId?: string): Promise<string> {
    const id = studyId ?? this.generateStudyId();
    this.currentStudy = {
      studyId: id,
      bestImplementation: null,
      worstImplementation: null,
      allScores: [],
      totalRuns: 0,
      averageScore: 0,
      scoreDistribution: {
        min: 0,
        max: 0,
        mean: 0,
        median: 0,
        stdDev: 0,
      },
    };

    await this.ensureResultsDirectory();
    await this.saveStudyResults();
    return id;
  }

  async addImplementation(
    code: GeneratedCode,
    score: ImplementationScore,
    runId?: string
  ): Promise<void> {
    const implementation: ScoredImplementation = {
      code,
      score,
      runId: runId ?? this.generateRunId(),
      timestamp: new Date().toISOString(),
    };

    // Update best implementation
    if (
      this.currentStudy.bestImplementation === null ||
      score.overallScore > this.currentStudy.bestImplementation.score.overallScore
    ) {
      this.currentStudy.bestImplementation = implementation;
    }

    // Update worst implementation
    if (
      this.currentStudy.worstImplementation === null ||
      score.overallScore < this.currentStudy.worstImplementation.score.overallScore
    ) {
      this.currentStudy.worstImplementation = implementation;
    }

    // Add to scores tracking (lightweight)
    this.currentStudy.allScores.push({
      runId: implementation.runId,
      overallScore: score.overallScore,
      timestamp: implementation.timestamp,
    });

    // Update statistics
    this.updateStatistics();

    // Save results
    await this.saveStudyResults();

    // eslint-disable-next-line no-console
    console.log(`✅ Added implementation ${implementation.runId} (Score: ${score.overallScore})`);
    // eslint-disable-next-line no-console
    console.log(
      `📊 Current best: ${this.currentStudy.bestImplementation.score.overallScore}, worst: ${this.currentStudy.worstImplementation.score.overallScore}`
    );
  }

  async getStudyResults(): Promise<StudyResults> {
    return { ...this.currentStudy };
  }

  async getBestImplementation(): Promise<ScoredImplementation | null> {
    return this.currentStudy.bestImplementation;
  }

  async getWorstImplementation(): Promise<ScoredImplementation | null> {
    return this.currentStudy.worstImplementation;
  }

  async getScoreSummary(): Promise<{
    totalRuns: number;
    averageScore: number;
    bestScore: number;
    worstScore: number;
    scoreRange: number;
  }> {
    const best = this.currentStudy.bestImplementation?.score.overallScore ?? 0;
    const worst = this.currentStudy.worstImplementation?.score.overallScore ?? 0;

    return {
      totalRuns: this.currentStudy.totalRuns,
      averageScore: this.currentStudy.averageScore,
      bestScore: best,
      worstScore: worst,
      scoreRange: best - worst,
    };
  }

  async exportResults(): Promise<string> {
    const exportData = {
      ...this.currentStudy,
      exportTimestamp: new Date().toISOString(),
      metadata: {
        version: '1.0',
        framework: 'QiCore Research Framework',
        description: 'Haskell Implementation Quality Study Results',
      },
    };

    const exportPath = join(this.resultsDir, `${this.currentStudy.studyId}-export.json`);
    await writeFile(exportPath, JSON.stringify(exportData, null, 2));

    // eslint-disable-next-line no-console
    console.log(`📁 Results exported to: ${exportPath}`);
    return exportPath;
  }

  async loadStudy(studyId: string): Promise<boolean> {
    try {
      const studyPath = join(this.resultsDir, `${studyId}.json`);
      const data = await readFile(studyPath, 'utf-8');
      this.currentStudy = JSON.parse(data) as StudyResults;
      // eslint-disable-next-line no-console
      console.log(`📂 Loaded study: ${studyId} (${this.currentStudy.totalRuns} runs)`);
      return true;
    } catch (error) {
      // eslint-disable-next-line no-console
      console.warn(`❌ Could not load study ${studyId}:`, error);
      return false;
    }
  }

  private initializeStudy(): StudyResults {
    return {
      studyId: this.generateStudyId(),
      bestImplementation: null,
      worstImplementation: null,
      allScores: [],
      totalRuns: 0,
      averageScore: 0,
      scoreDistribution: {
        min: 0,
        max: 0,
        mean: 0,
        median: 0,
        stdDev: 0,
      },
    };
  }

  private updateStatistics(): void {
    const scores = this.currentStudy.allScores.map((s) => s.overallScore);

    this.currentStudy.totalRuns = scores.length;

    if (scores.length > 0) {
      const sum = scores.reduce((a, b) => a + b, 0);
      this.currentStudy.averageScore = Math.round((sum / scores.length) * 100) / 100;

      const sortedScores = [...scores].sort((a, b) => a - b);
      const [min] = sortedScores;
      const max = sortedScores[sortedScores.length - 1];
      const median = this.calculateMedian(sortedScores);
      const stdDev = this.calculateStandardDeviation(scores, this.currentStudy.averageScore);

      this.currentStudy.scoreDistribution = {
        min,
        max,
        mean: this.currentStudy.averageScore,
        median,
        stdDev,
      };
    }
  }

  private calculateMedian(sortedScores: number[]): number {
    const mid = Math.floor(sortedScores.length / 2);
    return sortedScores.length % 2 === 0
      ? (sortedScores[mid - 1] + sortedScores[mid]) / 2
      : sortedScores[mid];
  }

  private calculateStandardDeviation(scores: number[], mean: number): number {
    const variance = scores.reduce((acc, score) => acc + (score - mean) ** 2, 0) / scores.length;
    return Math.round(Math.sqrt(variance) * 100) / 100;
  }

  private async ensureResultsDirectory(): Promise<void> {
    if (!existsSync(this.resultsDir)) {
      await mkdir(this.resultsDir, { recursive: true });
    }
  }

  private async saveStudyResults(): Promise<void> {
    await this.ensureResultsDirectory();
    const studyPath = join(this.resultsDir, `${this.currentStudy.studyId}.json`);
    await writeFile(studyPath, JSON.stringify(this.currentStudy, null, 2));
  }

  private generateStudyId(): string {
    const timestamp = new Date().toISOString().slice(0, 19).replace(/:/g, '-');
    return `haskell-study-${timestamp}`;
  }

  private generateRunId(): string {
    const timestamp = Date.now();
    const random = Math.random().toString(36).substring(2, 8);
    return `run-${timestamp}-${random}`;
  }
}
