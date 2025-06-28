import { Database } from 'bun:sqlite';
import type { GenerationResult, StudyStatistics } from '@/types/study';

interface GenerationDbRow {
  id: string;
  study_id: string;
  model_id: string;
  instruction_id: string;
  run_number: number;
  timestamp: string;
  generated_code: string;
  compilation_success: number; // SQLite boolean as integer
  compilation_errors: string;
  compilation_warnings: string;
  execution_time: number;
  syntactic_score: number;
  semantic_score: number;
  modern_score: number;
  completeness_score: number;
  documentation_score: number;
  performance_score: number;
  overall_score: number;
  tokens_used: number;
  response_time: number;
  prompt_tokens: number;
  completion_tokens: number;
  embedding_vector: string | null;
}

export class StudyDatabase {
  private readonly db: Database;

  constructor(dbPath = 'study-results.db') {
    this.db = new Database(dbPath);
    this.initializeDatabase();
  }

  private initializeDatabase(): void {
    // Create tables for storing study results
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS studies (
        id TEXT PRIMARY KEY,
        name TEXT NOT NULL,
        description TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        completed_at DATETIME,
        status TEXT DEFAULT 'running'
      )
    `);

    this.db.exec(`
      CREATE TABLE IF NOT EXISTS generations (
        id TEXT PRIMARY KEY,
        study_id TEXT NOT NULL,
        model_id TEXT NOT NULL,
        instruction_id TEXT NOT NULL,
        run_number INTEGER NOT NULL,
        timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
        generated_code TEXT NOT NULL,
        compilation_success BOOLEAN DEFAULT FALSE,
        compilation_errors TEXT DEFAULT '[]',
        compilation_warnings TEXT DEFAULT '[]',
        execution_time REAL DEFAULT 0,
        syntactic_score REAL DEFAULT 0,
        semantic_score REAL DEFAULT 0,
        modern_score REAL DEFAULT 0,
        completeness_score REAL DEFAULT 0,
        documentation_score REAL DEFAULT 0,
        performance_score REAL DEFAULT 0,
        overall_score REAL DEFAULT 0,
        tokens_used INTEGER DEFAULT 0,
        response_time REAL DEFAULT 0,
        prompt_tokens INTEGER DEFAULT 0,
        completion_tokens INTEGER DEFAULT 0,
        embedding_vector TEXT DEFAULT NULL,
        FOREIGN KEY (study_id) REFERENCES studies(id)
      )
    `);

    this.db.exec(`
      CREATE TABLE IF NOT EXISTS study_statistics (
        study_id TEXT PRIMARY KEY,
        total_generations INTEGER NOT NULL,
        mean_score REAL NOT NULL,
        standard_deviation REAL NOT NULL,
        coefficient_of_variation REAL NOT NULL,
        consistency_rank TEXT NOT NULL,
        model_comparison TEXT NOT NULL,
        instruction_comparison TEXT NOT NULL,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (study_id) REFERENCES studies(id)
      )
    `);

    // Create indexes for better query performance
    this.db.exec(`
      CREATE INDEX IF NOT EXISTS idx_generations_study_model 
      ON generations(study_id, model_id)
    `);

    this.db.exec(`
      CREATE INDEX IF NOT EXISTS idx_generations_instruction 
      ON generations(study_id, instruction_id)
    `);
  }

  async insertGenerationResult(result: GenerationResult): Promise<void> {
    const stmt = this.db.prepare(`
      INSERT INTO generations (
        id, study_id, model_id, instruction_id, run_number, 
        generated_code, compilation_success, compilation_errors, 
        compilation_warnings, execution_time, syntactic_score, 
        semantic_score, modern_score, completeness_score, 
        documentation_score, performance_score, overall_score,
        tokens_used, response_time, prompt_tokens, completion_tokens,
        embedding_vector
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    `);

    stmt.run(
      result.id,
      result.studyId,
      result.modelId,
      result.instructionId,
      result.runNumber,
      result.generatedCode,
      result.compilationResult?.success ?? false,
      JSON.stringify(result.compilationResult?.errors ?? []),
      JSON.stringify(result.compilationResult?.warnings ?? []),
      result.compilationResult?.executionTime ?? 0,
      result.scores.syntactic,
      result.scores.semantic,
      result.scores.modern,
      result.scores.completeness,
      result.scores.documentation,
      result.scores.performance,
      result.scores.overall,
      result.metadata.tokensUsed,
      result.metadata.responseTime,
      result.metadata.promptTokens,
      result.metadata.completionTokens,
      JSON.stringify(result.metadata.embeddingVector)
    );
  }

  async getGenerationResults(studyId: string): Promise<GenerationResult[]> {
    const stmt = this.db.prepare(`
      SELECT * FROM generations WHERE study_id = ? ORDER BY timestamp
    `);

    const rows = stmt.all(studyId) as GenerationDbRow[];

    return rows.map((row) => ({
      id: row.id,
      studyId: row.study_id,
      modelId: row.model_id,
      instructionId: row.instruction_id,
      runNumber: row.run_number,
      timestamp: new Date(row.timestamp),
      generatedCode: row.generated_code,
      compilationResult: {
        success: Boolean(row.compilation_success),
        errors: JSON.parse(row.compilation_errors) as string[],
        warnings: JSON.parse(row.compilation_warnings) as string[],
        executionTime: row.execution_time,
      },
      scores: {
        syntactic: row.syntactic_score,
        semantic: row.semantic_score,
        modern: row.modern_score,
        completeness: row.completeness_score,
        documentation: row.documentation_score,
        performance: row.performance_score,
        overall: row.overall_score,
      },
      metadata: {
        tokensUsed: row.tokens_used,
        responseTime: row.response_time,
        promptTokens: row.prompt_tokens,
        completionTokens: row.completion_tokens,
        embeddingVector:
          row.embedding_vector !== null && row.embedding_vector !== ''
            ? (JSON.parse(row.embedding_vector) as number[])
            : undefined,
      },
    }));
  }

  async createStudy(id: string, name: string, description: string): Promise<void> {
    const stmt = this.db.prepare(`
      INSERT INTO studies (id, name, description) VALUES (?, ?, ?)
    `);
    stmt.run(id, name, description);
  }

  async updateStudyStatus(studyId: string, status: string): Promise<void> {
    const stmt = this.db.prepare(`
      UPDATE studies SET status = ?, completed_at = CURRENT_TIMESTAMP WHERE id = ?
    `);
    stmt.run(status, studyId);
  }

  async saveStatistics(stats: StudyStatistics): Promise<void> {
    const stmt = this.db.prepare(`
      INSERT OR REPLACE INTO study_statistics (
        study_id, total_generations, mean_score, standard_deviation,
        coefficient_of_variation, consistency_rank, model_comparison,
        instruction_comparison
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    `);

    stmt.run(
      stats.studyId,
      stats.totalGenerations,
      stats.meanScore,
      stats.standardDeviation,
      stats.coefficientOfVariation,
      stats.consistencyRank,
      JSON.stringify(stats.modelComparison),
      JSON.stringify(stats.instructionComparison)
    );
  }

  close(): void {
    this.db.close();
  }
}
