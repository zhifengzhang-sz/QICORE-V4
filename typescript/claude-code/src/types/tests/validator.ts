import { readFile, access } from 'node:fs/promises';
import { join } from 'node:path';
import type { ExecutionResult, ValidationResult, TestResult } from '@/types';

/**
 * Validates execution results against quality criteria
 */
export class ResultValidator {
  constructor(private baseDir: string) {}

  /**
   * Validate an execution result against QiCore quality standards
   */
  async validateExecution(result: ExecutionResult, expectedFiles?: string[]): Promise<TestResult> {
    const validationResults: ValidationResult[] = [];

    // Basic execution validation
    validationResults.push(this.validateBasicExecution(result));

    // Content quality validation
    if (result.response.success && result.response.content) {
      validationResults.push(await this.validateContentQuality(result.response.content));
    }

    // File existence validation
    if (expectedFiles) {
      const fileValidation = await this.validateGeneratedFiles(expectedFiles);
      validationResults.push(...fileValidation);
    }

    // Haskell-specific validation if applicable
    if (this.isHaskellInstruction(result.instruction)) {
      const haskellValidation = await this.validateHaskellQuality(expectedFiles ?? []);
      validationResults.push(...haskellValidation);
    }

    // Calculate overall score
    const overallScore = this.calculateOverallScore(validationResults);
    const success = overallScore >= 0.8 && result.response.success;

    return {
      testName: this.generateTestName(result),
      instruction: result.instruction,
      executionResult: result,
      validationResults,
      overallScore,
      success,
      generatedFiles: expectedFiles,
      notes: this.generateNotes(validationResults, result),
    };
  }

  /**
   * Validate basic execution success
   */
  private validateBasicExecution(result: ExecutionResult): ValidationResult {
    const passed = result.response.success;

    return {
      passed,
      criteria: 'Basic execution success',
      details: passed
        ? 'Execution completed successfully'
        : `Execution failed: ${result.response.error}`,
      score: passed ? 1.0 : 0.0,
    };
  }

  /**
   * Validate content quality
   */
  private async validateContentQuality(content: string): Promise<ValidationResult> {
    const checks = [
      {
        name: 'Content length',
        check: content.length > 100,
        weight: 0.2,
      },
      {
        name: 'Contains code',
        check: /```|function|class|module|data|type/.test(content),
        weight: 0.3,
      },
      {
        name: 'Contains documentation',
        check: /--|\/\*|\{-|@param|@return|\/\//.test(content),
        weight: 0.3,
      },
      {
        name: 'Contains error handling',
        check: /error|exception|result|maybe|either/i.test(content),
        weight: 0.2,
      },
    ];

    const passedChecks = checks.filter((check) => check.check);
    const score = passedChecks.reduce((sum, check) => sum + check.weight, 0);
    const passed = score >= 0.7;

    return {
      passed,
      criteria: 'Content quality',
      details: `Passed ${passedChecks.length}/${checks.length} quality checks`,
      score,
    };
  }

  /**
   * Validate generated files exist
   */
  private async validateGeneratedFiles(expectedFiles: string[]): Promise<ValidationResult[]> {
    const results: ValidationResult[] = [];

    for (const file of expectedFiles) {
      const filePath = join(this.baseDir, file);

      try {
        await access(filePath);
        results.push({
          passed: true,
          criteria: `File existence: ${file}`,
          details: 'File was generated successfully',
          score: 1.0,
        });
              } catch {
        results.push({
          passed: false,
          criteria: `File existence: ${file}`,
          details: `File not found: ${filePath}`,
          score: 0.0,
        });
      }
    }

    return results;
  }

  /**
   * Validate Haskell-specific quality standards
   */
  private async validateHaskellQuality(files: string[]): Promise<ValidationResult[]> {
    const results: ValidationResult[] = [];

    for (const file of files.filter((f) => f.endsWith('.hs'))) {
      try {
        const content = await readFile(join(this.baseDir, file), 'utf-8');

        // Check for modern Haskell patterns
        const modernPatterns = [
          { pattern: /\{-# LANGUAGE.*GHC2021/, name: 'GHC2021 usage', weight: 0.2 },
          { pattern: /deriving stock/, name: 'Modern deriving', weight: 0.15 },
          { pattern: /!/, name: 'Strict fields', weight: 0.15 },
          { pattern: /\{-\|/, name: 'Haddock documentation', weight: 0.2 },
          { pattern: /import.*qualified/, name: 'Qualified imports', weight: 0.1 },
          {
            pattern: /instance\s+(Functor|Applicative|Monad)/,
            name: 'Type class instances',
            weight: 0.2,
          },
        ];

        const foundPatterns = modernPatterns.filter((p) => p.pattern.test(content));
        const score = foundPatterns.reduce((sum, p) => sum + p.weight, 0);

        results.push({
          passed: score >= 0.6,
          criteria: `Haskell quality: ${file}`,
          details: `Found ${foundPatterns.length}/${modernPatterns.length} modern patterns`,
          score: Math.min(score, 1.0),
        });

        // Check for QiCore integration
        const qicorePatterns = [/Result\s*</, /QiError/, /success\s*::/, /failure\s*::/];

        const qicoreIntegration = qicorePatterns.some((p) => p.test(content));
        results.push({
          passed: qicoreIntegration,
          criteria: `QiCore integration: ${file}`,
          details: qicoreIntegration
            ? 'Proper QiCore type integration found'
            : 'Missing QiCore Result/Error integration',
          score: qicoreIntegration ? 1.0 : 0.0,
        });
      } catch (error) {
        results.push({
          passed: false,
          criteria: `Haskell validation: ${file}`,
          details: `Failed to read file: ${error}`,
          score: 0.0,
        });
      }
    }

    return results;
  }

  /**
   * Calculate overall score from validation results
   */
  private calculateOverallScore(results: ValidationResult[]): number {
    if (results.length === 0) return 0;

    const totalScore = results.reduce((sum, result) => sum + (result.score ?? 0), 0);
    return totalScore / results.length;
  }

  /**
   * Check if instruction is Haskell-related
   */
  private isHaskellInstruction(instruction: unknown): boolean {
    const content = JSON.stringify(instruction).toLowerCase();
    return content.includes('haskell') || content.includes('.hs');
  }

  /**
   * Generate test name from result
   */
  private generateTestName(result: ExecutionResult): string {
    const method = result.method;
    const timestamp = new Date(result.timestamp).toISOString().slice(0, 16);
    return `${method}_execution_${timestamp}`;
  }

  /**
   * Generate validation notes
   */
  private generateNotes(validationResults: ValidationResult[], result: ExecutionResult): string {
    const failedValidations = validationResults.filter((v) => !v.passed);

    if (failedValidations.length === 0) {
      return 'All validations passed successfully';
    }

    const notes = [
      `${failedValidations.length} validation(s) failed:`,
      ...failedValidations.map((v) => `- ${v.criteria}: ${v.details}`),
    ];

    if (!result.response.success) {
      notes.push(`Execution error: ${result.response.error}`);
    }

    return notes.join('\n');
  }

  /**
   * Generate detailed validation report
   */
  generateReport(testResult: TestResult): string {
    const { testName, overallScore, success, validationResults } = testResult;

    const report = [
      `# Validation Report: ${testName}`,
      '',
      `**Overall Score:** ${(overallScore * 100).toFixed(1)}%`,
      `**Success:** ${success ? '✅ PASS' : '❌ FAIL'}`,
      `**Execution Time:** ${testResult.executionResult.executionTime}ms`,
      '',
      '## Validation Results',
      '',
    ];

    for (const result of validationResults) {
      const icon = result.passed ? '✅' : '❌';
      const score = result.score ? `(${(result.score * 100).toFixed(1)}%)` : '';

      report.push(`${icon} **${result.criteria}** ${score}`);
      if (result.details) {
        report.push(`   ${result.details}`);
      }
      report.push('');
    }

    if (testResult.notes) {
      report.push('## Notes');
      report.push(testResult.notes);
    }

    return report.join('\n');
  }
}
