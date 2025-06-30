import type { GeneratedCode } from '@/types/study';

// Regex patterns for contract compliance scoring (moved to top level for performance)
const RESULT_TYPE_PATTERN = /data\s+Result\s+\w+/i;
const QIERROR_TYPE_PATTERN = /data\s+QiError/i;
const SUCCESS_FUNCTION_PATTERN = /success\s*::|success\s*=/i;
const FAILURE_FUNCTION_PATTERN = /failure\s*::|failure\s*=/i;
const MAP_OPERATION_PATTERN = /map\s*::|map\s*=|fmap/i;
const FLATMAP_OPERATION_PATTERN = /flatMap|bind|>>=|=<</i;
const UNWRAP_OPERATION_PATTERN = /unwrap\s*::|unwrap\s*=/i;
const UNWRAPOR_OPERATION_PATTERN = /unwrapOr|fromMaybe/i;

// Regex patterns for modernity scoring (moved to top level for performance)
const GHC2021_PRAGMA_PATTERN = /{-#\s*LANGUAGE\s+GHC2021\s*#-}/i;
const STRICT_FIELDS_PATTERN = /!\w+/g;
const DERIVING_STOCK_PATTERN = /deriving\s+stock/i;
const LAMBDA_CASE_PATTERN = /\\case/i;
const MODERN_IMPORTS_PATTERN = /import\s+qualified.*as/i;

// Regex patterns for completeness scoring (moved to top level for performance)
const SUCCESS_TYPE_PATTERN = /success\s*::/i;
const FAILURE_TYPE_PATTERN = /failure\s*::/i;
const MAP_TYPE_PATTERN = /map\s*::|fmap\s*::/i;
const FLATMAP_TYPE_PATTERN = /flatMap|bind|>>=\s*::/i;
const UNWRAP_TYPE_PATTERN = /unwrap\s*::/i;
const UNWRAPOR_TYPE_PATTERN = /unwrapOr\s*::/i;

// Regex patterns for quality scoring (moved to top level for performance)
const MODULE_DOC_PATTERN = /-- \|.*|{-\|[\s\S]*?-}/m;
const HADDOCK_COMMENT_PATTERN = /-- \|/g;
const EXAMPLES_PATTERN = />>> |Example:|@/i;
const INPUT_VALIDATION_PATTERN = /guard|when|unless|validate/i;
const EXCEPTION_HANDLING_PATTERN = /catch|handle|try|exception/i;
const PARTIAL_FUNCTIONS_PATTERN = /undefined|error\s*"/i;
const STRICT_FIELD_CHECK_PATTERN = /!\w+/;
const EFFICIENT_STRUCTURES_PATTERN = /Vector|Array|ByteString/i;
const CONSISTENT_NAMING_PATTERN = /^[a-z][a-zA-Z0-9]*$/m;
const TAB_CHECK_PATTERN = /\t/;

export interface ImplementationScore {
  contractCompliance: number; // 0-100 (contracts satisfaction)
  modernityScore: number; // 0-100 (modern Haskell features)
  completenessScore: number; // 0-100 (API coverage)
  qualityScore: number; // 0-100 (documentation, safety)
  overallScore: number; // weighted average
}

export class ImplementationScorer {
  async scoreImplementation(code: GeneratedCode): Promise<ImplementationScore> {
    const codeContent = code.code;

    // Score each category
    const contractCompliance = this.scoreContractCompliance(codeContent);
    const modernityScore = this.scoreModernity(codeContent);
    const completenessScore = this.scoreCompleteness(codeContent);
    const qualityScore = this.scoreQuality(codeContent);

    // Weighted average (contracts are most important)
    const overallScore = Math.round(
      contractCompliance * 0.4 + completenessScore * 0.3 + modernityScore * 0.2 + qualityScore * 0.1
    );

    return {
      contractCompliance,
      modernityScore,
      completenessScore,
      qualityScore,
      overallScore,
    };
  }

  private scoreContractCompliance(code: string): number {
    const requiredContracts = [
      { name: 'Result_type_definition', pattern: RESULT_TYPE_PATTERN, points: 20 },
      { name: 'QiError_type_definition', pattern: QIERROR_TYPE_PATTERN, points: 20 },
      { name: 'success_function', pattern: SUCCESS_FUNCTION_PATTERN, points: 15 },
      { name: 'failure_function', pattern: FAILURE_FUNCTION_PATTERN, points: 15 },
      { name: 'map_operation', pattern: MAP_OPERATION_PATTERN, points: 10 },
      { name: 'flatMap_operation', pattern: FLATMAP_OPERATION_PATTERN, points: 10 },
      { name: 'unwrap_operation', pattern: UNWRAP_OPERATION_PATTERN, points: 5 },
      { name: 'unwrapOr_operation', pattern: UNWRAPOR_OPERATION_PATTERN, points: 5 },
    ];

    let totalScore = 0;
    let maxScore = 0;

    for (const contract of requiredContracts) {
      maxScore += contract.points;
      if (contract.pattern.test(code)) {
        totalScore += contract.points;
      }
    }

    return Math.round((totalScore / maxScore) * 100);
  }

  private scoreModernity(code: string): number {
    const modernFeatures = [
      { name: 'GHC2021_language_pragma', pattern: GHC2021_PRAGMA_PATTERN, points: 15 },
      { name: 'strict_fields', pattern: STRICT_FIELDS_PATTERN, points: 10 },
      { name: 'deriving_stock', pattern: DERIVING_STOCK_PATTERN, points: 8 },
      { name: 'lambda_case', pattern: LAMBDA_CASE_PATTERN, points: 7 },
      { name: 'modern_imports', pattern: MODERN_IMPORTS_PATTERN, points: 5 },
    ];

    let totalScore = 0;
    let maxScore = 0;

    for (const feature of modernFeatures) {
      maxScore += feature.points;
      const matches = code.match(feature.pattern);
      if (matches) {
        totalScore += feature.points;
      }
    }

    return Math.round((totalScore / maxScore) * 100);
  }

  private scoreCompleteness(code: string): number {
    const requiredFunctions = [
      { name: 'success', pattern: SUCCESS_TYPE_PATTERN, points: 12 },
      { name: 'failure', pattern: FAILURE_TYPE_PATTERN, points: 12 },
      { name: 'map', pattern: MAP_TYPE_PATTERN, points: 10 },
      { name: 'flatMap', pattern: FLATMAP_TYPE_PATTERN, points: 10 },
      { name: 'unwrap', pattern: UNWRAP_TYPE_PATTERN, points: 8 },
      { name: 'unwrapOr', pattern: UNWRAPOR_TYPE_PATTERN, points: 8 },
    ];

    let totalScore = 0;
    let maxScore = 0;

    for (const func of requiredFunctions) {
      maxScore += func.points;
      if (func.pattern.test(code)) {
        totalScore += func.points;
      }
    }

    return Math.round((totalScore / maxScore) * 100);
  }

  private scoreQuality(code: string): number {
    // Handle empty code - no quality score for no code
    if (code.trim().length === 0) {
      return 0;
    }

    let totalScore = 0;
    const maxScore = 100;

    // Documentation quality (30 points)
    const hasModuleDoc = MODULE_DOC_PATTERN.test(code);
    const hasHaddockComments = (code.match(HADDOCK_COMMENT_PATTERN) ?? []).length;
    const hasExamples = EXAMPLES_PATTERN.test(code);

    totalScore += hasModuleDoc ? 10 : 0;
    totalScore += Math.min(hasHaddockComments * 2, 15);
    totalScore += hasExamples ? 5 : 0;

    // Safety patterns (40 points)
    const hasInputValidation = INPUT_VALIDATION_PATTERN.test(code);
    const hasExceptionHandling = EXCEPTION_HANDLING_PATTERN.test(code);
    const hasTotalFunctions = !PARTIAL_FUNCTIONS_PATTERN.test(code);

    totalScore += hasInputValidation ? 10 : 0;
    totalScore += hasExceptionHandling ? 10 : 0;
    totalScore += hasTotalFunctions ? 15 : 0;

    // Performance considerations (20 points)
    const hasStrictFields = STRICT_FIELD_CHECK_PATTERN.test(code);
    const hasEfficientStructures = EFFICIENT_STRUCTURES_PATTERN.test(code);

    totalScore += hasStrictFields ? 10 : 0;
    totalScore += hasEfficientStructures ? 5 : 0;

    // Code style (10 points)
    const hasConsistentNaming = CONSISTENT_NAMING_PATTERN.test(code);
    const hasProperIndentation = !TAB_CHECK_PATTERN.test(code); // No tabs

    totalScore += hasConsistentNaming ? 5 : 0;
    totalScore += hasProperIndentation ? 5 : 0;

    return Math.round((totalScore / maxScore) * 100);
  }
}
