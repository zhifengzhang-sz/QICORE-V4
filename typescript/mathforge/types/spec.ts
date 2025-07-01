/**
 * MathForge - Universal Mathematical Code Generator
 * Type definitions for YAML specification format
 */

/**
 * Supported target languages for code generation
 */
export type TargetLanguage = "typescript" | "python" | "haskell";

/**
 * Error categories for generated error types
 */
export type ErrorCategory =
	| "VALIDATION"
	| "CONFIGURATION"
	| "NETWORK"
	| "CACHE"
	| "SYSTEM"
	| "UNKNOWN";

/**
 * Mathematical law types
 */
export type MathematicalLaw =
	| "monad.left_identity"
	| "monad.right_identity"
	| "monad.associativity"
	| "functor.identity"
	| "functor.composition"
	| "monoid.identity"
	| "monoid.associativity";

/**
 * Complete formal specification structure
 */
export interface FormalSpec {
	readonly metadata: SpecMetadata;
	readonly operations: Record<string, Operation>;
	readonly type_definitions: Record<string, TypeDefinition>;
	readonly generation_targets: Record<TargetLanguage, GenerationTarget>;
}

/**
 * Specification metadata
 */
export interface SpecMetadata {
	readonly component: string;
	readonly mathematical_foundation: string;
	readonly package_strategy: string;
	readonly laws_verified: MathematicalLaw[];
	readonly description?: string;
	readonly version?: string;
	readonly author?: string;
}

/**
 * Operation specification with mathematical properties
 */
export interface Operation {
	readonly signature: string;
	readonly semantics: string;
	readonly description: string;
	readonly mathematical_laws: MathematicalLaw[];
	readonly properties: Record<string, Property>;
	readonly examples?: Example[];
	readonly performance_target?: string;
	readonly error_conditions?: string[];
}

/**
 * Mathematical property with formal specification
 */
export interface Property {
	readonly formula: string;
	readonly description: string;
	readonly test_strategy?: "property" | "unit" | "integration";
	readonly complexity?: "O(1)" | "O(log n)" | "O(n)" | "O(n log n)" | "O(n²)";
}

/**
 * Usage example with language-specific mappings
 */
export interface Example {
	readonly input: string;
	readonly output: string;
	readonly language_mapping?: Partial<Record<TargetLanguage, string>>;
	readonly description?: string;
}

/**
 * Type definition for generated types
 */
export interface TypeDefinition {
	readonly description: string;
	readonly type_parameters?: string[];
	readonly constructors?: string[];
	readonly fields?: string[];
	readonly constraints?: string[];
	readonly derives?: string[];
}

/**
 * Language-specific generation configuration
 */
export interface GenerationTarget {
	readonly formal_verification_tools: string[];
	readonly package_dependencies: string[];
	readonly test_framework?: string;
	readonly performance_tools?: string[];
	readonly custom_types?: Record<string, string>;
}

/**
 * Code generation options
 */
export interface GenerationOptions {
	readonly target_language: TargetLanguage;
	readonly include_tests: boolean;
	readonly include_benchmarks: boolean;
	readonly include_documentation: boolean;
	readonly strict_mode: boolean;
	readonly output_path?: string;
	readonly template_overrides?: Record<string, string>;
}

/**
 * Generated code result
 */
export interface GeneratedCode {
	readonly language: TargetLanguage;
	readonly main_code: string;
	readonly test_code?: string;
	readonly benchmark_code?: string;
	readonly documentation?: string;
	readonly dependencies: string[];
	readonly metadata: {
		readonly lines_of_code: number;
		readonly test_count: number;
		readonly property_count: number;
		readonly law_count: number;
	};
}

/**
 * Generation statistics for reporting
 */
export interface GenerationStats {
	readonly spec_file: string;
	readonly target_language: TargetLanguage;
	readonly generation_time_ms: number;
	readonly total_lines: number;
	readonly operations_generated: number;
	readonly properties_generated: number;
	readonly tests_generated: number;
	readonly laws_verified: MathematicalLaw[];
}

/**
 * Validation result for specifications
 */
export interface ValidationResult {
	readonly valid: boolean;
	readonly errors: ValidationError[];
	readonly warnings: ValidationWarning[];
}

/**
 * Validation error
 */
export interface ValidationError {
	readonly type: "missing_field" | "invalid_type" | "invalid_value" | "constraint_violation";
	readonly path: string;
	readonly message: string;
	readonly severity: "error" | "warning";
}

/**
 * Validation warning
 */
export interface ValidationWarning {
	readonly type: "deprecated" | "performance" | "best_practice";
	readonly path: string;
	readonly message: string;
	readonly suggestion?: string;
}

/**
 * Template context for code generation
 */
export interface TemplateContext {
	readonly spec: FormalSpec;
	readonly options: GenerationOptions;
	readonly helpers: {
		readonly capitalize: (str: string) => string;
		readonly camelCase: (str: string) => string;
		readonly snakeCase: (str: string) => string;
		readonly pascalCase: (str: string) => string;
		readonly formatType: (type: string, language: TargetLanguage) => string;
		readonly formatComment: (comment: string, language: TargetLanguage) => string;
	};
}
