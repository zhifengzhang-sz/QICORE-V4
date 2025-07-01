/**
 * MathForge - Universal Mathematical Code Generator
 * Main API exports for library usage
 */

import type {
	GeneratedCode,
	GenerationOptions,
	TargetLanguage,
	ValidationResult,
} from "./types/spec.js";

// Agent interface
export {
	MathForgeAgent,
	quickAgent,
	runAgentCLI,
} from "./agent/index.js";
// CLI interface
export { main as runCli } from "./cli/index.js";
export {
	generateCode,
	generateStats,
	MathForgeGenerator,
	templateHelpers,
} from "./core/generator.js";
// Core functionality
export {
	parseSpecContent,
	parseSpecFile,
	validateForLanguage,
	validateSpec,
} from "./core/parser.js";

// Type definitions
export type {
	ErrorCategory,
	Example,
	FormalSpec,
	GeneratedCode,
	GenerationOptions,
	GenerationStats,
	GenerationTarget,
	MathematicalLaw,
	Operation,
	Property,
	SpecMetadata,
	TargetLanguage,
	TemplateContext,
	TypeDefinition,
	ValidationError,
	ValidationResult,
	ValidationWarning,
} from "./types/spec.js";

/**
 * MathForge library version
 */
export const VERSION = "1.0.0";

/**
 * Supported languages
 */
export const SUPPORTED_LANGUAGES = ["typescript", "python", "haskell"] as const;

/**
 * Supported mathematical laws
 */
export const SUPPORTED_LAWS = [
	"monad.left_identity",
	"monad.right_identity",
	"monad.associativity",
	"functor.identity",
	"functor.composition",
	"monoid.identity",
	"monoid.associativity",
] as const;

/**
 * Quick generation helper for common use cases
 */
export async function quickGenerate(
	specFilePath: string,
	language: TargetLanguage = "typescript",
	options: Partial<GenerationOptions> = {}
): Promise<GeneratedCode> {
	const { parseSpecFile } = await import("./core/parser.js");
	const { generateCode } = await import("./core/generator.js");

	const { spec, validation } = parseSpecFile(specFilePath);

	if (!validation.valid) {
		throw new Error(
			`Specification validation failed: ${validation.errors.map((e) => e.message).join(", ")}`
		);
	}

	if (!spec) {
		throw new Error("Failed to parse specification");
	}

	const generationOptions: GenerationOptions = {
		target_language: language,
		include_tests: true,
		include_benchmarks: false,
		include_documentation: true,
		strict_mode: true,
		...options,
	};

	return generateCode(spec, generationOptions);
}

/**
 * Validate specification file helper
 */
export function validateSpecFile(specFilePath: string): ValidationResult {
	const { parseSpecFile } = require("./core/parser.js");
	const { validation } = parseSpecFile(specFilePath);
	return validation;
}

/**
 * Get default generation options for a language
 */
export function getDefaultOptions(language: TargetLanguage): GenerationOptions {
	return {
		target_language: language,
		include_tests: true,
		include_benchmarks: false,
		include_documentation: true,
		strict_mode: true,
	};
}

/**
 * Library information
 */
export const MATHFORGE_INFO = {
	name: "MathForge",
	version: VERSION,
	description: "Universal Mathematical Code Generator",
	supported_languages: SUPPORTED_LANGUAGES,
	supported_laws: SUPPORTED_LAWS,
	repository: "https://github.com/qi/mathforge",
	documentation: "https://mathforge.dev/docs",
} as const;
