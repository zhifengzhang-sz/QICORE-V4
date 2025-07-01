/**
 * MathForge - YAML Specification Parser
 * Parses and validates formal specification YAML files
 */

import { readFileSync } from "node:fs";
import YAML from "yaml";
import type {
	FormalSpec,
	MathematicalLaw,
	TargetLanguage,
	ValidationError,
	ValidationResult,
	ValidationWarning,
} from "../types/spec.js";

// Security: Configure YAML parser with safe options
const YAML_OPTIONS = {
	// Prevent prototype pollution
	merge: false,
	// Disable custom tags for security
	customTags: [],
	// Limit recursion depth
	maxAliasCount: 100,
	// Disable YAML 1.1 features that can be unsafe
	version: "1.2" as const,
};

/**
 * Parse YAML specification file with validation
 */
export function parseSpecFile(filePath: string): {
	spec: FormalSpec | null;
	validation: ValidationResult;
} {
	try {
		const yamlContent = readFileSync(filePath, "utf-8");

		// Security: Validate file size (prevent DoS)
		if (yamlContent.length > 1024 * 1024) {
			// 1MB limit
			return {
				spec: null,
				validation: {
					valid: false,
					errors: [
						{
							type: "invalid_value",
							path: "file",
							message: "File too large (max 1MB allowed)",
							severity: "error",
						},
					],
					warnings: [],
				},
			};
		}

		return parseSpecContent(yamlContent, filePath);
	} catch (error) {
		return {
			spec: null,
			validation: {
				valid: false,
				errors: [
					{
						type: "invalid_value",
						path: "file",
						message: `Failed to read file: ${error}`,
						severity: "error",
					},
				],
				warnings: [],
			},
		};
	}
}

/**
 * Parse YAML specification content with validation
 */
export function parseSpecContent(
	yamlContent: string,
	_filePath = "unknown"
): { spec: FormalSpec | null; validation: ValidationResult } {
	try {
		// Security: Use safe YAML parsing options
		const parsed = YAML.parse(yamlContent, YAML_OPTIONS);
		const validation = validateSpec(parsed);

		if (validation.valid) {
			return { spec: parsed as FormalSpec, validation };
		} else {
			return { spec: null, validation };
		}
	} catch (error) {
		return {
			spec: null,
			validation: {
				valid: false,
				errors: [
					{
						type: "invalid_value",
						path: "yaml",
						message: `YAML parsing failed: ${error}`,
						severity: "error",
					},
				],
				warnings: [],
			},
		};
	}
}

/**
 * Comprehensive specification validation
 */
export function validateSpec(spec: unknown): ValidationResult {
	const errors: ValidationError[] = [];
	const warnings: ValidationWarning[] = [];

	if (!spec || typeof spec !== "object") {
		errors.push({
			type: "invalid_type",
			path: "root",
			message: "Specification must be an object",
			severity: "error",
		});
		return { valid: false, errors, warnings };
	}

	const s = spec as Record<string, unknown>;

	// Validate metadata
	validateMetadata(s.metadata, errors, warnings);

	// Validate operations
	validateOperations(s.operations, errors, warnings);

	// Validate type definitions
	validateTypeDefinitions(s.type_definitions, errors, warnings);

	// Validate generation targets
	validateGenerationTargets(s.generation_targets, errors, warnings);

	return {
		valid: errors.length === 0,
		errors,
		warnings,
	};
}

/**
 * Validate metadata section
 */
function validateMetadata(
	metadata: unknown,
	errors: ValidationError[],
	warnings: ValidationWarning[]
): void {
	if (!metadata || typeof metadata !== "object") {
		errors.push({
			type: "missing_field",
			path: "metadata",
			message: "Missing required metadata section",
			severity: "error",
		});
		return;
	}

	const m = metadata as Record<string, unknown>;

	// Required fields
	if (!m.component || typeof m.component !== "string") {
		errors.push({
			type: "missing_field",
			path: "metadata.component",
			message: "Component name is required",
			severity: "error",
		});
	}

	if (!m.mathematical_foundation || typeof m.mathematical_foundation !== "string") {
		errors.push({
			type: "missing_field",
			path: "metadata.mathematical_foundation",
			message: "Mathematical foundation is required",
			severity: "error",
		});
	}

	if (!m.package_strategy || typeof m.package_strategy !== "string") {
		errors.push({
			type: "missing_field",
			path: "metadata.package_strategy",
			message: "Package strategy is required",
			severity: "error",
		});
	}

	// Validate laws_verified
	if (Array.isArray(m.laws_verified)) {
		const validLaws: MathematicalLaw[] = [
			"monad.left_identity",
			"monad.right_identity",
			"monad.associativity",
			"functor.identity",
			"functor.composition",
			"monoid.identity",
			"monoid.associativity",
		];

		m.laws_verified.forEach((law, index) => {
			if (typeof law !== "string" || !validLaws.includes(law as MathematicalLaw)) {
				errors.push({
					type: "invalid_value",
					path: `metadata.laws_verified[${index}]`,
					message: `Invalid mathematical law: ${law}. Must be one of: ${validLaws.join(", ")}`,
					severity: "error",
				});
			}
		});
	} else {
		errors.push({
			type: "invalid_type",
			path: "metadata.laws_verified",
			message: "laws_verified must be an array",
			severity: "error",
		});
	}

	// Optional field warnings
	if (!m.description) {
		warnings.push({
			type: "best_practice",
			path: "metadata.description",
			message: "Consider adding a description for better documentation",
			suggestion: "Add a description field explaining the component's purpose",
		});
	}
}

/**
 * Validate operations section
 */
function validateOperations(
	operations: unknown,
	errors: ValidationError[],
	warnings: ValidationWarning[]
): void {
	if (!operations || typeof operations !== "object") {
		errors.push({
			type: "missing_field",
			path: "operations",
			message: "Missing required operations section",
			severity: "error",
		});
		return;
	}

	const ops = operations as Record<string, unknown>;

	if (Object.keys(ops).length === 0) {
		warnings.push({
			type: "best_practice",
			path: "operations",
			message: "No operations defined",
			suggestion: "Consider adding at least one operation",
		});
	}

	Object.entries(ops).forEach(([opName, operation]) => {
		if (!operation || typeof operation !== "object") {
			errors.push({
				type: "invalid_type",
				path: `operations.${opName}`,
				message: "Operation must be an object",
				severity: "error",
			});
			return;
		}

		const op = operation as Record<string, unknown>;

		// Required fields
		["signature", "semantics", "description"].forEach((field) => {
			if (!op[field] || typeof op[field] !== "string") {
				errors.push({
					type: "missing_field",
					path: `operations.${opName}.${field}`,
					message: `${field} is required`,
					severity: "error",
				});
			}
		});

		// Validate mathematical_laws
		if (!Array.isArray(op.mathematical_laws)) {
			errors.push({
				type: "invalid_type",
				path: `operations.${opName}.mathematical_laws`,
				message: "mathematical_laws must be an array",
				severity: "error",
			});
		}

		// Validate properties
		if (!op.properties || typeof op.properties !== "object") {
			warnings.push({
				type: "best_practice",
				path: `operations.${opName}.properties`,
				message: "Consider adding properties for formal verification",
				suggestion: "Add mathematical properties to verify correctness",
			});
		} else {
			const props = op.properties as Record<string, unknown>;
			Object.entries(props).forEach(([propName, property]) => {
				if (!property || typeof property !== "object") {
					errors.push({
						type: "invalid_type",
						path: `operations.${opName}.properties.${propName}`,
						message: "Property must be an object",
						severity: "error",
					});
					return;
				}

				const p = property as Record<string, unknown>;
				if (!p.formula || typeof p.formula !== "string") {
					errors.push({
						type: "missing_field",
						path: `operations.${opName}.properties.${propName}.formula`,
						message: "Property formula is required",
						severity: "error",
					});
				}

				if (!p.description || typeof p.description !== "string") {
					errors.push({
						type: "missing_field",
						path: `operations.${opName}.properties.${propName}.description`,
						message: "Property description is required",
						severity: "error",
					});
				}
			});
		}
	});
}

/**
 * Validate type definitions section
 */
function validateTypeDefinitions(
	typeDefinitions: unknown,
	errors: ValidationError[],
	warnings: ValidationWarning[]
): void {
	if (!typeDefinitions || typeof typeDefinitions !== "object") {
		warnings.push({
			type: "best_practice",
			path: "type_definitions",
			message: "Consider adding type definitions for better code generation",
			suggestion: "Add type_definitions section with component types",
		});
		return;
	}

	const types = typeDefinitions as Record<string, unknown>;

	Object.entries(types).forEach(([typeName, typeDef]) => {
		if (!typeDef || typeof typeDef !== "object") {
			errors.push({
				type: "invalid_type",
				path: `type_definitions.${typeName}`,
				message: "Type definition must be an object",
				severity: "error",
			});
			return;
		}

		const t = typeDef as Record<string, unknown>;
		if (!t.description || typeof t.description !== "string") {
			errors.push({
				type: "missing_field",
				path: `type_definitions.${typeName}.description`,
				message: "Type description is required",
				severity: "error",
			});
		}
	});
}

/**
 * Validate generation targets section
 */
function validateGenerationTargets(
	generationTargets: unknown,
	errors: ValidationError[],
	warnings: ValidationWarning[]
): void {
	if (!generationTargets || typeof generationTargets !== "object") {
		errors.push({
			type: "missing_field",
			path: "generation_targets",
			message: "Missing required generation_targets section",
			severity: "error",
		});
		return;
	}

	const targets = generationTargets as Record<string, unknown>;
	const validLanguages: TargetLanguage[] = ["typescript", "python", "haskell"];

	validLanguages.forEach((lang) => {
		if (!targets[lang]) {
			warnings.push({
				type: "best_practice",
				path: `generation_targets.${lang}`,
				message: `Consider adding ${lang} generation target`,
				suggestion: `Add ${lang} configuration for cross-language support`,
			});
			return;
		}

		const target = targets[lang] as Record<string, unknown>;
		if (!Array.isArray(target.formal_verification_tools)) {
			errors.push({
				type: "invalid_type",
				path: `generation_targets.${lang}.formal_verification_tools`,
				message: "formal_verification_tools must be an array",
				severity: "error",
			});
		}

		if (!Array.isArray(target.package_dependencies)) {
			errors.push({
				type: "invalid_type",
				path: `generation_targets.${lang}.package_dependencies`,
				message: "package_dependencies must be an array",
				severity: "error",
			});
		}
	});
}

/**
 * Check if specification has required fields for a specific language
 */
export function validateForLanguage(spec: FormalSpec, language: TargetLanguage): ValidationResult {
	const errors: ValidationError[] = [];
	const warnings: ValidationWarning[] = [];

	// Check if generation target exists for language
	if (!spec.generation_targets[language]) {
		errors.push({
			type: "missing_field",
			path: `generation_targets.${language}`,
			message: `No generation target defined for ${language}`,
			severity: "error",
		});
	}

	// Language-specific validations
	switch (language) {
		case "typescript":
			validateTypeScriptRequirements(spec, errors, warnings);
			break;
		case "python":
			validatePythonRequirements(spec, errors, warnings);
			break;
		case "haskell":
			validateHaskellRequirements(spec, errors, warnings);
			break;
	}

	return {
		valid: errors.length === 0,
		errors,
		warnings,
	};
}

function validateTypeScriptRequirements(
	spec: FormalSpec,
	_errors: ValidationError[],
	warnings: ValidationWarning[]
): void {
	const target = spec.generation_targets.typescript;
	if (!target) return;

	// Check for TypeScript-specific tools
	const requiredTools = ["zod", "fast-check"];
	requiredTools.forEach((tool) => {
		if (!target.formal_verification_tools.includes(tool)) {
			warnings.push({
				type: "performance",
				path: "generation_targets.typescript.formal_verification_tools",
				message: `Consider adding ${tool} for better TypeScript verification`,
				suggestion: `Add "${tool}" to formal_verification_tools array`,
			});
		}
	});
}

function validatePythonRequirements(
	spec: FormalSpec,
	_errors: ValidationError[],
	warnings: ValidationWarning[]
): void {
	const target = spec.generation_targets.python;
	if (!target) return;

	// Check for Python-specific tools
	const requiredTools = ["pydantic", "hypothesis"];
	requiredTools.forEach((tool) => {
		if (!target.formal_verification_tools.includes(tool)) {
			warnings.push({
				type: "performance",
				path: "generation_targets.python.formal_verification_tools",
				message: `Consider adding ${tool} for better Python verification`,
				suggestion: `Add "${tool}" to formal_verification_tools array`,
			});
		}
	});
}

function validateHaskellRequirements(
	spec: FormalSpec,
	_errors: ValidationError[],
	warnings: ValidationWarning[]
): void {
	const target = spec.generation_targets.haskell;
	if (!target) return;

	// Check for Haskell-specific tools
	const requiredTools = ["quickcheck"];
	requiredTools.forEach((tool) => {
		if (!target.formal_verification_tools.includes(tool)) {
			warnings.push({
				type: "performance",
				path: "generation_targets.haskell.formal_verification_tools",
				message: `Consider adding ${tool} for better Haskell verification`,
				suggestion: `Add "${tool}" to formal_verification_tools array`,
			});
		}
	});
}
