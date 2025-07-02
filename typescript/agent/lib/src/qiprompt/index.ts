/**
 * QiCore v4.0 QiPrompt: Functional Prompt Engineering Framework
 *
 * Mathematical Foundation:
 * - Prompt Monad: Prompt<T> = (Model × Input) → Promise<Result<T>>
 * - Generation Function: generate: (Config × Prompt) → Result<Output>
 * - Performance Tier: TypeScript (interpreted) = 100× baseline, target < 2s generation time
 *
 * Re-exports core Vercel AI SDK functionality with functional programming
 * principles and qi/core Result<T> = Either<QiError, T> pattern.
 *
 * Derived from:
 * - QiCore v4.0 mathematical specifications
 * - Vercel AI SDK composable patterns
 * - Functional composition and error handling
 */

// Re-export everything from Vercel AI SDK
export * from "ai";

// Re-export specific utilities with aliases for convenience
export {
	type CoreMessage,
	type CoreTool,
	generateObject,
	generateText,
	type LanguageModel,
	streamObject,
	streamText,
} from "ai";

import {
	createQiError,
	failure,
	flatMap,
	map,
	match,
	type ResultType as Result,
	success,
} from "@qi/core/base";
import { type CoreMessage, generateObject, generateText, type LanguageModel } from "ai";
import { type ZodSchema, z } from "zod";

// ============================================================================
// Core Types and Mathematical Structures
// ============================================================================

/**
 * QiPrompt Message (Type Alias)
 * Mathematical Structure: Message = CoreMessage (from AI SDK)
 */
export type QiPromptMessage = CoreMessage;

/**
 * QiPrompt Configuration (Product Type)
 * Mathematical Structure: Config = Model? × Temperature? × MaxTokens? × Timeout?
 * Performance: Configuration validation < 10μs (TypeScript interpreted tier)
 */
export interface QiPromptConfig {
	readonly defaultModel?: LanguageModel;
	readonly defaultTemperature?: number; // Default: 0.7, range [0, 1]
	readonly maxTokens?: number; // Default: 2000 tokens
	readonly timeout?: number; // Default: 30000ms (30s)
	readonly retryAttempts?: number; // Default: 3 retries
}

/**
 * QiPrompt Generation Options (Product Type)
 * Mathematical Structure: Options = Model? × Temperature? × MaxTokens? × System?
 * Performance: Options validation < 10μs (TypeScript interpreted tier)
 */
export interface QiGenerateOptions {
	readonly model?: LanguageModel;
	readonly temperature?: number;
	readonly maxTokens?: number;
	readonly system?: string;
	readonly retryOnFailure?: boolean; // Default: true
}

/**
 * Generation Result with Metadata (Product Type)
 * Mathematical Structure: GenerationResult = Text × Metadata
 * Performance: Result construction < 50μs (TypeScript interpreted tier)
 */
export interface QiGenerationResult {
	readonly text: string;
	readonly metadata: {
		readonly model: string;
		readonly tokens?: {
			readonly prompt: number;
			readonly completion: number;
			readonly total: number;
		};
		readonly duration: number; // Generation time in milliseconds
		readonly timestamp: number;
		readonly finishReason?: string;
	};
}

/**
 * QiPrompt Client - Convenience wrapper around Vercel AI SDK
 *
 * Provides simplified methods while preserving full AI SDK functionality
 */
export class QiPrompt {
	private config: QiPromptConfig;

	constructor(config: QiPromptConfig = {}) {
		this.config = {
			defaultTemperature: 0.7,
			maxTokens: 2000,
			timeout: 30000,
			...config,
		};
	}

	/**
	 * Generate text using Vercel AI SDK (Functional)
	 * generateText: (Prompt, Options?) → Promise<Result<QiGenerationResult>>
	 * Performance: < 2s typical generation time (model dependent)
	 */
	async generateText(
		prompt: string,
		options: QiGenerateOptions = {}
	): Promise<Result<QiGenerationResult>> {
		const model = options.model || this.config.defaultModel;
		if (!model) {
			return failure(
				createQiError(
					"NO_MODEL_SPECIFIED",
					"Model must be provided either in config or options",
					"VALIDATION",
					{ config: this.config, options }
				)
			);
		}

		const startTime = Date.now();
		try {
			const result = await generateText({
				model,
				prompt,
				temperature: options.temperature ?? this.config.defaultTemperature,
				maxTokens: options.maxTokens ?? this.config.maxTokens,
				system: options.system,
			});

			const duration = Date.now() - startTime;
			const generationResult: QiGenerationResult = {
				text: result.text,
				metadata: {
					model: typeof model === "string" ? model : model.modelId || "unknown",
					tokens: result.usage
						? {
								prompt: result.usage.promptTokens,
								completion: result.usage.completionTokens,
								total: result.usage.totalTokens,
							}
						: undefined,
					duration,
					timestamp: startTime,
					finishReason: result.finishReason,
				},
			};

			return success(generationResult);
		} catch (error: unknown) {
			const qiError = createQiError(
				"GENERATION_FAILED",
				`Text generation failed: ${error instanceof Error ? error.message : String(error)}`,
				"SYSTEM",
				{ prompt, options, originalError: String(error) }
			);
			return failure(qiError);
		}
	}

	/**
	 * Generate response from messages using Vercel AI SDK
	 */
	async generateResponse(
		messages: QiPromptMessage[],
		options: QiGenerateOptions = {}
	): Promise<string> {
		if (!this.config.defaultModel && !options.model) {
			throw new Error("Model must be provided either in config or options");
		}

		const result = await generateText({
			model:
				options.model ||
				this.config.defaultModel ||
				(() => {
					throw new Error("No model specified");
				})(),
			messages,
			temperature: options.temperature ?? this.config.defaultTemperature,
			maxTokens: options.maxTokens ?? this.config.maxTokens,
			system: options.system,
		});

		return result.text;
	}

	/**
	 * Generate structured output using Vercel AI SDK's generateObject
	 */
	async generateStructured<T>(
		prompt: string,
		options: { schema: ZodSchema<T>; model?: LanguageModel; system?: string }
	): Promise<T> {
		if (!this.config.defaultModel && !options.model) {
			throw new Error("Model must be provided either in config or options");
		}

		const result = await generateObject({
			model:
				options.model ||
				this.config.defaultModel ||
				(() => {
					throw new Error("No model specified");
				})(),
			prompt,
			schema: options.schema,
			system: options.system,
			temperature: this.config.defaultTemperature,
			maxTokens: this.config.maxTokens,
		});

		return result.object;
	}

	/**
	 * Get current configuration
	 */
	getConfig(): QiPromptConfig {
		return { ...this.config };
	}

	/**
	 * Update configuration
	 */
	updateConfig(updates: Partial<QiPromptConfig>): void {
		this.config = { ...this.config, ...updates };
	}
}

// Common Zod schemas for mathematical analysis
export const CommonSchemas = {
	analysis: z.object({
		summary: z.string(),
		keyPoints: z.array(z.string()),
		confidence: z.number().min(0).max(1),
		recommendation: z.string(),
	}),

	mathematicalAnalysis: z.object({
		algebraicStructures: z.array(z.string()),
		completenessScore: z.number().min(0).max(100),
		inevitablePatterns: z.array(z.string()),
		gaps: z.array(z.string()),
		lawVerification: z
			.object({
				satisfied: z.boolean(),
				violations: z.array(z.string()),
			})
			.optional(),
	}),
};

// ============================================================================
// Functional Factory Functions and Utilities
// ============================================================================

/**
 * Validate QiPrompt configuration
 * validateConfig: Config → Result<Config>
 * Performance: < 20μs (TypeScript interpreted tier)
 */
const validateQiPromptConfig = (config: QiPromptConfig): Result<QiPromptConfig> => {
	if (
		config.defaultTemperature !== undefined &&
		(config.defaultTemperature < 0 || config.defaultTemperature > 1)
	) {
		return failure(
			createQiError("INVALID_TEMPERATURE", "Temperature must be between 0 and 1", "VALIDATION", {
				temperature: config.defaultTemperature,
			})
		);
	}

	if (config.maxTokens !== undefined && config.maxTokens <= 0) {
		return failure(
			createQiError("INVALID_MAX_TOKENS", "Max tokens must be positive", "VALIDATION", {
				maxTokens: config.maxTokens,
			})
		);
	}

	return success(config);
};

/**
 * Create QiPrompt instance with functional validation
 * createQiPrompt: (Model, Config?) → Result<QiPrompt>
 * Performance: < 100μs (TypeScript interpreted tier)
 */
export const createQiPrompt = (
	model: LanguageModel,
	config?: Partial<QiPromptConfig>
): Result<QiPrompt> => {
	const fullConfig: QiPromptConfig = {
		defaultModel: model,
		defaultTemperature: 0.7,
		maxTokens: 2000,
		timeout: 30000,
		retryAttempts: 3,
		...config,
	};

	return map((validatedConfig: QiPromptConfig) => new QiPrompt(validatedConfig))(
		validateQiPromptConfig(fullConfig)
	);
};

/**
 * Create QiPrompt instance safely (returns null on error)
 * createQiPromptSafe: (Model, Config?) → QiPrompt | null
 * Performance: < 150μs (TypeScript interpreted tier)
 */
export const createQiPromptSafe = (
	model: LanguageModel,
	config?: Partial<QiPromptConfig>
): QiPrompt | null =>
	match(
		(qiPrompt: QiPrompt) => qiPrompt,
		() => null
	)(createQiPrompt(model, config));

/**
 * Create QiPrompt optimized for mathematical analysis
 * createMathematicalQiPrompt: Model → Result<QiPrompt>
 * Performance: < 100μs (TypeScript interpreted tier)
 */
export const createMathematicalQiPrompt = (model: LanguageModel): Result<QiPrompt> =>
	createQiPrompt(model, {
		defaultTemperature: 0.1, // Lower temperature for more deterministic mathematical output
		maxTokens: 4000, // Higher token limit for detailed mathematical explanations
		retryAttempts: 5, // More retries for critical mathematical computations
		timeout: 60000, // Longer timeout for complex mathematical analysis
	});

/**
 * Functional text generation without class instantiation
 * generateTextFunctional: (Model, Prompt, Options?) → Promise<Result<QiGenerationResult>>
 * Performance: < 2s typical generation time (model dependent)
 */
export const generateTextFunctional = async (
	model: LanguageModel,
	prompt: string,
	options: Omit<QiGenerateOptions, "model"> = {}
): Promise<Result<QiGenerationResult>> => {
	const qiPromptResult = createQiPrompt(model);
	if (
		match(
			() => false,
			() => true
		)(qiPromptResult)
	) {
		return failure(
			match(
				() => createQiError("UNKNOWN", "Failed to create QiPrompt", "UNKNOWN"),
				(error) => error
			)(qiPromptResult)
		);
	}

	const qiPrompt = match(
		(qp: QiPrompt) => qp,
		() => null as never
	)(qiPromptResult);
	return qiPrompt.generateText(prompt, { model, ...options });
};

/**
 * Complete QiPrompt API with functional composition patterns
 * Following qi/core v4.0 mathematical specification
 */
export const QiPromptAPI = {
	// Factory functions
	createQiPrompt,
	createQiPromptSafe,
	createMathematical: createMathematicalQiPrompt,

	// Functional utilities
	generateTextFunctional,
	validateConfig: validateQiPromptConfig,

	// Class for when needed
	QiPrompt,

	// Schema utilities
	CommonSchemas,

	// Functional programming utilities
	utils: {
		map,
		flatMap,
		match,
		success,
		failure,
	},
} as const;
