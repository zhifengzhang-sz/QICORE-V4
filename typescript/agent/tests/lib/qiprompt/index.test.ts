import { beforeEach, describe, expect, it, vi } from "vitest";
import { getError, isFailure, isSuccess } from "../../../lib/src/qicore/base/index.js";
import {
	CommonSchemas,
	createMathematicalQiPrompt,
	createQiPrompt,
	createQiPromptSafe,
	generateTextFunctional,
	type QiGenerateOptions,
	QiPrompt,
	QiPromptAPI,
	type QiPromptConfig,
} from "../../../lib/src/qiprompt/index.js";

// Mock AI SDK
vi.mock("ai", () => ({
	generateText: vi.fn().mockResolvedValue({
		text: "Generated response text",
		usage: {
			promptTokens: 15,
			completionTokens: 25,
			totalTokens: 40,
		},
		finishReason: "stop",
	}),
	generateObject: vi.fn().mockResolvedValue({
		object: { summary: "Test summary", keyPoints: ["Point 1", "Point 2"] },
	}),
}));

// Mock language model
const mockModel = {
	modelId: "test-model",
	provider: "test-provider",
	specificationVersion: "v1" as const,
	defaultObjectGenerationMode: "tool" as const,
};

describe("QiPrompt", () => {
	describe("QiPrompt Class", () => {
		let qiPrompt: QiPrompt;

		beforeEach(() => {
			qiPrompt = new QiPrompt({
				defaultModel: mockModel,
				defaultTemperature: 0.7,
				maxTokens: 2000,
			});
		});

		it("should create instance with default config", () => {
			const defaultPrompt = new QiPrompt();
			const config = defaultPrompt.getConfig();

			expect(config.defaultTemperature).toBe(0.7);
			expect(config.maxTokens).toBe(2000);
			expect(config.timeout).toBe(30000);
		});

		it("should create instance with custom config", () => {
			const config = qiPrompt.getConfig();

			expect(config.defaultModel).toBe(mockModel);
			expect(config.defaultTemperature).toBe(0.7);
			expect(config.maxTokens).toBe(2000);
		});

		describe("generateText", () => {
			it("should generate text successfully", async () => {
				const result = await qiPrompt.generateText("Hello, world!");

				expect(isSuccess(result)).toBe(true);
				if (isSuccess(result)) {
					const response = result.right;
					expect(response.text).toBe("Generated response text");
					expect(response.metadata.model).toBe("test-model");
					expect(response.metadata.tokens).toEqual({
						prompt: 15,
						completion: 25,
						total: 40,
					});
					expect(response.metadata.finishReason).toBe("stop");
					expect(typeof response.metadata.duration).toBe("number");
					expect(typeof response.metadata.timestamp).toBe("number");
				}
			});

			it("should fail without model", async () => {
				const promptWithoutModel = new QiPrompt();
				const result = await promptWithoutModel.generateText("Hello!");

				expect(isFailure(result)).toBe(true);
				if (isFailure(result)) {
					const error = getError(result);
					expect(error?.code).toBe("NO_MODEL_SPECIFIED");
					expect(error?.category).toBe("VALIDATION");
				}
			});

			it("should use provided options", async () => {
				const options: QiGenerateOptions = {
					model: mockModel,
					temperature: 0.2,
					maxTokens: 1000,
					system: "You are a helpful assistant",
				};

				const result = await qiPrompt.generateText("Hello!", options);

				expect(isSuccess(result)).toBe(true);
				// The mock should have been called with the right parameters
			});
		});

		describe("generateResponse", () => {
			it("should generate response from messages", async () => {
				const messages = [{ role: "user" as const, content: "Hello!" }];

				const response = await qiPrompt.generateResponse(messages);

				expect(response).toBe("Generated response text");
			});

			it("should throw without model", async () => {
				const promptWithoutModel = new QiPrompt();
				const messages = [{ role: "user" as const, content: "Hello!" }];

				await expect(promptWithoutModel.generateResponse(messages)).rejects.toThrow(
					"Model must be provided either in config or options"
				);
			});
		});

		describe("generateStructured", () => {
			it("should generate structured output", async () => {
				const result = await qiPrompt.generateStructured("Analyze this", {
					schema: CommonSchemas.analysis,
				});

				expect(result).toEqual({
					summary: "Test summary",
					keyPoints: ["Point 1", "Point 2"],
				});
			});

			it("should throw without model", async () => {
				const promptWithoutModel = new QiPrompt();

				await expect(
					promptWithoutModel.generateStructured("Analyze this", {
						schema: CommonSchemas.analysis,
					})
				).rejects.toThrow("Model must be provided either in config or options");
			});
		});

		describe("configuration management", () => {
			it("should update configuration", () => {
				const updates = {
					defaultTemperature: 0.9,
					maxTokens: 3000,
				};

				qiPrompt.updateConfig(updates);
				const config = qiPrompt.getConfig();

				expect(config.defaultTemperature).toBe(0.9);
				expect(config.maxTokens).toBe(3000);
				expect(config.defaultModel).toBe(mockModel); // Unchanged
			});
		});
	});

	describe("Factory Functions", () => {
		describe("createQiPrompt", () => {
			it("should create QiPrompt with valid config", () => {
				const result = createQiPrompt(mockModel, {
					defaultTemperature: 0.5,
					maxTokens: 1500,
				});

				expect(isSuccess(result)).toBe(true);
				if (isSuccess(result)) {
					const qiPrompt = result.right;
					const config = qiPrompt.getConfig();
					expect(config.defaultModel).toBe(mockModel);
					expect(config.defaultTemperature).toBe(0.5);
					expect(config.maxTokens).toBe(1500);
				}
			});

			it("should fail with invalid temperature", () => {
				const result = createQiPrompt(mockModel, {
					defaultTemperature: 1.5, // Invalid
				});

				expect(isFailure(result)).toBe(true);
				if (isFailure(result)) {
					const error = getError(result);
					expect(error?.code).toBe("INVALID_TEMPERATURE");
					expect(error?.category).toBe("VALIDATION");
				}
			});

			it("should fail with invalid maxTokens", () => {
				const result = createQiPrompt(mockModel, {
					maxTokens: -100, // Invalid
				});

				expect(isFailure(result)).toBe(true);
				if (isFailure(result)) {
					const error = getError(result);
					expect(error?.code).toBe("INVALID_MAX_TOKENS");
					expect(error?.category).toBe("VALIDATION");
				}
			});
		});

		describe("createQiPromptSafe", () => {
			it("should return QiPrompt on success", () => {
				const qiPrompt = createQiPromptSafe(mockModel);

				expect(qiPrompt).not.toBeNull();
				expect(qiPrompt?.getConfig().defaultModel).toBe(mockModel);
			});

			it("should return null on failure", () => {
				const qiPrompt = createQiPromptSafe(mockModel, {
					defaultTemperature: 1.5, // Invalid
				});

				expect(qiPrompt).toBeNull();
			});
		});

		describe("createMathematicalQiPrompt", () => {
			it("should create optimized mathematical QiPrompt", () => {
				const result = createMathematicalQiPrompt(mockModel);

				expect(isSuccess(result)).toBe(true);
				if (isSuccess(result)) {
					const qiPrompt = result.right;
					const config = qiPrompt.getConfig();
					expect(config.defaultTemperature).toBe(0.1); // Lower for deterministic output
					expect(config.maxTokens).toBe(4000); // Higher for detailed explanations
					expect(config.retryAttempts).toBe(5); // More retries
					expect(config.timeout).toBe(60000); // Longer timeout
				}
			});
		});

		describe("generateTextFunctional", () => {
			it("should generate text functionally", async () => {
				const result = await generateTextFunctional(mockModel, "Hello, world!");

				expect(isSuccess(result)).toBe(true);
				if (isSuccess(result)) {
					const response = result.right;
					expect(response.text).toBe("Generated response text");
					expect(response.metadata.model).toBe("test-model");
				}
			});

			it("should use provided options", async () => {
				const options = {
					temperature: 0.2,
					maxTokens: 1000,
					system: "You are helpful",
				};

				const result = await generateTextFunctional(mockModel, "Hello!", options);

				expect(isSuccess(result)).toBe(true);
			});
		});
	});

	describe("QiPromptAPI", () => {
		it("should provide factory functions", () => {
			expect(typeof QiPromptAPI.createQiPrompt).toBe("function");
			expect(typeof QiPromptAPI.createQiPromptSafe).toBe("function");
			expect(typeof QiPromptAPI.createMathematical).toBe("function");
			expect(typeof QiPromptAPI.generateTextFunctional).toBe("function");
		});

		it("should provide validation function", () => {
			expect(typeof QiPromptAPI.validateConfig).toBe("function");
		});

		it("should provide QiPrompt class", () => {
			expect(QiPromptAPI.QiPrompt).toBe(QiPrompt);
		});

		it("should provide common schemas", () => {
			expect(QiPromptAPI.CommonSchemas).toBe(CommonSchemas);
		});

		it("should provide functional utilities", () => {
			expect(typeof QiPromptAPI.utils.map).toBe("function");
			expect(typeof QiPromptAPI.utils.flatMap).toBe("function");
			expect(typeof QiPromptAPI.utils.match).toBe("function");
			expect(typeof QiPromptAPI.utils.success).toBe("function");
			expect(typeof QiPromptAPI.utils.failure).toBe("function");
		});
	});

	describe("Common Schemas", () => {
		it("should provide analysis schema", () => {
			const schema = CommonSchemas.analysis;

			expect(schema).toBeDefined();
			// Should be a Zod schema that can validate analysis objects
			const testData = {
				summary: "Test summary",
				keyPoints: ["Point 1", "Point 2"],
				confidence: 0.85,
				recommendation: "Test recommendation",
			};

			const result = schema.safeParse(testData);
			expect(result.success).toBe(true);
		});

		it("should provide mathematical analysis schema", () => {
			const schema = CommonSchemas.mathematicalAnalysis;

			expect(schema).toBeDefined();
			// Should validate mathematical analysis objects
			const testData = {
				algebraicStructures: ["Monad", "Functor"],
				completenessScore: 85,
				inevitablePatterns: ["Composition", "Identity"],
				gaps: ["Missing associativity proof"],
				lawVerification: {
					satisfied: true,
					violations: [],
				},
			};

			const result = schema.safeParse(testData);
			expect(result.success).toBe(true);
		});

		it("should reject invalid analysis data", () => {
			const schema = CommonSchemas.analysis;
			const invalidData = {
				summary: "Test",
				// Missing required fields
			};

			const result = schema.safeParse(invalidData);
			expect(result.success).toBe(false);
		});
	});

	describe("Configuration Validation", () => {
		const validateConfig = QiPromptAPI.validateConfig;

		it("should validate valid configuration", () => {
			const config: QiPromptConfig = {
				defaultTemperature: 0.7,
				maxTokens: 2000,
				timeout: 30000,
				retryAttempts: 3,
			};

			const result = validateConfig(config);
			expect(isSuccess(result)).toBe(true);
		});

		it("should reject invalid temperature", () => {
			const config: QiPromptConfig = {
				defaultTemperature: 1.5, // Invalid
			};

			const result = validateConfig(config);
			expect(isFailure(result)).toBe(true);
			if (isFailure(result)) {
				const error = getError(result);
				expect(error?.code).toBe("INVALID_TEMPERATURE");
			}
		});

		it("should reject invalid maxTokens", () => {
			const config: QiPromptConfig = {
				maxTokens: -100, // Invalid
			};

			const result = validateConfig(config);
			expect(isFailure(result)).toBe(true);
			if (isFailure(result)) {
				const error = getError(result);
				expect(error?.code).toBe("INVALID_MAX_TOKENS");
			}
		});
	});
});
