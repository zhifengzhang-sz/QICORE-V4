import { beforeEach, describe, expect, it, vi } from "vitest";
import {
	ClaudeCode,
	type ClaudeCodeConfig,
	createClaudeCodeAgent,
	createClaudeCodeAgentSafe,
	createClaudeCodeModel,
} from "../../../lib/src/qiagent/claude-code.js";
import { getError, isFailure, isSuccess } from "../../../lib/src/qicore/base/index.js";

// Mock Anthropic SDK
vi.mock("@anthropic-ai/sdk", () => {
	return {
		default: vi.fn().mockImplementation(() => ({
			messages: {
				create: vi.fn().mockResolvedValue({
					id: "msg_123",
					type: "message",
					role: "assistant",
					content: [
						{
							type: "text",
							text: "Test response from Claude",
						},
					],
					model: "claude-3-5-sonnet-20241022",
					stop_reason: "end_turn",
					usage: {
						input_tokens: 10,
						output_tokens: 20,
					},
				}),
			},
		})),
	};
});

describe("Claude Code Agent", () => {
	describe("Factory Functions", () => {
		describe("createClaudeCodeAgent", () => {
			it("should create agent with default config", () => {
				// Set mock API key for test
				process.env.ANTHROPIC_API_KEY = "test-api-key";

				const result = createClaudeCodeAgent();

				expect(isSuccess(result)).toBe(true);
				if (isSuccess(result)) {
					const agent = result.right;
					const config = agent.getConfig();
					expect(config.provider).toBe("claude-code");
					expect(config.model).toBe("claude-3-5-sonnet-20241022");
					expect(config.temperature).toBe(0.7);
					expect(config.maxTokens).toBe(4000);
				}
			});

			it("should create agent with custom config", () => {
				process.env.ANTHROPIC_API_KEY = "test-api-key";

				const customConfig: ClaudeCodeConfig = {
					model: "claude-3-haiku-20240307",
					temperature: 0.2,
					maxTokens: 1000,
					timeout: 10000,
				};

				const result = createClaudeCodeAgent(customConfig);

				expect(isSuccess(result)).toBe(true);
				if (isSuccess(result)) {
					const agent = result.right;
					const config = agent.getConfig();
					expect(config.model).toBe("claude-3-haiku-20240307");
					expect(config.temperature).toBe(0.2);
					expect(config.maxTokens).toBe(1000);
					expect(config.timeout).toBe(10000);
				}
			});

			it("should fail without API key", () => {
				delete process.env.ANTHROPIC_API_KEY;

				const result = createClaudeCodeAgent();

				expect(isFailure(result)).toBe(true);
				if (isFailure(result)) {
					const error = getError(result);
					// fromTryCatch converts constructor errors to "OPERATION_FAILED" with UNKNOWN category
					expect(error?.code).toBe("OPERATION_FAILED");
					expect(error?.category).toBe("UNKNOWN");
					expect(error?.message).toContain("ANTHROPIC_API_KEY");
				}
			});

			it("should fail with invalid temperature", () => {
				process.env.ANTHROPIC_API_KEY = "test-api-key";

				const invalidConfig: ClaudeCodeConfig = {
					temperature: 1.5, // Invalid: > 1
				};

				const result = createClaudeCodeAgent(invalidConfig);

				expect(isFailure(result)).toBe(true);
				if (isFailure(result)) {
					const error = getError(result);
					// fromTryCatch converts constructor errors to "OPERATION_FAILED" with UNKNOWN category
					expect(error?.code).toBe("OPERATION_FAILED");
					expect(error?.category).toBe("UNKNOWN");
					expect(error?.message).toContain("Temperature must be between 0 and 1");
				}
			});
		});

		describe("createClaudeCodeAgentSafe", () => {
			it("should return agent on success", () => {
				process.env.ANTHROPIC_API_KEY = "test-api-key";

				const agent = createClaudeCodeAgentSafe();

				expect(agent).not.toBeNull();
				expect(agent?.getConfig().provider).toBe("claude-code");
			});

			it("should return null on failure", () => {
				delete process.env.ANTHROPIC_API_KEY;

				const agent = createClaudeCodeAgentSafe();

				expect(agent).toBeNull();
			});
		});

		describe("createClaudeCodeModel", () => {
			it("should create LanguageModel from agent", () => {
				process.env.ANTHROPIC_API_KEY = "test-api-key";

				const result = createClaudeCodeModel();

				expect(isSuccess(result)).toBe(true);
				if (isSuccess(result)) {
					const model = result.right;
					expect(model.modelId).toBe("claude-3-5-sonnet-20241022");
					expect(model.provider).toBe("anthropic");
					expect(typeof model.doGenerate).toBe("function");
				}
			});
		});
	});

	describe("ClaudeCode API Object", () => {
		it("should provide factory functions", () => {
			expect(typeof ClaudeCode.createAgent).toBe("function");
			expect(typeof ClaudeCode.createModel).toBe("function");
			expect(typeof ClaudeCode.createAgentSafe).toBe("function");
			expect(typeof ClaudeCode.createDefault).toBe("function");
			expect(typeof ClaudeCode.createMathematical).toBe("function");
		});

		it("should provide utility functions", () => {
			expect(typeof ClaudeCode.validateConfig).toBe("function");
			expect(typeof ClaudeCode.mapAnthropicError).toBe("function");
			expect(typeof ClaudeCode.extractContent).toBe("function");
		});

		it("should provide Agent class", () => {
			expect(ClaudeCode.Agent).toBeDefined();
		});
	});

	describe("Specialized Agent Creation", () => {
		beforeEach(() => {
			process.env.ANTHROPIC_API_KEY = "test-api-key";
		});

		it("should create default agent", () => {
			const result = ClaudeCode.createDefault();

			expect(isSuccess(result)).toBe(true);
			if (isSuccess(result)) {
				const agent = result.right;
				const config = agent.getConfig();
				expect(config.temperature).toBe(0.7);
				expect(config.maxTokens).toBe(4000);
				expect(config.maxRetries).toBe(3);
			}
		});

		it("should create mathematical agent", () => {
			const result = ClaudeCode.createMathematical();

			expect(isSuccess(result)).toBe(true);
			if (isSuccess(result)) {
				const agent = result.right;
				const config = agent.getConfig();
				expect(config.temperature).toBe(0.1); // Lower for deterministic math
				expect(config.maxTokens).toBe(8000); // Higher for detailed explanations
				expect(config.maxRetries).toBe(5); // More retries for critical computations
				expect(config.timeout).toBe(60000); // Longer timeout
			}
		});
	});

	describe("Agent Generation", () => {
		let agent: unknown;

		beforeEach(() => {
			process.env.ANTHROPIC_API_KEY = "test-api-key";
			const result = createClaudeCodeAgent();
			if (isSuccess(result)) {
				agent = result.right;
			}
		});

		it("should generate response successfully", async () => {
			const result = await agent.generate({
				prompt: "Hello, Claude!",
				systemPrompt: "You are a helpful assistant.",
			});

			expect(isSuccess(result)).toBe(true);
			if (isSuccess(result)) {
				const response = result.right;
				expect(response.content).toBe("Test response from Claude");
				expect(response.model).toBe("claude-3-5-sonnet-20241022");
				expect(response.finishReason).toBe("end_turn");
				expect(response.usage).toEqual({
					promptTokens: 10,
					completionTokens: 20,
					totalTokens: 30,
				});
				expect(response.id).toBe("msg_123");
			}
		});

		it("should handle generation errors", async () => {
			// Mock an error response
			const mockError = new Error("API Error");
			agent.anthropic.messages.create.mockRejectedValueOnce(mockError);

			const result = await agent.generate({
				prompt: "This will fail",
			});

			expect(isFailure(result)).toBe(true);
			if (isFailure(result)) {
				const error = getError(result);
				expect(error?.message).toContain("API Error");
			}
		});
	});

	describe("Language Model Integration", () => {
		let model: unknown;

		beforeEach(() => {
			process.env.ANTHROPIC_API_KEY = "test-api-key";
			const result = createClaudeCodeModel();
			if (isSuccess(result)) {
				model = result.right;
			}
		});

		it("should implement LanguageModel interface", () => {
			expect(model.specificationVersion).toBe("v1");
			expect(model.modelId).toBe("claude-3-5-sonnet-20241022");
			expect(model.provider).toBe("anthropic");
			expect(model.defaultObjectGenerationMode).toBe("tool");
			expect(typeof model.doGenerate).toBe("function");
			expect(typeof model.doStream).toBe("function");
		});

		it("should handle doGenerate calls", async () => {
			const options = {
				prompt: [{ role: "user" as const, content: "Hello, Claude!" }],
			};

			const result = await model.doGenerate(options);

			expect(result.text).toBe("Test response from Claude");
			expect(result.usage).toEqual({
				promptTokens: 10,
				completionTokens: 20,
			});
			expect(result.finishReason).toBe("end_turn");
			expect(result.response.id).toBe("msg_123");
			expect(result.response.modelId).toBe("claude-3-5-sonnet-20241022");
		});
	});

	describe("Configuration Validation", () => {
		it("should validate temperature range", () => {
			const validateConfig = ClaudeCode.validateConfig;

			// Valid temperature
			const validResult = validateConfig({ temperature: 0.5 });
			expect(isSuccess(validResult)).toBe(true);

			// Invalid temperature (too low)
			const lowResult = validateConfig({ temperature: -0.1 });
			expect(isFailure(lowResult)).toBe(true);

			// Invalid temperature (too high)
			const highResult = validateConfig({ temperature: 1.1 });
			expect(isFailure(highResult)).toBe(true);
		});

		it("should provide default values", () => {
			process.env.ANTHROPIC_API_KEY = "test-api-key";
			const validateConfig = ClaudeCode.validateConfig;

			const result = validateConfig({});

			expect(isSuccess(result)).toBe(true);
			if (isSuccess(result)) {
				const config = result.right;
				expect(config.model).toBe("claude-3-5-sonnet-20241022");
				expect(config.temperature).toBe(0.7);
				expect(config.maxTokens).toBe(4000);
				expect(config.timeout).toBe(30000);
				expect(config.maxRetries).toBe(3);
			}
		});
	});

	describe("Error Mapping", () => {
		it("should map rate limit errors", () => {
			const mapAnthropicError = ClaudeCode.mapAnthropicError;
			const rateLimitError = { status: 429, message: "Rate limited" };

			const qiError = mapAnthropicError(rateLimitError);

			expect(qiError.code).toBe("RATE_LIMITED");
			expect(qiError.category).toBe("NETWORK");
			expect(qiError.message).toBe("Claude API rate limit exceeded");
		});

		it("should map server errors", () => {
			const mapAnthropicError = ClaudeCode.mapAnthropicError;
			const serverError = { status: 500, message: "Internal server error" };

			const qiError = mapAnthropicError(serverError);

			expect(qiError.code).toBe("SERVICE_UNAVAILABLE");
			expect(qiError.category).toBe("NETWORK");
			expect(qiError.message).toBe("Claude API service error");
		});

		it("should map client errors", () => {
			const mapAnthropicError = ClaudeCode.mapAnthropicError;
			const clientError = { status: 400, message: "Bad request" };

			const qiError = mapAnthropicError(clientError);

			expect(qiError.code).toBe("CLIENT_ERROR");
			expect(qiError.category).toBe("BUSINESS");
			expect(qiError.message).toContain("Bad request");
		});
	});
});
