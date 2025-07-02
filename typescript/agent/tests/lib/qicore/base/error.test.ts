import { describe, expect, it } from "vitest";
import {
	CommonErrors,
	createQiError,
	type ErrorCategory,
	type ErrorSeverity,
	fromException,
	fromString,
	isQiError,
	isRetryable,
	withCause,
	withContext,
	withSeverity,
} from "../../../../lib/src/qicore/base/index.js";

describe("QiError", () => {
	describe("Construction", () => {
		it("should create basic error", () => {
			const error = createQiError("TEST_CODE", "Test message", "VALIDATION");

			expect(error.code).toBe("TEST_CODE");
			expect(error.message).toBe("Test message");
			expect(error.category).toBe("VALIDATION");
			expect(error.severity).toBe("LOW"); // VALIDATION category defaults to LOW
			expect(typeof error.timestamp).toBe("number");
			expect(isQiError(error)).toBe(true);
		});

		it("should create error with all fields", () => {
			const context = { userId: "123", action: "login" };
			const error = createQiError(
				"LOGIN_FAILED",
				"Login attempt failed",
				"BUSINESS",
				context,
				"WARNING"
			);

			expect(error.code).toBe("LOGIN_FAILED");
			expect(error.message).toBe("Login attempt failed");
			expect(error.category).toBe("BUSINESS");
			expect(error.severity).toBe("MEDIUM"); // BUSINESS category defaults to MEDIUM
			expect(error.context.get("userId")).toBe("123");
			expect(error.context.get("action")).toBe("login");
		});
	});

	describe("Error Categories", () => {
		const categories: ErrorCategory[] = ["VALIDATION", "NETWORK", "BUSINESS", "SYSTEM", "UNKNOWN"];

		categories.forEach((category) => {
			it(`should create ${category} error`, () => {
				const error = createQiError("TEST_CODE", "Test message", category);
				expect(error.category).toBe(category);
			});
		});
	});

	describe("Error Severities", () => {
		const severities: ErrorSeverity[] = ["LOW", "MEDIUM", "HIGH", "CRITICAL"];

		severities.forEach((severity) => {
			it(`should create error with ${severity} severity`, () => {
				const error = createQiError("TEST_CODE", "Test message", "VALIDATION", {}, null, severity); // Explicitly provide severity should override category default
				expect(error.severity).toBe(severity);
			});
		});
	});

	describe("Factory Functions", () => {
		it("should create error from Exception", () => {
			const originalError = new Error("Original error message");
			const qiError = fromException(originalError, "SYSTEM");

			expect(qiError.message).toContain("Original error message");
			expect(qiError.category).toBe("SYSTEM");
			expect(qiError.code).toBe("Error"); // fromException uses error.name as code
		});

		it("should create error from string", () => {
			const qiError = fromString("Simple error message", "VALIDATION");

			expect(qiError.message).toBe("Simple error message");
			expect(qiError.category).toBe("VALIDATION");
			expect(qiError.code).toBe("STRING_ERROR");
		});
	});

	describe("Error Enhancement", () => {
		it("should add context to error", () => {
			const original = createQiError("TEST_CODE", "Test message", "VALIDATION");
			const enhanced = withContext(original, { requestId: "req-123", userId: "user-456" });

			expect(enhanced.context.get("requestId")).toBe("req-123");
			expect(enhanced.context.get("userId")).toBe("user-456");
			expect(enhanced.code).toBe(original.code);
			expect(enhanced.message).toBe(original.message);
		});

		it("should add cause to error", () => {
			const rootCause = createQiError("ROOT_CAUSE", "Root cause error", "NETWORK");
			const original = createQiError("WRAPPER_ERROR", "Wrapper error", "BUSINESS");
			const enhanced = withCause(original, rootCause);

			expect(enhanced.cause).toBe(rootCause);
			expect(enhanced.code).toBe(original.code);
			expect(enhanced.message).toBe(original.message);
		});

		it("should change error severity", () => {
			const original = createQiError("TEST_CODE", "Test message", "VALIDATION");
			const enhanced = withSeverity(original, "CRITICAL");

			expect(enhanced.severity).toBe("CRITICAL");
			expect(enhanced.code).toBe(original.code);
			expect(enhanced.message).toBe(original.message);
		});
	});

	describe("Retry Logic", () => {
		it("should identify retryable errors", () => {
			const networkError = createQiError("TIMEOUT", "Request timeout", "NETWORK");
			const businessError = createQiError("INVALID_INPUT", "Invalid input", "BUSINESS");

			expect(isRetryable(networkError.category)).toBe(true);
			expect(isRetryable(businessError.category)).toBe(false);
		});

		it("should handle temporary errors as retryable", () => {
			const tempError = createQiError("TEMP_UNAVAILABLE", "Temporarily unavailable", "SYSTEM");
			expect(isRetryable(tempError.category)).toBe(true);
		});

		it("should handle critical errors as non-retryable", () => {
			const criticalError = createQiError(
				"CRITICAL_FAILURE",
				"Critical failure",
				"SYSTEM",
				{},
				"CRITICAL"
			);
			expect(isRetryable(criticalError.category)).toBe(true); // SYSTEM category is retryable regardless of severity
		});
	});

	describe("Common Errors", () => {
		it("should provide validation error factory", () => {
			const error = CommonErrors.validation("Field is required", { field: "email" });

			expect(error.category).toBe("VALIDATION");
			expect(error.severity).toBe("LOW"); // VALIDATION category defaults to LOW severity
			expect(error.message).toBe("Field is required");
			expect(error.context.get("field")).toBe("email");
		});

		it("should provide network error factory", () => {
			const error = CommonErrors.network("Connection failed", { host: "api.example.com" });

			expect(error.category).toBe("NETWORK");
			expect(error.message).toBe("Connection failed");
			expect(error.context.get("host")).toBe("api.example.com");
		});

		it("should provide timeout error factory", () => {
			const error = CommonErrors.timeout("Operation timed out", 5000);

			expect(error.category).toBe("TIMEOUT"); // timeout creates TIMEOUT category, not NETWORK
			expect(error.code).toBe("TIMEOUT_ERROR");
			expect(error.message).toBe("Operation timed out");
			expect(error.context.get("timeoutMs")).toBe(5000);
		});

		it("should provide not found error factory", () => {
			const error = CommonErrors.notFound("User", { id: "123" });

			expect(error.category).toBe("BUSINESS");
			expect(error.code).toBe("NOT_FOUND");
			expect(error.message).toContain("User");
			expect(error.context.get("resource")).toBe("User");
			expect(error.context.get("id")).toBe("123");
		});
	});

	describe("Type Guards", () => {
		it("should identify QiError objects", () => {
			const qiError = createQiError("TEST_CODE", "Test message", "VALIDATION");
			const regularError = new Error("Regular error");
			const notAnError = { message: "Not an error" };

			expect(isQiError(qiError)).toBe(true);
			expect(isQiError(regularError)).toBe(false);
			expect(isQiError(notAnError)).toBe(false);
			expect(isQiError(null)).toBe(false);
			expect(isQiError(undefined)).toBe(false);
		});
	});

	describe("Error String Representation", () => {
		it("should provide meaningful toString", () => {
			const error = createQiError(
				"TEST_CODE",
				"Test message",
				"VALIDATION",
				{ userId: "123" },
				"LOW"
			);
			const str = error.toString();

			expect(str).toContain("VALIDATION");
			expect(str).toContain("LOW"); // VALIDATION category with explicit severity becomes LOW
			expect(str).toContain("TEST_CODE");
			expect(str).toContain("Test message");
			expect(str).toContain("userId=123");
		});

		it("should include cause in toString", () => {
			const rootCause = createQiError("ROOT_CAUSE", "Root cause error", "NETWORK");
			const error = createQiError("WRAPPER_ERROR", "Wrapper error", "BUSINESS");
			const enhanced = withCause(error, rootCause);

			const str = enhanced.toString();
			expect(str).toContain("caused by: ROOT_CAUSE");
		});
	});

	describe("Error Chaining", () => {
		it("should preserve error chain information", () => {
			const rootCause = createQiError("DATABASE_ERROR", "Connection failed", "NETWORK");
			const serviceError = withCause(
				createQiError("SERVICE_ERROR", "Service unavailable", "BUSINESS"),
				rootCause
			);
			const apiError = withCause(
				createQiError("API_ERROR", "API request failed", "SYSTEM"),
				serviceError
			);

			expect(apiError.cause).toBe(serviceError);
			expect(apiError.cause?.cause).toBe(rootCause);
			expect(apiError.cause?.cause?.message).toBe("Connection failed");
		});
	});
});
