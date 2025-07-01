import { describe, expect, it } from "vitest";
import {
	ErrorCategory,
	QiErrorImpl,
	createQiError,
} from "../../../src/qicore/base/error.js";

describe("QiError", () => {
	describe("createQiError", () => {
		it("should create a basic error", () => {
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
			);

			expect(error.code).toBe("TEST_ERROR");
			expect(error.message).toBe("Test message");
			expect(error.category).toBe(ErrorCategory.VALIDATION);
			expect(error.context.size).toBe(0);
			expect(error.cause).toBeUndefined();
			expect(error.timestamp).toBeGreaterThan(0);
		});

		it("should create error with context as object", () => {
			const context = { userId: 123, operation: "test" };
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
				context,
			);

			expect(error.context.get("userId")).toBe(123);
			expect(error.context.get("operation")).toBe("test");
		});

		it("should create error with context as Map", () => {
			const context = new Map([
				["userId", 123],
				["operation", "test"],
			]);
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
				context,
			);

			expect(error.context.get("userId")).toBe(123);
			expect(error.context.get("operation")).toBe("test");
		});

		it("should create error with cause", () => {
			const cause = createQiError(
				"CAUSE_ERROR",
				"Cause message",
				ErrorCategory.NETWORK,
			);
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
				{},
				cause,
			);

			expect(error.cause).toBe(cause);
		});

		it("should throw on empty code", () => {
			expect(() => {
				new QiErrorImpl("", "Test message", ErrorCategory.VALIDATION);
			}).toThrow("Error code must be non-empty string");
		});

		it("should throw on whitespace-only code", () => {
			expect(() => {
				new QiErrorImpl("   ", "Test message", ErrorCategory.VALIDATION);
			}).toThrow("Error code must be non-empty string");
		});
	});

	describe("toString", () => {
		it("should format basic error", () => {
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
			);
			const str = error.toString();

			expect(str).toBe("[VALIDATION] TEST_ERROR: Test message");
		});

		it("should include context in string", () => {
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
				{ userId: 123 },
			);
			const str = error.toString();

			expect(str).toBe("[VALIDATION] TEST_ERROR: Test message (userId=123)");
		});

		it("should include cause in string", () => {
			const cause = createQiError(
				"CAUSE_ERROR",
				"Cause message",
				ErrorCategory.NETWORK,
			);
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
				{},
				cause,
			);
			const str = error.toString();

			expect(str).toBe(
				"[VALIDATION] TEST_ERROR: Test message [Caused by: [NETWORK] CAUSE_ERROR: Cause message]",
			);
		});
	});

	describe("toStructuredData", () => {
		it("should convert to structured data", () => {
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
				{ userId: 123 },
			);
			const data = error.toStructuredData();

			expect(data).toEqual({
				code: "TEST_ERROR",
				message: "Test message",
				category: ErrorCategory.VALIDATION,
				context: { userId: 123 },
				cause: undefined,
				timestamp: error.timestamp,
			});
		});

		it("should include cause in structured data", () => {
			const cause = createQiError(
				"CAUSE_ERROR",
				"Cause message",
				ErrorCategory.NETWORK,
			);
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
				{},
				cause,
			);
			const data = error.toStructuredData();

			expect(data.cause).toEqual(cause.toStructuredData());
		});
	});

	describe("withContext", () => {
		it("should add context to existing error", () => {
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
			);
			const newError = error.withContext({ userId: 123 });

			expect(newError.context.get("userId")).toBe(123);
			expect(error.context.size).toBe(0); // Original unchanged
		});

		it("should merge with existing context", () => {
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
				{ operation: "test" },
			);
			const newError = error.withContext({ userId: 123 });

			expect(newError.context.get("operation")).toBe("test");
			expect(newError.context.get("userId")).toBe(123);
		});

		it("should work with Map context", () => {
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
			);
			const newError = error.withContext(new Map([["userId", 123]]));

			expect(newError.context.get("userId")).toBe(123);
		});
	});

	describe("withCause", () => {
		it("should add cause to error", () => {
			const cause = createQiError(
				"CAUSE_ERROR",
				"Cause message",
				ErrorCategory.NETWORK,
			);
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.VALIDATION,
			);
			const newError = error.withCause(cause);

			expect(newError.cause).toBe(cause);
			expect(error.cause).toBeUndefined(); // Original unchanged
		});
	});

	describe("cause chain validation", () => {
		it("should prevent circular cause chains", () => {
			const error1 = createQiError(
				"ERROR1",
				"Message 1",
				ErrorCategory.VALIDATION,
			);
			const error2 = createQiError(
				"ERROR2",
				"Message 2",
				ErrorCategory.VALIDATION,
				{},
				error1,
			);

			expect(() => {
				// Try to create a chain: error3 -> error2 -> error1 -> error3 (circular)
				const error3 = createQiError(
					"ERROR3",
					"Message 3",
					ErrorCategory.VALIDATION,
					{},
					error2,
				);
				error1.withCause(error3); // This creates the circular reference
			}).toThrow("Circular cause chain detected");
		});

		it("should prevent excessive cause chain depth", () => {
			let error = createQiError("ERROR0", "Message", ErrorCategory.VALIDATION);

			// Create a chain of 9 errors (depth will be 10 with the original)
			for (let i = 1; i <= 9; i++) {
				error = createQiError(
					`ERROR${i}`,
					`Message ${i}`,
					ErrorCategory.VALIDATION,
					{},
					error,
				);
			}

			// Adding one more should throw (depth would exceed 10)
			expect(() => {
				createQiError(
					"ERROR10",
					"Message 10",
					ErrorCategory.VALIDATION,
					{},
					error,
				);
			}).toThrow("Cause chain depth exceeds maximum of 10");
		});
	});

	describe("getCategory", () => {
		it("should return error category", () => {
			const error = createQiError(
				"TEST_ERROR",
				"Test message",
				ErrorCategory.NETWORK,
			);
			expect(error.getCategory()).toBe(ErrorCategory.NETWORK);
		});
	});

	describe("ErrorCategory enum", () => {
		it("should have all expected categories", () => {
			expect(ErrorCategory.VALIDATION).toBe("VALIDATION");
			expect(ErrorCategory.NETWORK).toBe("NETWORK");
			expect(ErrorCategory.FILESYSTEM).toBe("FILESYSTEM");
			expect(ErrorCategory.CONFIGURATION).toBe("CONFIGURATION");
			expect(ErrorCategory.CACHE).toBe("CACHE");
			expect(ErrorCategory.TIMEOUT).toBe("TIMEOUT");
			expect(ErrorCategory.PERMISSION).toBe("PERMISSION");
			expect(ErrorCategory.UNKNOWN).toBe("UNKNOWN");
		});
	});
});
