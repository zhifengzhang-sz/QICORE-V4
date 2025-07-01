import { describe, expect, it } from "bun:test";
import fc from "fast-check";
import { generateCode, MathForgeGenerator } from "../core/generator";
import type { FormalSpec, GenerationOptions } from "../types/spec";

const mockSpec: FormalSpec = {
	metadata: {
		component: "TestComponent",
		mathematical_foundation: "Category Theory",
		package_strategy: "Functional",
		laws_verified: ["monad.left_identity", "functor.identity"],
	},
	type_definitions: {
		Result: {
			description: "Either type for error handling",
			type_parameters: ["T"],
			constructors: ["Success", "Error"],
			fields: ["value", "error"],
			constraints: [],
			derives: ["Eq", "Show"],
		},
	},
	operations: {
		map: {
			signature: "(f: A => B) => Result<A> => Result<B>",
			semantics: "Functor map operation",
			description: "Maps a function over the result value",
			mathematical_laws: ["functor.identity"],
			properties: {
				identity: {
					formula: "map(id) = id",
					description: "Mapping identity function returns the same result",
				},
			},
		},
	},
	generation_targets: {
		typescript: {
			formal_verification_tools: ["TypeScript"],
			package_dependencies: ["@types/node"],
			test_framework: "bun:test",
		},
		python: {
			formal_verification_tools: ["mypy"],
			package_dependencies: ["typing"],
			test_framework: "pytest",
		},
		haskell: {
			formal_verification_tools: ["GHC"],
			package_dependencies: ["base"],
			test_framework: "QuickCheck",
		},
	},
};

const mockOptions: GenerationOptions = {
	target_language: "typescript",
	include_tests: true,
	include_benchmarks: false,
	include_documentation: true,
	strict_mode: true,
};

describe("MathForgeGenerator", () => {
	it("should create a generator instance", () => {
		const generator = new MathForgeGenerator(mockSpec, mockOptions);
		expect(generator).toBeDefined();
	});

	it("should generate TypeScript code", async () => {
		const result = await generateCode(mockSpec, mockOptions);
		expect(result.language).toBe("typescript");
		expect(result.main_code).toContain("interface");
		expect(result.main_code).toContain("Result");
	});

	it("should generate code with tests when requested", async () => {
		const optionsWithTests: GenerationOptions = {
			...mockOptions,
			include_tests: true,
		};

		const result = await generateCode(mockSpec, optionsWithTests);
		expect(result.test_code).toBeDefined();
		expect(result.test_code).toContain("test");
	});

	it("should generate code for different languages", async () => {
		const pythonOptions: GenerationOptions = {
			...mockOptions,
			target_language: "python",
		};

		const result = await generateCode(mockSpec, pythonOptions);
		expect(result.language).toBe("python");
		expect(result.main_code).toContain("class");
	});

	it("should use the generateCode helper function", async () => {
		const result = await generateCode(mockSpec, mockOptions);
		expect(result).toBeDefined();
		expect(result.language).toBe("typescript");
	});

	// Property-based tests using fast-check
	it("should always generate valid code structure", async () => {
		await fc.assert(
			fc.asyncProperty(
				fc.constantFrom("typescript", "python", "haskell"),
				fc.boolean(),
				fc.boolean(),
				async (language, includeTests, includeDocs) => {
					const options: GenerationOptions = {
						target_language: language,
						include_tests: includeTests,
						include_benchmarks: false,
						include_documentation: includeDocs,
						strict_mode: true,
					};

					const result = await generateCode(mockSpec, options);

					// Properties that should always hold
					expect(result.language).toBe(language);
					expect(result.main_code).toBeDefined();
					expect(result.main_code.length).toBeGreaterThan(0);
					expect(result.dependencies).toBeDefined();
					expect(Array.isArray(result.dependencies)).toBe(true);

					if (includeTests) {
						expect(result.test_code).toBeDefined();
					}

					if (includeDocs) {
						expect(result.documentation).toBeDefined();
					}
				}
			),
			{ numRuns: 20 } // Run 20 different combinations
		);
	});

	it("should generate consistent metadata", async () => {
		await fc.assert(
			fc.asyncProperty(fc.constantFrom("typescript", "python", "haskell"), async (language) => {
				const options: GenerationOptions = {
					...mockOptions,
					target_language: language,
				};

				const result = await generateCode(mockSpec, options);

				// Metadata should be consistent
				expect(result.metadata.lines_of_code).toBeGreaterThan(0);
				expect(result.metadata.property_count).toBeGreaterThanOrEqual(0);
				expect(result.metadata.law_count).toBeGreaterThanOrEqual(0);
				expect(result.metadata.test_count).toBeGreaterThanOrEqual(0);
			}),
			{ numRuns: 10 }
		);
	});

	it("should handle edge cases in spec structure", async () => {
		await fc.assert(
			fc.asyncProperty(
				fc.record({
					component: fc.string({ minLength: 1, maxLength: 50 }),
					mathematical_foundation: fc.constantFrom("Category Theory", "Type Theory", "Set Theory"),
					package_strategy: fc.string({ minLength: 1, maxLength: 100 }),
				}),
				async (metadata) => {
					const testSpec: FormalSpec = {
						...mockSpec,
						metadata: {
							...mockSpec.metadata,
							...metadata,
						},
					};

					const result = await generateCode(testSpec, mockOptions);

					// Should handle various metadata combinations
					expect(result).toBeDefined();
					expect(result.main_code).toContain(metadata.component);
				}
			),
			{ numRuns: 15 }
		);
	});
});
