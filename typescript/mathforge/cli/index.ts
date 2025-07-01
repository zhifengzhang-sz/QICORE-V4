/**
 * MathForge CLI - Command Line Interface
 * Professional CLI for universal mathematical code generation
 */

import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { basename, join } from "node:path";
import { parseArgs } from "node:util";
import { generateCode } from "../core/generator.js";
import { parseSpecFile, validateForLanguage } from "../core/parser.js";
import type { GeneratedCode, GenerationOptions, TargetLanguage } from "../types/spec.js";

interface CliOptions {
	input: string;
	output?: string;
	language: TargetLanguage;
	tests: boolean;
	benchmarks: boolean;
	docs: boolean;
	strict: boolean;
	verbose: boolean;
	help: boolean;
	version: boolean;
}

/**
 * CLI argument configuration
 */
const CLI_CONFIG = {
	options: {
		input: {
			type: "string" as const,
			short: "i",
			description: "Input YAML spec file path",
		},
		output: {
			type: "string" as const,
			short: "o",
			description: "Output directory (default: current directory)",
		},
		language: {
			type: "string" as const,
			short: "l",
			description: "Target language: typescript, python, haskell",
		},
		tests: {
			type: "boolean" as const,
			short: "t",
			description: "Generate test files",
		},
		benchmarks: {
			type: "boolean" as const,
			short: "b",
			description: "Generate benchmark files",
		},
		docs: {
			type: "boolean" as const,
			short: "d",
			description: "Generate documentation",
		},
		strict: {
			type: "boolean" as const,
			short: "s",
			description: "Enable strict mode",
		},
		verbose: {
			type: "boolean" as const,
			short: "v",
			description: "Verbose output",
		},
		help: {
			type: "boolean" as const,
			short: "h",
			description: "Show help information",
		},
		version: {
			type: "boolean" as const,
			description: "Show version information",
		},
	},
	allowPositionals: true,
};

/**
 * MathForge version information
 */
const MATHFORGE_VERSION = "1.0.0";

/**
 * Main CLI entry point
 */
export async function main(args: string[] = process.argv.slice(2)): Promise<void> {
	try {
		const options = parseCliArgs(args);

		if (options.help) {
			showHelp();
			return;
		}

		if (options.version) {
			showVersion();
			return;
		}

		await executeGeneration(options);
	} catch (error) {
		console.error(`❌ Error: ${error}`);
		process.exit(1);
	}
}

/**
 * Parse CLI arguments with validation
 */
function parseCliArgs(args: string[]): CliOptions {
	const parsed = parseArgs({
		args,
		options: CLI_CONFIG.options,
		allowPositionals: CLI_CONFIG.allowPositionals,
	});

	const options = extractOptionsFromParsed(parsed);
	validateCliOptions(options);

	return options;
}

/**
 * Extract options from parsed arguments with defaults
 */
function extractOptionsFromParsed(parsed: ReturnType<typeof parseArgs>): CliOptions {
	return {
		input: (parsed.values.input as string) || parsed.positionals[0] || "",
		output: parsed.values.output as string,
		language: (parsed.values.language as TargetLanguage) || "typescript",
		tests: (parsed.values.tests as boolean) || false,
		benchmarks: (parsed.values.benchmarks as boolean) || false,
		docs: (parsed.values.docs as boolean) || false,
		strict: (parsed.values.strict as boolean) || false,
		verbose: (parsed.values.verbose as boolean) || false,
		help: (parsed.values.help as boolean) || false,
		version: (parsed.values.version as boolean) || false,
	};
}

/**
 * Validate CLI options
 */
function validateCliOptions(options: CliOptions): void {
	if (options.help || options.version) {
		return; // Skip validation for help/version
	}

	if (!options.input) {
		throw new Error("Input file is required. Use -i <file> or provide as positional argument.");
	}

	if (!existsSync(options.input)) {
		throw new Error(`Input file not found: ${options.input}`);
	}

	if (!["typescript", "python", "haskell"].includes(options.language)) {
		throw new Error(
			`Invalid language: ${options.language}. Must be one of: typescript, python, haskell`
		);
	}
}

/**
 * Execute code generation with comprehensive error handling
 */
async function executeGeneration(options: CliOptions): Promise<void> {
	const startTime = performance.now();

	if (options.verbose) {
		console.log("🔧 MathForge - Universal Mathematical Code Generator");
		console.log(`📄 Input: ${options.input}`);
		console.log(`🎯 Language: ${options.language}`);
		console.log(`📁 Output: ${options.output || "current directory"}`);
		console.log("---");
	}

	// Parse specification
	console.log("🔍 Parsing specification...");
	const { spec, validation } = parseSpecFile(options.input);

	if (!validation.valid) {
		console.error("❌ Specification validation failed:");
		validation.errors.forEach((error) => {
			console.error(`  • ${error.path}: ${error.message}`);
		});

		if (validation.warnings.length > 0) {
			console.warn("⚠️  Warnings:");
			validation.warnings.forEach((warning) => {
				console.warn(`  • ${warning.path}: ${warning.message}`);
			});
		}
		throw new Error("Invalid specification");
	}

	if (!spec) {
		throw new Error("Failed to parse specification");
	}

	// Language-specific validation
	console.log(`🔍 Validating for ${options.language}...`);
	const langValidation = validateForLanguage(spec, options.language);

	if (!langValidation.valid) {
		console.error(`❌ ${options.language} validation failed:`);
		langValidation.errors.forEach((error) => {
			console.error(`  • ${error.path}: ${error.message}`);
		});
	}

	if (langValidation.warnings.length > 0) {
		console.warn("⚠️  Warnings:");
		langValidation.warnings.forEach((warning) => {
			console.warn(`  • ${warning.path}: ${warning.message}`);
		});
	}

	// Generate code
	console.log(`🚀 Generating ${options.language} code...`);

	const generationOptions: GenerationOptions = {
		target_language: options.language,
		include_tests: options.tests,
		include_benchmarks: options.benchmarks,
		include_documentation: options.docs,
		strict_mode: options.strict,
		output_path: options.output,
	};

	const result = await generateCode(spec, generationOptions);

	// Write output files
	const outputDir = options.output || ".";
	const baseFileName = basename(options.input, ".yaml");

	console.log("💾 Writing output files...");
	await writeOutputFiles(result, outputDir, baseFileName, options);

	// Success reporting
	const endTime = performance.now();
	const duration = Math.round(endTime - startTime);

	console.log("✅ Generation completed successfully!");
	console.log(`📊 Statistics:`);
	console.log(`  • Language: ${result.language}`);
	console.log(`  • Main code: ${result.metadata.lines_of_code} lines`);
	console.log(`  • Dependencies: ${result.dependencies.length}`);
	console.log(`  • Properties: ${result.metadata.property_count}`);
	console.log(`  • Laws verified: ${result.metadata.law_count}`);
	if (result.test_code) {
		console.log(`  • Test code: ${result.metadata.test_count} lines`);
	}
	console.log(`  • Duration: ${duration}ms`);

	if (options.verbose) {
		console.log("\n📦 Dependencies:");
		result.dependencies.forEach((dep) => console.log(`  • ${dep}`));
	}
}

/**
 * Write generated code to output files
 */
async function writeOutputFiles(
	result: GeneratedCode,
	outputDir: string,
	baseFileName: string,
	options: CliOptions
): Promise<void> {
	// Ensure output directory exists
	if (!existsSync(outputDir)) {
		mkdirSync(outputDir, { recursive: true });
	}

	// File extensions by language
	const extensions = {
		typescript: "ts",
		python: "py",
		haskell: "hs",
	};

	const ext = extensions[options.language];

	// Write main code
	const mainFile = join(outputDir, `${baseFileName}.${ext}`);
	writeFileSync(mainFile, result.main_code);
	console.log(`  📄 ${mainFile}`);

	// Write test code
	if (result.test_code) {
		const testFile = join(outputDir, `${baseFileName}.test.${ext}`);
		writeFileSync(testFile, result.test_code);
		console.log(`  🧪 ${testFile}`);
	}

	// Write benchmark code
	if (result.benchmark_code) {
		const benchFile = join(outputDir, `${baseFileName}.bench.${ext}`);
		writeFileSync(benchFile, result.benchmark_code);
		console.log(`  📈 ${benchFile}`);
	}

	// Write documentation
	if (result.documentation) {
		const docFile = join(outputDir, `${baseFileName}.md`);
		writeFileSync(docFile, result.documentation);
		console.log(`  📚 ${docFile}`);
	}
}

/**
 * Show help information
 */
function showHelp(): void {
	console.log(`
🔧 MathForge - Universal Mathematical Code Generator v${MATHFORGE_VERSION}

USAGE:
  mathforge [OPTIONS] <spec-file>

ARGUMENTS:
  <spec-file>          YAML specification file

OPTIONS:
  -i, --input <FILE>   Input YAML spec file
  -o, --output <DIR>   Output directory (default: current directory)
  -l, --language <LANG> Target language: typescript, python, haskell (default: typescript)
  -t, --tests          Generate test files
  -b, --benchmarks     Generate benchmark files  
  -d, --docs           Generate documentation
  -s, --strict         Enable strict mode
  -v, --verbose        Verbose output
  -h, --help           Show this help
      --version        Show version

EXAMPLES:
  mathforge spec.yaml
  mathforge -l python -t -d spec.yaml
  mathforge --language haskell --tests --output ./generated spec.yaml

For more information, visit: https://github.com/qi/mathforge
`);
}

/**
 * Show version information
 */
function showVersion(): void {
	console.log(`MathForge v${MATHFORGE_VERSION}`);
	console.log("Universal Mathematical Code Generator");
	console.log("Built with TypeScript, Bun, and modern DevOps");
}

/**
 * Handle uncaught errors gracefully
 */
process.on("uncaughtException", (error) => {
	console.error("❌ Uncaught Exception:", error.message);
	process.exit(1);
});

process.on("unhandledRejection", (reason) => {
	console.error("❌ Unhandled Rejection:", reason);
	process.exit(1);
});

// Run CLI if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
	main();
}
