/**
 * MathForge Agent Demo
 * Shows natural language to code generation workflow
 */

import { MathForgeAgent, quickAgent } from "./agent/index.js";

async function demoBasicUsage(): Promise<void> {
	console.log("🎯 MathForge Agent Demo - Basic Usage\n");

	try {
		// Quick one-liner usage
		console.log("📝 Request: 'Create a Result type with map and flatMap operations'");

		const result = await quickAgent(
			"Create a Result type with map and flatMap operations for error handling",
			"typescript",
			{
				model: "qwen2.5-coder:14b", // Use your best coding model
				temperature: 0.1,
			}
		);

		console.log("\n✨ Generated Code Preview:");
		console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
		console.log(`${result.main_code.substring(0, 500)}...`);
		console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

		console.log(
			`\n📊 Results: ${result.metadata.lines_of_code} lines, ${result.metadata.law_count} laws verified`
		);
	} catch (error) {
		console.error("❌ Demo failed:", error);
	}
}

async function demoAdvancedUsage(): Promise<void> {
	console.log("\n🚀 MathForge Agent Demo - Advanced Usage\n");

	const agent = new MathForgeAgent({
		model: "qwen2.5-coder:14b",
		temperature: 0.1,
		outputDir: "./demo-output",
	});

	const requests = [
		"Create a configuration system with validation and merging capabilities",
		"Build a caching system with TTL and LRU eviction",
		"Design a logger interface with different levels and formatters",
	];

	for (const request of requests) {
		try {
			console.log(`📝 Processing: "${request}"`);

			const result = await agent.generateFromNaturalLanguage(request, "typescript");
			await agent.saveResults(result, `demo-${Date.now()}`);

			console.log(`✅ Generated ${result.metadata.lines_of_code} lines\n`);
		} catch (error) {
			console.error(`❌ Failed for "${request}":`, error);
		}
	}
}

async function demoMultiLanguage(): Promise<void> {
	console.log("\n🌍 MathForge Agent Demo - Multi-Language Generation\n");

	const request = "Create a Maybe type for handling nullable values";

	const languages = ["typescript", "python", "haskell"] as const;

	for (const language of languages) {
		try {
			console.log(`📝 Generating ${language.toUpperCase()} implementation...`);

			const result = await quickAgent(request, language);

			console.log(`✅ ${language}: ${result.metadata.lines_of_code} lines generated`);
		} catch (error) {
			console.error(`❌ ${language} generation failed:`, error);
		}
	}
}

async function main(): Promise<void> {
	console.log("🤖 MathForge Agent - Full Demo Suite");
	console.log("=====================================\n");

	// Check if Ollama is running
	try {
		const response = await fetch("http://localhost:11434/api/tags");
		if (!response.ok) {
			throw new Error("Ollama not accessible");
		}
		console.log("✅ Ollama is running\n");
	} catch (_error) {
		console.error("❌ Ollama is not running. Please start Ollama first:");
		console.error("   ollama serve");
		console.error("   ollama pull qwen2.5-coder:14b");
		process.exit(1);
	}

	await demoBasicUsage();
	await demoAdvancedUsage();
	await demoMultiLanguage();

	console.log("\n🎉 Demo completed! Check the demo-output directory for generated files.");
}

// Run demo if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
	main().catch(console.error);
}
