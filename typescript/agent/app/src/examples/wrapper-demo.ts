#!/usr/bin/env bun

/**
 * QiCore Wrapper Demo
 *
 * Demonstrates the @qi/* wrappers in action:
 * - @qi/prompt (Vercel AI SDK wrapper)
 * - @qi/mcp (MCP SDK wrapper)
 * - @qi/agent (AI Orchestra wrapper)
 */

import { MCPClient } from "@qi/mcp";
import type { MathematicalPromptManager } from "@qi/prompt";
import { createMathematicalPromptManager } from "@qi/prompt";
import {
	testLawVerification,
	testMathematicalContract,
} from "../utils/test-mathematical-contracts.js";

console.log("🎭 QiCore Wrapper Demo");
console.log("======================\n");

/**
 * Demo 1: @qi/prompt wrapper (Vercel AI SDK)
 */
async function demoPromptWrapper() {
	console.log("📝 Demo 1: @qi/prompt (Vercel AI SDK Wrapper)");
	console.log("----------------------------------------------");

	try {
		const promptManager: MathematicalPromptManager = createMathematicalPromptManager();

		console.log("✅ Created MathematicalPromptManager");
		console.log("📊 Manager stats:", promptManager.getStats());

		// Test prompt creation
		const context = {
			component: "DemoType",
			contractText: "interface DemoType { value: number; }",
			domain: "algebraic_structures" as const,
			complexity: "graduate" as const,
		};

		const templateResult = await promptManager.createAnalysisPrompt(context);

		if (templateResult._tag === "Right") {
			console.log("✅ Created analysis prompt template");
			console.log("📋 Template ID:", templateResult.right.id);
			console.log("🏷️  Template metadata:", templateResult.right.metadata);
		} else {
			console.log("❌ Failed to create template:", templateResult.left.message);
		}
	} catch (error) {
		console.log("❌ Prompt wrapper demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 2: @qi/mcp wrapper (MCP SDK)
 */
async function demoMCPWrapper() {
	console.log("🔌 Demo 2: @qi/mcp (MCP SDK Wrapper)");
	console.log("------------------------------------");

	const logger = {
		info: (msg: string) => console.log(`[MCP] ℹ️  ${msg}`),
		warn: (msg: string) => console.log(`[MCP] ⚠️  ${msg}`),
		error: (msg: string) => console.log(`[MCP] ❌ ${msg}`),
	};

	try {
		const mcpClient = new MCPClient(logger);
		console.log("✅ Created MCPClient instance");

		// Try to connect to a memory server (this might fail if no server is running)
		const memoryConnected = await mcpClient.connectToServer({
			name: "memory-demo",
			command: "bunx",
			args: ["--bun", "@modelcontextprotocol/server-memory"],
			env: { NODE_ENV: "development" },
		});

		if (memoryConnected) {
			console.log("✅ Connected to MCP memory server");
			console.log("🔧 Connected servers:", mcpClient.getConnectedServers());

			// Test if connected
			console.log("🔍 Is connected to memory-demo:", mcpClient.isConnected("memory-demo"));

			// Disconnect
			await mcpClient.disconnect();
			console.log("🔌 Disconnected from servers");
		} else {
			console.log("⚠️  Could not connect to MCP server (this is normal if no server is running)");
		}
	} catch (error) {
		console.log("❌ MCP wrapper demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 3: Mathematical analysis using the wrappers
 */
async function demoMathematicalAnalysis() {
	console.log("🧮 Demo 3: Mathematical Analysis using @qi/prompt");
	console.log("------------------------------------------------");

	try {
		// Test simple contract analysis
		const simpleContract = `
interface Result<T> {
	readonly value: T | undefined;
	readonly error: Error | undefined;
	map<U>(fn: (value: T) => U): Result<U>;
	flatMap<U>(fn: (value: T) => Result<U>): Result<U>;
}`;

		console.log("🔍 Testing mathematical contract analysis...");
		const result = await testMathematicalContract("Result<T>", simpleContract);

		console.log("📊 Analysis Result:");
		console.log(`   Component: ${result.component}`);
		console.log(`   Passed: ${result.passed ? "✅" : "❌"}`);
		console.log(`   Duration: ${result.duration}ms`);
		console.log(`   Errors: ${result.errors.length}`);

		if (result.analysis) {
			console.log("📝 Analysis Preview:");
			console.log(`${result.analysis.substring(0, 200)}...`);
		}

		// Test law verification
		console.log("\n⚖️  Testing algebraic law verification...");
		const lawResult = await testLawVerification(simpleContract, "Monad", [
			"Identity law",
			"Associativity law",
		]);

		console.log("📊 Law Verification Result:");
		console.log(`   Structure: ${lawResult.component}`);
		console.log(`   Passed: ${lawResult.passed ? "✅" : "❌"}`);
		console.log(`   Duration: ${lawResult.duration}ms`);
	} catch (error) {
		console.log("❌ Mathematical analysis demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 4: Show wrapper re-exports
 */
async function demoWrapperReExports() {
	console.log("📦 Demo 4: Wrapper Re-exports");
	console.log("-----------------------------");

	console.log("🔍 Checking @qi/prompt re-exports:");

	try {
		// Import from our wrapper
		const { generateText, QiPrompt, CommonSchemas } = await import("@qi/prompt");

		console.log("✅ generateText function available:", typeof generateText === "function");
		console.log("✅ QiPrompt class available:", typeof QiPrompt === "function");
		console.log("✅ CommonSchemas available:", typeof CommonSchemas === "object");
		console.log("📋 Available schemas:", Object.keys(CommonSchemas));
	} catch (error) {
		console.log("❌ Failed to import from @qi/prompt:", error);
	}

	console.log("\n🔍 Checking @qi/mcp re-exports:");

	try {
		const { MCPClient, AnalysisFileManager } = await import("@qi/mcp");

		console.log("✅ MCPClient available:", typeof MCPClient === "function");
		console.log("✅ AnalysisFileManager available:", typeof AnalysisFileManager === "function");
	} catch (error) {
		console.log("❌ Failed to import from @qi/mcp:", error);
	}

	console.log("\n🔍 Checking @qi/agent re-exports:");

	try {
		const { createMathematicalWorkflow, createQiWorkflow } = await import("@qi/agent");

		console.log(
			"✅ createMathematicalWorkflow available:",
			typeof createMathematicalWorkflow === "function"
		);
		console.log("✅ createQiWorkflow available:", typeof createQiWorkflow === "function");
	} catch (error) {
		console.log("❌ Failed to import from @qi/agent:", error);
	}

	console.log("");
}

/**
 * Main demo execution
 */
async function runDemo() {
	console.log("🚀 Starting QiCore Wrapper Demonstration...\n");

	await demoPromptWrapper();
	await demoMCPWrapper();
	await demoMathematicalAnalysis();
	await demoWrapperReExports();

	console.log("🎉 Demo completed!");
	console.log("\n✨ Summary:");
	console.log("   • @qi/prompt: Vercel AI SDK wrapper with mathematical prompting");
	console.log("   • @qi/mcp: MCP SDK wrapper with client and tools");
	console.log("   • @qi/agent: AI Orchestra wrapper with workflow orchestration");
	console.log("   • All wrappers re-export their underlying libraries");
	console.log("   • All wrappers add domain-specific functionality");
}

// Run if this file is executed directly
if (import.meta.main) {
	runDemo().catch(console.error);
}
