#!/usr/bin/env bun

/**
 * Claude Code Quickstart Demo
 *
 * A simple 5-minute introduction to getting started with Claude Code SDK
 * via the @qi/agent unified framework.
 *
 * Perfect for developers new to the QiCore ecosystem.
 */

import { ClaudeCode } from "@qi/agent";

console.log("🚀 Claude Code Quickstart - 5 Minutes to AI Power");
console.log("================================================");

/**
 * Step 1: Create your first Claude Code agent
 */
async function step1_CreateAgent() {
	console.log("\n📦 Step 1: Create Claude Code Agent");
	console.log("-----------------------------------");

	// Simplest possible agent creation
	const result = ClaudeCode.createAgent();

	if (result._tag === "Right") {
		const agent = result.right;
		console.log("✅ Agent created successfully!");
		console.log("📊 Config:", agent.getConfig());
		return agent;
	} else {
		console.log("❌ Failed to create agent:", result.left.message);
		console.log("💡 Make sure ANTHROPIC_API_KEY is set in your environment");
		return null;
	}
}

/**
 * Step 2: Generate your first response
 */
async function step2_FirstGeneration(agent: unknown) {
	console.log("\n💬 Step 2: Generate Your First Response");
	console.log("---------------------------------------");

	const result = await agent.generate({
		prompt: "Hello! Can you explain what you are and what you can help with in 2-3 sentences?",
	});

	if (result._tag === "Right") {
		const response = result.right;
		console.log("✅ Generation successful!");
		console.log("📝 Claude says:");
		console.log(`"${response.content}"`);
		console.log(`📊 Used ${response.usage?.totalTokens} tokens`);
	} else {
		console.log("❌ Generation failed:", result.left.message);
	}
}

/**
 * Step 3: Ask Claude to help with code
 */
async function step3_CodeHelp(agent: unknown) {
	console.log("\n🔧 Step 3: Get Help with Code");
	console.log("-----------------------------");

	const result = await agent.generate({
		prompt: `I have this TypeScript function and I'm not sure if it's correct:

function fibonacci(n: number): number {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

Can you review it and suggest improvements?`,
		systemPrompt: "You are a helpful coding assistant. Be concise but thorough.",
	});

	if (result._tag === "Right") {
		const response = result.right;
		console.log("✅ Code review completed!");
		console.log("📝 Claude's feedback:");
		console.log(`"${response.content.substring(0, 400)}..."`);
	} else {
		console.log("❌ Code review failed:", result.left.message);
	}
}

/**
 * Step 4: Try streaming for real-time responses
 */
async function step4_StreamingExample(agent: unknown) {
	console.log("\n🌊 Step 4: Streaming Responses");
	console.log("------------------------------");

	try {
		const streamIterator = await agent.generateStream({
			prompt:
				"Count from 1 to 5, explaining what each number represents in computer science (like binary, etc.). Keep each explanation brief.",
		});

		console.log("📡 Streaming response in real-time:");
		console.log("---");

		let fullResponse = "";
		for await (const result of streamIterator) {
			if (result._tag === "Right") {
				const chunk = result.right;
				process.stdout.write(chunk);
				fullResponse += chunk;
			} else {
				console.log("\n❌ Stream error:", result.left.message);
				break;
			}
		}

		console.log("\n---");
		console.log("✅ Streaming completed!");
		console.log(`📊 Total response length: ${fullResponse.length} characters`);
	} catch (error) {
		console.log("❌ Streaming example failed:", error);
	}
}

/**
 * Step 5: What's next?
 */
function step5_WhatsNext() {
	console.log("\n🎯 Step 5: What's Next?");
	console.log("-----------------------");

	console.log("🎉 Congratulations! You've mastered Claude Code basics.");
	console.log("\n📚 Ready for more? Check out these advanced features:");
	console.log("   • claude-code-agent-demo.ts - Full featured workflows");
	console.log("   • Mathematical analysis workflows");
	console.log("   • Multi-agent orchestration with AI Orchestra");
	console.log("   • Hybrid workflows combining Claude + local models");

	console.log("\n🔧 Available through @qi/agent:");
	console.log("   • ClaudeCode.createAgent() - Individual agents");
	console.log("   • createClaudeCodeMathematicalWorkflow() - Mathematical workflows");
	console.log("   • createHybridWorkflow() - Combine multiple AI models");
	console.log("   • QiAgent.ClaudeCode.* - Factory pattern access");

	console.log("\n💡 Pro Tips:");
	console.log("   • Set ANTHROPIC_API_KEY in your environment");
	console.log("   • Use Result<T> pattern for robust error handling");
	console.log("   • Explore streaming for real-time applications");
	console.log("   • Try different temperature values (0.1-1.0) for creativity control");
}

/**
 * Main quickstart execution
 */
async function runQuickstart() {
	console.log("\n🌟 Starting your Claude Code journey...");

	// Step 1: Create agent
	const agent = await step1_CreateAgent();
	if (!agent) {
		console.log("\n❌ Cannot continue without a valid agent. Please check your setup.");
		return;
	}

	// Step 2: First generation
	await step2_FirstGeneration(agent);

	// Step 3: Code help
	await step3_CodeHelp(agent);

	// Step 4: Streaming
	await step4_StreamingExample(agent);

	// Step 5: What's next
	step5_WhatsNext();

	console.log("\n🎊 Quickstart complete! You're ready to build amazing things with Claude Code.");
}

// Run if this file is executed directly
if (import.meta.main) {
	runQuickstart().catch(console.error);
}

export { runQuickstart };
