#!/usr/bin/env bun

/**
 * AI Orchestra Workflow Demo
 *
 * Demonstrates the @qi/agent wrapper for AI Orchestra workflows
 */

import type { Dispatch, HandlerResult } from "@qi/agent";
import { createQiWorkflow, type QiAgentHandler, type QiWorkflowContext } from "@qi/agent";

console.log("🎭 AI Orchestra Workflow Demo");
console.log("============================\n");

/**
 * Demo 1: Simple Sequential Workflow
 */
async function demoSimpleWorkflow() {
	console.log("🔄 Demo 1: Simple Sequential Workflow");
	console.log("------------------------------------");

	try {
		// Create simple handlers for demonstration
		const handlers: Record<string, QiAgentHandler> = {
			analyzer: async (
				context: QiWorkflowContext,
				dispatch: Dispatch
			): Promise<HandlerResult<"analyzer" | "reporter", QiWorkflowContext>> => {
				console.log("🔍 Analyzer: Processing input...");

				// Simulate analysis work
				await new Promise((resolve) => setTimeout(resolve, 100));

				await dispatch("analysis_started", { component: "demo" });

				return {
					nextState: "reporter",
					context: {
						...context,
						data: { ...context.data, analysis: "completed" },
					},
				};
			},

			reporter: async (
				context: QiWorkflowContext,
				dispatch: Dispatch
			): Promise<HandlerResult<"analyzer" | "reporter", QiWorkflowContext>> => {
				console.log("📊 Reporter: Generating report...");

				// Simulate report generation
				await new Promise((resolve) => setTimeout(resolve, 50));

				await dispatch("report_generated", {
					analysis: context.data.analysis,
					timestamp: new Date().toISOString(),
				});

				return {
					context: {
						...context,
						data: { ...context.data, report: "completed" },
					},
				};
			},
		};

		// Create the workflow
		console.log("⚙️  Creating workflow orchestrator...");
		const workflow = createQiWorkflow(handlers);

		console.log("✅ Workflow created successfully");
		console.log("🎯 Available handlers: analyzer, reporter");

		// Create a workflow run
		const initialContext: QiWorkflowContext = {
			messages: [{ role: "user", content: "Analyze this mathematical structure" }],
			currentAgent: "analyzer",
			data: {},
			metadata: {
				startTime: Date.now(),
				stepHistory: [],
			},
		};

		console.log("🚀 Starting workflow run...");
		const _run = workflow.createRun({
			agent: "analyzer",
			context: initialContext,
			onFinish: async (finalState) => {
				console.log(`✅ Workflow completed in agent: ${finalState.agent}`);
				console.log(`📊 Final context:`, finalState.context.data);
			},
		});

		console.log("🎊 Workflow run created successfully");
		console.log("📋 Run object available for streaming");
	} catch (error) {
		console.log("❌ Simple workflow demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 2: Mathematical Workflow (showing the wrapper structure)
 */
async function demoMathematicalWorkflow() {
	console.log("🧮 Demo 2: Mathematical Workflow Structure");
	console.log("-----------------------------------------");

	try {
		// Note: We can't fully execute this without actual LLM models,
		// but we can show the structure and creation

		console.log("⚙️  Attempting to create mathematical workflow...");
		console.log("📝 Note: This requires actual LLM models to execute");

		// This would normally require actual models:
		// const workflow = createMathematicalWorkflow({
		//     researcherModel: ollama('qwen3:0.6b'),
		//     verifierModel: ollama('qwen3:0.6b'),
		//     reporterModel: ollama('qwen3:0.6b'),
		// });

		console.log("🏗️  Mathematical workflow structure:");
		console.log("   • researcher: Analyzes mathematical concepts");
		console.log("   • verifier: Verifies mathematical properties");
		console.log("   • reporter: Generates analysis summaries");
		console.log("   • Uses AI Orchestra state machine orchestration");
		console.log("   • Integrates with Vercel AI SDK for LLM calls");
	} catch (error) {
		console.log("❌ Mathematical workflow demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 3: Show AI Orchestra Integration
 */
async function demoOrchestraIntegration() {
	console.log("🎼 Demo 3: AI Orchestra Integration");
	console.log("----------------------------------");

	try {
		console.log("🔍 Checking AI Orchestra re-exports from @qi/agent:");

		// Import directly to check what's available
		const { createOrchestra, processStream, orchestraToAIStream } = await import("@qi/agent");

		console.log("✅ createOrchestra available:", typeof createOrchestra === "function");
		console.log("✅ processStream available:", typeof processStream === "function");
		console.log("✅ orchestraToAIStream available:", typeof orchestraToAIStream === "function");

		console.log("\n🎯 QiAgent wrapper provides:");
		console.log("   • Complete AI Orchestra re-export");
		console.log("   • QiWorkflowContext for enhanced context management");
		console.log("   • Mathematical workflow patterns");
		console.log("   • Integration with @qi/prompt and @qi/mcp");
		console.log("   • Streaming support for real-time workflow progress");
	} catch (error) {
		console.log("❌ Orchestra integration demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 4: Workflow Context Management
 */
async function demoContextManagement() {
	console.log("🗂️  Demo 4: Workflow Context Management");
	console.log("-------------------------------------");

	try {
		// Demonstrate the enhanced context structure
		const sampleContext: QiWorkflowContext = {
			messages: [
				{ role: "user", content: "Analyze this algebraic structure" },
				{ role: "assistant", content: "I'll analyze the mathematical properties..." },
			],
			currentAgent: "researcher",
			workflowStep: "analysis",
			data: {
				component: "Result<T>",
				analysisType: "monad",
				completenessScore: 85,
				findings: ["Functor laws satisfied", "Monad laws verified"],
			},
			metadata: {
				startTime: Date.now(),
				stepHistory: ["researcher", "verifier"],
				performance: {
					averageStepTime: 150,
					totalSteps: 2,
				},
			},
		};

		console.log("📊 Sample QiWorkflowContext structure:");
		console.log("   messages:", sampleContext.messages.length, "messages");
		console.log("   currentAgent:", sampleContext.currentAgent);
		console.log("   workflowStep:", sampleContext.workflowStep);
		console.log("   data keys:", Object.keys(sampleContext.data || {}));
		console.log("   metadata:", sampleContext.metadata);

		console.log("\n✨ Context features:");
		console.log("   • Preserves conversation history");
		console.log("   • Tracks current workflow state");
		console.log("   • Maintains analysis data across agents");
		console.log("   • Records performance metrics");
		console.log("   • Supports custom metadata");
	} catch (error) {
		console.log("❌ Context management demo failed:", error);
	}

	console.log("");
}

/**
 * Main demo execution
 */
async function runAIOrchestraDemo() {
	console.log("🚀 Starting AI Orchestra Wrapper Demonstration...\n");

	await demoSimpleWorkflow();
	await demoMathematicalWorkflow();
	await demoOrchestraIntegration();
	await demoContextManagement();

	console.log("🎉 AI Orchestra Demo completed!");
	console.log("\n✨ Summary:");
	console.log("   • @qi/agent provides complete AI Orchestra wrapper");
	console.log("   • Supports multi-agent workflow orchestration");
	console.log("   • Integrates with Vercel AI SDK for LLM calls");
	console.log("   • Provides enhanced context management");
	console.log("   • Includes mathematical analysis workflow patterns");
	console.log("   • Supports real-time streaming and progress tracking");
}

// Run if this file is executed directly
if (import.meta.main) {
	runAIOrchestraDemo().catch(console.error);
}
