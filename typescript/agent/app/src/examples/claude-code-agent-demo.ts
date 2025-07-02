#!/usr/bin/env bun

/**
 * Claude Code Agent Demo
 *
 * Demonstrates the @qi/agent wrapper with Claude Code SDK integration
 * for sophisticated AI-powered code generation and mathematical analysis
 */

import {
	ClaudeCode,
	createClaudeCodeContractWorkflow,
	createClaudeCodeMathematicalWorkflow,
	createHybridWorkflow,
	QiAgent,
	type QiWorkflowContext,
} from "@qi/agent";
import { ollama } from "ollama-ai-provider";

console.log("🤖 Claude Code Agent Demo");
console.log("=========================\n");

/**
 * Demo 1: Basic Claude Code Agent Usage
 */
async function demoBasicClaudeCodeAgent() {
	console.log("🧠 Demo 1: Basic Claude Code Agent");
	console.log("----------------------------------");

	try {
		// Create Claude Code agent
		const claudeAgent = ClaudeCode.createAgent({
			model: "claude-3-5-sonnet-20241022",
			temperature: 0.3,
			maxTokens: 1000,
		});

		console.log("✅ Created Claude Code agent");
		console.log("📊 Agent config:", claudeAgent.getConfig());

		// Generate mathematical analysis
		const analysisResult = await claudeAgent.generate({
			prompt: `Analyze this TypeScript algebraic data type for mathematical correctness:

interface Maybe<T> {
    readonly _tag: 'Some' | 'None';
    readonly value?: T;
    
    map<U>(f: (value: T) => U): Maybe<U>;
    flatMap<U>(f: (value: T) => Maybe<U>): Maybe<U>;
    filter(predicate: (value: T) => boolean): Maybe<T>;
    getOrElse(defaultValue: T): T;
}

Focus on:
1. Functor laws verification
2. Monad laws verification  
3. Implementation correctness
4. Type safety analysis`,
			systemPrompt: `You are a formal verification expert specializing in category theory and functional programming. Provide precise mathematical analysis.`,
		});

		if (analysisResult._tag === "Right") {
			const response = analysisResult.right;
			console.log("✅ Analysis completed");
			console.log("📊 Model:", response.model);
			console.log(
				"⚡ Tokens:",
				`${response.usage?.promptTokens}→${response.usage?.completionTokens}`
			);
			console.log("📝 Analysis preview:", `${response.content.substring(0, 300)}...`);
		} else {
			console.log("❌ Analysis failed:", analysisResult.left.message);
		}
	} catch (error) {
		console.log("❌ Basic Claude Code agent demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 2: Claude Code Streaming Generation
 */
async function demoClaudeCodeStreaming() {
	console.log("🌊 Demo 2: Claude Code Streaming");
	console.log("-------------------------------");

	try {
		const claudeAgent = ClaudeCode.createAgent({
			temperature: 0.4,
			maxTokens: 800,
		});

		const streamIterator = await claudeAgent.generateStream({
			prompt: `Provide a step-by-step proof that the List functor satisfies the functor laws:

1. Identity law: fmap(id) = id
2. Composition law: fmap(g ∘ f) = fmap(g) ∘ fmap(f)

Use TypeScript syntax for the List type and provide rigorous mathematical reasoning.`,
			systemPrompt: "You are a theorem prover. Provide clear, step-by-step mathematical proofs.",
		});

		console.log("📡 Starting streaming proof generation...");

		let content = "";
		let chunkCount = 0;

		for await (const result of streamIterator) {
			if (result._tag === "Right") {
				const chunk = result.right;
				content += chunk;
				chunkCount++;

				// Show progress every 10 chunks
				if (chunkCount % 10 === 0) {
					process.stdout.write(".");
				}
			} else {
				console.log("\n❌ Stream error:", result.left.message);
				break;
			}
		}

		console.log(`\n✅ Streaming completed (${chunkCount} chunks)`);
		console.log("📝 Proof preview:", `${content.substring(0, 400)}...`);
	} catch (error) {
		console.log("❌ Claude Code streaming demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 3: Claude Code Mathematical Workflow
 */
async function demoClaudeCodeMathematicalWorkflow() {
	console.log("🧮 Demo 3: Claude Code Mathematical Workflow");
	console.log("--------------------------------------------");

	try {
		// Create Claude Code powered mathematical workflow
		const workflow = createClaudeCodeMathematicalWorkflow({
			model: "claude-3-5-sonnet-20241022",
			temperature: 0.2,
			maxTokens: 1200,
		});

		console.log("⚙️  Created Claude Code mathematical workflow");

		const initialContext: QiWorkflowContext = {
			messages: [
				{
					role: "user",
					content: "Analyze the Either<L,R> type and verify it forms a valid Bifunctor",
				},
			],
			currentAgent: "researcher",
			workflowStep: "mathematical_analysis",
			data: {
				component: "Either<L,R>",
				analysisType: "bifunctor_verification",
				laws: ["bimap_identity", "bimap_composition"],
			},
			metadata: {
				startTime: Date.now(),
				stepHistory: [],
			},
		};

		console.log("🚀 Starting Claude Code mathematical workflow...");
		console.log(`📊 Component: ${initialContext.data.component}`);
		console.log(`🎯 Analysis: ${initialContext.data.analysisType}`);

		const _run = workflow.createRun({
			agent: "researcher",
			context: initialContext,
			onFinish: async (finalState) => {
				console.log("\n🎯 Claude Code mathematical workflow completed!");
				console.log(`📈 Final agent: ${finalState.agent}`);
				console.log(
					`⏱️  Duration: ${Date.now() - (finalState.context.metadata?.startTime || Date.now())}ms`
				);

				if (finalState.context.data.finalReport) {
					console.log(
						`📋 Report preview: ${finalState.context.data.finalReport.substring(0, 250)}...`
					);
				}
			},
		});

		console.log("✅ Claude Code workflow initiated");
	} catch (error) {
		console.log("❌ Claude Code mathematical workflow demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 4: Claude Code Contract Verification Workflow
 */
async function demoClaudeCodeContractWorkflow() {
	console.log("⚖️  Demo 4: Claude Code Contract Verification");
	console.log("--------------------------------------------");

	try {
		const contractWorkflow = createClaudeCodeContractWorkflow({
			model: "claude-3-5-sonnet-20241022",
			temperature: 0.1,
			maxTokens: 1000,
		});

		console.log("⚙️  Created Claude Code contract verification workflow");

		const contractContext: QiWorkflowContext = {
			messages: [
				{
					role: "user",
					content: `Verify this State monad implementation satisfies monad laws:

class State<S, A> {
    constructor(private runState: (state: S) => [A, S]) {}
    
    static of<S, A>(value: A): State<S, A> {
        return new State(state => [value, state]);
    }
    
    flatMap<B>(f: (value: A) => State<S, B>): State<S, B> {
        return new State(state => {
            const [a, newState] = this.runState(state);
            return f(a).runState(newState);
        });
    }
    
    map<B>(f: (value: A) => B): State<S, B> {
        return this.flatMap(a => State.of(f(a)));
    }
}`,
				},
			],
			currentAgent: "analyzer",
			data: {
				component: "State<S,A>",
				contractType: "monad",
				verificationTarget: "monad_laws",
			},
			metadata: {
				startTime: Date.now(),
				stepHistory: [],
			},
		};

		console.log("🚀 Starting Claude Code contract verification...");
		console.log(`📊 Contract: ${contractContext.data.component}`);
		console.log(`🎯 Target: ${contractContext.data.verificationTarget}`);

		const _contractRun = contractWorkflow.createRun({
			agent: "analyzer",
			context: contractContext,
			onFinish: async (finalState) => {
				console.log("\n🎯 Claude Code contract verification completed!");
				console.log(`📈 Final agent: ${finalState.agent}`);
				console.log(`✅ Verification status: ${finalState.context.data.verification || "pending"}`);

				if (finalState.context.data.report) {
					console.log(`📋 Report: ${finalState.context.data.report.substring(0, 200)}...`);
				}
			},
		});

		console.log("✅ Claude Code contract verification initiated");
	} catch (error) {
		console.log("❌ Claude Code contract verification demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 5: Hybrid Workflow (Claude Code + Ollama)
 */
async function demoHybridWorkflow() {
	console.log("🔀 Demo 5: Hybrid Workflow (Claude Code + Ollama)");
	console.log("------------------------------------------------");

	try {
		// Create hybrid workflow using Claude Code for analysis and Ollama for verification
		const hybridWorkflow = createHybridWorkflow({
			claudeConfig: {
				model: "claude-3-5-sonnet-20241022",
				temperature: 0.3,
			},
			otherModels: {
				// Use Ollama for verification (faster, local)
				verifier: ollama("qwen3:0.6b"),
				// Keep Claude Code for research and reporting
			},
		});

		console.log("⚙️  Created hybrid workflow (Claude Code + Ollama)");
		console.log("📊 Researcher: Claude Code");
		console.log("🔍 Verifier: Ollama qwen3:0.6b");
		console.log("📝 Reporter: Claude Code");

		const hybridContext: QiWorkflowContext = {
			messages: [
				{
					role: "user",
					content: "Analyze the IO monad and verify it preserves referential transparency",
				},
			],
			currentAgent: "researcher",
			data: {
				component: "IO<T>",
				analysisType: "referential_transparency",
				hybridMode: true,
			},
			metadata: {
				startTime: Date.now(),
				stepHistory: [],
				workflowType: "hybrid",
			},
		};

		console.log("🚀 Starting hybrid workflow...");
		console.log(`📊 Component: ${hybridContext.data.component}`);
		console.log(`🎯 Focus: ${hybridContext.data.analysisType}`);

		const _hybridRun = hybridWorkflow.createRun({
			agent: "researcher",
			context: hybridContext,
			onFinish: async (finalState) => {
				console.log("\n🎯 Hybrid workflow completed!");
				console.log(`📈 Workflow type: ${finalState.context.metadata?.workflowType}`);
				console.log(
					`⏱️  Total duration: ${Date.now() - (finalState.context.metadata?.startTime || Date.now())}ms`
				);
				console.log(`✅ Analysis complete: ${finalState.context.data.analysisComplete || false}`);
			},
		});

		console.log("✅ Hybrid workflow initiated");
	} catch (error) {
		console.log("❌ Hybrid workflow demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 6: QiAgent Factory Usage
 */
async function demoQiAgentFactory() {
	console.log("🏭 Demo 6: QiAgent Factory Usage");
	console.log("--------------------------------");

	try {
		console.log("🔧 Available QiAgent capabilities:");
		console.log("   • Core workflow creation");
		console.log("   • Mathematical workflows");
		console.log("   • Contract verification workflows");
		console.log("   • Claude Code powered workflows");
		console.log("   • Hybrid workflows");
		console.log("   • Individual agent access");

		// Demonstrate factory pattern usage
		console.log("\n📦 Creating agents through QiAgent factory:");

		// Create Claude Code agent through factory
		const _claudeAgent = QiAgent.ClaudeCode.createAgent({
			model: "claude-3-5-sonnet-20241022",
		});
		console.log("✅ Claude Code agent created via QiAgent.ClaudeCode.createAgent()");

		// Create Claude Code model for workflows
		const claudeModel = QiAgent.ClaudeCode.createModel({
			temperature: 0.4,
		});
		console.log("✅ Claude Code model created via QiAgent.ClaudeCode.createModel()");

		// Create mathematical workflow
		const _mathWorkflow = QiAgent.createMathematicalWorkflow({
			researcherModel: claudeModel,
			verifierModel: claudeModel,
			reporterModel: claudeModel,
		});
		console.log("✅ Mathematical workflow created via QiAgent.createMathematicalWorkflow()");

		// Create Claude Code specific workflow
		const _claudeWorkflow = QiAgent.createClaudeCodeMathematicalWorkflow();
		console.log(
			"✅ Claude Code workflow created via QiAgent.createClaudeCodeMathematicalWorkflow()"
		);

		console.log("\n🎯 QiAgent factory provides unified access to:");
		console.log("   • Individual AI agents (Claude Code, OpenAI, Ollama)");
		console.log("   • Multi-agent workflows (AI Orchestra)");
		console.log("   • Specialized mathematical analysis patterns");
		console.log("   • Hybrid model combinations");
	} catch (error) {
		console.log("❌ QiAgent factory demo failed:", error);
	}

	console.log("");
}

/**
 * Main demo execution
 */
async function runClaudeCodeAgentDemo() {
	console.log("🚀 Starting Claude Code Agent Demonstration...\n");

	await demoBasicClaudeCodeAgent();
	await demoClaudeCodeStreaming();
	await demoClaudeCodeMathematicalWorkflow();
	await demoClaudeCodeContractWorkflow();
	await demoHybridWorkflow();
	await demoQiAgentFactory();

	console.log("🎉 Claude Code Agent Demo completed!");
	console.log("\n✨ Summary:");
	console.log("   • Basic agent: Direct Claude Code SDK integration");
	console.log("   • Streaming: Real-time response generation");
	console.log("   • Mathematical workflows: Multi-agent mathematical analysis");
	console.log("   • Contract verification: Automated formal verification");
	console.log("   • Hybrid workflows: Claude Code + Ollama combinations");
	console.log("   • Factory pattern: Unified agent and workflow creation");
	console.log("   • Full integration: Seamless @qi/agent ecosystem");
}

// Run if this file is executed directly
if (import.meta.main) {
	runClaudeCodeAgentDemo().catch(console.error);
}
