#!/usr/bin/env bun

/**
 * QiAgent + Ollama Demo
 *
 * Demonstrates the @qi/agent wrapper with AI Orchestra and Ollama
 * for sophisticated multi-agent mathematical workflows
 */

import {
	createMathematicalWorkflow,
	createOrchestra,
	createQiWorkflow,
	type Dispatch,
	type HandlerResult,
	processStream,
	type QiAgentHandler,
	type QiWorkflowContext,
} from "@qi/agent";
import { streamText } from "@qi/prompt";
import { ollama } from "ollama-ai-provider";

console.log("🎭 QiAgent + Ollama Demo");
console.log("========================\n");

/**
 * Demo 1: Basic Agent Workflow with AI Orchestra
 */
async function demoBasicAgentWorkflow() {
	console.log("🤖 Demo 1: Basic Agent Workflow");
	console.log("-------------------------------");

	try {
		// Define simple agents for mathematical analysis
		const handlers: Record<string, QiAgentHandler> = {
			analyzer: async (
				context: QiWorkflowContext,
				dispatch: Dispatch
			): Promise<HandlerResult<"analyzer" | "reviewer" | "reporter", QiWorkflowContext>> => {
				console.log("🔍 Analyzer: Starting mathematical analysis...");

				await dispatch("analysis_started", {
					agent: "analyzer",
					component: context.data.component || "unknown",
				});

				// Use Ollama for analysis
				const stream = streamText({
					model: ollama("qwen3:0.6b"),
					system: "You are a mathematical analysis expert. Provide concise, technical analysis.",
					prompt: `Analyze the mathematical properties of: ${context.data.component || "mathematical structure"}

Focus on:
1. Categorical structure (Functor, Monad, etc.)
2. Algebraic properties
3. Verification of mathematical laws`,
					temperature: 0.3,
					maxTokens: 400,
				});

				// Process the stream
				const result = await processStream(stream, dispatch);

				console.log("✅ Analysis completed");

				return {
					nextState: "reviewer",
					context: {
						...context,
						data: {
							...context.data,
							analysis: result.text,
							analysisComplete: true,
						},
					},
				};
			},

			reviewer: async (
				context: QiWorkflowContext,
				dispatch: Dispatch
			): Promise<HandlerResult<"analyzer" | "reviewer" | "reporter", QiWorkflowContext>> => {
				console.log("🔎 Reviewer: Validating analysis...");

				await dispatch("review_started", {
					agent: "reviewer",
					analysisLength: context.data.analysis?.length || 0,
				});

				// Review the analysis
				const stream = streamText({
					model: ollama("qwen3:0.6b"),
					system:
						"You are a mathematical verification expert. Review analysis for correctness and completeness.",
					prompt: `Review this mathematical analysis for accuracy:

${context.data.analysis || "No analysis provided"}

Provide:
1. Verification of mathematical claims
2. Identification of any errors or gaps
3. Suggestions for improvement
4. Confidence score (0-100)`,
					temperature: 0.2,
					maxTokens: 300,
				});

				const result = await processStream(stream, dispatch);

				console.log("✅ Review completed");

				return {
					nextState: "reporter",
					context: {
						...context,
						data: {
							...context.data,
							review: result.text,
							reviewComplete: true,
						},
					},
				};
			},

			reporter: async (
				context: QiWorkflowContext,
				dispatch: Dispatch
			): Promise<HandlerResult<"analyzer" | "reviewer" | "reporter", QiWorkflowContext>> => {
				console.log("📊 Reporter: Generating final report...");

				await dispatch("report_started", {
					agent: "reporter",
					dataComplete: context.data.analysisComplete && context.data.reviewComplete,
				});

				// Generate comprehensive report
				const stream = streamText({
					model: ollama("qwen3:0.6b"),
					system:
						"You are a technical report writer. Create clear, structured reports from mathematical analysis.",
					prompt: `Create a comprehensive report based on:

ANALYSIS: ${context.data.analysis || "No analysis"}

REVIEW: ${context.data.review || "No review"}

Structure the report with:
1. Executive Summary
2. Mathematical Findings
3. Verification Results
4. Recommendations
5. Conclusion`,
					temperature: 0.4,
					maxTokens: 500,
				});

				const result = await processStream(stream, dispatch);

				console.log("✅ Report generated");

				return {
					context: {
						...context,
						data: {
							...context.data,
							finalReport: result.text,
							workflowComplete: true,
						},
					},
				};
			},
		};

		// Create the workflow
		console.log("⚙️  Creating mathematical analysis workflow...");
		const workflow = createQiWorkflow(handlers);

		// Initial context
		const initialContext: QiWorkflowContext = {
			messages: [{ role: "user", content: "Analyze the Result<T> monad implementation" }],
			currentAgent: "analyzer",
			data: {
				component: "Result<T>",
				timestamp: new Date().toISOString(),
			},
			metadata: {
				startTime: Date.now(),
				stepHistory: [],
			},
		};

		// Create and run the workflow
		console.log("🚀 Starting workflow execution...");

		const _run = workflow.createRun({
			agent: "analyzer",
			context: initialContext,
			onFinish: async (finalState) => {
				console.log(`\n🎯 Workflow completed!`);
				console.log(`📊 Final agent: ${finalState.agent}`);
				console.log(`📈 Steps executed: ${finalState.context.metadata?.stepHistory?.length || 0}`);
				console.log(
					`⏱️  Total time: ${Date.now() - (finalState.context.metadata?.startTime || Date.now())}ms`
				);

				if (finalState.context.data.finalReport) {
					console.log(
						`📋 Report preview: ${finalState.context.data.finalReport.substring(0, 200)}...`
					);
				}
			},
		});

		console.log("✅ Workflow created and initiated");
	} catch (error) {
		console.log("❌ Basic workflow demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 2: Advanced Mathematical Workflow
 */
async function demoMathematicalWorkflow() {
	console.log("🧮 Demo 2: Advanced Mathematical Workflow");
	console.log("----------------------------------------");

	try {
		// Create a sophisticated mathematical analysis workflow
		console.log("🏗️  Creating advanced mathematical workflow...");

		// Note: This demonstrates the structure - full execution requires actual models
		const _mathematicalWorkflow = createMathematicalWorkflow({
			researcherModel: ollama("qwen3:0.6b"),
			verifierModel: ollama("qwen3:0.6b"),
			reporterModel: ollama("qwen3:0.6b"),
		});

		console.log("✅ Mathematical workflow created");
		console.log("🎯 Workflow capabilities:");
		console.log("   • Researcher: Deep mathematical analysis");
		console.log("   • Verifier: Formal verification of properties");
		console.log("   • Reporter: Comprehensive documentation");
		console.log("   • Streaming: Real-time progress updates");
		console.log("   • Context: Persistent mathematical context");

		// Demonstrate workflow structure
		const workflowContext: QiWorkflowContext = {
			messages: [
				{
					role: "user",
					content:
						"Analyze the mathematical properties of the Either<L,R> type and verify it satisfies the Monad laws",
				},
			],
			currentAgent: "researcher",
			workflowStep: "initial_analysis",
			data: {
				component: "Either<L,R>",
				analysisType: "category_theory",
				complexity: "advanced",
				laws: ["left_identity", "right_identity", "associativity"],
			},
			metadata: {
				startTime: Date.now(),
				stepHistory: [],
				requiresVerification: true,
			},
		};

		console.log("📊 Workflow context prepared:");
		console.log(`   Component: ${workflowContext.data.component}`);
		console.log(`   Analysis type: ${workflowContext.data.analysisType}`);
		console.log(`   Laws to verify: ${workflowContext.data.laws?.join(", ")}`);
	} catch (error) {
		console.log("❌ Mathematical workflow demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 3: Streaming Multi-Agent Workflow
 */
async function demoStreamingWorkflow() {
	console.log("🌊 Demo 3: Streaming Multi-Agent Workflow");
	console.log("----------------------------------------");

	try {
		// Create a streaming workflow with real-time updates
		const streamingHandlers: Record<string, QiAgentHandler> = {
			researcher: async (
				context: QiWorkflowContext,
				dispatch: Dispatch
			): Promise<HandlerResult<"researcher" | "synthesizer", QiWorkflowContext>> => {
				console.log("🔬 Researcher: Beginning investigation...");

				// Streaming research with progress updates
				const researchStream = streamText({
					model: ollama("qwen3:0.6b"),
					system:
						"You are a mathematical researcher. Investigate the given topic thoroughly with streaming updates.",
					prompt: `Research the mathematical concept: ${context.data.topic || "Category Theory"}

Provide streaming insights as you research:
1. Historical context
2. Mathematical foundations  
3. Key theorems and properties
4. Modern applications
5. Open problems`,
					temperature: 0.5,
					maxTokens: 600,
				});

				console.log("📡 Starting streaming research...");

				let chunkCount = 0;
				let researchContent = "";

				// Process stream with real-time dispatch
				for await (const chunk of researchStream) {
					researchContent += chunk;
					chunkCount++;

					// Dispatch progress every 5 chunks
					if (chunkCount % 5 === 0) {
						await dispatch("research_progress", {
							agent: "researcher",
							chunks: chunkCount,
							contentLength: researchContent.length,
							preview: `${researchContent.substring(0, 100)}...`,
						});
						process.stdout.write(".");
					}
				}

				console.log(`\n✅ Research completed (${chunkCount} chunks)`);

				return {
					nextState: "synthesizer",
					context: {
						...context,
						data: {
							...context.data,
							research: researchContent,
							researchChunks: chunkCount,
						},
					},
				};
			},

			synthesizer: async (
				context: QiWorkflowContext,
				dispatch: Dispatch
			): Promise<HandlerResult<"researcher" | "synthesizer", QiWorkflowContext>> => {
				console.log("🧬 Synthesizer: Creating synthesis...");

				await dispatch("synthesis_started", {
					agent: "synthesizer",
					inputLength: context.data.research?.length || 0,
				});

				// Synthesize the research into actionable insights
				const synthesisStream = streamText({
					model: ollama("qwen3:0.6b"),
					system:
						"You are a mathematical synthesizer. Create clear, actionable insights from research.",
					prompt: `Synthesize this mathematical research into key insights:

${context.data.research || "No research provided"}

Create:
1. Key mathematical insights (3-5 points)
2. Practical applications
3. Learning recommendations
4. Further research directions`,
					temperature: 0.3,
					maxTokens: 400,
				});

				const synthesisResult = await processStream(synthesisStream, dispatch);

				console.log("✅ Synthesis completed");

				return {
					context: {
						...context,
						data: {
							...context.data,
							synthesis: synthesisResult.text,
							workflowComplete: true,
						},
					},
				};
			},
		};

		// Create streaming workflow
		const streamingWorkflow = createQiWorkflow(streamingHandlers);

		const streamingContext: QiWorkflowContext = {
			messages: [
				{ role: "user", content: "Research and synthesize insights about Monad Transformers" },
			],
			currentAgent: "researcher",
			data: {
				topic: "Monad Transformers",
				researchDepth: "comprehensive",
			},
			metadata: {
				startTime: Date.now(),
				stepHistory: [],
				streamingEnabled: true,
			},
		};

		console.log("🚀 Starting streaming workflow...");

		const _streamingRun = streamingWorkflow.createRun({
			agent: "researcher",
			context: streamingContext,
			onFinish: async (finalState) => {
				console.log("\n🎊 Streaming workflow completed!");
				console.log(`📊 Research chunks: ${finalState.context.data.researchChunks || 0}`);
				console.log(
					`📝 Synthesis preview: ${finalState.context.data.synthesis?.substring(0, 150) || "None"}...`
				);
			},
		});

		console.log("✅ Streaming workflow initiated");
	} catch (error) {
		console.log("❌ Streaming workflow demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 4: AI Orchestra Core Integration
 */
async function demoOrchestraCore() {
	console.log("🎼 Demo 4: AI Orchestra Core Integration");
	console.log("---------------------------------------");

	try {
		// Demonstrate direct AI Orchestra usage through @qi/agent
		console.log("🔧 Creating AI Orchestra instance...");

		const orchestra = createOrchestra<{
			topic: string;
			analysis: string;
			verified: boolean;
		}>()({
			analyzer: async (context, dispatch) => {
				console.log("🔍 Orchestra Analyzer: Processing...");

				await dispatch("analysis", {
					type: "mathematical_analysis",
					topic: context.topic,
				});

				// Simulate analysis
				const analysisResult = `Mathematical analysis of ${context.topic}: This demonstrates category theory principles...`;

				return {
					nextState: "verifier",
					context: {
						...context,
						analysis: analysisResult,
					},
				};
			},

			verifier: async (context, dispatch) => {
				console.log("✅ Orchestra Verifier: Validating...");

				await dispatch("verification", {
					type: "mathematical_verification",
					analysisLength: context.analysis.length,
				});

				// Simulate verification
				const verified = context.analysis.length > 50; // Simple check

				return {
					context: {
						...context,
						verified,
					},
				};
			},
		});

		console.log("✅ Orchestra created successfully");

		// Create a run
		const _orchestraRun = orchestra.createRun({
			agent: "analyzer",
			context: {
				topic: "Applicative Functors",
				analysis: "",
				verified: false,
			},
			onFinish: async (finalState) => {
				console.log("🎯 Orchestra workflow completed!");
				console.log(`📊 Topic: ${finalState.context.topic}`);
				console.log(`✅ Verified: ${finalState.context.verified}`);
				console.log(`📝 Analysis: ${finalState.context.analysis.substring(0, 100)}...`);
			},
		});

		console.log("🎊 Orchestra run created and ready");

		// Demonstrate orchestraToAIStream capability
		console.log("\n📡 Stream conversion capability available");
		console.log("🔄 Can convert orchestra runs to AI streams for web integration");
	} catch (error) {
		console.log("❌ Orchestra core demo failed:", error);
	}

	console.log("");
}

/**
 * Demo 5: Complex Mathematical Reasoning Chain
 */
async function demoComplexReasoning() {
	console.log("🧠 Demo 5: Complex Mathematical Reasoning Chain");
	console.log("----------------------------------------------");

	try {
		// Create a complex reasoning workflow
		const reasoningHandlers: Record<string, QiAgentHandler> = {
			problem_analyzer: async (
				context: QiWorkflowContext,
				dispatch: Dispatch
			): Promise<
				HandlerResult<
					"problem_analyzer" | "theorem_prover" | "solution_validator" | "result_formatter",
					QiWorkflowContext
				>
			> => {
				console.log("🔍 Problem Analyzer: Breaking down the problem...");

				const problemAnalysis = await streamText({
					model: ollama("qwen3:0.6b"),
					system:
						"You are a mathematical problem analyzer. Break down complex problems into manageable components.",
					prompt: `Analyze this mathematical problem and break it into components:

Problem: ${context.data.problem || "Verify that the composition of two functors is also a functor"}

Provide:
1. Problem decomposition
2. Required mathematical concepts
3. Proof strategy
4. Expected complexity`,
					temperature: 0.2,
					maxTokens: 300,
				});

				const analysisResult = await processStream(problemAnalysis, dispatch);

				return {
					nextState: "theorem_prover",
					context: {
						...context,
						data: {
							...context.data,
							problemAnalysis: analysisResult.text,
						},
					},
				};
			},

			theorem_prover: async (
				context: QiWorkflowContext,
				dispatch: Dispatch
			): Promise<
				HandlerResult<
					"problem_analyzer" | "theorem_prover" | "solution_validator" | "result_formatter",
					QiWorkflowContext
				>
			> => {
				console.log("🎓 Theorem Prover: Constructing mathematical proof...");

				const proofConstruction = await streamText({
					model: ollama("qwen3:0.6b"),
					system: "You are a theorem prover. Construct rigorous mathematical proofs step by step.",
					prompt: `Based on this problem analysis, construct a mathematical proof:

${context.data.problemAnalysis || "No analysis provided"}

Provide a step-by-step proof with mathematical rigor.`,
					temperature: 0.1,
					maxTokens: 500,
				});

				const proofResult = await processStream(proofConstruction, dispatch);

				return {
					nextState: "solution_validator",
					context: {
						...context,
						data: {
							...context.data,
							proof: proofResult.text,
						},
					},
				};
			},

			solution_validator: async (
				context: QiWorkflowContext,
				dispatch: Dispatch
			): Promise<
				HandlerResult<
					"problem_analyzer" | "theorem_prover" | "solution_validator" | "result_formatter",
					QiWorkflowContext
				>
			> => {
				console.log("🔎 Solution Validator: Verifying proof correctness...");

				const validation = await streamText({
					model: ollama("qwen3:0.6b"),
					system:
						"You are a mathematical proof validator. Check proofs for logical consistency and completeness.",
					prompt: `Validate this mathematical proof:

${context.data.proof || "No proof provided"}

Check for:
1. Logical consistency
2. Completeness of argument
3. Mathematical accuracy
4. Clarity of presentation`,
					temperature: 0.1,
					maxTokens: 300,
				});

				const validationResult = await processStream(validation, dispatch);

				return {
					nextState: "result_formatter",
					context: {
						...context,
						data: {
							...context.data,
							validation: validationResult.text,
						},
					},
				};
			},

			result_formatter: async (
				context: QiWorkflowContext,
				dispatch: Dispatch
			): Promise<
				HandlerResult<
					"problem_analyzer" | "theorem_prover" | "solution_validator" | "result_formatter",
					QiWorkflowContext
				>
			> => {
				console.log("📋 Result Formatter: Creating final presentation...");

				const formatting = await streamText({
					model: ollama("qwen3:0.6b"),
					system:
						"You are a mathematical document formatter. Create clear, professional presentations of mathematical results.",
					prompt: `Format these mathematical results into a professional presentation:

PROBLEM ANALYSIS: ${context.data.problemAnalysis || ""}

PROOF: ${context.data.proof || ""}

VALIDATION: ${context.data.validation || ""}

Create a well-structured mathematical document.`,
					temperature: 0.3,
					maxTokens: 400,
				});

				const formattedResult = await processStream(formatting, dispatch);

				return {
					context: {
						...context,
						data: {
							...context.data,
							finalResult: formattedResult.text,
							completed: true,
						},
					},
				};
			},
		};

		// Create the complex reasoning workflow
		const reasoningWorkflow = createQiWorkflow(reasoningHandlers);

		const reasoningContext: QiWorkflowContext = {
			messages: [
				{ role: "user", content: "Prove that the composition of two functors is also a functor" },
			],
			currentAgent: "problem_analyzer",
			data: {
				problem: "Prove that the composition of two functors is also a functor",
				complexity: "advanced",
				domain: "category_theory",
			},
			metadata: {
				startTime: Date.now(),
				stepHistory: [],
				requiresProof: true,
			},
		};

		console.log("🚀 Starting complex reasoning workflow...");
		console.log(`📊 Problem: ${reasoningContext.data.problem}`);

		const _reasoningRun = reasoningWorkflow.createRun({
			agent: "problem_analyzer",
			context: reasoningContext,
			onFinish: async (finalState) => {
				console.log("\n🎯 Complex reasoning completed!");
				console.log(`📈 Workflow steps: ${finalState.context.metadata?.stepHistory?.length || 4}`);
				console.log(
					`⏱️  Total time: ${Date.now() - (finalState.context.metadata?.startTime || Date.now())}ms`
				);
				console.log(`✅ Completed: ${finalState.context.data.completed || false}`);

				if (finalState.context.data.finalResult) {
					console.log(
						`📋 Result preview: ${finalState.context.data.finalResult.substring(0, 200)}...`
					);
				}
			},
		});

		console.log("✅ Complex reasoning workflow initiated");
	} catch (error) {
		console.log("❌ Complex reasoning demo failed:", error);
	}

	console.log("");
}

/**
 * Main demo execution
 */
async function runQiAgentOllamaDemo() {
	console.log("🚀 Starting QiAgent + Ollama Demonstration...\n");

	await demoBasicAgentWorkflow();
	await demoMathematicalWorkflow();
	await demoStreamingWorkflow();
	await demoOrchestraCore();
	await demoComplexReasoning();

	console.log("🎉 QiAgent + Ollama Demo completed!");
	console.log("\n✨ Summary:");
	console.log("   • Basic workflows: Multi-agent mathematical analysis");
	console.log("   • Mathematical workflows: Specialized category theory analysis");
	console.log("   • Streaming workflows: Real-time progress and updates");
	console.log("   • Orchestra core: Direct AI Orchestra integration");
	console.log("   • Complex reasoning: Multi-step theorem proving");
	console.log("   • Full AI Orchestra integration through @qi/agent wrapper");
	console.log("   • Seamless Ollama integration for local LLM processing");
}

// Run if this file is executed directly
if (import.meta.main) {
	runQiAgentOllamaDemo().catch(console.error);
}
