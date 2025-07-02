#!/usr/bin/env bun

/**
 * QiPrompt + Ollama Demo
 *
 * Demonstrates the @qi/prompt wrapper with Ollama for mathematical analysis
 * Based on Vercel AI SDK best practices with local LLM integration
 */

import { generateObject, generateText, streamText } from "@qi/prompt";
import { ollama } from "ollama-ai-provider";
import { z } from "zod";

console.log("🦙 QiPrompt + Ollama Demo");
console.log("=========================\n");

// Check if Ollama is available
async function checkOllamaConnection() {
	try {
		const response = await fetch("http://localhost:11434/api/tags");
		if (response.ok) {
			const data = await response.json();
			console.log("✅ Ollama connection successful");
			console.log(
				`📋 Available models: ${data.models?.map((m: { name: string }) => m.name).join(", ") || "none"}`
			);
			return data.models || [];
		}
	} catch (_error) {
		console.log("❌ Ollama not available at localhost:11434");
		console.log(
			"💡 Please start Ollama: `ollama serve` and pull a model: `ollama pull qwen3:0.6b`"
		);
		return [];
	}
	return [];
}

/**
 * Demo 1: Basic Text Generation with Mathematical Analysis
 */
async function demoTextGeneration() {
	console.log("📝 Demo 1: Mathematical Text Generation");
	console.log("--------------------------------------");

	try {
		const { text } = await generateText({
			model: ollama("qwen3:0.6b"), // Use available model
			system: `You are a mathematical analysis expert specializing in category theory and functional programming.
            Provide precise, technical explanations with mathematical rigor.`,
			prompt: `Analyze the mathematical properties of the Result<T> type:

interface Result<T> {
    readonly isSuccess: boolean;
    readonly value: T | undefined;
    readonly error: Error | undefined;
    map<U>(fn: (value: T) => U): Result<U>;
    flatMap<U>(fn: (value: T) => Result<U>): Result<U>;
}

Focus on its categorical structure and algebraic properties.`,
			temperature: 0.7,
			maxTokens: 800,
		});

		console.log("✅ Generated mathematical analysis:");
		console.log("📊 Response:", `${text.substring(0, 500)}...`);
		console.log(`📏 Length: ${text.length} characters`);
	} catch (error) {
		console.log("❌ Text generation failed:", error);
	}

	console.log("");
}

/**
 * Demo 2: Streaming Text with Real-time Analysis
 */
async function demoStreamText() {
	console.log("🌊 Demo 2: Streaming Mathematical Analysis");
	console.log("-----------------------------------------");

	try {
		const { textStream } = await streamText({
			model: ollama("qwen3:0.6b"),
			system: `You are a formal verification expert. Analyze code for mathematical correctness step by step.`,
			prompt: `Verify if this TypeScript implementation satisfies the Monad laws:

class Maybe<T> {
    constructor(private value: T | null) {}
    
    static of<T>(value: T): Maybe<T> { return new Maybe(value); }
    
    flatMap<U>(fn: (value: T) => Maybe<U>): Maybe<U> {
        return this.value === null ? new Maybe<U>(null) : fn(this.value);
    }
}

Check: Left Identity, Right Identity, and Associativity laws.`,
			temperature: 0.3,
			maxTokens: 1000,
		});

		console.log("🔄 Streaming analysis...");
		let streamedText = "";
		let chunkCount = 0;

		for await (const chunk of textStream) {
			streamedText += chunk;
			chunkCount++;

			// Show progress every 10 chunks
			if (chunkCount % 10 === 0) {
				process.stdout.write(".");
			}
		}

		console.log("\n✅ Streaming completed");
		console.log(`📊 Total chunks: ${chunkCount}`);
		console.log(`📏 Final length: ${streamedText.length} characters`);
		console.log("📝 Analysis preview:", `${streamedText.substring(0, 300)}...`);
	} catch (error) {
		console.log("❌ Streaming failed:", error);
	}

	console.log("");
}

/**
 * Demo 3: Structured Object Generation with Zod Schema
 */
async function demoObjectGeneration() {
	console.log("🏗️  Demo 3: Structured Mathematical Analysis");
	console.log("--------------------------------------------");

	// Define schema for mathematical analysis
	const analysisSchema = z.object({
		component: z.string().describe("The mathematical component being analyzed"),
		category: z
			.enum(["functor", "applicative", "monad", "monoid", "semigroup"])
			.describe("Primary categorical structure"),
		properties: z
			.array(
				z.object({
					name: z.string(),
					satisfied: z.boolean(),
					explanation: z.string(),
				})
			)
			.describe("Mathematical properties and their verification"),
		completenessScore: z.number().min(0).max(100).describe("Completeness score (0-100)"),
		recommendations: z.array(z.string()).describe("Suggestions for improvement"),
		mathematicalNotes: z.string().describe("Additional mathematical insights"),
	});

	try {
		const { object } = await generateObject({
			model: ollama("qwen3:0.6b"),
			system: `You are a category theory expert. Analyze mathematical structures with precision.
            Always provide specific, actionable insights and accurate completeness scores.`,
			prompt: `Analyze the mathematical structure of this Either type:

type Either<L, R> = Left<L> | Right<R>;

interface Left<L> { readonly _tag: 'Left'; readonly left: L; }
interface Right<R> { readonly _tag: 'Right'; readonly right: R; }

const map = <L, R, B>(fa: Either<L, R>, f: (a: R) => B): Either<L, B> =>
    fa._tag === 'Left' ? fa : { _tag: 'Right', right: f(fa.right) };

const flatMap = <L, R, B>(fa: Either<L, R>, f: (a: R) => Either<L, B>): Either<L, B> =>
    fa._tag === 'Left' ? fa : f(fa.right);`,
			schema: analysisSchema,
			temperature: 0.4,
		});

		console.log("✅ Generated structured analysis:");
		console.log("📊 Component:", object.component);
		console.log("🏷️  Category:", object.category);
		console.log("📈 Completeness:", `${object.completenessScore}%`);
		console.log("⚖️  Properties:");
		object.properties.forEach((prop, i) => {
			console.log(`   ${i + 1}. ${prop.name}: ${prop.satisfied ? "✅" : "❌"}`);
			console.log(`      ${prop.explanation}`);
		});
		console.log("💡 Recommendations:");
		object.recommendations.forEach((rec, i) => {
			console.log(`   ${i + 1}. ${rec}`);
		});
		console.log("📝 Mathematical Notes:", `${object.mathematicalNotes.substring(0, 200)}...`);
	} catch (error) {
		console.log("❌ Object generation failed:", error);
	}

	console.log("");
}

/**
 * Demo 4: Tool Integration with Mathematical Operations
 */
async function demoToolIntegration() {
	console.log("🔧 Demo 4: Tool Integration for Mathematical Verification");
	console.log("--------------------------------------------------------");

	try {
		const { text, toolCalls } = await generateText({
			model: ollama("qwen3:0.6b"),
			system: `You are a mathematical verification assistant. Use the provided tools to analyze and verify mathematical structures.`,
			prompt: `I need to verify if the function composition operation forms a monoid. 
            Please check the identity and associativity properties using the verification tools.`,
			tools: {
				verifyIdentityLaw: {
					description: "Verify identity law for a mathematical structure",
					parameters: z.object({
						structure: z.string().describe("The mathematical structure to verify"),
						identityElement: z.string().describe("The proposed identity element"),
						operation: z.string().describe("The binary operation"),
					}),
					execute: async ({ structure, identityElement, operation }) => {
						// Simulate mathematical verification
						await new Promise((resolve) => setTimeout(resolve, 100));

						const isValid =
							structure === "function composition" &&
							identityElement === "identity function" &&
							operation === "compose";

						return {
							verified: isValid,
							explanation: isValid
								? "Identity law satisfied: compose(f, id) = compose(id, f) = f"
								: "Identity law verification failed",
							confidence: isValid ? 0.95 : 0.3,
						};
					},
				},
				verifyAssociativityLaw: {
					description: "Verify associativity law for a binary operation",
					parameters: z.object({
						operation: z.string().describe("The binary operation to verify"),
						example: z.string().describe("Example demonstrating associativity"),
					}),
					execute: async ({ operation }) => {
						await new Promise((resolve) => setTimeout(resolve, 100));

						return {
							verified: true,
							explanation: `Associativity verified for ${operation}: (f ∘ g) ∘ h = f ∘ (g ∘ h)`,
							mathematicalProof: "Function composition is inherently associative by definition",
							confidence: 0.99,
						};
					},
				},
			},
			maxSteps: 3,
		});

		console.log("✅ Tool-assisted verification completed");
		console.log("📊 Analysis result:", `${text.substring(0, 400)}...`);
		console.log("🔧 Tools called:", toolCalls.length);

		toolCalls.forEach((call, i) => {
			console.log(`   ${i + 1}. ${call.toolName}:`, JSON.stringify(call.result, null, 2));
		});
	} catch (error) {
		console.log("❌ Tool integration failed:", error);
	}

	console.log("");
}

/**
 * Demo 5: Advanced Pattern - Multi-step Mathematical Reasoning
 */
async function demoMultiStepReasoning() {
	console.log("🧠 Demo 5: Multi-step Mathematical Reasoning");
	console.log("--------------------------------------------");

	try {
		const steps = [
			"Define the mathematical structure",
			"Identify categorical properties",
			"Verify algebraic laws",
			"Assess completeness and suggest improvements",
		];

		console.log("🔄 Starting multi-step analysis...");

		const context = {
			structure: "Option<T> type",
			definition: `type Option<T> = Some<T> | None;
interface Some<T> { readonly _tag: 'Some'; readonly value: T; }
interface None { readonly _tag: 'None'; }`,
			findings: [] as string[],
		};

		for (let i = 0; i < steps.length; i++) {
			console.log(`\n📋 Step ${i + 1}: ${steps[i]}`);

			const { text } = await generateText({
				model: ollama("qwen3:0.6b"),
				system: `You are conducting step ${i + 1} of ${steps.length} in a mathematical analysis.
                Previous findings: ${context.findings.join("; ")}
                Be concise and focus on this specific step.`,
				prompt: `${steps[i]} for: ${context.structure}

Definition: ${context.definition}

Provide a focused analysis for this step only.`,
				temperature: 0.3,
				maxTokens: 300,
			});

			context.findings.push(`Step ${i + 1}: ${text.substring(0, 100)}...`);
			console.log(`✅ Result: ${text.substring(0, 200)}...`);
		}

		console.log("\n🎯 Multi-step Analysis Summary:");
		context.findings.forEach((finding, i) => {
			console.log(`   ${i + 1}. ${finding}`);
		});
	} catch (error) {
		console.log("❌ Multi-step reasoning failed:", error);
	}

	console.log("");
}

/**
 * Main demo execution
 */
async function runQiPromptOllamaDemo() {
	console.log("🚀 Starting QiPrompt + Ollama Demonstration...\n");

	// Check Ollama availability
	const models = await checkOllamaConnection();
	if (models.length === 0) {
		console.log("⚠️  Ollama not available - some demos will be simulated");
		console.log("");
	}

	await demoTextGeneration();
	await demoStreamText();
	await demoObjectGeneration();
	await demoToolIntegration();
	await demoMultiStepReasoning();

	console.log("🎉 QiPrompt + Ollama Demo completed!");
	console.log("\n✨ Summary:");
	console.log("   • generateText: Mathematical analysis with local LLM");
	console.log("   • streamText: Real-time mathematical verification");
	console.log("   • generateObject: Structured analysis with Zod schemas");
	console.log("   • Tool integration: Mathematical verification tools");
	console.log("   • Multi-step reasoning: Complex mathematical workflows");
	console.log("   • Full Vercel AI SDK integration through @qi/prompt wrapper");
}

// Run if this file is executed directly
if (import.meta.main) {
	runQiPromptOllamaDemo().catch(console.error);
}
