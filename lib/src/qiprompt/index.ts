#!/usr/bin/env bun

/**
 * QiPrompt: Lightweight wrapper around Vercel AI SDK
 *
 * True lightweight wrapper - simply re-exports AI SDK functionality
 * with convenience functions and common schemas.
 * 
 * Latest AI SDK 4.1 features supported:
 * - generateText, generateObject, streamText, streamObject, generateImage
 * - Multi-modal support (text, images, structured outputs)
 * - Tool calling with tool() function
 * - Multiple provider support (OpenAI, Anthropic, Google, etc.)
 * - Type-safe structured outputs with Zod
 */

// Re-export everything from AI SDK core
export * from "ai";

// Re-export Zod for structured generation
export { z } from "zod";

// Re-export Ollama provider for local inference
export { createOllama } from "ollama-ai-provider";

// Re-export popular providers
export { openai } from "@ai-sdk/openai";
export { anthropic } from "@ai-sdk/anthropic";
export { google } from "@ai-sdk/google";

/**
 * Convenience function to create a default Ollama instance
 * Points to standard local Ollama installation
 */
export function createDefaultOllama() {
	return createOllama({
		baseURL: "http://localhost:11434/api",
	});
}

/**
 * Common Zod schemas for typical AI use cases
 */
export const commonSchemas = {
	// Simple text analysis result
	analysis: z.object({
		summary: z.string().describe("Brief summary of the content"),
		sentiment: z.enum(["positive", "negative", "neutral"]).describe("Overall sentiment"),
		confidence: z.number().min(0).max(1).describe("Confidence score 0-1"),
	}),

	// Mathematical calculation result
	calculation: z.object({
		result: z.number().describe("The calculated result"),
		formula: z.string().describe("The formula used"),
		steps: z.array(z.string()).describe("Step-by-step solution"),
	}),

	// Code generation result
	codeGeneration: z.object({
		code: z.string().describe("The generated code"),
		language: z.string().describe("Programming language"),
		explanation: z.string().describe("Brief explanation of the code"),
		dependencies: z.array(z.string()).optional().describe("Required dependencies"),
	}),

	// Decision making result
	decision: z.object({
		choice: z.string().describe("The recommended choice"),
		reasoning: z.string().describe("Explanation of the reasoning"),
		alternatives: z.array(z.string()).describe("Other viable options"),
		confidence: z.number().min(0).max(1).describe("Confidence in the decision"),
	}),
};

/**
 * Convenience function for common text generation with error handling
 */
export async function generateTextSafe(options: Parameters<typeof import("ai").generateText>[0]) {
	try {
		const result = await import("ai").then(m => m.generateText(options));
		return { success: true as const, data: result };
	} catch (error) {
		return { success: false as const, error: error instanceof Error ? error.message : "Unknown error" };
	}
}

/**
 * Convenience function for common object generation with error handling
 */
export async function generateObjectSafe<T>(options: Parameters<typeof import("ai").generateObject>[0]) {
	try {
		const result = await import("ai").then(m => m.generateObject(options));
		return { success: true as const, data: result };
	} catch (error) {
		return { success: false as const, error: error instanceof Error ? error.message : "Unknown error" };
	}
} 