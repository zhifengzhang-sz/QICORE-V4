#!/usr/bin/env bun

/**
 * Test Mathematical Contracts Utility
 *
 * Tests mathematical contract analysis using the new mathematical prompt manager
 */

import { MathematicalOllamaAgent } from "../agents/ollama.js";

export interface ContractTestResult {
	component: string;
	analysis: string;
	passed: boolean;
	duration: number;
	errors: string[];
}

/**
 * Test mathematical contract analysis
 */
export async function testMathematicalContract(
	component: string,
	contractText: string
): Promise<ContractTestResult> {
	const agent = new MathematicalOllamaAgent();
	const startTime = Date.now();

	try {
		const analysis = await agent.analyzeMathematicalContracts(component, contractText, {
			domain: "algebraic_structures",
			complexity: "graduate",
		});

		const duration = Date.now() - startTime;

		return {
			component,
			analysis,
			passed: true,
			duration,
			errors: [],
		};
	} catch (error) {
		const duration = Date.now() - startTime;

		return {
			component,
			analysis: "",
			passed: false,
			duration,
			errors: [String(error)],
		};
	} finally {
		agent.reset();
	}
}

/**
 * Test law verification
 */
export async function testLawVerification(
	implementation: string,
	algebraicType: string,
	laws: string[]
): Promise<ContractTestResult> {
	const agent = new MathematicalOllamaAgent();
	const startTime = Date.now();

	try {
		const verification = await agent.verifyAlgebraicLaws(implementation, algebraicType, laws);

		const duration = Date.now() - startTime;

		return {
			component: algebraicType,
			analysis: verification,
			passed: true,
			duration,
			errors: [],
		};
	} catch (error) {
		const duration = Date.now() - startTime;

		return {
			component: algebraicType,
			analysis: "",
			passed: false,
			duration,
			errors: [String(error)],
		};
	} finally {
		agent.reset();
	}
}
