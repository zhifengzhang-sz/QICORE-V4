#!/usr/bin/env bun

/**
 * Contract Test Runner Example
 *
 * Demonstrates the mathematical contract testing utility
 */

import {
	testLawVerification,
	testMathematicalContract,
} from "../utils/test-mathematical-contracts.js";

console.log("🧮 Mathematical Contract Test Example");
console.log("====================================\n");

// Example contract for a Result type (common in functional programming)
const resultContract = `
/**
 * Result<T> represents a computation that may fail
 * Following the Either monad pattern from category theory
 */
interface Result<T> {
	readonly isSuccess: boolean;
	readonly isFailure: boolean;
	readonly value: T | undefined;
	readonly error: Error | undefined;

	// Functor: map preserves structure
	map<U>(fn: (value: T) => U): Result<U>;
	
	// Monad: flatMap allows chaining computations
	flatMap<U>(fn: (value: T) => Result<U>): Result<U>;
	
	// Applicative: apply function wrapped in Result
	apply<U>(fnResult: Result<(value: T) => U>): Result<U>;
	
	// Utility methods
	getOrElse(defaultValue: T): T;
	filter(predicate: (value: T) => boolean): Result<T>;
}`;

async function runContractTests() {
	console.log("🔍 Testing Result<T> contract analysis...");

	// Test 1: Mathematical contract analysis
	const analysisResult = await testMathematicalContract("Result<T>", resultContract);

	console.log("📊 Contract Analysis Results:");
	console.log(`   Component: ${analysisResult.component}`);
	console.log(`   Test Passed: ${analysisResult.passed ? "✅" : "❌"}`);
	console.log(`   Duration: ${analysisResult.duration}ms`);
	console.log(
		`   Errors: ${analysisResult.errors.length > 0 ? analysisResult.errors.join(", ") : "None"}`
	);

	if (analysisResult.analysis) {
		console.log("\n📝 Analysis Summary:");
		// Show first few lines of the analysis
		const lines = analysisResult.analysis.split("\n").slice(0, 10);
		lines.forEach((line) => console.log(`   ${line}`));
		if (analysisResult.analysis.split("\n").length > 10) {
			console.log("   ...");
		}
	}

	console.log("\n⚖️  Testing Monad laws for Result<T>...");

	// Test 2: Algebraic law verification for Monad
	const monadLaws = [
		"Left Identity: return(a).flatMap(f) === f(a)",
		"Right Identity: m.flatMap(return) === m",
		"Associativity: m.flatMap(f).flatMap(g) === m.flatMap(x => f(x).flatMap(g))",
	];

	const lawResult = await testLawVerification(resultContract, "Monad", monadLaws);

	console.log("📊 Law Verification Results:");
	console.log(`   Algebraic Structure: ${lawResult.component}`);
	console.log(`   Test Passed: ${lawResult.passed ? "✅" : "❌"}`);
	console.log(`   Duration: ${lawResult.duration}ms`);
	console.log(`   Errors: ${lawResult.errors.length > 0 ? lawResult.errors.join(", ") : "None"}`);

	if (lawResult.analysis) {
		console.log("\n📝 Law Verification Summary:");
		const lines = lawResult.analysis.split("\n").slice(0, 8);
		lines.forEach((line) => console.log(`   ${line}`));
		if (lawResult.analysis.split("\n").length > 8) {
			console.log("   ...");
		}
	}

	console.log("\n🎯 Test Summary:");
	console.log(`   • Contract analysis: ${analysisResult.passed ? "PASSED" : "FAILED"}`);
	console.log(`   • Law verification: ${lawResult.passed ? "PASSED" : "FAILED"}`);
	console.log(`   • Total duration: ${analysisResult.duration + lawResult.duration}ms`);

	console.log("\n✨ This demonstrates:");
	console.log("   • @qi/prompt wrapper generating sophisticated mathematical analysis");
	console.log("   • Mathematical prompt management with template caching");
	console.log("   • Category theory and algebraic structure analysis");
	console.log("   • Formal verification of mathematical laws");
}

if (import.meta.main) {
	runContractTests().catch(console.error);
}
