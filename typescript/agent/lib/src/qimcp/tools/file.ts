#!/usr/bin/env bun

/**
 * Analysis File Output Module
 *
 * Handles saving analysis results to files with proper structure and formatting
 */

import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

export interface AnalysisResult {
	component: string;
	timestamp: string;
	algebraicStructures: string[];
	completenessScore: number;
	inevitablePatterns: string[];
	gaps: string[];
	claudeAnalysis: string;
	ollamaVerification: string;
	lawVerification?: {
		structure: string;
		laws: string[];
		satisfied: boolean;
		violations: string[];
		verificationText: string;
	};
}

export class AnalysisFileManager {
	private outputDir: string;

	constructor(baseDir: string = "output") {
		const timestamp = new Date().toISOString().replace(/[:.]/g, "-");
		this.outputDir = join(process.cwd(), baseDir, `analysis-${timestamp}`);
		mkdirSync(this.outputDir, { recursive: true });
	}

	getOutputDir(): string {
		return this.outputDir;
	}

	/**
	 * Save individual analysis result
	 */
	saveAnalysisResult(result: AnalysisResult): { reportPath: string; dataPath: string } {
		const filename = `${result.component.toLowerCase().replace(/[<>]/g, "")}-analysis.md`;
		const reportPath = join(this.outputDir, filename);
		const reportContent = this.generateIndividualReport(result);
		writeFileSync(reportPath, reportContent);

		// Save raw data as JSON
		const jsonFilename = `${result.component.toLowerCase().replace(/[<>]/g, "")}-data.json`;
		const dataPath = join(this.outputDir, jsonFilename);
		writeFileSync(dataPath, JSON.stringify(result, null, 2));

		return { reportPath, dataPath };
	}

	/**
	 * Generate summary report from all results
	 */
	saveSummaryReport(results: AnalysisResult[]): string {
		const timestamp = new Date().toISOString();
		const summaryPath = join(this.outputDir, "analysis-summary.md");
		const summaryContent = this.generateSummaryReport(results, timestamp);
		writeFileSync(summaryPath, summaryContent);

		// Save all results as JSON
		const jsonPath = join(this.outputDir, "analysis-results.json");
		writeFileSync(jsonPath, JSON.stringify(results, null, 2));

		return summaryPath;
	}

	private generateIndividualReport(result: AnalysisResult): string {
		return `# ${result.component} Mathematical Analysis Report

**Generated**: ${result.timestamp}
**Component**: ${result.component}

## Analysis Summary

**Algebraic Structures Identified**: ${result.algebraicStructures.join(", ")}
**Completeness Score**: ${result.completenessScore}%
**Inevitable Patterns**: ${result.inevitablePatterns.join(", ") || "None identified"}

## Identified Gaps

${result.gaps.length > 0 ? result.gaps.map((gap) => `- ${gap}`).join("\n") : "No gaps identified"}

${
	result.lawVerification
		? `## Law Verification

**Structure**: ${result.lawVerification.structure}
**Laws Verified**: ${result.lawVerification.laws.length}
**Satisfied**: ${result.lawVerification.satisfied ? "✅ Yes" : "❌ No"}
**Violations**: ${result.lawVerification.violations.length}

### Verification Details
${result.lawVerification.verificationText}

### Violations Found
${result.lawVerification.violations.length > 0 ? result.lawVerification.violations.map((v) => `- ${v}`).join("\n") : "None"}
`
		: ""
}

## Detailed Claude Analysis

${result.claudeAnalysis}

## Ollama Verification

${result.ollamaVerification}

---
*Generated by QiCore Mathematical Verification Agent*
`;
	}

	private generateSummaryReport(results: AnalysisResult[], timestamp: string): string {
		const totalStructures = [...new Set(results.flatMap((r) => r.algebraicStructures))];
		const avgCompleteness = Math.round(
			results.reduce((sum, r) => sum + r.completenessScore, 0) / results.length
		);
		const totalGaps = results.reduce((sum, r) => sum + r.gaps.length, 0);
		const totalViolations = results.reduce(
			(sum, r) => sum + (r.lawVerification?.violations.length || 0),
			0
		);

		return `# Mathematical Analysis Summary Report

**Generated**: ${timestamp}
**Components Analyzed**: ${results.length}

## Overview

This report summarizes the mathematical analysis of ${results.length} QiCore components, evaluating their algebraic properties and mathematical completeness.

## Key Findings

### Algebraic Structures Identified
${totalStructures
	.map(
		(structure) =>
			`- **${structure}**: Found in ${results
				.filter((r) => r.algebraicStructures.includes(structure))
				.map((r) => r.component)
				.join(", ")}`
	)
	.join("\n")}

### Completeness Scores
${results.map((r) => `- **${r.component}**: ${r.completenessScore}%`).join("\n")}

**Average Completeness**: ${avgCompleteness}%

### Mathematical Law Verification
${results
	.filter((r) => r.lawVerification)
	.map(
		(r) =>
			`- **${r.component}** (${r.lawVerification?.structure}): ${r.lawVerification?.satisfied ? "✅ Satisfied" : "❌ Failed"} (${r.lawVerification?.violations.length} violations)`
	)
	.join("\n")}

## Issues Identified

### Total Gaps: ${totalGaps}
${results.flatMap((r) => r.gaps.map((gap) => `- **${r.component}**: ${gap}`)).join("\n")}

### Total Law Violations: ${totalViolations}
${results
	.filter((r) => r.lawVerification && r.lawVerification.violations.length > 0)
	.flatMap((r) =>
		r.lawVerification?.violations.map((violation) => `- **${r.component}**: ${violation}`)
	)
	.join("\n")}

## Recommendations

1. **Address Law Violations**: Focus on the ${totalViolations} mathematical law violations identified
2. **Complete Missing Laws**: Add the ${totalGaps} missing mathematical properties
3. **Enhance Specifications**: Improve completeness scores for components below 90%
4. **Pattern Implementation**: Implement the inevitable patterns identified by the analysis

## Individual Reports

${results.map((r) => `- [${r.component} Analysis](${r.component.toLowerCase().replace(/[<>]/g, "")}-analysis.md)`).join("\n")}

---
*Generated by QiCore Mathematical Verification Agent*
`;
	}
}
