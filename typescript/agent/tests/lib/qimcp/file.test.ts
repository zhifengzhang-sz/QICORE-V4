import { beforeEach, describe, expect, it } from "vitest";
import { AnalysisFileManager, type AnalysisResult } from "../../../lib/src/qimcp/tools/file.js";

describe("AnalysisFileManager", () => {
	let fileManager: AnalysisFileManager;

	beforeEach(() => {
		fileManager = new AnalysisFileManager("./test-output");
	});

	it("should create a file manager instance", () => {
		expect(fileManager).toBeInstanceOf(AnalysisFileManager);
	});

	it("should save summary report", () => {
		const results: AnalysisResult[] = [];

		const summaryPath = fileManager.saveSummaryReport(results);
		expect(typeof summaryPath).toBe("string");
		expect(summaryPath).toContain("analysis-summary.md");
	});

	it("should return output directory", () => {
		const outputDir = fileManager.getOutputDir();
		expect(typeof outputDir).toBe("string");
		expect(outputDir).toContain("analysis-");
	});
});
