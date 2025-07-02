#!/usr/bin/env bun

/**
 * Mathematical Analysis with File Output
 * 
 * Enhanced version of the mathematical verification agent that saves results to files
 */

import { OllamaOnlyAgent, Result } from './ollama-only-agent';
import { readFileSync, writeFileSync, mkdirSync } from 'fs';
import { join, dirname } from 'path';

interface AnalysisResult {
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

async function runMathematicalAnalysisWithFiles() {
  console.log("🔬 Mathematical Analysis with File Output");
  console.log("=========================================\n");

  const agent = new OllamaOnlyAgent();
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
  const outputDir = join(process.cwd(), 'output', `analysis-${timestamp}`);

  // Create output directory
  mkdirSync(outputDir, { recursive: true });
  console.log(`📁 Created output directory: ${outputDir}\n`);

  try {
    // Read the mathematical contracts
    const contractsPath = join(process.cwd(), 'docs/qi/core/nl/class.contracts.0.md');
    const contractsText = readFileSync(contractsPath, 'utf-8');

    console.log("📖 Loaded mathematical contracts");
    console.log(`   Contract length: ${contractsText.length} characters`);
    console.log(`   Source: ${contractsPath}\n`);

    const allResults: AnalysisResult[] = [];

    // Analyze QiError contract
    console.log("🔍 Analyzing QiError Semi-group...");
    const qiErrorSection = extractContractSection(contractsText, 'QiError Contract');
    if (qiErrorSection) {
      const result = await analyzeAndSave(agent, 'QiError', qiErrorSection, outputDir, 'Semi-group');
      if (result) allResults.push(result);
    }

    // Analyze Result<T> contract
    console.log("\n🔍 Analyzing Result<T> Monad...");
    const resultSection = extractContractSection(contractsText, 'Result<T> Contract');
    if (resultSection) {
      const result = await analyzeAndSave(agent, 'Result<T>', resultSection, outputDir, 'Monad');
      if (result) allResults.push(result);
    }

    // Analyze Configuration contract
    console.log("\n🔍 Analyzing Configuration Monoid...");
    const configSection = extractContractSection(contractsText, 'Configuration Contract');
    if (configSection) {
      const result = await analyzeAndSave(agent, 'Configuration', configSection, outputDir, 'Monoid');
      if (result) allResults.push(result);
    }

    // Generate summary report
    console.log("\n📊 Generating summary report...");
    const summaryPath = join(outputDir, 'analysis-summary.md');
    const summaryContent = generateSummaryReport(allResults, timestamp);
    writeFileSync(summaryPath, summaryContent);
    console.log(`   ✅ Summary saved: ${summaryPath}`);

    // Save all results as JSON
    const jsonPath = join(outputDir, 'analysis-results.json');
    writeFileSync(jsonPath, JSON.stringify(allResults, null, 2));
    console.log(`   ✅ JSON data saved: ${jsonPath}`);

    console.log("\n🎯 Analysis Complete!");
    console.log(`   📁 Output directory: ${outputDir}`);
    console.log(`   📄 Files generated: ${allResults.length * 2 + 2}`);
    console.log(`   🏗️  Algebraic structures identified: ${[...new Set(allResults.flatMap(r => r.algebraicStructures))].length}`);
    console.log(`   📊 Average completeness: ${Math.round(allResults.reduce((sum, r) => sum + r.completenessScore, 0) / allResults.length)}%`);

  } catch (error) {
    console.error("❌ Analysis failed:", error);
    process.exit(1);
  }
}

async function analyzeAndSave(
  agent: OllamaOnlyAgent, 
  component: string, 
  contractText: string, 
  outputDir: string, 
  algebraicType: string
): Promise<AnalysisResult | null> {
  try {
    // Run mathematical analysis
    const analysisResult = await agent.analyzeMathematicalContracts(contractText, component);
    if (!Result.isSuccess(analysisResult)) {
      console.log(`   ❌ Analysis failed: ${analysisResult.error.message}`);
      return null;
    }

    const analysis = analysisResult.data;
    console.log(`   ✅ Analysis completed`);
    console.log(`   🏗️  Algebraic structures: ${analysis.algebraicStructures.join(', ')}`);
    console.log(`   📊 Completeness score: ${analysis.completenessScore}%`);

    // Create result object
    const result: AnalysisResult = {
      component,
      timestamp: new Date().toISOString(),
      algebraicStructures: analysis.algebraicStructures,
      completenessScore: analysis.completenessScore,
      inevitablePatterns: analysis.inevitablePatterns,
      gaps: analysis.gaps,
      claudeAnalysis: analysis.claudeAnalysis,
      ollamaVerification: analysis.ollamaVerification
    };

    // Run law verification if applicable
    if (algebraicType === 'Monad' || algebraicType === 'Monoid') {
      console.log(`   ⚖️  Verifying ${algebraicType} laws...`);
      const lawResult = await agent.verifyAlgebraicLaws(algebraicType as any, contractText);
      if (Result.isSuccess(lawResult)) {
        const verification = lawResult.data;
        result.lawVerification = {
          structure: verification.structure,
          laws: verification.laws,
          satisfied: verification.satisfied,
          violations: verification.violations,
          verificationText: verification.verification
        };
        console.log(`   📜 Laws verified: ${verification.laws.length}`);
        console.log(`   ✅ Laws satisfied: ${verification.satisfied ? 'Yes' : 'No'}`);
        if (verification.violations.length > 0) {
          console.log(`   🚨 Violations: ${verification.violations.length}`);
        }
      }
    }

    // Save individual analysis report
    const filename = `${component.toLowerCase().replace(/[<>]/g, '')}-analysis.md`;
    const filepath = join(outputDir, filename);
    const reportContent = generateIndividualReport(result);
    writeFileSync(filepath, reportContent);
    console.log(`   💾 Report saved: ${filename}`);

    // Save raw data as JSON
    const jsonFilename = `${component.toLowerCase().replace(/[<>]/g, '')}-data.json`;
    const jsonFilepath = join(outputDir, jsonFilename);
    writeFileSync(jsonFilepath, JSON.stringify(result, null, 2));
    console.log(`   💾 Data saved: ${jsonFilename}`);

    return result;

  } catch (error) {
    console.log(`   ❌ Error analyzing ${component}: ${error}`);
    return null;
  }
}

function generateIndividualReport(result: AnalysisResult): string {
  return `# ${result.component} Mathematical Analysis Report

**Generated**: ${result.timestamp}
**Component**: ${result.component}

## Analysis Summary

**Algebraic Structures Identified**: ${result.algebraicStructures.join(', ')}
**Completeness Score**: ${result.completenessScore}%
**Inevitable Patterns**: ${result.inevitablePatterns.join(', ') || 'None identified'}

## Identified Gaps

${result.gaps.length > 0 ? result.gaps.map(gap => `- ${gap}`).join('\n') : 'No gaps identified'}

${result.lawVerification ? `## Law Verification

**Structure**: ${result.lawVerification.structure}
**Laws Verified**: ${result.lawVerification.laws.length}
**Satisfied**: ${result.lawVerification.satisfied ? '✅ Yes' : '❌ No'}
**Violations**: ${result.lawVerification.violations.length}

### Verification Details
${result.lawVerification.verificationText}

### Violations Found
${result.lawVerification.violations.length > 0 ? result.lawVerification.violations.map(v => `- ${v}`).join('\n') : 'None'}
` : ''}

## Detailed Claude Analysis

${result.claudeAnalysis}

## Ollama Verification

${result.ollamaVerification}

---
*Generated by Mathematical Verification Agent*
`;
}

function generateSummaryReport(results: AnalysisResult[], timestamp: string): string {
  const totalStructures = [...new Set(results.flatMap(r => r.algebraicStructures))];
  const avgCompleteness = Math.round(results.reduce((sum, r) => sum + r.completenessScore, 0) / results.length);
  const totalGaps = results.reduce((sum, r) => sum + r.gaps.length, 0);
  const totalViolations = results.reduce((sum, r) => sum + (r.lawVerification?.violations.length || 0), 0);

  return `# Mathematical Analysis Summary Report

**Generated**: ${timestamp}
**Components Analyzed**: ${results.length}

## Overview

This report summarizes the mathematical analysis of ${results.length} QiCore components, evaluating their algebraic properties and mathematical completeness.

## Key Findings

### Algebraic Structures Identified
${totalStructures.map(structure => `- **${structure}**: Found in ${results.filter(r => r.algebraicStructures.includes(structure)).map(r => r.component).join(', ')}`).join('\n')}

### Completeness Scores
${results.map(r => `- **${r.component}**: ${r.completenessScore}%`).join('\n')}

**Average Completeness**: ${avgCompleteness}%

### Mathematical Law Verification
${results.filter(r => r.lawVerification).map(r => `- **${r.component}** (${r.lawVerification!.structure}): ${r.lawVerification!.satisfied ? '✅ Satisfied' : '❌ Failed'} (${r.lawVerification!.violations.length} violations)`).join('\n')}

## Issues Identified

### Total Gaps: ${totalGaps}
${results.flatMap(r => r.gaps.map(gap => `- **${r.component}**: ${gap}`)).join('\n')}

### Total Law Violations: ${totalViolations}
${results.filter(r => r.lawVerification && r.lawVerification.violations.length > 0).flatMap(r => r.lawVerification!.violations.map(violation => `- **${r.component}**: ${violation}`)).join('\n')}

## Recommendations

1. **Address Law Violations**: Focus on the ${totalViolations} mathematical law violations identified
2. **Complete Missing Laws**: Add the ${totalGaps} missing mathematical properties
3. **Enhance Specifications**: Improve completeness scores for components below 90%
4. **Pattern Implementation**: Implement the inevitable patterns identified by the analysis

## Individual Reports

${results.map(r => `- [${r.component} Analysis](${r.component.toLowerCase().replace(/[<>]/g, '')}-analysis.md)`).join('\n')}

---
*Generated by Mathematical Verification Agent*
`;
}

function extractContractSection(text: string, sectionTitle: string): string | null {
  const lines = text.split('\n');
  let startIndex = -1;
  let endIndex = -1;

  // Find the start of the section
  for (let i = 0; i < lines.length; i++) {
    if (lines[i]?.includes(sectionTitle)) {
      startIndex = i;
      break;
    }
  }

  if (startIndex === -1) {
    return null;
  }

  // Find the end of the section (next ## heading or end of file)
  for (let i = startIndex + 1; i < lines.length; i++) {
    if (lines[i]?.startsWith('## ') && !lines[i]?.includes('Contract')) {
      endIndex = i;
      break;
    }
  }

  if (endIndex === -1) {
    endIndex = lines.length;
  }

  return lines.slice(startIndex, endIndex).join('\n');
}

// Run the analysis
if (import.meta.main) {
  runMathematicalAnalysisWithFiles().catch(console.error);
} 