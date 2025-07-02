#!/usr/bin/env bun

/**
 * Compare Specification Analysis
 * Compare how the tiny model (qwen3:0.6b) analyzes original vs corrected specifications
 */

import { OllamaOnlyAgent, Result } from './ollama-only-agent';
import { readFileSync } from 'fs';
import { join } from 'path';

async function compareSpecifications() {
  console.log("🔬 Tiny Model vs Sonnet 4 Specification Analysis");
  console.log("================================================\n");

  const agent = new OllamaOnlyAgent();

  try {
    // Load original specification (the realistic one)
    const originalPath = join(process.cwd(), 'docs/qi/core/nl/class.contracts.0.md');
    const originalText = readFileSync(originalPath, 'utf-8');

    console.log("📖 Loaded original specification (realistic baseline)");
    console.log(`   Length: ${originalText.length} characters\n`);

    // Analyze QiError contract
    console.log("🔍 QiError Contract Analysis");
    console.log("────────────────────────────");
    
    const qiErrorSection = extractContractSection(originalText, 'QiError Contract');
    
    if (qiErrorSection) {
      console.log("📋 Original Specification Structure:");
      console.log("   Pre-assigned: Semi-group with Context Composition");
      console.log("   Properties: Identity, Associativity, Immutability");
      console.log("   Operations: create, withContext, causedBy, etc.\n");
      
      console.log("🤖 Tiny Model (qwen3:0.6b) Analysis:");
      const startTime = Date.now();
      const analysis = await agent.analyzeMathematicalContracts(qiErrorSection, 'QiError');
      const duration = Date.now() - startTime;

      if (Result.isSuccess(analysis)) {
        const result = analysis.data;
        console.log(`   🏗️  Discovered Structures: ${result.algebraicStructures.join(', ')}`);
        console.log(`   📊 Completeness Score: ${result.completenessScore}%`);
        console.log(`   🎯 Inevitable Patterns: ${result.inevitablePatterns.join(', ')}`);
        console.log(`   🚨 Gaps Identified: ${result.gaps.length}`);
        console.log(`   ⏱️  Analysis Time: ${duration}ms`);
        
        // Show the actual analysis content from the tiny model
        console.log(`\n   📄 Tiny Model Analysis Content:`);
        console.log(`   ${'-'.repeat(50)}`);
        console.log(result.claudeAnalysis.substring(0, 800) + (result.claudeAnalysis.length > 800 ? '...' : ''));
        console.log(`   ${'-'.repeat(50)}`);
        
        // Check if it discovered the pre-assigned structure
        const foundSemigroup = result.algebraicStructures.some(s => 
          s.toLowerCase().includes('semi') || s.toLowerCase().includes('group')
        );
        const extraStructures = result.algebraicStructures.filter(s => 
          !s.toLowerCase().includes('semi') && !s.toLowerCase().includes('group')
        );
        
        console.log(`\n   📈 Discovery Analysis:`);
        console.log(`   • Found pre-assigned Semi-group: ${foundSemigroup ? '✅' : '❌'}`);
        if (extraStructures.length > 0) {
          console.log(`   • Additional discoveries: ${extraStructures.join(', ')}`);
        }
        
        if (result.gaps.length > 0) {
          console.log(`   • Gaps: ${result.gaps.join('; ')}`);
        }
      } else {
        console.log(`   ❌ Analysis failed: ${analysis.error.message}`);
      }
    }

    console.log("\n" + "=".repeat(60));
    console.log("🔍 Result<T> Contract Analysis");
    console.log("──────────────────────────────");
    
    const resultSection = extractContractSection(originalText, 'Result<T> Contract');
    
    if (resultSection) {
      console.log("📋 Original Specification Structure:");
      console.log("   Pre-assigned: Monad with Error Context");
      console.log("   Properties: Functor Laws, Monad Laws, Error Propagation");
      console.log("   Operations: success, failure, map, flatMap, etc.\n");
      
      console.log("🤖 Tiny Model (qwen3:0.6b) Analysis:");
      const startTime = Date.now();
      const analysis = await agent.analyzeMathematicalContracts(resultSection, 'Result<T>');
      const duration = Date.now() - startTime;

      if (Result.isSuccess(analysis)) {
        const result = analysis.data;
        console.log(`   🏗️  Discovered Structures: ${result.algebraicStructures.join(', ')}`);
        console.log(`   📊 Completeness Score: ${result.completenessScore}%`);
        console.log(`   🎯 Inevitable Patterns: ${result.inevitablePatterns.join(', ')}`);
        console.log(`   🚨 Gaps Identified: ${result.gaps.length}`);
        console.log(`   ⏱️  Analysis Time: ${duration}ms`);
        
        // Show the actual analysis content from the tiny model
        console.log(`\n   📄 Tiny Model Analysis Content:`);
        console.log(`   ${'-'.repeat(50)}`);
        console.log(result.claudeAnalysis.substring(0, 800) + (result.claudeAnalysis.length > 800 ? '...' : ''));
        console.log(`   ${'-'.repeat(50)}`);
        
        // Check if it discovered the pre-assigned structure
        const foundMonad = result.algebraicStructures.some(s => 
          s.toLowerCase().includes('monad')
        );
        const foundFunctor = result.algebraicStructures.some(s => 
          s.toLowerCase().includes('functor')
        );
        const extraStructures = result.algebraicStructures.filter(s => 
          !s.toLowerCase().includes('monad') && !s.toLowerCase().includes('functor')
        );
        
        console.log(`\n   📈 Discovery Analysis:`);
        console.log(`   • Found pre-assigned Monad: ${foundMonad ? '✅' : '❌'}`);
        console.log(`   • Found implied Functor: ${foundFunctor ? '✅' : '❌'}`);
        if (extraStructures.length > 0) {
          console.log(`   • Additional discoveries: ${extraStructures.join(', ')}`);
        }
        
        if (result.gaps.length > 0) {
          console.log(`   • Gaps: ${result.gaps.join('; ')}`);
        }
      } else {
        console.log(`   ❌ Analysis failed: ${analysis.error.message}`);
      }
    }

    console.log("\n" + "=".repeat(60));
    console.log("🎯 TINY MODEL CAPABILITIES ASSESSMENT");
    console.log("=".repeat(60));
    console.log("This analysis demonstrates the tiny model's ability to:");
    console.log("• 🔍 Discover mathematical structures from property specifications");
    console.log("• 📊 Assess mathematical completeness of specifications");
    console.log("• 🎯 Identify inevitable API patterns from algebraic laws");
    console.log("• 🔍 Detect gaps and missing mathematical properties");
    console.log("• ⚡ Perform analysis in reasonable time (~60-90 seconds)");
    console.log("\nReady for comparison with Sonnet 4 analysis on the same specifications!");

  } catch (error) {
    console.error("❌ Analysis failed:", error);
    process.exit(1);
  }
}

/**
 * Extract a specific contract section from the full contracts text
 */
function extractContractSection(text: string, sectionTitle: string): string | null {
  const lines = text.split('\n');
  let startIndex = -1;
  let endIndex = -1;

  // Find the start of the section
  for (let i = 0; i < lines.length; i++) {
    if (lines[i].includes(sectionTitle)) {
      startIndex = i;
      break;
    }
  }

  if (startIndex === -1) {
    return null;
  }

  // Find the end of the section (next ## heading or end of file)
  for (let i = startIndex + 1; i < lines.length; i++) {
    if (lines[i].startsWith('## ') && !lines[i].includes('Contract')) {
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
  compareSpecifications().catch(console.error);
}
