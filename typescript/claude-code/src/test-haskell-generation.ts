#!/usr/bin/env tsx

import { ClaudeRequestManager } from './managers/request-manager';
import { createHaskellInstruction } from './instructions/builder';
import { ResultValidator } from './types/tests/validator';
import type { ClaudeInstruction } from './types';

/**
 * Test CLI method for Haskell generation
 */
async function testCliMethod(instruction: ClaudeInstruction, baseDir: string, targetFile: string): Promise<void> {
  console.log('🔧 Testing CLI Method');
  console.log('=====================');

  const cliManager = new ClaudeRequestManager({
    method: 'cli',
    workingDir: baseDir,
    outputFormat: 'json',
    model: 'sonnet',
    maxTurns: 10,
    verbose: true,
  });

  try {
    const cliStatus = await cliManager.getStatus();
    console.log('CLI Status:', cliStatus.available ? '✅ Available' : '❌ Not available');

    if (!cliStatus.available) {
      console.log('⚠️  CLI not available, skipping CLI test');
      console.log('Make sure Claude Code CLI is installed and available in PATH');
      return;
    }

    console.log('Executing CLI instruction...');
    const cliResult = await cliManager.executeInstruction(instruction);

    console.log('CLI Result:');
    console.log(`- Success: ${cliResult.response.success}`);
    console.log(`- Execution time: ${cliResult.executionTime}ms`);
    console.log(`- Content length: ${cliResult.response.content?.length ?? 0} chars`);

    if (cliResult.response.error) {
      console.log(`- Error: ${cliResult.response.error}`);
    }

    const validator = new ResultValidator(baseDir);
    const testResult = await validator.validateExecution(cliResult, [targetFile]);

    console.log('\n📊 Validation Results:');
    console.log(`- Overall Score: ${(testResult.overallScore * 100).toFixed(1)}%`);
    console.log(`- Success: ${testResult.success ? '✅ PASS' : '❌ FAIL'}`);
    console.log(`- Validations: ${testResult.validationResults.length}`);

    const report = validator.generateReport(testResult);
    console.log('\n📄 Detailed Report:');
    console.log(report);
  } catch (error) {
    console.error('CLI test failed:', error);
  }
}

/**
 * Test SDK method for Haskell generation
 */
async function testSdkMethod(instruction: ClaudeInstruction, baseDir: string, targetFile: string): Promise<void> {
  console.log('🔧 Testing SDK Method');
  console.log('=====================');

  const sdkManager = new ClaudeRequestManager({
    method: 'sdk',
    workingDir: baseDir,
    maxTurns: 10,
    verbose: true,
    allowedTools: ['Read', 'Write', 'Edit', 'MultiEdit', 'Glob', 'Grep', 'Bash', 'LS'],
  });

  try {
    const sdkStatus = await sdkManager.getStatus();
    console.log('SDK Status:', sdkStatus.available ? '✅ Available' : '❌ Not available');

    if (!sdkStatus.available) {
      console.log('⚠️  SDK not available, skipping SDK test');
      console.log('Note: SDK may not be fully implemented yet');
      return;
    }

    console.log('Executing SDK instruction...');
    const sdkResult = await sdkManager.executeInstruction(instruction);

    console.log('SDK Result:');
    console.log(`- Success: ${sdkResult.response.success}`);
    console.log(`- Execution time: ${sdkResult.executionTime}ms`);
    console.log(`- Content length: ${sdkResult.response.content?.length ?? 0} chars`);
    console.log(`- Messages: ${sdkResult.response.messages?.length ?? 0}`);

    if (sdkResult.response.error) {
      console.log(`- Error: ${sdkResult.response.error}`);
    }

    const validator = new ResultValidator(baseDir);
    const testResult = await validator.validateExecution(sdkResult, [targetFile]);

    console.log('\n📊 Validation Results:');
    console.log(`- Overall Score: ${(testResult.overallScore * 100).toFixed(1)}%`);
    console.log(`- Success: ${testResult.success ? '✅ PASS' : '❌ FAIL'}`);
    console.log(`- Validations: ${testResult.validationResults.length}`);

    const report = validator.generateReport(testResult);
    console.log('\n📄 Detailed Report:');
    console.log(report);
  } catch (error) {
    console.error('SDK test failed:', error);
  }
}

/**
 * Test Haskell code generation using Claude Code
 */
async function testHaskellGeneration() {
  const baseDir = '/home/zzhang/dev/qi/github/mcp-server/qicore-v4';
  const targetFile = 'haskell/QiCore/Core/Cache.hs';

  console.log('🧪 Testing Claude Code Haskell Generation');
  console.log('==========================================');
  console.log(`Base directory: ${baseDir}`);
  console.log(`Target file: ${targetFile}`);
  console.log('');

  const instruction = createHaskellInstruction(targetFile, baseDir);

  console.log('📋 Generated instruction preview:');
  console.log('Knowledge updates:', instruction.knowledgeUpdate.length);
  console.log('Quality standards:', instruction.qualityStandards.length);
  console.log('Validation criteria:', instruction.validationCriteria.length);
  console.log('');

  await testCliMethod(instruction, baseDir, targetFile);
  console.log('\n');
  await testSdkMethod(instruction, baseDir, targetFile);

  console.log('\n');
  console.log('🏁 Test Completed');
  console.log('=================');
  console.log('Check the generated files and compare with existing QiCore base components:');
  console.log(`- Target: ${targetFile}`);
  console.log('- Compare with: haskell/QiCore/Base/Result.hs');
  console.log('- Compare with: haskell/QiCore/Base/Error.hs');
}

/**
 * Test multiple code generation scenarios
 */
async function testMultipleScenarios() {
  console.log('🧪 Testing Multiple Generation Scenarios');
  console.log('=========================================');

  const baseDir = '/home/zzhang/dev/qi/github/mcp-server/qicore-v4';
  const scenarios = [
    {
      name: 'Cache Module',
      targetFile: 'haskell/QiCore/Core/Cache.hs',
      description: 'Generate comprehensive Cache module with TTL and LRU support',
    },
    {
      name: 'Logger Module Enhancement',
      targetFile: 'haskell/QiCore/Core/Logger.hs',
      description: 'Enhance existing Logger with structured logging',
    },
  ];

  const manager = new ClaudeRequestManager({
    method: 'cli',
    workingDir: baseDir,
    outputFormat: 'text',
    model: 'sonnet',
    maxTurns: 8,
    verbose: false,
  });

  const validator = new ResultValidator(baseDir);
  const results = [];

  for (const scenario of scenarios) {
    console.log(`\n📋 Scenario: ${scenario.name}`);
    console.log(`Description: ${scenario.description}`);

    try {
      const instruction = createHaskellInstruction(scenario.targetFile, baseDir);
      const result = await manager.executeInstruction(instruction);
      const testResult = await validator.validateExecution(result, [scenario.targetFile]);

      results.push({
        scenario: scenario.name,
        result: testResult,
        success: testResult.success,
        score: testResult.overallScore,
      });

      console.log(`✅ Completed - Score: ${(testResult.overallScore * 100).toFixed(1)}%`);
    } catch (error) {
      console.log(`❌ Failed: ${error}`);
      results.push({
        scenario: scenario.name,
        result: null,
        success: false,
        score: 0,
      });
    }
  }

  // Summary
  console.log('\n📊 Summary Report');
  console.log('=================');

  const totalScore = results.reduce((sum, r) => sum + r.score, 0) / results.length;
  const successCount = results.filter((r) => r.success).length;

  console.log(
    `Overall Success Rate: ${successCount}/${results.length} (${((successCount / results.length) * 100).toFixed(1)}%)`
  );
  console.log(`Average Score: ${(totalScore * 100).toFixed(1)}%`);

  for (const result of results) {
    const status = result.success ? '✅' : '❌';
    console.log(`${status} ${result.scenario}: ${(result.score * 100).toFixed(1)}%`);
  }
}

// Main execution
async function main() {
  const args = process.argv.slice(2);

  if (args.includes('--multiple')) {
    await testMultipleScenarios();
  } else {
    await testHaskellGeneration();
  }
}

// Run if called directly
if (require.main === module) {
  main().catch(console.error);
}

export { testHaskellGeneration, testMultipleScenarios };
