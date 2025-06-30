#!/usr/bin/env bun

/* eslint-disable no-console */

import { HaskellImplementationStudy } from './src/app/runners/haskell-implementation-study';
import type { AIModel, InstructionSet } from '@/types/study';

async function main(): Promise<void> {
  const args = (globalThis as { process: { argv: string[] } }).process.argv.slice(2);
  const command = args[0] ?? 'help';

  try {
    const study = new HaskellImplementationStudy();

    switch (command) {
      case 'test':
        await testClaudeCode(study);
        break;
      case 'single':
        await runSingleGeneration(study);
        break;
      default:
        showHelp();
        break;
    }
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

async function testClaudeCode(study: HaskellImplementationStudy): Promise<void> {
  console.log('🧪 Testing Claude Code connection...\n');
  
  try {
    const isAvailable = await study.testClaudeCodeConnection();
    
    if (isAvailable) {
      console.log('✅ Claude Code is ready!');
      console.log('🚀 Run: bun run-haskell-study.ts single');
    } else {
      console.log('❌ Claude Code needs setup');
      console.log('1. Run: claude');
      console.log('2. Re-authenticate');
    }
  } catch (error) {
    console.error('❌ Connection test failed:', error);
    throw error;
  }
}

async function runSingleGeneration(study: HaskellImplementationStudy): Promise<void> {
  console.log('🔄 Running single generation...\n');

  const model: AIModel = {
    id: 'claude-sonnet',
    name: 'Claude 3.5 Sonnet',
    provider: 'anthropic',
    modelName: 'claude-3-5-sonnet-20241022',
    temperature: 0.1,
    maxTokens: 4000,
  };

  const instruction: InstructionSet = {
    id: 'modern-haskell-base',
    name: 'Modern Haskell Base',
    filePath: 'docs/experiment/sources/impl/base.hs.modern.yaml',
    content: 'Implement QiCore Base components using modern Haskell practices',
    category: 'modern',
  };

  try {
    const result = await study.runSingleGeneration(model, instruction);
    console.log(`\n✅ Score: ${result.score}/100`);
    console.log(`📝 Run ID: ${result.runId}`);
    console.log(`📄 Code: ${result.code.length} chars`);
  } catch (error) {
    console.error('❌ Generation failed:', error);
    throw error;
  }
}

function showHelp(): void {
  console.log(`
🔬 Haskell Implementation Study

COMMANDS:
  test     Test Claude Code connection
  single   Run single generation test
  help     Show this help

USAGE:
  bun run-haskell-study.ts test
  bun run-haskell-study.ts single
  `);
}

main().catch((error: unknown) => {
  console.error('❌ Unexpected error:', error);
  process.exit(1);
}); 