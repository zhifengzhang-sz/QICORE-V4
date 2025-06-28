#!/usr/bin/env bun

import { getStudyConfig, quickStudyConfig } from './config/study-config';
import { StudyDatabase } from './database/study-db';
import { StudyRunner } from './generators/study-runner';

const BANNER = `
+----------------------------------------------------------+
|  🚀 AI Code Generation Consistency Study (Bun + TypeScript) |
|                                                          |
|  Measuring AI consistency with modern YAML instructions  |
|  Built with 2024-2025 best practices                    |
+----------------------------------------------------------+
`;

async function main() {
  console.log(BANNER);

  const args = process.argv.slice(2);
  const isQuickRun = args.includes('--quick');
  const isAnalyzeOnly = args.includes('--analyze-only');
  const studyId = args.find((arg) => arg.startsWith('--study-id='))?.split('=')[1];

  try {
    const config = isQuickRun ? quickStudyConfig : getStudyConfig();
    const db = new StudyDatabase();
    const runner = new StudyRunner(config, db);

    console.log('📊 Study Configuration:');
    console.log(`   Name: ${config.name}`);
    console.log(`   Models: ${config.models.map((m) => m.name).join(', ')}`);
    console.log(`   Instructions: ${config.instructions.map((i) => i.name).join(', ')}`);
    console.log(`   Runs per combination: ${config.runsPerCombination}`);
    console.log(
      `   Total generations: ${config.models.length * config.instructions.length * config.runsPerCombination}`
    );
    console.log();

    if (isAnalyzeOnly && studyId !== undefined && studyId !== '') {
      console.log(`🔍 Analyzing existing study: ${studyId}`);
      await runner.analyzeStudy(studyId);
    } else {
      console.log('🏃‍♂️ Starting new study...');
      const newStudyId = await runner.runCompleteStudy();
      console.log(`✅ Study completed! ID: ${newStudyId}`);

      console.log('📈 Generating analysis...');
      await runner.analyzeStudy(newStudyId);
    }

    db.close();
    console.log('🎉 All done!');
  } catch (error) {
    console.error('❌ Study failed:', error);
    process.exit(1);
  }
}

// Handle cleanup on exit
process.on('SIGINT', () => {
  console.log('\\n👋 Study interrupted. Cleaning up...');
  process.exit(0);
});

process.on('SIGTERM', () => {
  console.log('\\n👋 Study terminated. Cleaning up...');
  process.exit(0);
});

if (import.meta.main) {
  main().catch(console.error);
}
