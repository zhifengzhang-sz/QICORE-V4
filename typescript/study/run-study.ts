#!/usr/bin/env bun

/* eslint-disable @typescript-eslint/no-unsafe-assignment */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/strict-boolean-expressions */
/* eslint-disable @typescript-eslint/prefer-nullish-coalescing */
/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable sort-imports */

import { createDefaultAppConfig, createStudyOrchestrator } from './src/app/study-orchestrator';
import type { StudyConfig } from './src/app/types/study';
import { isSuccess, isFailure, getData, getError } from '@/qicore/base/result';

async function main(): Promise<void> {
  try {
    const appConfig = createDefaultAppConfig();
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const orchestrator = createStudyOrchestrator(appConfig) as any;
    
    // Mock configuration for now (would load from file)  
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const config: any = {
      name: 'Test Study',
      description: 'Testing the framework',
      models: [],
      instructions: [],
      runsPerCombination: 1,
      outputDir: './results',
      timeout: 30000,
    };
    
    console.log('🚀 Starting Study Orchestrator...');
    console.log(`📋 Study: ${config.name}`);
    console.log(`🎯 Target: ${config.description}`);
    
    // Run the study
    const studyResult = await orchestrator(config);
    
    if (isSuccess(studyResult)) {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const data = getData(studyResult) as any;
      console.log(`✅ Study completed successfully! ID: ${data.id}`);
      console.log(`📊 Total generations: ${data.statistics?.totalGenerations || 0}`);
      console.log(`📈 Success rate: ${((data.statistics?.successRate || 0) * 100).toFixed(1)}%`);
    } else {
      const error = getError(studyResult);
      console.error('❌ Study failed:', error?.message || 'Unknown error');
      process.exit(1);
    }
    
  } catch (error) {
    console.error('❌ Study failed:', error);
    process.exit(1);
  }
}

main().catch((error: unknown) => {
  console.error('❌ Unexpected error:', error);
  process.exit(1);
}); 