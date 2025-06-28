import type { StudyConfig } from '@/types/study';

export const defaultStudyConfig: StudyConfig = {
  name: 'AI Code Generation Consistency Study 2025',
  description:
    'One-day study measuring AI consistency with modern YAML instructions using Bun + TypeScript stack',
  models: [
    {
      id: 'gpt4-turbo',
      name: 'GPT-4 Turbo',
      provider: 'openai',
      modelName: 'gpt-4-turbo-preview',
      temperature: 0.1,
      maxTokens: 4000,
    },
    {
      id: 'claude-3-5-sonnet',
      name: 'Claude 3.5 Sonnet',
      provider: 'anthropic',
      modelName: 'claude-3-5-sonnet-20241022',
      temperature: 0.1,
      maxTokens: 4000,
    },
  ],
  instructions: [
    {
      id: 'modern-yaml',
      name: 'Modern Haskell (Knowledge-First)',
      filePath: '../../docs/experiment/sources/impl/base.hs.modern.yaml',
      content: '', // Will be loaded dynamically
      category: 'modern',
    },
    {
      id: 'simple-yaml',
      name: 'Simple Haskell (Traditional)',
      filePath: '../../docs/experiment/sources/impl/base.hs.simple.yaml',
      content: '', // Will be loaded dynamically
      category: 'simple',
    },
  ],
  runsPerCombination: 10,
  outputDir: './study-results',
  timeout: 120000, // 2 minutes per generation
};

// Environment-specific overrides
export const getStudyConfig = (): StudyConfig => {
  const config = { ...defaultStudyConfig };

  // Load from environment variables if available
  if (
    process.env.STUDY_RUNS_PER_COMBINATION !== undefined &&
    process.env.STUDY_RUNS_PER_COMBINATION !== ''
  ) {
    config.runsPerCombination = Number.parseInt(process.env.STUDY_RUNS_PER_COMBINATION, 10);
  }

  if (process.env.STUDY_OUTPUT_DIR !== undefined && process.env.STUDY_OUTPUT_DIR !== '') {
    config.outputDir = process.env.STUDY_OUTPUT_DIR;
  }

  if (process.env.STUDY_TIMEOUT !== undefined && process.env.STUDY_TIMEOUT !== '') {
    config.timeout = Number.parseInt(process.env.STUDY_TIMEOUT, 10);
  }

  return config;
};

// Quick study config for testing
export const quickStudyConfig: StudyConfig = {
  ...defaultStudyConfig,
  name: 'Quick AI Consistency Test',
  runsPerCombination: 3,
  timeout: 60000, // 1 minute
};
