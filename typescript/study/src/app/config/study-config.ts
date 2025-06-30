import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import type { StudyConfig } from '@/types/study';
import yaml from 'js-yaml';

// Load YAML configuration
export const loadStudyConfig = async (configPath?: string): Promise<StudyConfig> => {
  const yamlPath = configPath ?? resolve(process.cwd(), 'config/haskell-consistency-study.yaml');

  try {
    const yamlContent = readFileSync(yamlPath, 'utf8');
    const config = yaml.load(yamlContent) as StudyConfig;

    // Load instruction file contents
    for (const instruction of config.instructions) {
      if (instruction.filePath) {
        const instructionPath = resolve(process.cwd(), instruction.filePath);
        try {
          instruction.content = readFileSync(instructionPath, 'utf8');
        } catch (error) {
          // eslint-disable-next-line no-console
          console.warn(`Warning: Could not load instruction file ${instructionPath}:`, error);
          instruction.content = `# Placeholder for ${instruction.name}\n\nInstruction file not found.`;
        }
      }
    }

    return config;
  } catch (error) {
    // eslint-disable-next-line no-console
    console.error(`Error loading study config from ${yamlPath}:`, error);
    throw new Error(
      `Failed to load study configuration: ${error instanceof Error ? error.message : String(error)}`
    );
  }
};

// Fallback default config (for backwards compatibility and testing)
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
      id: 'basic-module',
      name: 'Basic Haskell Module',
      filePath: './instructions/haskell-basic-module.md',
      // biome-ignore lint/nursery/noSecrets: This is a placeholder content string, not a secret
      content: '# Basic Haskell Module\n\nPlaceholder instruction',
      category: 'simple',
    },
    {
      id: 'advanced-types',
      name: 'Advanced Haskell Types',
      filePath: './instructions/haskell-advanced-types.md',
      // biome-ignore lint/nursery/noSecrets: This is a placeholder content string, not a secret
      content: '# Advanced Haskell Types\n\nPlaceholder instruction',
      category: 'modern',
    },
  ],
  runsPerCombination: 5,
  outputDir: './results/haskell-study',
  timeout: 30000,
};

// Environment-specific overrides
export const getStudyConfig = async (configPath?: string): Promise<StudyConfig> => {
  let config: StudyConfig;

  try {
    // Try to load from YAML first
    config = await loadStudyConfig(configPath);
    // eslint-disable-next-line no-console
    console.log(`✓ Loaded study configuration from YAML: ${config.name}`);
  } catch (_error) {
    // eslint-disable-next-line no-console
    console.warn('Warning: Could not load YAML config, using default config');
    config = { ...defaultStudyConfig };
  }

  // Load from environment variables if available (for CI/CD overrides)
  if (
    process.env.STUDY_RUNS_PER_COMBINATION !== undefined &&
    process.env.STUDY_RUNS_PER_COMBINATION !== ''
  ) {
    config.runsPerCombination = Number.parseInt(process.env.STUDY_RUNS_PER_COMBINATION, 10);
    // eslint-disable-next-line no-console
    console.log(`✓ Override: runs per combination = ${config.runsPerCombination}`);
  }

  if (process.env.STUDY_OUTPUT_DIR !== undefined && process.env.STUDY_OUTPUT_DIR !== '') {
    config.outputDir = process.env.STUDY_OUTPUT_DIR;
    // eslint-disable-next-line no-console
    console.log(`✓ Override: output directory = ${config.outputDir}`);
  }

  if (process.env.STUDY_TIMEOUT !== undefined && process.env.STUDY_TIMEOUT !== '') {
    config.timeout = Number.parseInt(process.env.STUDY_TIMEOUT, 10);
    // eslint-disable-next-line no-console
    console.log(`✓ Override: timeout = ${config.timeout}ms`);
  }

  return config;
};

// Quick study config for testing (loads from YAML but overrides certain values)
export const getQuickStudyConfig = async (configPath?: string): Promise<StudyConfig> => {
  const config = await getStudyConfig(configPath);

  return {
    ...config,
    name: `Quick Test - ${config.name}`,
    runsPerCombination: 2, // Reduce for quick testing
    timeout: 15000, // 15 seconds for quick testing
  };
};
