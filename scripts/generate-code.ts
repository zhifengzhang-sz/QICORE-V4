#!/usr/bin/env bun
/**
 * QiCore v4.0 Source Code Generation Script
 * Implements template selection logic from source-code-generation.yaml
 */

import { existsSync } from 'fs';
import { resolve, join } from 'path';
import yaml from 'js-yaml';
import { readFileSync } from 'fs';

interface GenerationConfig {
  language: string;
  basePaths: string[];
  outputDir: string;
  dryRun?: boolean;
}

interface TemplateInfo {
  path: string;
  type: 'corrected' | 'original';
  exists: boolean;
  priority: number;
}

class CodeGenerator {
  private config: any;
  
  constructor(configPath: string) {
    const configContent = readFileSync(configPath, 'utf-8');
    this.config = yaml.load(configContent);
  }

  /**
   * Core template selection logic
   * Priority: corrected template > original template > error
   */
  selectTemplate(language: string, basePaths: string[] = []): TemplateInfo {
    const paths = basePaths.length > 0 ? basePaths : this.config.template_selection.base_paths;
    
    console.log(`🔍 Selecting template for language: ${language}`);
    
    for (const basePath of paths) {
      const resolvedBasePath = resolve(basePath);
      
      // Priority 1: Check for corrected template
      const correctedTemplate = join(resolvedBasePath, `qi.v4.${language}.template.corrected.md`);
      if (existsSync(correctedTemplate)) {
        console.log(`✅ Found corrected template: ${correctedTemplate}`);
        return {
          path: correctedTemplate,
          type: 'corrected',
          exists: true,
          priority: 1
        };
      }
      
      // Priority 2: Check for original template
      const originalTemplate = join(resolvedBasePath, `qi.v4.${language}.template.md`);
      if (existsSync(originalTemplate)) {
        console.log(`📄 Found original template: ${originalTemplate}`);
        return {
          path: originalTemplate,
          type: 'original', 
          exists: true,
          priority: 2
        };
      }
    }
    
    throw new Error(`❌ No templates found for language: ${language} in paths: ${paths.join(', ')}`);
  }

  /**
   * Generate code using selected template
   */
  async generate(config: GenerationConfig): Promise<void> {
    console.log(`🚀 Starting code generation for ${config.language}`);
    
    try {
      // Step 1: Template Resolution
      const template = this.selectTemplate(config.language, config.basePaths);
      
      // Step 2: Environment Setup
      await this.setupEnvironment(config.language);
      
      // Step 3: Code Generation  
      await this.generateCode(template, config);
      
      // Step 4: Quality Validation
      await this.validateQuality(config.language);
      
      // Step 5: Template Feedback (if issues found)
      await this.collectFeedback(template, config.language);
      
      console.log(`✅ Code generation completed successfully for ${config.language}`);
      
    } catch (error) {
      console.error(`❌ Code generation failed:`, error);
      throw error;
    }
  }

  private async setupEnvironment(language: string): Promise<void> {
    console.log(`🔧 Setting up ${language} environment...`);
    
    const langConfig = this.config.languages.find((l: any) => l.name === language);
    if (!langConfig) {
      throw new Error(`Unsupported language: ${language}`);
    }
    
    console.log(`   Runtime: ${langConfig.runtime}`);
    console.log(`   Test Framework: ${langConfig.test_framework}`);
    console.log(`   Linter: ${langConfig.linter}`);
    console.log(`   Type Checker: ${langConfig.type_checker}`);
  }

  private async generateCode(template: TemplateInfo, config: GenerationConfig): Promise<void> {
    console.log(`📝 Generating code using ${template.type} template...`);
    
    if (config.dryRun) {
      console.log(`   [DRY RUN] Would generate code from: ${template.path}`);
      console.log(`   [DRY RUN] Output directory: ${config.outputDir}`);
      return;
    }
    
    // Here you would implement the actual code generation logic
    // This could involve:
    // - Reading the template file
    // - Processing template directives  
    // - Generating component files
    // - Setting up project structure
    
    console.log(`   Template: ${template.path}`);
    console.log(`   Priority: ${template.priority} (${template.type})`);
    console.log(`   Output: ${config.outputDir}`);
  }

  private async validateQuality(language: string): Promise<void> {
    console.log(`🔍 Validating code quality...`);
    
    const criteria = this.config.success_criteria;
    
    console.log(`   Target compilation errors: ${criteria.compilation.errors}`);
    console.log(`   Target test pass rate: ${criteria.testing.pass_rate}`);
    console.log(`   Target linting errors: ${criteria.linting.errors}`);
    console.log(`   Required operations: ${criteria.mathematical_contracts.operations_total}`);
  }

  private async collectFeedback(template: TemplateInfo, language: string): Promise<void> {
    console.log(`📊 Collecting template feedback...`);
    
    // This would collect metrics like:
    // - Compilation errors encountered
    // - Test failures
    // - Time to completion
    // - Solutions applied
    
    if (template.type === 'original') {
      console.log(`   ℹ️  Consider creating corrected template: qi.v4.${language}.template.corrected.md`);
    } else {
      console.log(`   ✅ Using empirically validated corrected template`);
    }
  }

  /**
   * List available templates for all languages
   */
  listTemplates(): void {
    console.log(`📋 Available templates:`);
    
    for (const language of this.config.languages) {
      console.log(`\n${language.name.toUpperCase()}:`);
      
      try {
        const template = this.selectTemplate(language.name);
        console.log(`   ✅ ${template.type}: ${template.path}`);
      } catch (error) {
        console.log(`   ❌ No templates found`);
      }
    }
  }
}

// CLI Interface
async function main() {
  const args = process.argv.slice(2);
  const command = args[0];
  
  const configPath = resolve(__dirname, '../docs/sources/agent/build/source-code-generation.yaml');
  const generator = new CodeGenerator(configPath);
  
  switch (command) {
    case 'list':
      generator.listTemplates();
      break;
      
    case 'generate':
      const language = args[1];
      if (!language) {
        console.error('Usage: bun generate-code.ts generate <language> [output-dir]');
        process.exit(1);
      }
      
      const outputDir = args[2] || `./generated-${language}`;
      
      await generator.generate({
        language,
        basePaths: [],
        outputDir,
        dryRun: args.includes('--dry-run')
      });
      break;
      
    case 'typescript':
    case 'ts':
      await generator.generate({
        language: 'typescript',
        basePaths: ['./docs/build/impl'],
        outputDir: './typescript',
        dryRun: args.includes('--dry-run')
      });
      break;
      
    case 'python':
    case 'py':
      await generator.generate({
        language: 'python', 
        basePaths: ['./docs/build/impl'],
        outputDir: './python',
        dryRun: args.includes('--dry-run')
      });
      break;
      
    default:
      console.log(`QiCore v4.0 Code Generator
      
Usage:
  bun generate-code.ts list                    # List available templates
  bun generate-code.ts generate <lang> [dir]   # Generate code for language
  bun generate-code.ts typescript [--dry-run]  # Generate TypeScript (uses corrected template)
  bun generate-code.ts python [--dry-run]      # Generate Python
  
Examples:
  bun generate-code.ts list
  bun generate-code.ts typescript --dry-run
  bun generate-code.ts generate rust ./rust-output
      `);
      break;
  }
}

if (import.meta.main) {
  main().catch(console.error);
}

export { CodeGenerator };