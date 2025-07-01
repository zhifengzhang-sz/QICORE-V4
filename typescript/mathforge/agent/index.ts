/**
 * MathForge Agent - Simplified Implementation
 * Basic natural language to code generation without complex dependencies
 */

import { writeFileSync } from "node:fs";
import { join } from "node:path";
import { generateCode, parseSpecContent } from "../index.js";
import type { GeneratedCode, GenerationOptions, TargetLanguage } from "../types/spec.js";

interface MathForgeAgentOptions {
  ollamaEndpoint: string;
  model: string;
  temperature: number;
  outputDir: string;
  timeout: number;
}

/**
 * Simple MathForge Agent for natural language code generation
 */
export class MathForgeAgent {
  private options: MathForgeAgentOptions;

  constructor(options: Partial<MathForgeAgentOptions> = {}) {
    this.options = {
      ollamaEndpoint: "http://localhost:11434",
      model: "qwen3:8b", // Smaller, faster model
      temperature: 0.1,
      outputDir: "./generated",
      timeout: 120000, // 2 minutes
      ...options,
    };
  }

  /**
   * Generate code from natural language description
   */
  async generateFromNaturalLanguage(
    request: string,
    language: TargetLanguage = "typescript",
    options: Partial<GenerationOptions> = {}
  ): Promise<GeneratedCode> {
    console.log("🤖 MathForge Agent: Converting natural language to formal spec...");

    // Step 1: Convert natural language to YAML spec
    const yamlSpec = await this.naturalLanguageToYaml(request);
    console.log("📋 Generated YAML spec");

    // Step 2: Parse and validate the spec
    const { spec, validation } = parseSpecContent(yamlSpec);

    if (!validation.valid) {
      throw new Error(`Spec validation failed: ${validation.errors.map((e) => e.message).join(", ")}`);
    }

    if (!spec) {
      throw new Error("Failed to parse generated specification");
    }

    console.log("✅ Spec validation successful");

    // Step 3: Generate code using MathForge
    const generationOptions: GenerationOptions = {
      target_language: language,
      include_tests: true,
      include_benchmarks: false,
      include_documentation: true,
      strict_mode: true,
      output_path: this.options.outputDir,
      ...options,
    };

    console.log(`🚀 Generating ${language} code...`);
    const result = await generateCode(spec, generationOptions);

    console.log("✨ Code generation completed");
    return result;
  }

  /**
   * Convert natural language to YAML specification
   */
  private async naturalLanguageToYaml(request: string): Promise<string> {
    const prompt = `Convert this to YAML spec:

Request: "${request}"

Format (include ALL required fields):
metadata:
  component: "ComponentName"
  mathematical_foundation: "Either Monad"
  package_strategy: "Use fp-ts for Either"
  laws_verified:
    - "monad.left_identity"
  description: "Brief description"

operations:
  operation_name:
    signature: "Type signature"
    semantics: "What it does mathematically"
    description: "What it does"
    mathematical_laws:
      - "monad.left_identity"
    properties:
      identity:
        formula: "map(id) = id"
        description: "Identity law"

type_definitions:
  TypeName:
    description: "Type description"

generation_targets:
  typescript:
    formal_verification_tools: ["fast-check"]
    package_dependencies: ["fp-ts"]

Respond with ONLY the YAML, no explanations.`;

    try {
      const response = await fetch(`${this.options.ollamaEndpoint}/api/chat`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          model: this.options.model,
          messages: [
            {
              role: "user", 
              content: prompt
            }
          ],
          stream: false,
          format: "json",
          options: {
            temperature: this.options.temperature,
            num_predict: 500,
          },
        }),
        signal: AbortSignal.timeout(this.options.timeout),
      });

      if (!response.ok) {
        throw new Error(`Ollama API error: ${response.status} ${response.statusText}`);
      }

      const data = await response.json() as { message: { content: string } };
      let yamlResponse = data.message.content.trim();
      
      // Clean up common model artifacts
      yamlResponse = yamlResponse
        .replace(/<think>[\s\S]*?<\/think>/gi, '') // Remove thinking tags completely
        .replace(/^```yaml\s*/i, '') // Remove yaml code block start
        .replace(/\s*```\s*$/i, '') // Remove code block end
        .trim();
      
      return yamlResponse;
    } catch (error) {
      throw new Error(`Failed to generate YAML spec: ${error}`);
    }
  }

  /**
   * Save generated results to files
   */
  async saveResults(result: GeneratedCode, baseFileName: string): Promise<void> {
    const ext = this.getFileExtension(result.language);
    
    // Save main code
    const mainPath = join(this.options.outputDir, `${baseFileName}.${ext}`);
    writeFileSync(mainPath, result.main_code);
    console.log(`💾 Saved main code: ${mainPath}`);

    // Save test code if available
    if (result.test_code) {
      const testPath = join(this.options.outputDir, `${baseFileName}.test.${ext}`);
      writeFileSync(testPath, result.test_code);
      console.log(`🧪 Saved test code: ${testPath}`);
    }

    // Save documentation if available
    if (result.documentation) {
      const docPath = join(this.options.outputDir, `${baseFileName}.md`);
      writeFileSync(docPath, result.documentation);
      console.log(`📚 Saved documentation: ${docPath}`);
    }
  }

  private getFileExtension(language: TargetLanguage): string {
    switch (language) {
      case "typescript":
        return "ts";
      case "python":
        return "py";
      case "haskell":
        return "hs";
      default:
        return "txt";
    }
  }
}

/**
 * Quick helper function for one-off generation
 */
export async function quickAgent(
  request: string,
  language: TargetLanguage = "typescript",
  agentOptions: Partial<MathForgeAgentOptions> = {}
): Promise<GeneratedCode> {
  const agent = new MathForgeAgent(agentOptions);
  return agent.generateFromNaturalLanguage(request, language);
}

/**
 * CLI runner function
 */
export async function runAgentCLI(args: string[]): Promise<void> {
  if (args.length === 0 || args.includes("--help") || args.includes("-h")) {
    console.log(`
🤖 MathForge Agent - Natural Language Code Generator

Usage:
  bun run agent <request> [options]

Arguments:
  <request>     Natural language description of what to generate

Options:
  --language    Target language (typescript, python, haskell) [default: typescript]
  --model       Ollama model to use [default: qwen3:8b]
  --output      Output directory [default: ./generated]
  --temp        Temperature (0.0-1.0) [default: 0.1]
  --help, -h    Show this help message

Examples:
  bun run agent "Create a Result type for error handling"
  bun run agent "Build a configuration loader with validation" --language python
  bun run agent "Make a cache with TTL support" --output ./cache-impl
`);
    return;
  }

  const request = args[0];
  if (!request) {
    console.error("❌ Error: Please provide a natural language request");
    return;
  }

  // Parse options
  const language = (args.includes("--language") 
    ? args[args.indexOf("--language") + 1] || "typescript"
    : "typescript") as TargetLanguage;
  
  const model = args.includes("--model") 
    ? args[args.indexOf("--model") + 1] || "qwen3:8b"
    : "qwen3:8b";
  
  const outputDir = args.includes("--output") 
    ? args[args.indexOf("--output") + 1] || "./generated"
    : "./generated";
  
  const temperature = args.includes("--temp") 
    ? parseFloat(args[args.indexOf("--temp") + 1] || "0.1")
    : 0.1;

  console.log(`🚀 MathForge Agent Starting...`);
  console.log(`📝 Request: "${request}"`);
  console.log(`🎯 Language: ${language}`);
  console.log(`🧠 Model: ${model}`);
  console.log(`📁 Output: ${outputDir}`);

  try {
    const agent = new MathForgeAgent({
      model,
      outputDir,
      temperature,
    });

    const result = await agent.generateFromNaturalLanguage(request, language);
    
    // Save results
    const timestamp = new Date().toISOString().replace(/[:.]/g, "-");
    const baseFileName = `mathforge-${timestamp}`;
    await agent.saveResults(result, baseFileName);

    console.log("✅ Generation completed successfully!");
    console.log(`📊 Generated ${result.metadata.lines_of_code} lines of code`);
    console.log(`📁 Files saved to: ${outputDir}/`);
    
  } catch (error) {
    console.error("❌ Generation failed:", error);
    process.exit(1);
  }
}

// Run CLI if called directly
if (import.meta.main) {
  const args = process.argv.slice(2);
  await runAgentCLI(args);
} 