/**
 * MathForge Enterprise Agent v2.0
 * 
 * Production-ready agent integrating:
 * - LM Studio SDK for professional local LLM access
 * - QiCore for enterprise-grade error handling, logging, and performance monitoring
 * - Fallback to Ollama for compatibility
 * 
 * Features:
 * - Result<T> types instead of exceptions
 * - Structured logging with context
 * - Performance monitoring and benchmarking
 * - Circuit breaker pattern (future: QiAgent integration)
 * - Professional error handling and categorization
 */

import { LMStudioClient } from "@lmstudio/sdk";
import {
  type Result,
  type QiError,
  success,
  failure,
  createQiError,
  fromAsyncTryCatch,
  isSuccess,
  isFailure,
  match,
  createLogger,
  LogLevel,
  type Logger,
  measureAsync,
  getPerformanceStats,
  type PerformanceMeasurement,
} from "qicore";

// ============================================================================
// Types and Configuration
// ============================================================================

export interface EnterpriseAgentConfig {
  // LLM Provider Configuration
  provider: "lmstudio" | "ollama" | "auto";
  
  // LM Studio Configuration
  lmstudio: {
    baseUrl?: string;
    timeout?: number;
    maxRetries?: number;
  };
  
  // Ollama Configuration (fallback)
  ollama: {
    baseUrl: string;
    timeout: number;
    model: string;
    temperature: number;
    maxTokens: number;
  };
  
  // Logging Configuration
  logging: {
    level: "DEBUG" | "INFO" | "WARN" | "ERROR";
    includeContext: boolean;
    includePerformance: boolean;
  };
  
  // Performance Configuration
  performance: {
    enableMonitoring: boolean;
    enableBenchmarking: boolean;
    slowOperationThreshold: number; // milliseconds
  };
}

export interface GeneratedCode {
  yaml: string;
  typescript: string;
  python?: string;
  haskell?: string;
  tests: string;
  documentation: string;
  metadata: {
    model: string;
    provider: string;
    generationTime: number;
    tokensUsed?: number;
  };
}

// ============================================================================
// Enterprise MathForge Agent
// ============================================================================

export class EnterpriseMathForgeAgent {
  private config: EnterpriseAgentConfig;
  private logger: Logger;
  private lmStudioClient?: LMStudioClient;

  constructor(config: Partial<EnterpriseAgentConfig> = {}) {
    this.config = this.createDefaultConfig(config);
    
    // Create logger with proper error handling
    const loggerResult = createLogger({
      level: this.parseLogLevel(this.config.logging.level),
      enableColors: true,
      enableTimestamp: true,
      timestampFormat: "iso",
      output: "console",
      enableConsole: true,
    });

    this.logger = match(
      (error: QiError) => {
        throw new Error(`Failed to create logger: ${error.message}`);
      },
      (logger: Logger) => logger
    )(loggerResult);

    this.logger.info("Enterprise MathForge Agent initializing", {
      config: {
        provider: this.config.provider,
        lmstudio: this.config.lmstudio,
        performance: this.config.performance,
      },
    });

    // Initialize LM Studio client if using lmstudio provider
    if (this.config.provider === "lmstudio" || this.config.provider === "auto") {
      try {
        this.lmStudioClient = new LMStudioClient({
          baseUrl: this.config.lmstudio.baseUrl,
        });
        this.logger.debug("LM Studio client initialized", {
          baseUrl: this.config.lmstudio.baseUrl,
        });
      } catch (error) {
        this.logger.warn("Failed to initialize LM Studio client", {
          error: String(error),
          fallback: "Will use Ollama",
        });
      }
    }
  }

  // ============================================================================
  // Main Generation Methods
  // ============================================================================

  /**
   * Generate YAML specification from natural language request
   */
  async generateYamlSpec(request: string): Promise<Result<string>> {
    return measureAsync("generateYamlSpec", async () => {
      this.logger.info("Starting YAML spec generation", {
        requestLength: request.length,
        provider: this.config.provider,
      });

      // Try LM Studio first, then fallback to Ollama
      if (this.lmStudioClient && (this.config.provider === "lmstudio" || this.config.provider === "auto")) {
        const lmStudioResult = await this.generateWithLMStudio(request);
        if (lmStudioResult.success) {
          return lmStudioResult;
        }
        
        this.logger.warn("LM Studio failed, falling back to Ollama", {
          error: lmStudioResult.error,
        });
      }

      // Fallback to Ollama
      return this.generateWithOllama(request);
    });
  }

  /**
   * Generate complete code from natural language request
   */
  async generateCode(request: string): Promise<Result<GeneratedCode>> {
    return measureAsync("generateCode", async () => {
      this.logger.info("Starting complete code generation", {
        requestLength: request.length,
      });

      // Step 1: Generate YAML specification
      const specResult = await this.generateYamlSpec(request);
      if (!specResult.success) {
        return failure(specResult.error);
      }

      // Step 2: Generate TypeScript code
      const tsResult = await this.generateTypeScriptFromYaml(specResult.value);
      if (!tsResult.success) {
        return failure(tsResult.error);
      }

      // Step 3: Generate tests
      const testsResult = await this.generateTests(specResult.value, tsResult.value);
      if (!testsResult.success) {
        return failure(testsResult.error);
      }

      // Step 4: Generate documentation
      const docsResult = await this.generateDocumentation(specResult.value, tsResult.value);
      if (!docsResult.success) {
        return failure(docsResult.error);
      }

      const generatedCode: GeneratedCode = {
        yaml: specResult.value,
        typescript: tsResult.value,
        tests: testsResult.value,
        documentation: docsResult.value,
        metadata: {
          model: this.getCurrentModel(),
          provider: this.getCurrentProvider(),
          generationTime: Date.now(),
        },
      };

      this.logger.info("Code generation completed successfully", {
        yamlLength: generatedCode.yaml.length,
        typescriptLength: generatedCode.typescript.length,
        testsLength: generatedCode.tests.length,
        docsLength: generatedCode.documentation.length,
      });

      return success(generatedCode);
    });
  }

  // ============================================================================
  // Provider-Specific Generation Methods
  // ============================================================================

  private async generateWithLMStudio(request: string): Promise<Result<string>> {
    if (!this.lmStudioClient) {
      return failure(createQiError(
        "LMSTUDIO_NOT_INITIALIZED",
        "LM Studio client not initialized",
        "SYSTEM",
        { request }
      ));
    }

    return fromAsyncTryCatch(
      async () => {
        this.logger.debug("Calling LM Studio API", {
          requestLength: request.length,
        });

        // List loaded models to find available model
        const models = await this.lmStudioClient!.llm.listLoaded();
        if (models.length === 0) {
          throw new Error("No models loaded in LM Studio");
        }

        // Use first available model
        const model = await this.lmStudioClient!.llm.model("llama-3.2-1b-instruct");
        const response = await model.respond(this.createYamlPrompt(request));

        this.logger.debug("LM Studio API call successful", {
          responseLength: response.content.length,
        });

        return response.content.trim();
      },
      (error) => createQiError(
        "LMSTUDIO_API_ERROR",
        `LM Studio API error: ${String(error)}`,
        "NETWORK",
        { request, error: String(error) }
      )
    );
  }

  private async generateWithOllama(request: string): Promise<Result<string>> {
    return fromAsyncTryCatch(
      async () => {
        this.logger.debug("Calling Ollama API", {
          baseUrl: this.config.ollama.baseUrl,
          model: this.config.ollama.model,
          requestLength: request.length,
        });

        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), this.config.ollama.timeout);

        const response = await fetch(`${this.config.ollama.baseUrl}/api/generate`, {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify({
            model: this.config.ollama.model,
            prompt: this.createYamlPrompt(request),
            stream: false,
            options: {
              temperature: this.config.ollama.temperature,
              num_predict: this.config.ollama.maxTokens,
            },
          }),
          signal: controller.signal,
        });

        clearTimeout(timeoutId);

        if (!response.ok) {
          throw new Error(`Ollama API error: HTTP ${response.status}`);
        }

        const data = await response.json() as { response: string };
        
        this.logger.debug("Ollama API call successful", {
          model: this.config.ollama.model,
          responseLength: data.response.length,
        });

        return data.response.trim();
      },
      (error) => createQiError(
        "OLLAMA_API_ERROR",
        `Ollama API error: ${String(error)}`,
        "NETWORK",
        { request, error: String(error) }
      )
    );
  }

  // ============================================================================
  // Code Generation Methods
  // ============================================================================

  private async generateTypeScriptFromYaml(yamlSpec: string): Promise<Result<string>> {
    const prompt = `Convert this YAML specification into TypeScript code with fp-ts Result types:

${yamlSpec}

Generate clean, production-ready TypeScript code with:
- Proper imports from fp-ts
- Result<T> return types
- Comprehensive error handling
- JSDoc documentation
- Type safety throughout

Respond with ONLY the TypeScript code, no explanations.`;

    return this.generateWithCurrentProvider(prompt);
  }

  private async generateTests(yamlSpec: string, tsCode: string): Promise<Result<string>> {
    const prompt = `Generate comprehensive Vitest tests for this TypeScript code:

YAML Spec:
${yamlSpec}

TypeScript Code:
${tsCode}

Generate tests that:
- Use Vitest framework
- Test all public methods
- Include edge cases and error conditions
- Use fast-check for property-based testing
- Follow AAA pattern (Arrange, Act, Assert)

Respond with ONLY the test code, no explanations.`;

    return this.generateWithCurrentProvider(prompt);
  }

  private async generateDocumentation(yamlSpec: string, tsCode: string): Promise<Result<string>> {
    const prompt = `Generate comprehensive documentation for this TypeScript code:

YAML Spec:
${yamlSpec}

TypeScript Code:
${tsCode}

Generate documentation that includes:
- Overview and purpose
- API reference with examples
- Mathematical foundations
- Usage patterns
- Performance considerations

Use markdown format. Respond with ONLY the documentation, no explanations.`;

    return this.generateWithCurrentProvider(prompt);
  }

  // ============================================================================
  // Utility Methods
  // ============================================================================

  private async generateWithCurrentProvider(prompt: string): Promise<Result<string>> {
    if (this.lmStudioClient && (this.config.provider === "lmstudio" || this.config.provider === "auto")) {
      const result = await this.generateWithLMStudio(prompt);
      if (result.success) {
        return result;
      }
    }
    return this.generateWithOllama(prompt);
  }

  private createYamlPrompt(request: string): string {
    return `Convert this natural language request into a formal YAML specification for mathematical code generation.

Request: "${request}"

Generate a YAML specification following this format:

metadata:
  component: "ComponentName"
  mathematical_foundation: "Either Monad" # or other mathematical concept
  package_strategy: "Use fp-ts for Either, custom implementation for domain logic"
  laws_verified:
    - "monad.left_identity"
    - "monad.right_identity"
    - "monad.associativity"
  description: "Brief description"

operations:
  operation_name:
    signature: "Type signature"
    semantics: "What it does mathematically"  
    description: "Human readable description"
    mathematical_laws:
      - "functor.identity"
    examples:
      - input: "success(42)"
        output: "Right(42)"

type_definitions:
  Result:
    description: "Either type for error handling"
  QiError:
    description: "Structured error type"

generation_targets:
  typescript:
    formal_verification_tools: ["zod", "fast-check"]
    package_dependencies: ["fp-ts"]

Respond with ONLY the YAML specification, no explanations or markdown formatting.`;
  }

  private getCurrentModel(): string {
    return this.config.provider === "lmstudio" ? "lmstudio-model" : this.config.ollama.model;
  }

  private getCurrentProvider(): string {
    return this.lmStudioClient ? "lmstudio" : "ollama";
  }

  private parseLogLevel(level: string): LogLevel {
    switch (level.toUpperCase()) {
      case "DEBUG": return LogLevel.DEBUG;
      case "INFO": return LogLevel.INFO;
      case "WARN": return LogLevel.WARN;
      case "ERROR": return LogLevel.ERROR;
      default: return LogLevel.INFO;
    }
  }

  private createDefaultConfig(partial: Partial<EnterpriseAgentConfig>): EnterpriseAgentConfig {
    return {
      provider: "auto",
      lmstudio: {
        baseUrl: "ws://localhost:1234",
        timeout: 30000,
        maxRetries: 3,
        ...partial.lmstudio,
      },
      ollama: {
        baseUrl: "http://localhost:11434",
        timeout: 30000,
        model: "qwen2.5-coder:7b",
        temperature: 0.1,
        maxTokens: 4000,
        ...partial.ollama,
      },
      logging: {
        level: "INFO",
        includeContext: true,
        includePerformance: true,
        ...partial.logging,
      },
      performance: {
        enableMonitoring: true,
        enableBenchmarking: true,
        slowOperationThreshold: 5000,
        ...partial.performance,
      },
      ...partial,
    };
  }

  // ============================================================================
  // Health Check and Diagnostics
  // ============================================================================

  async healthCheck(): Promise<Result<{
    lmstudio: boolean;
    ollama: boolean;
    performance: PerformanceMeasurement[];
  }>> {
    this.logger.info("Running health check");

    const lmstudioHealthy = await this.checkLMStudioHealth();
    const ollamaHealthy = await this.checkOllamaHealth();
    const performanceStats = getPerformanceStats();

    const health = {
      lmstudio: lmstudioHealthy,
      ollama: ollamaHealthy,
      performance: Object.values(performanceStats),
    };

    this.logger.info("Health check completed", health);
    return success(health);
  }

  private async checkLMStudioHealth(): Promise<boolean> {
    if (!this.lmStudioClient) return false;
    
    try {
      await this.lmStudioClient.llm.listLoaded();
      return true;
    } catch {
      return false;
    }
  }

  private async checkOllamaHealth(): Promise<boolean> {
    try {
      const response = await fetch(`${this.config.ollama.baseUrl}/api/tags`, {
        signal: AbortSignal.timeout(5000),
      });
      return response.ok;
    } catch {
      return false;
    }
  }
}

// ============================================================================
// Demo and Testing
// ============================================================================

export async function demoEnterpriseAgent() {
  console.log("🚀 Enterprise MathForge Agent Demo\n");

  const agent = new EnterpriseMathForgeAgent({
    provider: "auto",
    logging: {
      level: "DEBUG",
      includeContext: true,
      includePerformance: true,
    },
    performance: {
      enableMonitoring: true,
      enableBenchmarking: true,
      slowOperationThreshold: 3000,
    },
  });

  // Health check
  const healthResult = await agent.healthCheck();
  if (healthResult.success) {
    console.log("✅ Health check passed");
    console.log(`   LM Studio: ${healthResult.value.lmstudio ? '✅' : '❌'}`);
    console.log(`   Ollama: ${healthResult.value.ollama ? '✅' : '❌'}`);
  }

  // Test YAML generation
  const request = "Create a Result type with map and flatMap operations";
  console.log(`\n🎯 Testing YAML generation: "${request}"`);
  
  const yamlResult = await agent.generateYamlSpec(request);
  if (yamlResult.success) {
    console.log("✅ YAML generation successful");
    console.log(`📄 Generated ${yamlResult.value.length} characters`);
  } else {
    console.log("❌ YAML generation failed:", yamlResult.error.message);
  }

  // Test complete code generation
  console.log(`\n🏗️  Testing complete code generation`);
  const codeResult = await agent.generateCode(request);
  if (codeResult.success) {
    console.log("✅ Complete code generation successful");
    console.log(`📊 Results:`);
    console.log(`   YAML: ${codeResult.value.yaml.length} chars`);
    console.log(`   TypeScript: ${codeResult.value.typescript.length} chars`);
    console.log(`   Tests: ${codeResult.value.tests.length} chars`);
    console.log(`   Docs: ${codeResult.value.documentation.length} chars`);
    console.log(`   Provider: ${codeResult.value.metadata.provider}`);
    console.log(`   Model: ${codeResult.value.metadata.model}`);
  } else {
    console.log("❌ Complete code generation failed:", codeResult.error.message);
  }

  // Performance report
  const perfStats = getPerformanceStats();
  console.log(`\n📊 Performance Report:`);
  Object.entries(perfStats).forEach(([operation, stats]) => {
    console.log(`   ${operation}: ${stats.count} calls, avg ${stats.average.toFixed(0)}ms`);
  });

  console.log("\n🎉 Enterprise Agent Demo Complete!");
}

// Run demo if this file is executed directly
if (import.meta.main) {
  demoEnterpriseAgent().catch(console.error);
} 