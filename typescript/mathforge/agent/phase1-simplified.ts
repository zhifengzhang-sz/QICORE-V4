/**
 * MathForge Agent - Phase 1 Simplified: QiCore Concepts Demo
 * 
 * Demonstrating the benefits of using QiCore patterns:
 * - Result<T> types instead of throwing exceptions
 * - Structured logging instead of console.log
 * - Configuration management
 * - Proper error categorization
 */

import { generateCode, parseSpecContent } from "../index.js";
import type { GeneratedCode, GenerationOptions, TargetLanguage } from "../types/spec.js";

// ============================================================================
// SIMPLIFIED QICORE-STYLE RESULT TYPE
// ============================================================================

type ErrorCategory = "NETWORK" | "BUSINESS" | "SYSTEM" | "VALIDATION";

interface QiError {
  readonly code: string;
  readonly message: string;
  readonly category: ErrorCategory;
  readonly context?: Record<string, unknown>;
  readonly timestamp: number;
}

type Result<T> = 
  | { readonly success: true; readonly value: T }
  | { readonly success: false; readonly error: QiError };

const success = <T>(value: T): Result<T> => ({ success: true, value });

const failure = <T>(error: QiError): Result<T> => ({ success: false, error });

const isFailure = <T>(result: Result<T>): result is { success: false; error: QiError } => 
  !result.success;

const createQiError = (
  code: string,
  message: string,
  category: ErrorCategory,
  context?: Record<string, unknown>
): QiError => ({
  code,
  message,
  category,
  context,
  timestamp: Date.now(),
});

// ============================================================================
// SIMPLIFIED STRUCTURED LOGGER
// ============================================================================

type LogLevel = "DEBUG" | "INFO" | "WARN" | "ERROR";

interface LogContext {
  [key: string]: unknown;
}

class StructuredLogger {
  constructor(private level: LogLevel = "INFO") {}

  private shouldLog(level: LogLevel): boolean {
    const levels = { DEBUG: 0, INFO: 1, WARN: 2, ERROR: 3 };
    return levels[level] >= levels[this.level];
  }

  private formatMessage(level: LogLevel, message: string, context?: LogContext): string {
    const timestamp = new Date().toISOString();
    const contextStr = context ? ` ${JSON.stringify(context)}` : "";
    return `[${timestamp}] ${level}: ${message}${contextStr}`;
  }

  debug(message: string, context?: LogContext): void {
    if (this.shouldLog("DEBUG")) {
      console.log(this.formatMessage("DEBUG", message, context));
    }
  }

  info(message: string, context?: LogContext): void {
    if (this.shouldLog("INFO")) {
      console.log(this.formatMessage("INFO", message, context));
    }
  }

  warn(message: string, context?: LogContext): void {
    if (this.shouldLog("WARN")) {
      console.warn(this.formatMessage("WARN", message, context));
    }
  }

  error(message: string, context?: LogContext): void {
    if (this.shouldLog("ERROR")) {
      console.error(this.formatMessage("ERROR", message, context));
    }
  }
}

// ============================================================================
// CONFIGURATION MANAGEMENT
// ============================================================================

interface MathForgeConfig {
  ollama: {
    baseUrl: string;
    model: string;
    timeout: number;
    temperature: number;
    maxTokens: number;
  };
  logging: {
    level: LogLevel;
  };
  generation: {
    includeTests: boolean;
    includeBenchmarks: boolean;
    includeDocumentation: boolean;
    strictMode: boolean;
  };
}

const createDefaultConfig = (): MathForgeConfig => ({
  ollama: {
    baseUrl: "http://localhost:11434",
    model: "qwen2.5-coder:7b",
    timeout: 30000,
    temperature: 0.1,
    maxTokens: 4000,
  },
  logging: {
    level: "INFO",
  },
  generation: {
    includeTests: true,
    includeBenchmarks: false,
    includeDocumentation: true,
    strictMode: true,
  },
});

// ============================================================================
// PERFORMANCE MEASUREMENT
// ============================================================================

const measureAsync = async <T>(
  operationName: string,
  operation: () => Promise<T>
): Promise<T> => {
  const start = Date.now();
  try {
    const result = await operation();
    const duration = Date.now() - start;
    console.log(`⏱️  ${operationName} completed in ${duration}ms`);
    return result;
  } catch (error) {
    const duration = Date.now() - start;
    console.log(`⏱️  ${operationName} failed after ${duration}ms`);
    throw error;
  }
};

// ============================================================================
// OLLAMA RESPONSE TYPES
// ============================================================================

interface OllamaResponse {
  model: string;
  created_at: string;
  response: string;
  done: boolean;
}

// ============================================================================
// PHASE 1 IMPLEMENTATION WITH QICORE PATTERNS
// ============================================================================

export class QiCoreStyleMathForgeAgent {
  private config: MathForgeConfig;
  private logger: StructuredLogger;

  constructor(configOverrides?: Partial<MathForgeConfig>) {
    this.config = {
      ...createDefaultConfig(),
      ...configOverrides,
    };

    this.logger = new StructuredLogger(this.config.logging.level);

    this.logger.info("MathForge Agent initialized", {
      phase: "1-qicore-style",
      config: {
        model: this.config.ollama.model,
        baseUrl: this.config.ollama.baseUrl,
        logLevel: this.config.logging.level,
      },
    });
  }

  /**
   * Generate YAML specification using Result types and structured logging
   */
  async generateYamlSpec(request: string): Promise<Result<string>> {
    this.logger.info("Starting YAML spec generation", { request });

    const prompt = this.buildSpecGenerationPrompt(request);

    return measureAsync("generateYamlSpec", async () => {
      const response = await this.callOllamaAPI(prompt);
      if (isFailure(response)) {
        this.logger.error("Ollama API call failed", {
          error: response.error.message,
          request,
        });
        return response;
      }

      this.logger.info("YAML spec generated successfully", {
        responseLength: response.value.length,
        request,
      });

      return success(response.value.trim());
    });
  }

  /**
   * Generate code using Result types + existing MathForge pipeline
   */
  async generateMathCode(
    request: string,
    targetLanguage: TargetLanguage = "typescript"
  ): Promise<Result<GeneratedCode>> {
    this.logger.info("Starting math code generation", { request, targetLanguage });

    return measureAsync("generateMathCode", async () => {
      // Step 1: Generate YAML spec
      const specResult = await this.generateYamlSpec(request);
      if (isFailure(specResult)) {
        return failure(specResult.error);
      }

      try {
        // Step 2: Parse spec
        const parseResult = parseSpecContent(specResult.value);

        if (!parseResult.spec || !parseResult.validation.valid) {
          const qiError = createQiError(
            "SPEC_PARSING_FAILED",
            `Failed to parse YAML spec: ${parseResult.validation.errors.map(e => e.message).join(', ')}`,
            "BUSINESS",
            { parseResult, request }
          );
          this.logger.error("Spec parsing failed", { error: qiError });
          return failure(qiError);
        }

        this.logger.debug("Spec parsed successfully", {
          operations: Object.keys(parseResult.spec.operations).length,
          typeDefinitions: Object.keys(parseResult.spec.type_definitions).length,
        });

        // Step 3: Generate code
        const options: GenerationOptions = {
          target_language: targetLanguage,
          include_tests: this.config.generation.includeTests,
          include_benchmarks: this.config.generation.includeBenchmarks,
          include_documentation: this.config.generation.includeDocumentation,
          strict_mode: this.config.generation.strictMode,
        };

        const generatedCode = await generateCode(parseResult.spec, options);

        this.logger.info("Code generation completed successfully", {
          language: generatedCode.language,
          linesOfCode: generatedCode.metadata.lines_of_code,
          testCount: generatedCode.metadata.test_count,
          request,
        });

        return success(generatedCode);
      } catch (error) {
        const qiError = createQiError(
          "CODE_GENERATION_FAILED",
          `Failed to generate code: ${error}`,
          "SYSTEM",
          { request, targetLanguage, error: String(error) }
        );
        this.logger.error("Code generation failed", { error: qiError });
        return failure(qiError);
      }
    });
  }

  /**
   * Check if Ollama is available using Result types
   */
  async isOllamaAvailable(): Promise<Result<boolean>> {
    this.logger.debug("Checking Ollama availability", {
      baseUrl: this.config.ollama.baseUrl,
    });

    try {
      const response = await fetch(`${this.config.ollama.baseUrl}/api/version`, {
        method: "GET",
        signal: AbortSignal.timeout(5000),
      });

      const isAvailable = response.ok;
      this.logger.info("Ollama availability check completed", {
        available: isAvailable,
        status: response.status,
      });

      return success(isAvailable);
    } catch (error) {
      const qiError = createQiError(
        "OLLAMA_UNAVAILABLE",
        `Ollama is not available: ${error}`,
        "NETWORK",
        { baseUrl: this.config.ollama.baseUrl, error: String(error) }
      );
      this.logger.warn("Ollama availability check failed", { error: qiError });
      return failure(qiError);
    }
  }

  /**
   * Private method to call Ollama API with Result types
   */
  private async callOllamaAPI(prompt: string): Promise<Result<string>> {
    this.logger.debug("Calling Ollama API", {
      baseUrl: this.config.ollama.baseUrl,
      model: this.config.ollama.model,
      promptLength: prompt.length,
    });

    try {
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), this.config.ollama.timeout);

      const response = await fetch(`${this.config.ollama.baseUrl}/api/generate`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          model: this.config.ollama.model,
          prompt: prompt,
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
        const errorText = await response.text();
        const qiError = createQiError(
          "OLLAMA_API_ERROR",
          `Ollama API error: ${errorText || `HTTP ${response.status}`}`,
          "NETWORK",
          {
            status: response.status,
            statusText: response.statusText,
          }
        );
        this.logger.error("Ollama API returned error", { error: qiError });
        return failure(qiError);
      }

      const data = (await response.json()) as OllamaResponse;
      
      this.logger.debug("Ollama API call successful", {
        model: data.model,
        responseLength: data.response.length,
      });

      return success(data.response);
    } catch (error) {
      if (error instanceof Error && error.name === "AbortError") {
        const qiError = createQiError(
          "OLLAMA_TIMEOUT",
          `Request timeout after ${this.config.ollama.timeout}ms`,
          "NETWORK",
          { timeout: this.config.ollama.timeout }
        );
        this.logger.error("Ollama API request timed out", { error: qiError });
        return failure(qiError);
      }

      const qiError = createQiError(
        "OLLAMA_REQUEST_FAILED",
        `Failed to call Ollama API: ${error}`,
        "NETWORK",
        { error: String(error) }
      );
      this.logger.error("Ollama API request failed", { error: qiError });
      return failure(qiError);
    }
  }

  private buildSpecGenerationPrompt(request: string): string {
    return `Convert this natural language request into a formal YAML specification for mathematical code generation.

Request: "${request}"

Generate a YAML specification following this exact format:

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
      - "monad.left_identity"
    examples:
      - input: "success(42)"
        output: "Right(42)"

type_definitions:
  Result:
    description: "Either type for error handling"

generation_targets:
  typescript:
    formal_verification_tools: ["zod", "fast-check"]
    package_dependencies: ["fp-ts"]

Respond with ONLY the YAML specification, no explanations or markdown formatting.`;
  }
}

// ============================================================================
// DEMONSTRATION FUNCTION
// ============================================================================

export async function demonstratePhase1Simplified() {
  const logger = new StructuredLogger("INFO");

  logger.info("🎯 MathForge Agent - Phase 1: QiCore Patterns Demo");
  logger.info("=" .repeat(60));

  try {
    // Create agent with QiCore-style patterns
    const agent = new QiCoreStyleMathForgeAgent({
      logging: {
        level: "DEBUG",
      },
    });

    // Check Ollama availability
    const availabilityResult = await agent.isOllamaAvailable();
    if (isFailure(availabilityResult)) {
      logger.warn("Ollama not available, demo will be limited", {
        error: availabilityResult.error.message,
      });
      logger.info("💡 To test with real Ollama:");
      logger.info("   1. Install Ollama: https://ollama.ai");
      logger.info("   2. Run: ollama pull qwen2.5-coder:7b");
      logger.info("   3. Start server: ollama serve");
      
      // Show the benefits even without Ollama
      logger.info("✅ Benefits demonstrated even without Ollama:");
      logger.info("   • Structured logging with context");
      logger.info("   • Result<T> instead of exceptions");
      logger.info("   • Proper error categorization");
      logger.info("   • Configuration management");
      logger.info("   • Performance measurement");
      return;
    }

    logger.info("✅ Ollama is available, proceeding with full demo");

    // Test spec generation
    const request = "Create a Result type for error handling with map and flatMap operations";
    const specResult = await agent.generateYamlSpec(request);

    if (isFailure(specResult)) {
      logger.error("Spec generation failed", { error: specResult.error });
      return;
    }

    logger.info("✅ YAML spec generated successfully", {
      specLength: specResult.value.length,
    });

    // Test full code generation
    const codeResult = await agent.generateMathCode(request, "typescript");

    if (isFailure(codeResult)) {
      logger.error("Code generation failed", { error: codeResult.error });
      return;
    }

    logger.info("✅ Code generation completed successfully", {
      language: codeResult.value.language,
      linesOfCode: codeResult.value.metadata.lines_of_code,
      testCount: codeResult.value.metadata.test_count,
    });

    logger.info("🎉 Phase 1 Demo Completed Successfully!");
    logger.info("📊 Comparison with previous approach:");
    logger.info("   ❌ Before: console.log everywhere");
    logger.info("   ✅ After: Structured logging with context");
    logger.info("   ❌ Before: throw new Error()");  
    logger.info("   ✅ After: Result<T> types");
    logger.info("   ❌ Before: Hard-coded configuration");
    logger.info("   ✅ After: Configuration management");
    logger.info("   ❌ Before: No error categorization");
    logger.info("   ✅ After: Proper error categories");

  } catch (error) {
    logger.error("Demo failed with unexpected error", { error: String(error) });
  }
}

// Run demo if called directly
if (import.meta.main) {
  await demonstratePhase1Simplified();
} 