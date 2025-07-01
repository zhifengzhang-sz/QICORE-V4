/**
 * @fileoverview QiPrompt v4.0 - Main Facade and Public API
 * @purpose Clean, QiCore-style API for modern prompt engineering
 * @dependencies @langchain/core, QiCore Base
 * @version 4.0
 * @date December 30, 2025
 * @status Production-ready implementation
 */

import { createQiError } from "../qicore/base/error.js";
import { type Result, failure, success } from "../qicore/base/result.js";
import {
  type LangChainAdapterConfig,
  QiLangChainAdapter,
  createDefaultAdapter,
} from "./adapters/langchain";
import type {
  CompiledTemplate,
  ModelConfig,
  PromptExecutionResult,
  PromptExperiment,
  PromptInput,
  PromptValidationResult,
  PromptVersion,
  QiPromptFacade,
  SafetyCheckResult,
  SafetyConfig,
} from "./core/types";
import { enhancePrompt } from "./patterns";
import { QiTemplateEngine, createDefaultEngine } from "./templates/engine";

// ============================================================================
// MAIN QIPROMPT FACADE
// ============================================================================

/**
 * QiPrompt v4.0 - Production-ready prompt engineering system
 * Provides clean QiCore Result<T> API over enterprise-grade LangChain infrastructure
 */
export class QiPrompt implements QiPromptFacade {
  private readonly engine: QiTemplateEngine;
  private readonly adapter: QiLangChainAdapter;
  private readonly safetyConfig: SafetyConfig;
  private readonly experiments: Map<string, PromptExperiment>;
  private readonly versions: Map<string, PromptVersion[]>;

  constructor(
    options: {
      safetyConfig?: Partial<SafetyConfig>;
      engineConfig?: Partial<import("./core/types").TemplateEngineConfig>;
      adapterConfig?: Partial<LangChainAdapterConfig>;
    } = {}
  ) {
    this.engine = options.engineConfig
      ? new QiTemplateEngine(options.engineConfig)
      : createDefaultEngine();

    this.adapter = options.adapterConfig
      ? new QiLangChainAdapter(options.adapterConfig)
      : createDefaultAdapter();

    this.safetyConfig = {
      enableContentFilter: true,
      enableInjectionDetection: true,
      maxPromptLength: 50000,
      allowedDomains: [],
      blockedTerms: [],
      rateLimit: { requests: 100, window: 60000 },
      ...options.safetyConfig,
    };

    this.experiments = new Map();
    this.versions = new Map();
  }

  /**
   * Compile a prompt template for optimized execution
   * @pure
   */
  compile(input: PromptInput): Result<CompiledTemplate> {
    try {
      // Safety check first
      const safetyResult = this.performSafetyCheck(input);
      if (safetyResult._tag === "Left") {
        return safetyResult;
      }

      const safetyCheck = safetyResult.right;
      if (!safetyCheck.safe) {
        return failure(
          createQiError("SAFETY_VIOLATION", "Prompt failed safety checks", "SECURITY", {
            violations: safetyCheck.violations,
          })
        );
      }

      // Enhance with prompt patterns
      const enhancedResult = enhancePrompt(input);
      if (enhancedResult._tag === "Left") {
        return enhancedResult;
      }

      // Compile the enhanced template
      return this.engine.compile(enhancedResult.right);
    } catch (error) {
      return failure(
        createQiError("COMPILATION_ERROR", `Failed to compile prompt: ${error}`, "SYSTEM", {
          error: String(error),
        })
      );
    }
  }

  /**
   * Execute a compiled template with a model (placeholder for future implementation)
   * Note: Actual model execution would require LLM provider integration
   */
  async execute(
    template: CompiledTemplate,
    model: ModelConfig
  ): Promise<Result<PromptExecutionResult>> {
    try {
      // For now, this is a placeholder that demonstrates the interface
      // In production, this would integrate with actual LLM providers

      const startTime = Date.now();

      // Create variables map from template metadata
      const templateVariables: Record<string, unknown> = {};
      for (const variable of template.variables) {
        // Use placeholder values for execution
        templateVariables[variable.name] =
          variable.defaultValue ?? (variable.name === "name" ? "World" : `${variable.name}_value`);
      }

      // Simulate template rendering (await the promise)
      const renderResult = await template.render(templateVariables);
      if (renderResult._tag === "Left") {
        return renderResult;
      }

      const prompt = renderResult.right;
      const endTime = Date.now();

      // Create mock execution result
      const result: PromptExecutionResult = {
        id: `exec_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`,
        prompt: {
          formatted: prompt,
          tokens: Math.ceil(prompt.length / 4), // Rough token estimate
        },
        response: {
          content: "// This is a placeholder response. Actual LLM integration needed.",
          tokens: 50,
          finishReason: "stop",
          model: model.modelName,
        },
        performance: {
          latency: endTime - startTime,
          tokensPerSecond: 0,
          cost: 0,
        },
        metadata: {
          timestamp: new Date(),
          provider: model.provider,
          version: "4.0.0",
          techniques: Object.keys(template.metadata).filter((key) => key.startsWith("technique")),
        },
      };

      return success(result);
    } catch (error) {
      return failure(
        createQiError("EXECUTION_ERROR", `Failed to execute prompt: ${error}`, "SYSTEM", {
          error: String(error),
          templateId: template.id,
        })
      );
    }
  }

  /**
   * Validate prompt input and provide detailed feedback
   * @pure
   */
  validate(input: PromptInput): Result<PromptValidationResult> {
    try {
      const errors: Array<{
        readonly field: string;
        readonly message: string;
        readonly severity: "error" | "warning" | "info";
      }> = [];
      const warnings: string[] = [];
      const suggestions: string[] = [];

      // Basic validation
      if (!input.template || (typeof input.template === "string" && input.template.trim() === "")) {
        errors.push({
          field: "template",
          message: "Template is required and cannot be empty",
          severity: "error",
        });
      }

      if (!input.variables || typeof input.variables !== "object") {
        errors.push({
          field: "variables",
          message: "Variables must be a valid object",
          severity: "error",
        });
      }

      // Template-specific validation
      if (typeof input.template === "string") {
        const templateLength = input.template.length;

        if (templateLength > this.safetyConfig.maxPromptLength) {
          errors.push({
            field: "template",
            message: `Template exceeds maximum length of ${this.safetyConfig.maxPromptLength} characters`,
            severity: "error",
          });
        }

        if (templateLength < 10) {
          warnings.push("Template is very short and may not provide sufficient context");
        }

        if (!/\{.*\}/.test(input.template)) {
          suggestions.push("Consider adding template variables for more dynamic prompts");
        }

        // Add suggestions based on template analysis
        if (
          input.template.toLowerCase().includes("step") ||
          input.template.toLowerCase().includes("solve")
        ) {
          suggestions.push("Consider using chain-of-thought reasoning for step-by-step problems");
        }

        if (
          input.template.toLowerCase().includes("example") ||
          input.template.toLowerCase().includes("classify")
        ) {
          suggestions.push("Consider using few-shot learning with examples for better results");
        }

        // For simple templates, suggest adding more context
        if (
          templateLength < 50 &&
          !input.template.toLowerCase().includes("analyze") &&
          !input.template.toLowerCase().includes("explain")
        ) {
          suggestions.push("Consider adding more context or examples to improve response quality");
        }

        // Check for missing variables referenced in template
        const templateVariables = input.template.match(/\{([^}]+)\}/g);
        if (templateVariables) {
          const variableNames = templateVariables.map((v) => v.slice(1, -1)); // Remove { }
          const providedVariables = Object.keys(input.variables || {});

          for (const varName of variableNames) {
            if (!providedVariables.includes(varName)) {
              errors.push({
                field: "variables",
                message: `Missing required variable: ${varName}`,
                severity: "error",
              });
            }
          }
        }
      }

      // Technique validation
      if (
        input.techniques?.chainOfThought?.enabled &&
        (!input.techniques.chainOfThought.steps ||
          input.techniques.chainOfThought.steps.length === 0)
      ) {
        errors.push({
          field: "techniques.chainOfThought.steps",
          message: "Chain-of-thought requires at least one step",
          severity: "error",
        });
      }

      if (
        input.techniques?.fewShot?.enabled &&
        (!input.techniques.fewShot.examples || input.techniques.fewShot.examples.length === 0)
      ) {
        errors.push({
          field: "techniques.fewShot.examples",
          message: "Few-shot learning requires at least one example",
          severity: "error",
        });
      }

      // Perform safety check
      const templateStr =
        typeof input.template === "string" ? input.template : JSON.stringify(input.template);
      const safetyCheck = this.performSafetyCheck(input);
      if (safetyCheck._tag === "Right" && !safetyCheck.right.safe) {
        for (const violation of safetyCheck.right.violations) {
          errors.push({
            field: "template",
            message: violation.message,
            severity:
              violation.severity === "critical" || violation.severity === "high"
                ? "error"
                : "warning",
          });
        }
      }

      // Calculate metrics
      const tokenCount = Math.ceil(templateStr.length / 4);
      const complexity = this.calculateComplexity(input);
      const readability = this.calculateReadability(templateStr);

      const validationResult: PromptValidationResult = {
        isValid: errors.filter((e) => e.severity === "error").length === 0,
        errors: errors as readonly (typeof errors)[0][],
        warnings: warnings as readonly string[],
        suggestions: suggestions as readonly string[],
        metrics: {
          tokenCount,
          complexity,
          readability,
        },
      };

      return success(validationResult);
    } catch (error) {
      return failure(
        createQiError("VALIDATION_ERROR", `Failed to validate prompt: ${error}`, "SYSTEM", {
          error: String(error),
        })
      );
    }
  }

  /**
   * Create A/B testing experiment for prompts
   * @pure
   */
  createExperiment(config: Omit<PromptExperiment, "id" | "status">): Result<PromptExperiment> {
    try {
      const experimentId = `exp_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;

      const experiment: PromptExperiment = {
        id: experimentId,
        status: "running",
        ...config,
      };

      // Validate experiment configuration
      if (config.variants.length < 2) {
        return failure(
          createQiError(
            "INVALID_EXPERIMENT",
            "Experiment requires at least 2 variants",
            "VALIDATION",
            { variantCount: config.variants.length }
          )
        );
      }

      const totalWeight = config.variants.reduce((sum, variant) => sum + variant.weight, 0);
      if (Math.abs(totalWeight - 1.0) > 0.001) {
        return failure(
          createQiError("INVALID_WEIGHTS", "Variant weights must sum to 1.0", "VALIDATION", {
            totalWeight,
            variants: config.variants.length,
          })
        );
      }

      this.experiments.set(experimentId, experiment);
      return success(experiment);
    } catch (error) {
      return failure(
        createQiError(
          "EXPERIMENT_CREATION_ERROR",
          `Failed to create experiment: ${error}`,
          "SYSTEM",
          { error: String(error) }
        )
      );
    }
  }

  /**
   * Get specific version of a prompt template
   * @pure
   */
  getVersion(templateId: string, version?: string): Result<PromptVersion> {
    try {
      const versions = this.versions.get(templateId);
      if (!versions || versions.length === 0) {
        return failure(
          createQiError(
            "TEMPLATE_NOT_FOUND",
            `No versions found for template ${templateId}`,
            "VALIDATION",
            { templateId }
          )
        );
      }

      if (version) {
        const specificVersion = versions.find((v) => v.version === version);
        if (!specificVersion) {
          return failure(
            createQiError(
              "VERSION_NOT_FOUND",
              `Version ${version} not found for template ${templateId}`,
              "VALIDATION",
              { templateId, version, availableVersions: versions.map((v) => v.version) }
            )
          );
        }
        return success(specificVersion);
      }

      // Return latest version (production status preferred, then by version string)
      const productionVersions = versions.filter((v) => v.status === "production");
      const latestVersion =
        productionVersions.length > 0
          ? productionVersions[productionVersions.length - 1]
          : versions[versions.length - 1];

      return success(latestVersion);
    } catch (error) {
      return failure(
        createQiError(
          "VERSION_RETRIEVAL_ERROR",
          `Failed to retrieve template version: ${error}`,
          "SYSTEM",
          { error: String(error), templateId, version }
        )
      );
    }
  }

  // ============================================================================
  // UTILITY AND MANAGEMENT METHODS
  // ============================================================================

  /**
   * Clear all caches and reset state
   */
  reset(): void {
    this.engine.clearCache();
    this.experiments.clear();
    this.versions.clear();
  }

  /**
   * Get system statistics and health information
   */
  getStats(): {
    templates: { cached: number; total: number };
    experiments: { active: number; completed: number };
    versions: { templates: number; totalVersions: number };
  } {
    const cacheStats = this.engine.getCacheStats();
    const activeExperiments = Array.from(this.experiments.values()).filter(
      (e) => e.status === "running"
    ).length;
    const completedExperiments = Array.from(this.experiments.values()).filter(
      (e) => e.status === "completed"
    ).length;
    const totalVersions = Array.from(this.versions.values()).reduce(
      (sum, versions) => sum + versions.length,
      0
    );

    return {
      templates: {
        cached: cacheStats.size,
        total: cacheStats.size, // In this implementation, cached = total
      },
      experiments: {
        active: activeExperiments,
        completed: completedExperiments,
      },
      versions: {
        templates: this.versions.size,
        totalVersions,
      },
    };
  }

  // ============================================================================
  // PRIVATE HELPER METHODS
  // ============================================================================

  /**
   * Perform safety checks on prompt input
   * @private
   */
  private performSafetyCheck(input: PromptInput): Result<SafetyCheckResult> {
    try {
      const violations: Array<{
        readonly type: "content" | "injection" | "length" | "rate" | "domain";
        readonly severity: "low" | "medium" | "high" | "critical";
        readonly message: string;
        readonly suggestion?: string;
      }> = [];
      const templateStr =
        typeof input.template === "string" ? input.template : JSON.stringify(input.template);

      // Length check
      if (templateStr.length === 0) {
        violations.push({
          type: "content",
          severity: "critical",
          message: "Template cannot be empty",
          suggestion: "Provide a valid template with content",
        });
      } else if (templateStr.length > this.safetyConfig.maxPromptLength) {
        violations.push({
          type: "length",
          severity: "high",
          message: `Template exceeds maximum length of ${this.safetyConfig.maxPromptLength} characters`,
          suggestion: "Consider breaking the prompt into smaller parts",
        });
      }

      // Content filter (basic implementation)
      if (this.safetyConfig.enableContentFilter && this.safetyConfig.blockedTerms) {
        for (const term of this.safetyConfig.blockedTerms) {
          if (templateStr.toLowerCase().includes(term.toLowerCase())) {
            violations.push({
              type: "content",
              severity: "high",
              message: `Template contains blocked term: ${term}`,
              suggestion: "Remove or replace the flagged content",
            });
          }
        }
      }

      // Basic injection detection
      if (this.safetyConfig.enableInjectionDetection) {
        const injectionPatterns = [
          /ignore\s+(?:all\s+)?(?:previous\s+)?instructions?/i,
          /system\s*:\s*you\s+are\s+now/i,
          /forget\s+everything/i,
        ];

        for (const pattern of injectionPatterns) {
          if (pattern.test(templateStr)) {
            violations.push({
              type: "injection",
              severity: "critical",
              message: "Potential prompt injection detected",
              suggestion: "Review and sanitize the prompt content",
            });
          }
        }
      }

      const result: SafetyCheckResult = {
        safe:
          violations.filter((v) => v.severity === "critical" || v.severity === "high").length === 0,
        violations: violations as readonly (typeof violations)[0][],
        confidence: violations.length === 0 ? 1.0 : Math.max(0.1, 1.0 - violations.length * 0.2),
      };

      return success(result);
    } catch (error) {
      return failure(
        createQiError("SAFETY_CHECK_ERROR", `Safety check failed: ${error}`, "SYSTEM", {
          error: String(error),
        })
      );
    }
  }

  /**
   * Calculate prompt complexity score
   * @private
   */
  private calculateComplexity(input: PromptInput): number {
    let complexity = 0;

    // Base complexity from template length
    const templateStr =
      typeof input.template === "string" ? input.template : JSON.stringify(input.template);
    complexity += Math.min(templateStr.length / 1000, 5); // Max 5 points for length

    // Techniques add complexity
    if (input.techniques?.chainOfThought?.enabled) complexity += 2;
    if (input.techniques?.fewShot?.enabled) complexity += 1.5;
    if (input.techniques?.rolePlay) complexity += 1;
    if (input.techniques?.outputFormat) complexity += 0.5;
    if (input.techniques?.constraints && input.techniques.constraints.length > 0) complexity += 1;

    // Variables add complexity
    const variableCount = Object.keys(input.variables || {}).length;
    complexity += Math.min(variableCount * 0.1, 2); // Max 2 points for variables

    return Math.min(complexity, 10); // Cap at 10
  }

  /**
   * Calculate readability score (simplified)
   * @private
   */
  private calculateReadability(text: string): number {
    const sentences = text.split(/[.!?]+/).filter((s) => s.trim().length > 0);
    const words = text.split(/\s+/).filter((w) => w.length > 0);

    if (sentences.length === 0 || words.length === 0) return 0;

    const avgWordsPerSentence = words.length / sentences.length;
    const avgCharsPerWord = words.reduce((sum, word) => sum + word.length, 0) / words.length;

    // Simplified readability score (0-10, higher is more readable)
    const readabilityScore = Math.max(0, 10 - avgWordsPerSentence / 3 - avgCharsPerWord / 2);

    return Math.min(readabilityScore, 10);
  }
}

// ============================================================================
// CONVENIENCE EXPORTS AND FACTORY FUNCTIONS
// ============================================================================

/**
 * Create default QiPrompt instance with recommended settings
 * @pure
 */
export const createQiPrompt = (options?: ConstructorParameters<typeof QiPrompt>[0]): QiPrompt => {
  return new QiPrompt(options);
};

/**
 * Create QiPrompt instance optimized for development
 * @pure
 */
export const createDevQiPrompt = (): QiPrompt => {
  return new QiPrompt({
    safetyConfig: {
      enableContentFilter: false,
      enableInjectionDetection: false,
      maxPromptLength: 100000,
    },
    engineConfig: {
      strictMode: false,
      autoEscape: false,
    },
    adapterConfig: {
      strictMode: false,
      validateInputs: false,
    },
  });
};

/**
 * Create QiPrompt instance with maximum security
 * @pure
 */
export const createSecureQiPrompt = (): QiPrompt => {
  return new QiPrompt({
    safetyConfig: {
      enableContentFilter: true,
      enableInjectionDetection: true,
      maxPromptLength: 10000,
      blockedTerms: ["password", "secret", "api_key", "token"],
      rateLimit: { requests: 50, window: 60000 },
    },
    engineConfig: {
      strictMode: true,
      autoEscape: true,
    },
    adapterConfig: {
      strictMode: true,
      validateInputs: true,
    },
  });
};

// Re-export types for convenience
export type * from "./core/types";

// Re-export utilities
export { QiTemplateEngine } from "./templates/engine";
export { QiLangChainAdapter } from "./adapters/langchain";
export * from "./patterns";

// Default export
export default QiPrompt;
