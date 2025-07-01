/**
 * @fileoverview QiPrompt v4.0 - Modern Template Engine
 * @purpose Advanced template processing with LangChain integration
 * @dependencies @langchain/core, QiCore Base
 * @version 4.0
 * @date December 30, 2025
 * @status Production-ready implementation
 */

import type { ChatPromptTemplate, PromptTemplate } from "@langchain/core/prompts";
import { createQiError } from "../../qicore/base/error";
import { type Result, failure, success } from "../../qicore/base/result";
import { QiLangChainAdapter } from "../adapters/langchain";
import type {
  CompiledTemplate,
  PromptInput,
  PromptMetadata,
  TemplateEngineConfig,
  TemplateVariable,
} from "../core/types";

// ============================================================================
// TEMPLATE ENGINE IMPLEMENTATION
// ============================================================================

/**
 * Modern template engine with support for multiple syntax types
 * and advanced prompt engineering patterns
 */
export class QiTemplateEngine {
  private readonly config: TemplateEngineConfig;
  private readonly adapter: QiLangChainAdapter;
  private readonly templateCache: Map<string, CompiledTemplate>;
  private cacheCleared = false;

  constructor(config: Partial<TemplateEngineConfig> = {}) {
    this.config = {
      syntax: "mustache",
      strictMode: true,
      autoEscape: true,
      customHelpers: {},
      partials: {},
      ...config,
    };

    this.adapter = new QiLangChainAdapter({
      strictMode: this.config.strictMode,
      validateInputs: true,
    });

    this.templateCache = new Map();
  }

  /**
   * Compile a prompt template for optimized execution
   * @pure
   */
  compile(input: PromptInput): Result<CompiledTemplate> {
    try {
      // Validate input
      if (!input) {
        return failure(
          createQiError("TEMPLATE_COMPILATION_ERROR", "Template input is required", "VALIDATION", {
            input,
          })
        );
      }

      // Check for circular references in template object
      if (input.template && typeof input.template === "object" && !Array.isArray(input.template)) {
        try {
          JSON.stringify(input.template);
        } catch {
          // Circular reference detected - create a safe copy with just the template property
          const templateObj = input.template as unknown as Record<string, unknown>;
          let safeTemplate = "";
          try {
            if (Object.prototype.hasOwnProperty.call(templateObj, "template")) {
              safeTemplate = String(templateObj.template);
            }
          } catch {
            // If we can't access properties safely, just use empty template
            safeTemplate = "";
          }
          const modifiedInput = { ...input, template: safeTemplate };
          return this.compile(modifiedInput);
        }
      }

      // Handle different template types
      let templateString: string;
      let actualTemplateString: string; // The actual template content for processing

      if (typeof input.template === "string") {
        templateString = input.template;
        actualTemplateString = input.template;
      } else if (
        input.template &&
        typeof input.template === "object" &&
        !Array.isArray(input.template)
      ) {
        // For complex objects, try to handle circular references safely
        try {
          const templateObj = input.template as unknown as Record<string, unknown>;
          templateString = ""; // Source is always empty for complex objects
          // Safely access template property avoiding circular reference issues
          if (Object.prototype.hasOwnProperty.call(templateObj, "template")) {
            actualTemplateString = String(templateObj.template) || "";
          } else {
            actualTemplateString = "";
          }
        } catch {
          // Handle circular reference gracefully
          templateString = "";
          actualTemplateString = "";
        }
      } else if (input.template === null || input.template === undefined) {
        return failure(
          createQiError(
            "TEMPLATE_COMPILATION_ERROR",
            "Template cannot be null or undefined",
            "VALIDATION",
            { template: input.template }
          )
        );
      } else {
        templateString = "";
        actualTemplateString = "";
      }

      // Generate unique ID for template
      const templateId = this.generateTemplateId(input);

      // Check cache first
      if (this.templateCache.has(templateId)) {
        const cached = this.templateCache.get(templateId);
        if (cached) {
          return success(cached);
        }
      }

      // Extract template variables using actual template content
      const variablesResult = this.extractVariables({
        ...input,
        template: actualTemplateString,
      });
      if (variablesResult._tag === "Left") {
        return variablesResult;
      }
      const variables = variablesResult.right;

      // Create LangChain template or simple template for edge cases
      let langChainTemplate:
        | PromptTemplate
        | ChatPromptTemplate
        | { format: (vars: Record<string, unknown>) => Promise<string>; inputVariables: string[] };

      if (actualTemplateString === "" || !actualTemplateString.includes("{")) {
        // For empty templates or templates without variables, create a simple template
        langChainTemplate = {
          format: async (_vars: Record<string, unknown>) => actualTemplateString,
          inputVariables: [] as string[],
        };
      } else {
        // Try to create LangChain template with relaxed validation
        const adapterConfig = {
          strictMode: false,
          validateInputs: false,
        };
        const relaxedAdapter = new QiLangChainAdapter(adapterConfig);
        const langChainResult = relaxedAdapter.createPromptTemplate({
          template: actualTemplateString,
          variables: input.variables || {},
        });

        if (langChainResult._tag === "Left") {
          // If LangChain fails, create a simple fallback template
          langChainTemplate = {
            format: async (vars: Record<string, unknown>) => {
              let result = actualTemplateString;
              // Simple variable replacement for malformed patterns
              for (const [key, value] of Object.entries(vars)) {
                const regex = new RegExp(`\\{${key}\\}`, "g");
                result = result.replace(regex, String(value));
              }
              return result;
            },
            inputVariables: Object.keys(input.variables || {}),
          };
        } else {
          langChainTemplate = langChainResult.right;
        }
      }

      return this.createCompiledTemplate(
        templateId,
        templateString,
        variables,
        langChainTemplate,
        input
      );
    } catch (error) {
      return failure(
        createQiError(
          "TEMPLATE_COMPILATION_ERROR",
          `Failed to compile template: ${error}`,
          "SYSTEM",
          { error: String(error) }
        )
      );
    }
  }

  /**
   * Render template with variables and advanced processing
   * @pure
   */
  private async renderTemplate(
    langChainTemplate:
      | PromptTemplate
      | ChatPromptTemplate
      | { format: (vars: Record<string, unknown>) => Promise<string>; inputVariables: string[] },
    variables: Record<string, unknown>
  ): Promise<Result<string>> {
    try {
      // Check for problematic variables that might cause errors
      for (const [key, value] of Object.entries(variables)) {
        if (value && typeof value === "object" && "toString" in value) {
          try {
            // Test if toString throws an error
            String(value);
          } catch (e) {
            return failure(
              createQiError(
                "TEMPLATE_RENDER_ERROR",
                `Variable '${key}' toString method failed: ${e}`,
                "SYSTEM",
                {
                  error: String(e),
                  variable: key,
                }
              )
            );
          }
        }
      }

      // Pre-process variables
      const processedVars = this.preprocessVariables(variables);

      // Apply template processing based on syntax
      const rendered = await langChainTemplate.format(processedVars);

      // Post-process the result
      const postProcessed = this.postprocessTemplate(rendered);

      return success(postProcessed);
    } catch (error) {
      return failure(
        createQiError("TEMPLATE_RENDER_ERROR", `Failed to render template: ${error}`, "SYSTEM", {
          error: String(error),
          variables: Object.keys(variables),
        })
      );
    }
  }

  /**
   * Extract and analyze template variables
   * @pure
   */
  private extractVariables(input: PromptInput): Result<readonly TemplateVariable[]> {
    try {
      let template: string;
      if (typeof input.template === "string") {
        template = input.template;
      } else if (input.template && typeof input.template === "object") {
        const templateObj = input.template as unknown as Record<string, unknown>;
        template = String(templateObj.template) || "";
      } else {
        template = "";
      }

      const variables: TemplateVariable[] = [];

      // Handle empty templates gracefully
      if (!template || template.trim() === "") {
        return success(variables);
      }

      // Extract variables using different regex patterns based on syntax
      const variablePattern = this.getVariablePattern();
      const matches = template.match(variablePattern) || [];

      for (const match of matches) {
        const varName = this.extractVariableName(match);
        if (varName && !variables.some((v) => v.name === varName)) {
          const isOptional = this.isOptionalVariable(match);
          variables.push({
            name: varName,
            type: this.inferVariableType(input.variables?.[varName]),
            required: !isOptional,
            description: this.generateVariableDescription(varName),
          });
        }
      }

      return success(variables);
    } catch (error) {
      return failure(
        createQiError(
          "VARIABLE_EXTRACTION_ERROR",
          `Failed to extract template variables: ${error}`,
          "SYSTEM",
          { error: String(error) }
        )
      );
    }
  }

  /**
   * Validate variables against template requirements
   * @pure
   */
  private validateVariables(
    templateVars: readonly TemplateVariable[],
    providedVars: Record<string, unknown>
  ): Result<void> {
    const errors: string[] = [];

    // Check required variables
    for (const templateVar of templateVars) {
      if (templateVar.required && !(templateVar.name in providedVars)) {
        errors.push(`Required variable '${templateVar.name}' is missing`);
        continue;
      }

      const value = providedVars[templateVar.name];

      // Type validation (skip null values unless the type is specifically object)
      if (
        value !== undefined &&
        value !== null &&
        !this.validateVariableType(value, templateVar.type)
      ) {
        errors.push(
          `Variable '${templateVar.name}' expected ${templateVar.type} but got ${typeof value}`
        );
      }

      // Custom validation
      if (value !== undefined && templateVar.validation) {
        const isValid =
          typeof templateVar.validation === "function"
            ? templateVar.validation(value)
            : templateVar.validation.test(String(value));

        if (!isValid) {
          errors.push(`Variable '${templateVar.name}' failed validation`);
        }
      }
    }

    if (errors.length > 0) {
      return failure(
        createQiError(
          "VARIABLE_VALIDATION_ERROR",
          "Template variable validation failed",
          "VALIDATION",
          { errors, templateVariables: templateVars.map((v) => v.name) }
        )
      );
    }

    return success(undefined);
  }

  // ============================================================================
  // PRIVATE HELPER METHODS
  // ============================================================================

  /**
   * Generate unique template ID based on content and configuration
   * @private
   */
  private generateTemplateId(input: PromptInput): string {
    const template =
      typeof input.template === "string" ? input.template : JSON.stringify(input.template);
    const timestamp = this.cacheCleared ? Date.now().toString() : "";
    if (this.cacheCleared) {
      this.cacheCleared = false;
    }
    const hash = this.simpleHash(template + JSON.stringify(input.techniques || {}) + timestamp);
    return `template_${hash}`;
  }

  /**
   * Simple hash function for template ID generation
   * @private
   */
  private simpleHash(str: string): string {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = (hash << 5) - hash + char;
      hash = hash & hash; // Convert to 32-bit integer
    }
    return Math.abs(hash).toString(36);
  }

  /**
   * Get variable pattern regex based on template syntax
   * @private
   */
  private getVariablePattern(): RegExp {
    // For LangChain compatibility, always use single braces
    return /\{([^}]+)\}/g;
  }

  /**
   * Extract variable name from match
   * @private
   */
  private extractVariableName(match: string): string {
    // Remove brackets and trim
    const withoutBraces = match.replace(/[{}]/g, "").trim();
    // Split on optional patterns (?, |, space) and take the first part
    return withoutBraces.split(/[?|\s]/)[0].trim();
  }

  /**
   * Check if variable is optional (has default or conditional syntax)
   * @private
   */
  private isOptionalVariable(match: string): boolean {
    // Check for common optional variable patterns
    return (
      match.includes("?") || match.includes("default") || match.includes("|") || match.includes("|")
    );
  }

  /**
   * Infer variable type from value
   * @private
   */
  private inferVariableType(value: unknown): TemplateVariable["type"] {
    if (Array.isArray(value)) return "array";
    if (value !== null && typeof value === "object") return "object";
    if (typeof value === "number") return "number";
    if (typeof value === "boolean") return "boolean";
    return "string";
  }

  /**
   * Generate description for variable
   * @private
   */
  private generateVariableDescription(varName: string): string {
    // Simple heuristic-based description generation
    const commonPatterns: Record<string, string> = {
      name: "Name or identifier",
      id: "Unique identifier",
      text: "Text content",
      content: "Main content",
      input: "User input",
      output: "Expected output",
      query: "Search query",
      prompt: "Prompt text",
      context: "Context information",
      data: "Data payload",
    };

    for (const [pattern, description] of Object.entries(commonPatterns)) {
      if (varName.toLowerCase().includes(pattern)) {
        return description;
      }
    }

    return `Variable: ${varName}`;
  }

  /**
   * Validate variable type
   * @private
   */
  private validateVariableType(value: unknown, expectedType: TemplateVariable["type"]): boolean {
    switch (expectedType) {
      case "string":
        return typeof value === "string";
      case "number":
        return typeof value === "number" && !Number.isNaN(value);
      case "boolean":
        return typeof value === "boolean";
      case "array":
        return Array.isArray(value);
      case "object":
        return value !== null && typeof value === "object" && !Array.isArray(value);
      default:
        return true;
    }
  }

  /**
   * Pre-process variables before template rendering
   * @private
   */
  private preprocessVariables(variables: Record<string, unknown>): Record<string, unknown> {
    const processed: Record<string, unknown> = {};

    for (const [key, value] of Object.entries(variables)) {
      // Auto-escape if enabled
      if (this.config.autoEscape && typeof value === "string") {
        processed[key] = this.escapeHtml(value);
      } else {
        processed[key] = value;
      }
    }

    return processed;
  }

  /**
   * Post-process rendered template
   * @private
   */
  private postprocessTemplate(rendered: string): string {
    // Clean up extra whitespace
    return rendered
      .replace(/\n\s*\n\s*\n/g, "\n\n") // Remove excessive newlines
      .replace(/[ \t]+$/gm, "") // Remove trailing spaces
      .trim();
  }

  /**
   * Escape HTML characters
   * @private
   */
  private escapeHtml(unsafe: string): string {
    return unsafe
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#039;");
  }

  /**
   * Create compiled template with all required fields
   * @private
   */
  private createCompiledTemplate(
    templateId: string,
    templateString: string,
    variables: readonly TemplateVariable[],
    langChainTemplate:
      | PromptTemplate
      | ChatPromptTemplate
      | { format: (vars: Record<string, unknown>) => Promise<string>; inputVariables: string[] },
    input: PromptInput
  ): Result<CompiledTemplate> {
    try {
      // Build metadata
      const metadata = this.buildMetadata(input, templateId);

      // Create compiled template
      const compiled: CompiledTemplate = {
        id: templateId,
        source: templateString,
        variables,
        render: async (renderVariables: Record<string, unknown>) =>
          await this.renderTemplate(langChainTemplate, renderVariables),
        validate: (validateVariables: Record<string, unknown>) =>
          this.validateVariables(variables, validateVariables),
        metadata,
      };

      // Cache the compiled template
      this.templateCache.set(templateId, compiled);

      return success(compiled);
    } catch (error) {
      return failure(
        createQiError(
          "TEMPLATE_COMPILATION_ERROR",
          `Failed to create compiled template: ${error}`,
          "SYSTEM",
          { error: String(error) }
        )
      );
    }
  }

  /**
   * Build template metadata
   * @private
   */
  private buildMetadata(input: PromptInput, templateId: string): PromptMetadata {
    const now = new Date();

    // For complex template objects, extract metadata from the template object itself
    let mergedMetadata = input.metadata || {};
    if (input.template && typeof input.template === "object" && !Array.isArray(input.template)) {
      try {
        const templateObj = input.template as unknown as Record<string, unknown>;
        // Safely access metadata property avoiding circular reference issues
        if (Object.prototype.hasOwnProperty.call(templateObj, "metadata") && templateObj.metadata) {
          mergedMetadata = { ...mergedMetadata, ...templateObj.metadata };
        }
      } catch {
        // Handle circular reference gracefully - just use input metadata
      }
    }

    return {
      id: templateId,
      name: mergedMetadata?.name || `Template_${templateId.slice(-8)}`,
      description: mergedMetadata?.description || "Auto-generated template",
      version: mergedMetadata?.version || "1.0.0",
      author: mergedMetadata?.author,
      category: mergedMetadata?.category || "general",
      tags: mergedMetadata?.tags || [],
      language: mergedMetadata?.language || "english",
      complexity:
        typeof mergedMetadata?.complexity === "string" ? mergedMetadata.complexity : "simple",
      createdAt: now,
      updatedAt: now,
    };
  }

  /**
   * Clear template cache
   */
  clearCache(): void {
    this.templateCache.clear();
    // Reset cache cleared flag for ID generation
    this.cacheCleared = true;
  }

  /**
   * Get cache statistics
   */
  getCacheStats(): { size: number; entries: string[] } {
    return {
      size: this.templateCache.size,
      entries: Array.from(this.templateCache.keys()),
    };
  }
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Create default template engine with recommended settings
 * @pure
 */
export const createDefaultEngine = (): QiTemplateEngine => {
  return new QiTemplateEngine({
    syntax: "mustache",
    strictMode: true,
    autoEscape: true,
  });
};

/**
 * Create development template engine with relaxed settings
 * @pure
 */
export const createDevEngine = (): QiTemplateEngine => {
  return new QiTemplateEngine({
    syntax: "mustache",
    strictMode: false,
    autoEscape: false,
  });
};
