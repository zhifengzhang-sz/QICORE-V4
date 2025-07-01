/**
 * @fileoverview QiPrompt v4.0 - Advanced Prompt Engineering Patterns
 * @purpose Modern prompt patterns based on 2025 best practices
 * @dependencies QiCore Base, LangChain Core
 * @version 4.0
 * @date December 30, 2025
 * @status Production-ready implementation
 */

import { createQiError } from "../../qicore/base/error";
import { type Result, failure, success } from "../../qicore/base/result";
import type {
  ChainOfThoughtConfig,
  FewShotConfig,
  PromptInput,
  PromptTechniques,
} from "../core/types";

// ============================================================================
// CHAIN-OF-THOUGHT PATTERNS
// ============================================================================

/**
 * Build explicit chain-of-thought prompt
 * @pure
 */
export const buildExplicitChainOfThought = (
  basePrompt: string,
  config: ChainOfThoughtConfig
): Result<string> => {
  try {
    if (!config.enabled) {
      return success(basePrompt);
    }

    let cotPrompt = `${basePrompt}\n\n`;

    cotPrompt += "Let's think about this step by step:\n";
    config.steps.forEach((step, index) => {
      cotPrompt += `${index + 1}. ${step}\n`;
    });

    cotPrompt += "\nPlease work through each step systematically:\n";
    cotPrompt += "Step 1: ";

    return success(cotPrompt);
  } catch (error) {
    return failure(
      createQiError(
        "CHAIN_OF_THOUGHT_ERROR",
        `Failed to build chain-of-thought prompt: ${error}`,
        "SYSTEM",
        { error: String(error) }
      )
    );
  }
};

/**
 * Build implicit chain-of-thought prompt
 * @pure
 */
export const buildImplicitChainOfThought = (
  basePrompt: string,
  config: ChainOfThoughtConfig
): Result<string> => {
  try {
    if (!config.enabled) {
      return success(basePrompt);
    }

    let cotPrompt = `${basePrompt}\n\n`;
    cotPrompt += "Think about this carefully and explain your reasoning. ";
    cotPrompt += "Consider the key factors and walk through your thought process.";

    return success(cotPrompt);
  } catch (error) {
    return failure(
      createQiError(
        "COT_IMPLICIT_ERROR",
        `Failed to build implicit CoT prompt: ${error}`,
        "SYSTEM",
        { error: String(error) }
      )
    );
  }
};

/**
 * Build guided chain-of-thought prompt
 * @pure
 */
export const buildGuidedChainOfThought = (
  basePrompt: string,
  config: ChainOfThoughtConfig
): Result<string> => {
  try {
    if (!config.enabled) {
      return success(basePrompt);
    }

    let cotPrompt = `${basePrompt}\n\n`;
    cotPrompt += "When thinking about this, please consider:\n";

    config.steps.forEach((step, _index) => {
      cotPrompt += `• ${step}\n`;
    });

    cotPrompt += "\nPlease share your reasoning process as you work through this.";

    return success(cotPrompt);
  } catch (error) {
    return failure(
      createQiError("COT_GUIDED_ERROR", `Failed to build guided CoT prompt: ${error}`, "SYSTEM", {
        error: String(error),
      })
    );
  }
};

// ============================================================================
// FEW-SHOT LEARNING PATTERNS
// ============================================================================

/**
 * Select examples based on configured strategy
 * @private
 */
const selectExamples = (config: FewShotConfig): typeof config.examples => {
  const { examples, maxExamples, selectionStrategy } = config;

  if (examples.length <= maxExamples) {
    return examples;
  }

  switch (selectionStrategy) {
    case "random":
      return selectRandomExamples(examples, maxExamples);
    case "similarity":
      // For now, fall back to first N examples
      // In production, this would use semantic similarity
      return examples.slice(0, maxExamples);
    default:
      return examples.slice(0, maxExamples);
  }
};

/**
 * Select random examples
 * @private
 */
const selectRandomExamples = <T>(array: readonly T[], count: number): readonly T[] => {
  const shuffled = [...array].sort(() => 0.5 - Math.random());
  return shuffled.slice(0, count);
};

/**
 * Build few-shot prompt with examples
 * @pure
 */
export const buildFewShotPrompt = (basePrompt: string, config: FewShotConfig): Result<string> => {
  try {
    if (!config.enabled || config.examples.length === 0) {
      return success(basePrompt);
    }

    // Select examples based on strategy
    const selectedExamples = selectExamples(config);

    let fewShotPrompt = "Here are some examples:\n\n";

    selectedExamples.forEach((example, index) => {
      // Check if custom template is provided
      const customTemplate = (config as FewShotConfig & { template?: string }).template;

      if (customTemplate) {
        // Use custom template by replacing placeholders
        const formattedExample = customTemplate
          .replace(/\{input\}/g, example.input)
          .replace(/\{output\}/g, example.output);
        fewShotPrompt += `${formattedExample}\n`;
      } else {
        // Use default format
        fewShotPrompt += `Example ${index + 1}:\n`;
        fewShotPrompt += `Input: ${example.input}\n`;
        fewShotPrompt += `Expected Output: ${example.output}\n`;

        if (example.explanation) {
          fewShotPrompt += `Explanation: ${example.explanation}\n`;
        }
      }

      // Use custom separator if provided, otherwise use default
      const separator = (config as FewShotConfig & { separator?: string }).separator || "\n";
      fewShotPrompt += separator;
    });

    fewShotPrompt += `Now, please apply the same approach to this input:\n${basePrompt}`;

    return success(fewShotPrompt);
  } catch (error) {
    return failure(
      createQiError("FEW_SHOT_ERROR", `Failed to build few-shot prompt: ${error}`, "SYSTEM", {
        error: String(error),
      })
    );
  }
};

// ============================================================================
// ROLE-PLAY PATTERNS
// ============================================================================

/**
 * Build role-play system prompt
 * @pure
 */
export const buildRolePlaySystemPrompt = (
  persona: string,
  expertise: string,
  communication: string
): Result<string> => {
  try {
    // Check for null or undefined inputs
    if (
      persona === null ||
      persona === undefined ||
      expertise === null ||
      expertise === undefined ||
      communication === null ||
      communication === undefined
    ) {
      return failure(
        createQiError(
          "ROLE_PLAY_ERROR",
          "Persona, expertise, and communication parameters are required",
          "VALIDATION",
          { persona, expertise, communication }
        )
      );
    }

    const systemPrompt = `You are ${persona}, with expertise in ${expertise}.

Your communication style is ${communication}. You should:
- Respond authentically as this character
- Apply your expertise to provide valuable insights
- maintain consistency in your personality and knowledge
- Use appropriate language and terminology for your role
- Stay in character throughout the entire conversation
- respond according to this role

Remember: You are not just an AI assistant, you are ${persona} with deep knowledge of ${expertise}.`;

    return success(systemPrompt);
  } catch (error) {
    return failure(
      createQiError(
        "ROLE_PLAY_ERROR",
        `Failed to build role-play system prompt: ${error}`,
        "SYSTEM",
        { error: String(error) }
      )
    );
  }
};

/**
 * Build role-play user prompt
 * @pure
 */
export const buildRolePlayUserPrompt = (
  basePrompt: string,
  persona: string,
  context?: string
): Result<string> => {
  try {
    let rolePrompt = basePrompt;

    if (context) {
      rolePrompt = `Context: ${context}\n\n${rolePrompt}`;
    }

    rolePrompt += `\n\nPlease respond as ${persona}, drawing on your expertise and character.`;

    return success(rolePrompt);
  } catch (error) {
    return failure(
      createQiError(
        "ROLEPLAY_USER_ERROR",
        `Failed to build role-play user prompt: ${error}`,
        "SYSTEM",
        { error: String(error) }
      )
    );
  }
};

// ============================================================================
// OUTPUT FORMAT PATTERNS
// ============================================================================

/**
 * Build JSON output format prompt
 * @pure
 */
export const buildJsonFormat = (basePrompt: string, schema?: string): Result<string> => {
  try {
    let formatPrompt = `${basePrompt}\n\n`;
    formatPrompt += "Please provide your response in valid JSON format.";

    if (schema) {
      // Use a format that doesn't conflict with LangChain template variables
      formatPrompt += ` Follow this JSON schema format:\n\`\`\`\n${schema.replace(/\{/g, "[").replace(/\}/g, "]")}\n\`\`\`\nReplace [ and ] with actual braces in your response.`;
    } else {
      formatPrompt += " Structure your response as a JSON object with relevant fields.";
    }

    formatPrompt += "\n\nResponse:";

    return success(formatPrompt);
  } catch (error) {
    return failure(
      createQiError("JSON_FORMAT_ERROR", `Failed to build JSON format prompt: ${error}`, "SYSTEM", {
        error: String(error),
      })
    );
  }
};

/**
 * Build markdown output format prompt
 * @pure
 */
export const buildMarkdownFormat = (basePrompt: string): Result<string> => {
  try {
    const formatPrompt = `${basePrompt}\n\nPlease format your response using proper Markdown syntax. Use appropriate headers, lists, code blocks, and emphasis to make your response clear and well-structured.`;

    return success(formatPrompt);
  } catch (error) {
    return failure(
      createQiError(
        "MARKDOWN_FORMAT_ERROR",
        `Failed to build Markdown format prompt: ${error}`,
        "SYSTEM",
        { error: String(error) }
      )
    );
  }
};

/**
 * Build structured output format prompt
 * @pure
 */
export const buildStructuredFormat = (
  basePrompt: string,
  sections: readonly string[]
): Result<string> => {
  try {
    let formatPrompt = `${basePrompt}\n\n`;
    formatPrompt += "Please structure your response with the following sections:\n";

    sections.forEach((section, index) => {
      formatPrompt += `${index + 1}. ${section}\n`;
    });

    formatPrompt +=
      "\nMake sure each section is clearly labeled and contains relevant information.";

    return success(formatPrompt);
  } catch (error) {
    return failure(
      createQiError(
        "STRUCTURED_FORMAT_ERROR",
        `Failed to build structured format prompt: ${error}`,
        "SYSTEM",
        { error: String(error) }
      )
    );
  }
};

// ============================================================================
// CONSTRAINT PATTERNS
// ============================================================================

/**
 * Build constraint prompt with limitations
 * @pure
 */
export const buildConstraintPrompt = (
  basePrompt: string,
  constraints: readonly string[]
): Result<string> => {
  try {
    if (constraints.length === 0) {
      return success(basePrompt);
    }

    let constraintPrompt = `${basePrompt}\n\n`;
    constraintPrompt += "Important constraints and limitations:\n";

    constraints.forEach((constraint, _index) => {
      constraintPrompt += `• ${constraint}\n`;
    });

    constraintPrompt += "\nPlease ensure your response adheres to all of these constraints.";

    return success(constraintPrompt);
  } catch (error) {
    return failure(
      createQiError(
        "CONSTRAINT_BUILD_ERROR",
        `Failed to build constraint prompt: ${error}`,
        "SYSTEM",
        { error: String(error) }
      )
    );
  }
};

// ============================================================================
// PATTERN ORCHESTRATOR
// ============================================================================

/**
 * Apply chain-of-thought pattern
 * @private
 */
const applyChainOfThought = (template: string, config: ChainOfThoughtConfig): Result<string> => {
  switch (config.reasoning) {
    case "explicit":
      return buildExplicitChainOfThought(template, config);
    case "implicit":
      return buildImplicitChainOfThought(template, config);
    case "guided":
      return buildGuidedChainOfThought(template, config);
    default:
      return buildImplicitChainOfThought(template, config);
  }
};

/**
 * Apply output format pattern
 * @private
 */
const applyOutputFormat = (
  template: string,
  format: NonNullable<PromptTechniques["outputFormat"]>
): Result<string> => {
  switch (format.type) {
    case "json":
      return buildJsonFormat(template, format.schema);
    case "markdown":
      return buildMarkdownFormat(template);
    case "structured": {
      const sections = format.schema
        ? format.schema.split(",")
        : ["Introduction", "Analysis", "Conclusion"];
      return buildStructuredFormat(template, sections);
    }
    default:
      return success(template);
  }
};

/**
 * Analyze content and suggest appropriate techniques
 * @private
 */
const analyzeContent = (
  template: string
): {
  complexity: "simple" | "medium" | "complex";
  suggestedTechniques: Partial<PromptTechniques>;
  language?: string;
  category?: string;
  tags?: string[];
} => {
  const lowerTemplate = template.toLowerCase();

  // Complexity analysis
  const wordCount = template.split(/\s+/).length;
  const variableCount = (template.match(/\{[^}]+\}/g) || []).length;
  const sentenceCount = template.split(/[.!?]+/).length;

  let complexity: "simple" | "medium" | "complex" = "simple";
  if (wordCount > 50 || variableCount > 3 || sentenceCount > 5) {
    complexity = "complex";
  } else if (wordCount > 20 || variableCount > 1 || sentenceCount > 2) {
    complexity = "medium";
  }

  // Language detection (basic)
  let language: string | undefined;
  if (/[\u4e00-\u9fff]/.test(template)) {
    language = "chinese";
  }

  // Category analysis
  let category: string | undefined;
  const tags: string[] = [];

  if (
    lowerTemplate.includes("code") ||
    lowerTemplate.includes("program") ||
    lowerTemplate.includes("function")
  ) {
    category = "technical";
    tags.push("programming");
  } else if (
    lowerTemplate.includes("creative") ||
    lowerTemplate.includes("story") ||
    lowerTemplate.includes("imagine")
  ) {
    category = "creative";
    tags.push("creative");
  } else if (lowerTemplate.includes("analyze") || lowerTemplate.includes("research")) {
    category = "analytical";
    tags.push("analysis");
  }

  // Technique suggestions
  const suggestedTechniques: {
    chainOfThought?: PromptTechniques["chainOfThought"];
    fewShot?: PromptTechniques["fewShot"];
    rolePlay?: PromptTechniques["rolePlay"];
    constraints?: PromptTechniques["constraints"];
    outputFormat?: PromptTechniques["outputFormat"];
  } = {};

  // Chain-of-thought for math, problem-solving, step-by-step
  if (
    lowerTemplate.includes("step") ||
    lowerTemplate.includes("solve") ||
    lowerTemplate.includes("equation") ||
    lowerTemplate.includes("calculate") ||
    lowerTemplate.includes("problem")
  ) {
    suggestedTechniques.chainOfThought = {
      enabled: true,
      reasoning: "explicit",
      steps: ["Analyze the problem", "Apply the method", "Verify the result"],
    };
  }

  // Few-shot for classification, translation, pattern recognition
  if (
    lowerTemplate.includes("classify") ||
    lowerTemplate.includes("translate") ||
    lowerTemplate.includes("sentiment") ||
    lowerTemplate.includes("category")
  ) {
    suggestedTechniques.fewShot = {
      enabled: true,
      examples: [],
      maxExamples: 3,
      selectionStrategy: "manual",
    };
  }

  // Role-play for expert advice, professional contexts
  if (
    lowerTemplate.includes("doctor") ||
    lowerTemplate.includes("expert") ||
    lowerTemplate.includes("as a") ||
    lowerTemplate.includes("you are")
  ) {
    // Extract persona from "as a {persona}" or "you are {persona}"
    const personaMatch = lowerTemplate.match(/(?:as a|you are)\s+([^,\s]+)/);
    const persona = personaMatch ? personaMatch[1] : "expert";

    suggestedTechniques.rolePlay = {
      persona: persona,
      expertise: "relevant field",
      communication: "professional",
    };
  }

  return {
    complexity,
    suggestedTechniques,
    language,
    category,
    tags: tags.length > 0 ? tags : undefined,
  };
};

/**
 * Apply all configured techniques to create enhanced prompt with intelligent analysis
 * @pure
 */
export const enhancePrompt = (input: PromptInput): Result<PromptInput> => {
  try {
    const baseTemplate = typeof input.template === "string" ? input.template : "";
    let enhancedTemplate = baseTemplate;

    // Analyze content and generate metadata
    const analysis = analyzeContent(baseTemplate);

    // Merge existing techniques with suggested ones
    const existingTechniques = input.techniques || {};
    const enhancedTechniques: {
      chainOfThought?: PromptTechniques["chainOfThought"];
      fewShot?: PromptTechniques["fewShot"];
      rolePlay?: PromptTechniques["rolePlay"];
      constraints?: PromptTechniques["constraints"];
      outputFormat?: PromptTechniques["outputFormat"];
    } = {
      ...analysis.suggestedTechniques,
      ...existingTechniques,
    };

    // Preserve existing enabled techniques while adding suggestions for disabled ones
    if (existingTechniques.chainOfThought?.enabled) {
      enhancedTechniques.chainOfThought = existingTechniques.chainOfThought;
    }
    if (existingTechniques.fewShot?.enabled) {
      enhancedTechniques.fewShot = existingTechniques.fewShot;
    }
    if (existingTechniques.rolePlay) {
      enhancedTechniques.rolePlay = existingTechniques.rolePlay;
    }

    // Apply techniques to template (only if enabled)
    const techniques = enhancedTechniques;

    // Apply few-shot learning first
    if (techniques.fewShot?.enabled) {
      const fewShotResult = buildFewShotPrompt(enhancedTemplate, techniques.fewShot);
      if (fewShotResult._tag === "Left") {
        return fewShotResult;
      }
      enhancedTemplate = fewShotResult.right;
    }

    // Apply chain-of-thought
    if (techniques.chainOfThought?.enabled) {
      const cotResult = applyChainOfThought(enhancedTemplate, techniques.chainOfThought);
      if (cotResult._tag === "Left") {
        return cotResult;
      }
      enhancedTemplate = cotResult.right;
    }

    // Apply output format
    if (techniques.outputFormat) {
      const formatResult = applyOutputFormat(enhancedTemplate, techniques.outputFormat);
      if (formatResult._tag === "Left") {
        return formatResult;
      }
      enhancedTemplate = formatResult.right;
    }

    // Apply constraints
    if (techniques.constraints && techniques.constraints.length > 0) {
      const constraintResult = buildConstraintPrompt(enhancedTemplate, techniques.constraints);
      if (constraintResult._tag === "Left") {
        return constraintResult;
      }
      enhancedTemplate = constraintResult.right;
    }

    return success({
      ...input,
      template: enhancedTemplate,
      techniques: enhancedTechniques as PromptTechniques,
      metadata: {
        ...input.metadata,
        complexity: analysis.complexity,
        language: analysis.language,
        category: analysis.category,
        tags: [...(input.metadata?.tags || []), ...(analysis.tags || [])],
        generatedAt: new Date().toISOString(),
        version: input.metadata?.version || "4.0",
      },
    } as PromptInput);
  } catch (error) {
    return failure(
      createQiError(
        "PROMPT_ENHANCEMENT_ERROR",
        `Failed to enhance prompt with patterns: ${error}`,
        "SYSTEM",
        { error: String(error) }
      )
    );
  }
};

// Legacy exports for backward compatibility
export const PromptPatternOrchestrator = {
  enhance: enhancePrompt,
};

export const ChainOfThoughtBuilder = {
  build: buildExplicitChainOfThought,
  buildExplicit: buildExplicitChainOfThought,
  buildImplicit: buildImplicitChainOfThought,
  buildGuided: buildGuidedChainOfThought,
};

export const FewShotBuilder = {
  build: buildFewShotPrompt,
};

export const RolePlayBuilder = {
  build: buildRolePlaySystemPrompt,
  buildSystemPrompt: buildRolePlaySystemPrompt,
  buildUserPrompt: buildRolePlayUserPrompt,
};

export const OutputFormatBuilder = {
  buildJsonFormat: buildJsonFormat,
  buildMarkdownFormat: buildMarkdownFormat,
  buildStructuredFormat: buildStructuredFormat,
};

export const ConstraintBuilder = {
  build: buildConstraintPrompt,
};

export const PromptEnhancer = {
  enhance: enhancePrompt,
};
