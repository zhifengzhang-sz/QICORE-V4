/**
 * @fileoverview Comprehensive tests for QiPrompt Pattern Functions
 * @purpose Achieve >90% coverage for prompt engineering patterns
 */

import { describe, expect, it } from "vitest";
import type {
  ChainOfThoughtConfig,
  FewShotConfig,
  PromptInput,
} from "../../../src/qiprompt/core/types";
import {
  // Legacy imports for backward compatibility testing
  ChainOfThoughtBuilder,
  FewShotBuilder,
  PromptEnhancer,
  RolePlayBuilder,
  buildExplicitChainOfThought,
  buildFewShotPrompt,
  buildRolePlaySystemPrompt,
  enhancePrompt,
} from "../../../src/qiprompt/patterns/index";

describe("QiPrompt Pattern Functions", () => {
  describe("buildExplicitChainOfThought", () => {
    it("should build explicit chain of thought with steps", () => {
      const basePrompt = "Solve the equation: 2x + 5 = 11";
      const config: ChainOfThoughtConfig = {
        enabled: true,
        reasoning: "explicit",
        steps: [
          "Identify the equation type",
          "Isolate the variable x",
          "Calculate the final result",
          "Verify the solution",
        ],
      };

      const result = buildExplicitChainOfThought(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain(basePrompt);
        expect(result.right).toContain("Let's think about this step by step:");
        expect(result.right).toContain("1. Identify the equation type");
        expect(result.right).toContain("2. Isolate the variable x");
        expect(result.right).toContain("3. Calculate the final result");
        expect(result.right).toContain("4. Verify the solution");
        expect(result.right).toContain("Please work through each step systematically");
      }
    });

    it("should handle empty steps array", () => {
      const basePrompt = "Simple problem";
      const config: ChainOfThoughtConfig = {
        enabled: true,
        reasoning: "explicit",
        steps: [],
      };

      const result = buildExplicitChainOfThought(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain(basePrompt);
        expect(result.right).toContain("Let's think about this step by step:");
      }
    });

    it("should handle single step", () => {
      const basePrompt = "What is 2 + 2?";
      const config: ChainOfThoughtConfig = {
        enabled: true,
        reasoning: "explicit",
        steps: ["Add the numbers together"],
      };

      const result = buildExplicitChainOfThought(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("1. Add the numbers together");
      }
    });

    it("should handle very long steps", () => {
      const basePrompt = "Complex analysis";
      const longStep =
        "This is a very long step description that contains detailed instructions and explanations that might exceed normal length expectations for step descriptions in chain of thought reasoning".repeat(
          3
        );
      const config: ChainOfThoughtConfig = {
        enabled: true,
        reasoning: "explicit",
        steps: [longStep, "Short step"],
      };

      const result = buildExplicitChainOfThought(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain(longStep);
        expect(result.right).toContain("Short step");
      }
    });

    it("should handle steps with special characters", () => {
      const basePrompt = "Unicode test";
      const config: ChainOfThoughtConfig = {
        enabled: true,
        reasoning: "explicit",
        steps: [
          "Step with émojis 🤔 and symbols ∑",
          "Step with 中文 characters",
          "Step with mathematical symbols: ∀x ∈ ℝ, x² ≥ 0",
        ],
      };

      const result = buildExplicitChainOfThought(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("émojis 🤔");
        expect(result.right).toContain("中文");
        expect(result.right).toContain("∀x ∈ ℝ");
      }
    });

    it("should handle empty base prompt", () => {
      const config: ChainOfThoughtConfig = {
        enabled: true,
        reasoning: "explicit",
        steps: ["Step 1", "Step 2"],
      };

      const result = buildExplicitChainOfThought("", config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("Let's think about this step by step:");
        expect(result.right).toContain("1. Step 1");
        expect(result.right).toContain("2. Step 2");
      }
    });

    it("should handle error conditions", () => {
      const basePrompt = "Test";
      const config = null as any;

      const result = buildExplicitChainOfThought(basePrompt, config);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("CHAIN_OF_THOUGHT_ERROR");
      }
    });
  });

  describe("buildFewShotPrompt", () => {
    it("should build few-shot prompt with examples", () => {
      const basePrompt = "Classify the sentiment of the following text: {text}";
      const config: FewShotConfig = {
        enabled: true,
        examples: [
          { input: "I love this product!", output: "positive" },
          { input: "This is terrible quality", output: "negative" },
          { input: "The item is okay, nothing special", output: "neutral" },
        ],
        template: "Input: {input}\nOutput: {output}",
        separator: "\n\n",
      };

      const result = buildFewShotPrompt(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("Here are some examples:");
        expect(result.right).toContain("Input: I love this product!");
        expect(result.right).toContain("Output: positive");
        expect(result.right).toContain("Input: This is terrible quality");
        expect(result.right).toContain("Output: negative");
        expect(result.right).toContain("Input: The item is okay, nothing special");
        expect(result.right).toContain("Output: neutral");
        expect(result.right).toContain(basePrompt);
      }
    });

    it("should use default template when not provided", () => {
      const basePrompt = "Translate to French: {text}";
      const config: FewShotConfig = {
        enabled: true,
        examples: [
          { input: "Hello", output: "Bonjour" },
          { input: "Goodbye", output: "Au revoir" },
        ],
      };

      const result = buildFewShotPrompt(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("Input: Hello");
        expect(result.right).toContain("Expected Output: Bonjour");
        expect(result.right).toContain("Input: Goodbye");
        expect(result.right).toContain("Expected Output: Au revoir");
      }
    });

    it("should use custom separator", () => {
      const basePrompt = "Test prompt";
      const config: FewShotConfig = {
        enabled: true,
        examples: [
          { input: "A", output: "1" },
          { input: "B", output: "2" },
        ],
        separator: " | ",
      };

      const result = buildFewShotPrompt(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain(" | ");
      }
    });

    it("should handle single example", () => {
      const basePrompt = "Complete the pattern: {input}";
      const config: FewShotConfig = {
        enabled: true,
        examples: [{ input: "1, 2, 3, ?", output: "4" }],
      };

      const result = buildFewShotPrompt(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("Here are some examples:");
        expect(result.right).toContain("1, 2, 3, ?");
        expect(result.right).toContain("4");
      }
    });

    it("should handle empty examples array", () => {
      const basePrompt = "No examples";
      const config: FewShotConfig = {
        enabled: true,
        examples: [],
      };

      const result = buildFewShotPrompt(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBe(basePrompt);
      }
    });

    it("should handle examples with complex inputs/outputs", () => {
      const basePrompt = "Format data: {data}";
      const config: FewShotConfig = {
        enabled: true,
        examples: [
          {
            input: '{"name": "John", "age": 30}',
            output: "Name: John, Age: 30 years old",
          },
          {
            input: '{"name": "Alice", "age": 25}',
            output: "Name: Alice, Age: 25 years old",
          },
        ],
      };

      const result = buildFewShotPrompt(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain('{"name": "John", "age": 30}');
        expect(result.right).toContain("Name: John, Age: 30 years old");
      }
    });

    it("should handle examples with unicode and special characters", () => {
      const basePrompt = "Translate symbols";
      const config: FewShotConfig = {
        enabled: true,
        examples: [
          { input: "🚀", output: "rocket" },
          { input: "∑", output: "sum symbol" },
          { input: "café", output: "coffee shop" },
        ],
      };

      const result = buildFewShotPrompt(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("🚀");
        expect(result.right).toContain("rocket");
        expect(result.right).toContain("∑");
        expect(result.right).toContain("café");
      }
    });

    it("should handle custom template with multiple placeholders", () => {
      const basePrompt = "Math problems";
      const config: FewShotConfig = {
        enabled: true,
        examples: [
          { input: "2 + 3", output: "5" },
          { input: "4 * 2", output: "8" },
        ],
        template: "Problem: {input} → Solution: {output}",
      };

      const result = buildFewShotPrompt(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("Problem: 2 + 3 → Solution: 5");
        expect(result.right).toContain("Problem: 4 * 2 → Solution: 8");
      }
    });

    it("should handle error conditions", () => {
      const basePrompt = "Test";
      const config = null as any;

      const result = buildFewShotPrompt(basePrompt, config);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("FEW_SHOT_ERROR");
      }
    });
  });

  describe("buildRolePlaySystemPrompt", () => {
    it("should build complete role-play system prompt", () => {
      const persona = "experienced data scientist";
      const expertise = "machine learning and statistical analysis";
      const communication = "clear, educational, and encouraging";

      const result = buildRolePlaySystemPrompt(persona, expertise, communication);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("You are experienced data scientist");
        expect(result.right).toContain("expertise in machine learning and statistical analysis");
        expect(result.right).toContain(
          "communication style is clear, educational, and encouraging"
        );
        expect(result.right).toContain("respond according to this role");
        expect(result.right).toContain("maintain consistency");
      }
    });

    it("should handle single-word inputs", () => {
      const result = buildRolePlaySystemPrompt("teacher", "mathematics", "patient");
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("You are teacher");
        expect(result.right).toContain("expertise in mathematics");
        expect(result.right).toContain("communication style is patient");
      }
    });

    it("should handle complex persona descriptions", () => {
      const persona =
        "senior software engineer with 10+ years of experience in distributed systems";
      const expertise = "microservices architecture, cloud computing, and DevOps practices";
      const communication = "technical yet accessible, with practical examples and best practices";

      const result = buildRolePlaySystemPrompt(persona, expertise, communication);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain(persona);
        expect(result.right).toContain(expertise);
        expect(result.right).toContain(communication);
      }
    });

    it("should handle unicode and special characters", () => {
      const persona = "médecin français spécialisé";
      const expertise = "médecine générale et pédiatrie";
      const communication = "empathique et professionnelle";

      const result = buildRolePlaySystemPrompt(persona, expertise, communication);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("médecin français");
        expect(result.right).toContain("médecine générale");
        expect(result.right).toContain("empathique");
      }
    });

    it("should handle empty inputs", () => {
      const result = buildRolePlaySystemPrompt("", "", "");
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("You are ");
        expect(result.right).toContain("expertise in ");
        expect(result.right).toContain("communication style is ");
      }
    });

    it("should handle very long descriptions", () => {
      const longPersona =
        "highly experienced senior consultant with extensive background in multiple domains including technology, business strategy, organizational development, and change management".repeat(
          3
        );
      const longExpertise =
        "enterprise architecture, digital transformation, agile methodologies, lean processes, data analytics, artificial intelligence, machine learning, cloud computing, cybersecurity, and project management".repeat(
          2
        );
      const longCommunication =
        "articulate, thorough, analytical, strategic thinking, collaborative, results-oriented, client-focused, and able to explain complex concepts in simple terms while maintaining professional credibility".repeat(
          2
        );

      const result = buildRolePlaySystemPrompt(longPersona, longExpertise, longCommunication);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.length).toBeGreaterThan(1000);
        expect(result.right).toContain(longPersona);
      }
    });

    it("should handle error conditions", () => {
      const result = buildRolePlaySystemPrompt(null as any, undefined as any, "test");
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("ROLE_PLAY_ERROR");
      }
    });
  });

  describe("enhancePrompt", () => {
    it("should enhance simple prompt input", () => {
      const input: PromptInput = {
        template: "Explain {topic}",
        variables: { topic: "quantum computing" },
      };

      const result = enhancePrompt(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.template).toContain("Explain {topic}");
        expect(result.right.variables).toEqual({ topic: "quantum computing" });
        expect(result.right.metadata).toBeDefined();
        expect(result.right.metadata?.complexity).toBeDefined();
      }
    });

    it("should add complexity analysis", () => {
      const simpleInput: PromptInput = {
        template: "Say hello",
        variables: {},
      };

      const complexInput: PromptInput = {
        template:
          "Analyze the multifaceted implications of {topic} considering {context} and {constraints} while accounting for {variables}",
        variables: {
          topic: "economic policy",
          context: "global markets",
          constraints: "regulatory framework",
          variables: "political factors",
        },
      };

      const simpleResult = enhancePrompt(simpleInput);
      const complexResult = enhancePrompt(complexInput);

      expect(simpleResult._tag).toBe("Right");
      expect(complexResult._tag).toBe("Right");

      if (simpleResult._tag === "Right" && complexResult._tag === "Right") {
        expect(simpleResult.right.metadata?.complexity).toBe("simple");
        expect(complexResult.right.metadata?.complexity).toBe("complex");
      }
    });

    it("should suggest appropriate techniques based on content", () => {
      const mathematicalInput: PromptInput = {
        template: "Solve the equation {equation} step by step",
        variables: { equation: "x^2 + 5x + 6 = 0" },
      };

      const classificationInput: PromptInput = {
        template: "Classify the sentiment of {text}",
        variables: { text: "sample text" },
      };

      const expertInput: PromptInput = {
        template: "As a doctor, diagnose the symptoms: {symptoms}",
        variables: { symptoms: "fever, headache" },
      };

      const mathResult = enhancePrompt(mathematicalInput);
      const classifyResult = enhancePrompt(classificationInput);
      const expertResult = enhancePrompt(expertInput);

      expect(mathResult._tag).toBe("Right");
      expect(classifyResult._tag).toBe("Right");
      expect(expertResult._tag).toBe("Right");

      if (mathResult._tag === "Right") {
        expect(mathResult.right.techniques?.chainOfThought?.enabled).toBe(true);
        expect(mathResult.right.techniques?.chainOfThought?.reasoning).toBe("explicit");
      }

      if (classifyResult._tag === "Right") {
        expect(classifyResult.right.techniques?.fewShot?.enabled).toBe(true);
      }

      if (expertResult._tag === "Right") {
        expect(expertResult.right.techniques?.rolePlay?.persona).toBeDefined();
      }
    });

    it("should preserve existing techniques while adding new ones", () => {
      const input: PromptInput = {
        template: "Solve {problem} step by step",
        variables: { problem: "math equation" },
        techniques: {
          rolePlay: {
            persona: "math tutor",
            expertise: "algebra",
            communication: "patient",
          },
        },
      };

      const result = enhancePrompt(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.techniques?.rolePlay?.persona).toBe("math tutor");
        expect(result.right.techniques?.chainOfThought?.enabled).toBe(true);
      }
    });

    it("should add appropriate tags based on content analysis", () => {
      const codingInput: PromptInput = {
        template: "Debug this Python code: {code}",
        variables: { code: "def hello(): print('world')" },
      };

      const analysisInput: PromptInput = {
        template: "Analyze the data trends in {dataset}",
        variables: { dataset: "sales figures" },
      };

      const creativeInput: PromptInput = {
        template: "Write a story about {character}",
        variables: { character: "brave knight" },
      };

      const codingResult = enhancePrompt(codingInput);
      const analysisResult = enhancePrompt(analysisInput);
      const creativeResult = enhancePrompt(creativeInput);

      expect(codingResult._tag).toBe("Right");
      expect(analysisResult._tag).toBe("Right");
      expect(creativeResult._tag).toBe("Right");

      if (codingResult._tag === "Right") {
        expect(codingResult.right.metadata?.tags).toContain("programming");
        expect(codingResult.right.metadata?.category).toBe("technical");
      }

      if (analysisResult._tag === "Right") {
        expect(analysisResult.right.metadata?.tags).toContain("analysis");
        expect(analysisResult.right.metadata?.category).toBe("analytical");
      }

      if (creativeResult._tag === "Right") {
        expect(creativeResult.right.metadata?.tags).toContain("creative");
        expect(creativeResult.right.metadata?.category).toBe("creative");
      }
    });

    it("should handle inputs with existing metadata", () => {
      const input: PromptInput = {
        template: "Process {data}",
        variables: { data: "input" },
        metadata: {
          name: "Existing Template",
          description: "Pre-existing description",
          version: "1.0.0",
          tags: ["existing", "tag"],
        },
      };

      const result = enhancePrompt(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.metadata?.name).toBe("Existing Template");
        expect(result.right.metadata?.description).toBe("Pre-existing description");
        expect(result.right.metadata?.version).toBe("1.0.0");
        expect(result.right.metadata?.tags).toContain("existing");
        expect(result.right.metadata?.tags).toContain("tag");
        expect(result.right.metadata?.complexity).toBeDefined();
      }
    });

    it("should handle edge cases and complex scenarios", () => {
      const edgeCases = [
        {
          template: "",
          variables: {},
        },
        {
          template: "No variables here",
          variables: {},
        },
        {
          template: "{a}{b}{c}{d}{e}{f}{g}{h}{i}{j}",
          variables: { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8, i: 9, j: 10 },
        },
      ];

      edgeCases.forEach((input, index) => {
        const result = enhancePrompt(input);
        expect(result._tag).toBe("Right");
        if (result._tag === "Right") {
          expect(result.right.metadata).toBeDefined();
        }
      });
    });

    it("should handle unicode and special characters in analysis", () => {
      const input: PromptInput = {
        template: "分析{数据}并生成{报告}，考虑到{因素}和约束条件{限制}",
        variables: {
          数据: "销售数据",
          报告: "月度报告",
          因素: "市场因素",
          限制: "预算限制",
        },
      };

      const result = enhancePrompt(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.metadata?.language).toBe("chinese");
        expect(result.right.metadata?.complexity).toBeDefined();
      }
    });

    it("should handle error conditions", () => {
      const invalidInput = null as any;
      const result = enhancePrompt(invalidInput);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("PROMPT_ENHANCEMENT_ERROR");
      }
    });
  });

  describe("Legacy backward compatibility", () => {
    it("should provide ChainOfThoughtBuilder class", () => {
      expect(ChainOfThoughtBuilder).toBeDefined();
      expect(ChainOfThoughtBuilder.build).toBeInstanceOf(Function);
    });

    it("should provide FewShotBuilder class", () => {
      expect(FewShotBuilder).toBeDefined();
      expect(FewShotBuilder.build).toBeInstanceOf(Function);
    });

    it("should provide RolePlayBuilder class", () => {
      expect(RolePlayBuilder).toBeDefined();
      expect(RolePlayBuilder.build).toBeInstanceOf(Function);
    });

    it("should provide PromptEnhancer class", () => {
      expect(PromptEnhancer).toBeDefined();
      expect(PromptEnhancer.enhance).toBeInstanceOf(Function);
    });

    it("should have legacy methods work correctly", () => {
      const cotResult = ChainOfThoughtBuilder.build("Test", {
        enabled: true,
        reasoning: "explicit",
        steps: ["Step 1"],
      });
      expect(cotResult._tag).toBe("Right");

      const fewShotResult = FewShotBuilder.build("Test", {
        enabled: true,
        examples: [{ input: "A", output: "B" }],
      });
      expect(fewShotResult._tag).toBe("Right");

      const rolePlayResult = RolePlayBuilder.build("teacher", "math", "patient");
      expect(rolePlayResult._tag).toBe("Right");

      const enhancerResult = PromptEnhancer.enhance({
        template: "Test {input}",
        variables: { input: "value" },
      });
      expect(enhancerResult._tag).toBe("Right");
    });
  });

  describe("Performance and stress testing", () => {
    it("should handle large numbers of examples efficiently", () => {
      const basePrompt = "Classify text";
      const examples = Array.from({ length: 1000 }, (_, i) => ({
        input: `Example text ${i}`,
        output: `Category ${i % 10}`,
      }));

      const config: FewShotConfig = {
        enabled: true,
        examples,
      };

      const result = buildFewShotPrompt(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.length).toBeGreaterThan(10000);
      }
    });

    it("should handle very large chain of thought steps", () => {
      const basePrompt = "Complex problem";
      const steps = Array.from(
        { length: 100 },
        (_, i) =>
          `Step ${i + 1}: This is a detailed step description that explains what needs to be done in this particular phase of the problem-solving process`
      );

      const config: ChainOfThoughtConfig = {
        enabled: true,
        reasoning: "explicit",
        steps,
      };

      const result = buildExplicitChainOfThought(basePrompt, config);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toContain("100. Step 100:");
      }
    });

    it("should handle concurrent enhancement operations", async () => {
      const inputs = Array.from({ length: 10 }, (_, i) => ({
        template: `Process data item ${i} with {parameter}`,
        variables: { parameter: `value_${i}` },
      }));

      const results = await Promise.all(
        inputs.map((input) => Promise.resolve(enhancePrompt(input)))
      );

      results.forEach((result) => {
        expect(result._tag).toBe("Right");
      });
    });
  });

  describe("Integration scenarios", () => {
    it("should work with complex multi-technique prompts", () => {
      // Start with a base prompt
      const basePrompt = "Solve this complex math problem: {equation}";

      // Add chain of thought
      const cotConfig: ChainOfThoughtConfig = {
        enabled: true,
        reasoning: "explicit",
        steps: [
          "Identify the equation type",
          "Choose appropriate solution method",
          "Apply the method step by step",
          "Verify the solution",
        ],
      };
      const cotResult = buildExplicitChainOfThought(basePrompt, cotConfig);
      expect(cotResult._tag).toBe("Right");

      if (cotResult._tag === "Right") {
        // Add few-shot examples
        const fewShotConfig: FewShotConfig = {
          enabled: true,
          examples: [
            { input: "x^2 - 4 = 0", output: "x = ±2" },
            { input: "2x + 6 = 0", output: "x = -3" },
          ],
        };
        const fewShotResult = buildFewShotPrompt(cotResult.right, fewShotConfig);
        expect(fewShotResult._tag).toBe("Right");

        if (fewShotResult._tag === "Right") {
          // Add role-play context
          const rolePlayResult = buildRolePlaySystemPrompt(
            "experienced mathematics tutor",
            "algebra and calculus",
            "patient and encouraging"
          );
          expect(rolePlayResult._tag).toBe("Right");

          if (rolePlayResult._tag === "Right") {
            const finalPrompt = rolePlayResult.right + "\n\n" + fewShotResult.right;
            expect(finalPrompt).toContain("mathematics tutor");
            expect(finalPrompt).toContain("Here are some examples:");
            expect(finalPrompt).toContain("Let's think about this step by step:");
          }
        }
      }
    });
  });
});
