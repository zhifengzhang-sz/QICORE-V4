/**
 * @fileoverview Comprehensive tests for QiLangChain Adapter
 * @purpose Achieve >90% coverage for LangChain integration
 */

import { AIMessage, HumanMessage, SystemMessage } from "@langchain/core/messages";
import { ChatPromptTemplate, PromptTemplate } from "@langchain/core/prompts";
import { beforeEach, describe, expect, it } from "vitest";
import {
  QiLangChainAdapter,
  createDefaultAdapter,
  createDevAdapter,
  deserializeMessage,
  serializeMessage,
} from "../../../src/qiprompt/adapters/langchain";
import type { PromptInput } from "../../../src/qiprompt/core/types";

describe("QiLangChain Adapter", () => {
  let adapter: QiLangChainAdapter;

  beforeEach(() => {
    adapter = new QiLangChainAdapter();
  });

  describe("Constructor and Configuration", () => {
    it("should create adapter with default configuration", () => {
      const defaultAdapter = new QiLangChainAdapter();
      expect(defaultAdapter).toBeInstanceOf(QiLangChainAdapter);
    });

    it("should create adapter with custom configuration", () => {
      const customAdapter = new QiLangChainAdapter({
        strictMode: false,
        validateInputs: false,
      });
      expect(customAdapter).toBeInstanceOf(QiLangChainAdapter);
    });

    it("should create adapter with partial configuration", () => {
      const partialAdapter = new QiLangChainAdapter({
        strictMode: true,
      });
      expect(partialAdapter).toBeInstanceOf(QiLangChainAdapter);
    });
  });

  describe("createPromptTemplate method", () => {
    it("should create PromptTemplate from string input", () => {
      const input: PromptInput = {
        template: "Hello {name}, welcome to {place}!",
        variables: { name: "Alice", place: "Wonderland" },
      };

      const result = adapter.createPromptTemplate(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBeInstanceOf(PromptTemplate);
        expect(result.right.inputVariables).toContain("name");
        expect(result.right.inputVariables).toContain("place");
      }
    });

    it("should handle existing PromptTemplate instance", () => {
      const existingTemplate = PromptTemplate.fromTemplate("Test {input}");
      const input: PromptInput = {
        template: existingTemplate,
        variables: { input: "value" },
      };

      const result = adapter.createPromptTemplate(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBe(existingTemplate);
      }
    });

    it("should validate input when validation is enabled", () => {
      const validatingAdapter = new QiLangChainAdapter({ validateInputs: true });
      const invalidInput: PromptInput = {
        template: null as any,
        variables: null as any,
      };

      const result = validatingAdapter.createPromptTemplate(invalidInput);
      expect(result._tag).toBe("Left");
    });

    it("should skip validation when disabled", () => {
      const nonValidatingAdapter = new QiLangChainAdapter({ validateInputs: false });
      const input: PromptInput = {
        template: "Simple {test}",
        variables: { test: "value" },
      };

      const result = nonValidatingAdapter.createPromptTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle missing variables in strict mode", () => {
      const strictAdapter = new QiLangChainAdapter({ strictMode: true });
      const input: PromptInput = {
        template: "Hello {name} and {friend}",
        variables: { name: "Alice" }, // missing 'friend'
      };

      const result = strictAdapter.createPromptTemplate(input);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("MISSING_VARIABLES");
      }
    });

    it("should allow missing variables in non-strict mode", () => {
      const nonStrictAdapter = new QiLangChainAdapter({ strictMode: false });
      const input: PromptInput = {
        template: "Hello {name} and {friend}",
        variables: { name: "Alice" }, // missing 'friend'
      };

      const result = nonStrictAdapter.createPromptTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle invalid template types", () => {
      const input: PromptInput = {
        template: 123 as any, // Invalid type
        variables: {},
      };

      const result = adapter.createPromptTemplate(input);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("INVALID_TEMPLATE_TYPE");
      }
    });

    it("should handle template creation errors", () => {
      const input: PromptInput = {
        template: "{unclosed_variable",
        variables: {},
      };

      const result = adapter.createPromptTemplate(input);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("TEMPLATE_CREATION_ERROR");
      }
    });
  });

  describe("createChatTemplate method", () => {
    it("should create ChatPromptTemplate from string input", () => {
      const input: PromptInput = {
        template: "Analyze {topic}",
        variables: { topic: "machine learning" },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBeInstanceOf(ChatPromptTemplate);
      }
    });

    it("should handle existing ChatPromptTemplate instance", () => {
      const existingTemplate = ChatPromptTemplate.fromMessages([["human", "Test {input}"]]);
      const input: PromptInput = {
        template: existingTemplate,
        variables: { input: "value" },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBe(existingTemplate);
      }
    });

    it("should create chat template with role-play technique", () => {
      const input: PromptInput = {
        template: "Explain {concept}",
        variables: { concept: "quantum physics" },
        techniques: {
          rolePlay: {
            persona: "physics professor",
            expertise: "quantum mechanics",
            communication: "educational and clear",
          },
        },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.inputVariables).toContain("concept");
      }
    });

    it("should create chat template with system context", () => {
      const input: PromptInput = {
        template: "Process {data}",
        variables: { data: "information" },
        context: {
          systemContext: "You are a helpful data processing assistant.",
        },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should create chat template with few-shot examples", () => {
      const input: PromptInput = {
        template: "Classify {text}",
        variables: { text: "sample text" },
        techniques: {
          fewShot: {
            enabled: true,
            examples: [
              { input: "Great product!", output: "positive" },
              { input: "Terrible service", output: "negative" },
            ],
          },
        },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should create chat template with conversation history", () => {
      const input: PromptInput = {
        template: "Continue the conversation about {topic}",
        variables: { topic: "AI ethics" },
        context: {
          conversation: [
            { role: "user", content: "What do you think about AI ethics?" },
            { role: "assistant", content: "AI ethics is crucial..." },
          ],
        },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should create chat template with chain-of-thought (explicit)", () => {
      const input: PromptInput = {
        template: "Solve {problem}",
        variables: { problem: "2x + 5 = 11" },
        techniques: {
          chainOfThought: {
            enabled: true,
            reasoning: "explicit",
            steps: ["Identify the equation", "Isolate the variable", "Calculate the result"],
          },
        },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should create chat template with chain-of-thought (guided)", () => {
      const input: PromptInput = {
        template: "Analyze {situation}",
        variables: { situation: "market trends" },
        techniques: {
          chainOfThought: {
            enabled: true,
            reasoning: "guided",
            steps: ["Current market conditions", "Historical patterns", "Future predictions"],
          },
        },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle chat template creation errors", () => {
      const input: PromptInput = {
        template: null as any,
        variables: {},
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("CHAT_TEMPLATE_CREATION_ERROR");
      }
    });
  });

  describe("formatMessages method", () => {
    it("should format messages with valid variables", async () => {
      const template = ChatPromptTemplate.fromMessages([
        ["system", "You are a helpful assistant"],
        ["human", "Hello {name}"],
      ]);
      const variables = { name: "Alice" };

      const result = await adapter.formatMessages(template, variables);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toHaveLength(2);
        expect(result.right[0].content).toContain("helpful assistant");
        expect(result.right[1].content).toContain("Hello Alice");
      }
    });

    it("should handle missing required variables", async () => {
      const template = ChatPromptTemplate.fromMessages([["human", "Hello {name} and {friend}"]]);
      const variables = { name: "Alice" }; // missing 'friend'

      const result = await adapter.formatMessages(template, variables);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("MISSING_REQUIRED_VARIABLES");
      }
    });

    it("should handle formatting errors", async () => {
      const template = ChatPromptTemplate.fromMessages([["human", "Test {variable}"]]);
      const variables = { variable: undefined }; // Problematic value

      const result = await adapter.formatMessages(template, variables);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("MESSAGE_FORMATTING_ERROR");
      }
    });

    it("should format complex messages with multiple variables", async () => {
      const template = ChatPromptTemplate.fromMessages([
        ["system", "You are {role} with expertise in {domain}"],
        ["human", "Explain {topic} in the context of {context}"],
      ]);
      const variables = {
        role: "teacher",
        domain: "computer science",
        topic: "algorithms",
        context: "data structures",
      };

      const result = await adapter.formatMessages(template, variables);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toHaveLength(2);
      }
    });
  });

  describe("validateTemplate method", () => {
    it("should validate correct PromptTemplate", () => {
      const template = PromptTemplate.fromTemplate("Hello {name}");
      const result = adapter.validateTemplate(template);
      expect(result._tag).toBe("Right");
    });

    it("should validate correct ChatPromptTemplate", () => {
      const template = ChatPromptTemplate.fromMessages([["human", "Hello {name}"]]);
      const result = adapter.validateTemplate(template);
      expect(result._tag).toBe("Right");
    });

    it("should reject null template", () => {
      const result = adapter.validateTemplate(null as any);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("INVALID_TEMPLATE");
      }
    });

    it("should reject template with invalid input variables", () => {
      const template = PromptTemplate.fromTemplate("Hello {name}");
      // Manually corrupt the inputVariables
      (template as any).inputVariables = "not-an-array";

      const result = adapter.validateTemplate(template);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("INVALID_INPUT_VARIABLES");
      }
    });

    it("should detect duplicate variables", () => {
      const template = PromptTemplate.fromTemplate("Hello {name}");
      // Manually add duplicate variables
      (template as any).inputVariables = ["name", "name", "age"];

      const result = adapter.validateTemplate(template);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("DUPLICATE_VARIABLES");
      }
    });

    it("should handle validation errors", () => {
      const template = PromptTemplate.fromTemplate("Hello {name}");
      // Create a scenario that would throw an error
      Object.defineProperty(template, "inputVariables", {
        get() {
          throw new Error("Test error");
        },
      });

      const result = adapter.validateTemplate(template);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("TEMPLATE_VALIDATION_ERROR");
      }
    });
  });

  describe("Private helper methods coverage", () => {
    it("should cover extractVariablesFromTemplate", () => {
      const input: PromptInput = {
        template: "Process {data} with {config} and {options}",
        variables: { data: "test", config: "settings", options: "params" },
      };

      const result = adapter.createPromptTemplate(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.inputVariables).toHaveLength(3);
        expect(result.right.inputVariables).toContain("data");
        expect(result.right.inputVariables).toContain("config");
        expect(result.right.inputVariables).toContain("options");
      }
    });

    it("should cover buildSystemPrompt", () => {
      const input: PromptInput = {
        template: "Help with {task}",
        variables: { task: "coding" },
        techniques: {
          rolePlay: {
            persona: "senior developer",
            expertise: "software engineering",
            communication: "mentoring and supportive",
          },
        },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should cover buildChainOfThoughtTemplate with explicit reasoning", () => {
      const input: PromptInput = {
        template: "Solve {equation}",
        variables: { equation: "x^2 + 2x + 1 = 0" },
        techniques: {
          chainOfThought: {
            enabled: true,
            reasoning: "explicit",
            steps: [
              "Identify the quadratic equation",
              "Apply the quadratic formula",
              "Simplify the result",
            ],
          },
        },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should cover buildChainOfThoughtTemplate with guided reasoning", () => {
      const input: PromptInput = {
        template: "Analyze {data}",
        variables: { data: "sales figures" },
        techniques: {
          chainOfThought: {
            enabled: true,
            reasoning: "guided",
            steps: [
              "Examine the data quality",
              "Identify trends and patterns",
              "Draw actionable insights",
            ],
          },
        },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should cover buildChainOfThoughtTemplate when disabled", () => {
      const input: PromptInput = {
        template: "Simple {request}",
        variables: { request: "task" },
        techniques: {
          chainOfThought: {
            enabled: false,
            reasoning: "explicit",
            steps: [],
          },
        },
      };

      const result = adapter.createChatTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should cover validatePromptInput", () => {
      const invalidInputs = [
        null,
        undefined,
        { template: null, variables: {} },
        { template: "test", variables: null },
        { template: "test", variables: "not-object" },
      ];

      invalidInputs.forEach((input) => {
        const result = adapter.createPromptTemplate(input as any);
        expect(result._tag).toBe("Left");
      });
    });

    it("should cover validateVariables", async () => {
      const template = ChatPromptTemplate.fromMessages([["human", "Hello {name} and {age}"]]);

      const validResult = await adapter.formatMessages(template, { name: "Alice", age: 25 });
      expect(validResult._tag).toBe("Right");

      const invalidResult = await adapter.formatMessages(template, { name: "Alice" });
      expect(invalidResult._tag).toBe("Left");
    });

    it("should cover sanitizeInput", () => {
      const sensitiveInput: PromptInput = {
        template: "Process {secret_data}",
        variables: { secret_data: "confidential information" },
        metadata: { name: "Test Template" },
        techniques: {
          rolePlay: { persona: "assistant", expertise: "general", communication: "helpful" },
        },
      };

      // This should trigger sanitization when logging errors
      const result = adapter.createPromptTemplate({ ...sensitiveInput, template: 123 as any });
      expect(result._tag).toBe("Left");
    });
  });

  describe("Error handling and edge cases", () => {
    it("should handle empty template strings", () => {
      const input: PromptInput = {
        template: "",
        variables: {},
      };

      const result = adapter.createPromptTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle templates with no variables", () => {
      const input: PromptInput = {
        template: "This is a static template",
        variables: {},
      };

      const result = adapter.createPromptTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle complex variable extraction", () => {
      const input: PromptInput = {
        template: "Process {data.field} and {config[key]} with {nested.deep.value}",
        variables: {
          "data.field": "test1",
          "config[key]": "test2",
          "nested.deep.value": "test3",
        },
      };

      const result = adapter.createPromptTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle malformed variable patterns", () => {
      const input: PromptInput = {
        template: "Test {unclosed and }unopened{ and {{double}}",
        variables: {},
      };

      const result = adapter.createPromptTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle very long templates", () => {
      const longTemplate = "Process ".repeat(1000) + "{data}";
      const input: PromptInput = {
        template: longTemplate,
        variables: { data: "test" },
      };

      const result = adapter.createPromptTemplate(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle unicode and special characters in templates", () => {
      const input: PromptInput = {
        template: "生成关于{主题}的内容，使用🎨和⭐等符号",
        variables: { 主题: "人工智能" },
      };

      const result = adapter.createPromptTemplate(input);
      expect(result._tag).toBe("Right");
    });
  });
});

describe("Utility Functions", () => {
  describe("createDefaultAdapter", () => {
    it("should create adapter with default settings", () => {
      const adapter = createDefaultAdapter();
      expect(adapter).toBeInstanceOf(QiLangChainAdapter);
    });
  });

  describe("createDevAdapter", () => {
    it("should create adapter with development settings", () => {
      const adapter = createDevAdapter();
      expect(adapter).toBeInstanceOf(QiLangChainAdapter);
    });
  });

  describe("serializeMessage", () => {
    it("should serialize SystemMessage", () => {
      const message = new SystemMessage("You are a helpful assistant");
      const result = serializeMessage(message);

      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.type).toBe("system");
        expect(result.right.content).toBe("You are a helpful assistant");
      }
    });

    it("should serialize HumanMessage", () => {
      const message = new HumanMessage("Hello, how are you?");
      const result = serializeMessage(message);

      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.type).toBe("human");
        expect(result.right.content).toBe("Hello, how are you?");
      }
    });

    it("should serialize AIMessage", () => {
      const message = new AIMessage("I'm doing well, thank you!");
      const result = serializeMessage(message);

      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.type).toBe("ai");
        expect(result.right.content).toBe("I'm doing well, thank you!");
      }
    });

    it("should serialize message with metadata", () => {
      const message = new HumanMessage({
        content: "Test message",
        additional_kwargs: { source: "test", timestamp: 123456 },
      });
      const result = serializeMessage(message);

      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.metadata).toEqual({ source: "test", timestamp: 123456 });
      }
    });

    it("should handle complex content", () => {
      const complexContent = { text: "Hello", data: [1, 2, 3] };
      const message = new AIMessage(complexContent as any);
      const result = serializeMessage(message);

      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.content).toBe(JSON.stringify(complexContent));
      }
    });

    it("should handle serialization errors", () => {
      const problematicMessage = {
        _getType: () => {
          throw new Error("Test error");
        },
        content: "test",
      } as any;

      const result = serializeMessage(problematicMessage);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("MESSAGE_SERIALIZATION_ERROR");
      }
    });
  });

  describe("deserializeMessage", () => {
    it("should deserialize system message", () => {
      const data = {
        type: "system",
        content: "You are a helpful assistant",
        metadata: { role: "assistant" },
      };

      const result = deserializeMessage(data);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBeInstanceOf(SystemMessage);
        expect(result.right.content).toBe("You are a helpful assistant");
      }
    });

    it("should deserialize human message", () => {
      const data = {
        type: "human",
        content: "Hello world",
        metadata: { user_id: "123" },
      };

      const result = deserializeMessage(data);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBeInstanceOf(HumanMessage);
        expect(result.right.content).toBe("Hello world");
      }
    });

    it("should deserialize AI message", () => {
      const data = {
        type: "ai",
        content: "Hello back!",
        metadata: { model: "gpt-4" },
      };

      const result = deserializeMessage(data);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBeInstanceOf(AIMessage);
        expect(result.right.content).toBe("Hello back!");
      }
    });

    it("should handle missing metadata", () => {
      const data = {
        type: "human",
        content: "Test without metadata",
      };

      const result = deserializeMessage(data);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right).toBeInstanceOf(HumanMessage);
      }
    });

    it("should reject unknown message types", () => {
      const data = {
        type: "unknown",
        content: "Invalid message type",
      };

      const result = deserializeMessage(data);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("UNKNOWN_MESSAGE_TYPE");
      }
    });

    it("should handle deserialization errors", () => {
      const problematicData = {
        get type() {
          throw new Error("Test error");
        },
        content: "test",
      };

      const result = deserializeMessage(problematicData);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("MESSAGE_DESERIALIZATION_ERROR");
      }
    });
  });
});
