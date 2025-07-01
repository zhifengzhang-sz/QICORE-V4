/**
 * @fileoverview QiPrompt v4.0 - LangChain Adapter Tests
 * @purpose Test LangChain integration adapter functionality
 * @dependencies QiCore Base, LangChain Core, Vitest
 * @version 4.0
 * @date December 30, 2025
 * @status Production-ready implementation
 */

import { HumanMessage, SystemMessage } from "@langchain/core/messages";
import { describe, expect, it } from "vitest";
import { getData, getError, isFailure, isSuccess } from "../../../src/qicore/base/result";
import {
  QiLangChainAdapter,
  createDefaultAdapter,
  deserializeMessage,
  serializeMessage,
} from "../../../src/qiprompt/adapters/langchain";
import type { PromptInput } from "../../../src/qiprompt/core/types";

describe("QiLangChain Adapter", () => {
  let adapter: QiLangChainAdapter;

  beforeEach(() => {
    adapter = createDefaultAdapter();
  });

  describe("PromptTemplate Creation", () => {
    it("should create template from string", () => {
      const input: PromptInput = {
        template: "Hello, {name}! How are you?",
        variables: { name: "Alice" },
      };

      const result = adapter.createPromptTemplate(input);

      expect(isSuccess(result)).toBe(true);
      const template = getData(result);
      expect(template?.inputVariables).toContain("name");
    });

    it("should handle missing variables in strict mode", () => {
      const input: PromptInput = {
        template: "Hello, {name}! Your age is {age}.",
        variables: { name: "Bob" }, // missing 'age'
      };

      const result = adapter.createPromptTemplate(input);

      expect(isFailure(result)).toBe(true);
      const error = getError(result);
      expect(error?.code).toBe("MISSING_VARIABLES");
    });

    it("should extract variables from template", () => {
      const input: PromptInput = {
        template: "Process {data} with {method} and {format}",
        variables: { data: "test", method: "analysis", format: "json" },
      };

      const result = adapter.createPromptTemplate(input);

      expect(isSuccess(result)).toBe(true);
      const template = getData(result);
      expect(template?.inputVariables).toHaveLength(3);
      expect(template?.inputVariables).toEqual(
        expect.arrayContaining(["data", "method", "format"])
      );
    });
  });

  describe("ChatPromptTemplate Creation", () => {
    it("should create chat template with role-play", () => {
      const input: PromptInput = {
        template: "Help me with {task}",
        variables: { task: "coding" },
        techniques: {
          rolePlay: {
            persona: "a helpful programming mentor",
            expertise: "software development",
            communication: "encouraging and detailed",
          },
        },
      };

      const result = adapter.createChatTemplate(input);

      expect(isSuccess(result)).toBe(true);
      const template = getData(result);
      expect(template).toBeDefined();
    });

    it("should create chat template with few-shot examples", () => {
      const input: PromptInput = {
        template: "Classify: {text}",
        variables: { text: "I love this!" },
        techniques: {
          fewShot: {
            enabled: true,
            examples: [
              { input: "Great job!", output: "positive" },
              { input: "This is terrible", output: "negative" },
            ],
            maxExamples: 2,
            selectionStrategy: "manual",
          },
        },
      };

      const result = adapter.createChatTemplate(input);

      expect(isSuccess(result)).toBe(true);
      const template = getData(result);
      expect(template).toBeDefined();
    });

    it("should handle chain-of-thought reasoning", () => {
      const input: PromptInput = {
        template: "Solve: {problem}",
        variables: { problem: "2x + 5 = 15" },
        techniques: {
          chainOfThought: {
            enabled: true,
            steps: ["Isolate x", "Subtract 5", "Divide by 2"],
            reasoning: "explicit",
          },
        },
      };

      const result = adapter.createChatTemplate(input);

      expect(isSuccess(result)).toBe(true);
      const template = getData(result);
      expect(template).toBeDefined();
    });
  });

  describe("Message Formatting", () => {
    it("should format messages with variables", async () => {
      const input: PromptInput = {
        template: "Analyze {content}",
        variables: { content: "sample data" },
      };

      const templateResult = adapter.createChatTemplate(input);
      expect(isSuccess(templateResult)).toBe(true);

      const template = getData(templateResult)!;
      const messageResult = await adapter.formatMessages(template, { content: "test data" });

      expect(isSuccess(messageResult)).toBe(true);
      const messages = getData(messageResult);
      expect(Array.isArray(messages)).toBe(true);
      expect(messages!.length).toBeGreaterThan(0);
    });

    it("should fail with missing required variables", async () => {
      const input: PromptInput = {
        template: "Process {input} and {output}",
        variables: { input: "data", output: "result" },
      };

      const templateResult = adapter.createChatTemplate(input);
      expect(isSuccess(templateResult)).toBe(true);

      const template = getData(templateResult)!;
      const messageResult = await adapter.formatMessages(template, { input: "data" }); // missing 'output'

      expect(isFailure(messageResult)).toBe(true);
      const error = getError(messageResult);
      expect(error?.code).toBe("MISSING_REQUIRED_VARIABLES");
    });
  });

  describe("Template Validation", () => {
    it("should validate correct template", () => {
      const input: PromptInput = {
        template: "Valid template with {variable}",
        variables: { variable: "value" },
      };

      const templateResult = adapter.createPromptTemplate(input);
      expect(isSuccess(templateResult)).toBe(true);

      const template = getData(templateResult)!;
      const validationResult = adapter.validateTemplate(template);

      expect(isSuccess(validationResult)).toBe(true);
    });

    it("should detect invalid template", () => {
      const validationResult = adapter.validateTemplate(null as any);

      expect(isFailure(validationResult)).toBe(true);
      const error = getError(validationResult);
      expect(error?.code).toBe("INVALID_TEMPLATE");
    });
  });

  describe("Error Handling", () => {
    it("should handle invalid template types", () => {
      const input: PromptInput = {
        template: 123 as any, // Invalid type
        variables: {},
      };

      const result = adapter.createPromptTemplate(input);

      expect(isFailure(result)).toBe(true);
      const error = getError(result);
      expect(error?.code).toBe("INVALID_TEMPLATE_TYPE");
    });

    it("should handle template creation errors", () => {
      const input: PromptInput = {
        template: "", // Empty template
        variables: {},
      };

      const result = adapter.createPromptTemplate(input);

      expect(isFailure(result)).toBe(true);
      const error = getError(result);
      expect(error?.category).toBe("VALIDATION");
    });
  });

  describe("Message Serialization", () => {
    it("should serialize human message", () => {
      const message = new HumanMessage("Hello, world!");

      const result = serializeMessage(message);

      expect(isSuccess(result)).toBe(true);
      const serialized = getData(result);
      expect(serialized?.type).toBe("human");
      expect(serialized?.content).toBe("Hello, world!");
    });

    it("should serialize system message", () => {
      const message = new SystemMessage("You are a helpful assistant.");

      const result = serializeMessage(message);

      expect(isSuccess(result)).toBe(true);
      const serialized = getData(result);
      expect(serialized?.type).toBe("system");
      expect(serialized?.content).toBe("You are a helpful assistant.");
    });

    it("should deserialize message", () => {
      const data = {
        type: "human",
        content: "Test message",
        metadata: { timestamp: "2025-12-30" },
      };

      const result = deserializeMessage(data);

      expect(isSuccess(result)).toBe(true);
      const message = getData(result);
      expect(message).toBeInstanceOf(HumanMessage);
      expect(message?.content).toBe("Test message");
    });

    it("should handle unknown message type", () => {
      const data = {
        type: "unknown",
        content: "Test message",
      };

      const result = deserializeMessage(data);

      expect(isFailure(result)).toBe(true);
      const error = getError(result);
      expect(error?.code).toBe("UNKNOWN_MESSAGE_TYPE");
    });
  });

  describe("Development vs Production Modes", () => {
    it("should handle relaxed validation in dev mode", () => {
      const devAdapter = new QiLangChainAdapter({
        strictMode: false,
        validateInputs: false,
      });

      const input: PromptInput = {
        template: "Hello, {name}!",
        variables: {}, // Missing required variable
      };

      const result = devAdapter.createPromptTemplate(input);

      expect(isSuccess(result)).toBe(true);
    });

    it("should enforce strict validation in production mode", () => {
      const prodAdapter = new QiLangChainAdapter({
        strictMode: true,
        validateInputs: true,
      });

      const input: PromptInput = {
        template: "Hello, {name}!",
        variables: {}, // Missing required variable
      };

      const result = prodAdapter.createPromptTemplate(input);

      expect(isFailure(result)).toBe(true);
    });
  });
});
