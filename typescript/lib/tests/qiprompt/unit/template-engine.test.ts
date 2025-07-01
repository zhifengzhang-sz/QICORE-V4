/**
 * @fileoverview Comprehensive tests for QiTemplate Engine
 * @purpose Achieve >90% coverage for template processing
 */

import { beforeEach, describe, expect, it } from "vitest";
import type {
  CompiledTemplate,
  PromptInput,
  TemplateEngineConfig,
  TemplateVariable,
} from "../../../src/qiprompt/core/types";
import {
  QiTemplateEngine,
  createDefaultEngine,
  createDevEngine,
} from "../../../src/qiprompt/templates/engine";

describe("QiTemplate Engine", () => {
  let engine: QiTemplateEngine;

  beforeEach(() => {
    engine = new QiTemplateEngine();
  });

  describe("Constructor and Configuration", () => {
    it("should create engine with default configuration", () => {
      const defaultEngine = new QiTemplateEngine();
      expect(defaultEngine).toBeInstanceOf(QiTemplateEngine);
    });

    it("should create engine with custom configuration", () => {
      const config: Partial<TemplateEngineConfig> = {
        syntax: "handlebars",
        strictMode: false,
        autoEscape: false,
        customHelpers: {
          uppercase: (str: string) => str.toUpperCase(),
        },
        partials: {
          header: "Welcome to {app_name}",
        },
      };

      const customEngine = new QiTemplateEngine(config);
      expect(customEngine).toBeInstanceOf(QiTemplateEngine);
    });

    it("should merge default and custom configurations", () => {
      const partialConfig = {
        strictMode: false,
        autoEscape: false,
      };

      const engine = new QiTemplateEngine(partialConfig);
      expect(engine).toBeInstanceOf(QiTemplateEngine);
    });
  });

  describe("compile method", () => {
    it("should compile simple string template", () => {
      const input: PromptInput = {
        template: "Hello {name}, welcome to {place}!",
        variables: { name: "Alice", place: "Wonderland" },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.id).toBeDefined();
        expect(result.right.source).toBe(input.template);
        expect(result.right.variables).toBeDefined();
        expect(result.right.render).toBeInstanceOf(Function);
        expect(result.right.validate).toBeInstanceOf(Function);
        expect(result.right.metadata).toBeDefined();
      }
    });

    it("should compile template with complex object", () => {
      const templateObj = {
        template: "Process {data} with settings {config}",
        metadata: { name: "Data Processor" },
      };
      const input: PromptInput = {
        template: templateObj,
        variables: { data: "input", config: "settings" },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.source).toBe("");
        expect(result.right.metadata.name).toBe("Data Processor");
      }
    });

    it("should extract variables correctly", () => {
      const input: PromptInput = {
        template: "Analyze {dataset} using {algorithm} with {parameters}",
        variables: {
          dataset: "sales_data",
          algorithm: "regression",
          parameters: { learning_rate: 0.01 },
        },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.variables).toHaveLength(3);
        const varNames = result.right.variables.map((v) => v.name);
        expect(varNames).toContain("dataset");
        expect(varNames).toContain("algorithm");
        expect(varNames).toContain("parameters");
      }
    });

    it("should infer variable types correctly", () => {
      const input: PromptInput = {
        template: "Process {text}, count {number}, flag {boolean}, items {array}, config {object}",
        variables: {
          text: "hello",
          number: 42,
          boolean: true,
          array: [1, 2, 3],
          object: { key: "value" },
        },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const variables = result.right.variables;
        const textVar = variables.find((v) => v.name === "text");
        const numberVar = variables.find((v) => v.name === "number");
        const booleanVar = variables.find((v) => v.name === "boolean");
        const arrayVar = variables.find((v) => v.name === "array");
        const objectVar = variables.find((v) => v.name === "object");

        expect(textVar?.type).toBe("string");
        expect(numberVar?.type).toBe("number");
        expect(booleanVar?.type).toBe("boolean");
        expect(arrayVar?.type).toBe("array");
        expect(objectVar?.type).toBe("object");
      }
    });

    it("should detect optional variables", () => {
      const input: PromptInput = {
        template: "Hello {name}, {greeting?} and {farewell|default}",
        variables: {
          name: "Alice",
          greeting: "how are you",
          farewell: "goodbye",
        },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const greetingVar = result.right.variables.find((v) => v.name === "greeting");
        const farewellVar = result.right.variables.find((v) => v.name === "farewell");
        expect(greetingVar?.required).toBe(false);
        expect(farewellVar?.required).toBe(false);
      }
    });

    it("should generate variable descriptions", () => {
      const input: PromptInput = {
        template: "Show {name}, process {data}, query {id}, get {content}",
        variables: {
          name: "user",
          data: "information",
          id: "12345",
          content: "text",
        },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const variables = result.right.variables;
        expect(variables.every((v) => v.description && v.description.length > 0)).toBe(true);

        const nameVar = variables.find((v) => v.name === "name");
        const dataVar = variables.find((v) => v.name === "data");
        const idVar = variables.find((v) => v.name === "id");
        const contentVar = variables.find((v) => v.name === "content");

        expect(nameVar?.description).toContain("Name");
        expect(dataVar?.description).toContain("Data");
        expect(idVar?.description).toContain("identifier");
        expect(contentVar?.description).toContain("content");
      }
    });

    it("should cache compiled templates", () => {
      const input: PromptInput = {
        template: "Cached {template}",
        variables: { template: "test" },
      };

      const result1 = engine.compile(input);
      const result2 = engine.compile(input);

      expect(result1._tag).toBe("Right");
      expect(result2._tag).toBe("Right");
      if (result1._tag === "Right" && result2._tag === "Right") {
        expect(result1.right.id).toBe(result2.right.id);
      }
    });

    it("should build metadata correctly", () => {
      const input: PromptInput = {
        template: "Test template with {variable}",
        variables: { variable: "value" },
        metadata: {
          name: "Test Template",
          description: "A test template for validation",
          version: "2.1.0",
          author: "Test Author",
          category: "testing",
          tags: ["test", "validation"],
          language: "english",
          complexity: "advanced",
        },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const metadata = result.right.metadata;
        expect(metadata.name).toBe("Test Template");
        expect(metadata.description).toBe("A test template for validation");
        expect(metadata.version).toBe("2.1.0");
        expect(metadata.author).toBe("Test Author");
        expect(metadata.category).toBe("testing");
        expect(metadata.tags).toEqual(["test", "validation"]);
        expect(metadata.language).toBe("english");
        expect(metadata.complexity).toBe("advanced");
        expect(metadata.createdAt).toBeInstanceOf(Date);
        expect(metadata.updatedAt).toBeInstanceOf(Date);
      }
    });

    it("should generate default metadata when not provided", () => {
      const input: PromptInput = {
        template: "Simple {test}",
        variables: { test: "template" },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const metadata = result.right.metadata;
        expect(metadata.name).toMatch(/^Template_/);
        expect(metadata.description).toBe("Auto-generated template");
        expect(metadata.version).toBe("1.0.0");
        expect(metadata.category).toBe("general");
        expect(metadata.tags).toEqual([]);
        expect(metadata.language).toBe("english");
        expect(metadata.complexity).toBe("simple");
      }
    });

    it("should handle compilation errors", () => {
      const input: PromptInput = {
        template: null as any,
        variables: {},
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left.code).toBe("TEMPLATE_COMPILATION_ERROR");
      }
    });

    it("should handle variable extraction errors", () => {
      const problematicEngine = new QiTemplateEngine();
      // Force an error by providing a template that will cause issues
      const input: PromptInput = {
        template: "Test " + "x".repeat(100000), // Very long template
        variables: {},
      };

      const result = problematicEngine.compile(input);
      expect(result._tag).toBe("Right"); // Should handle gracefully
    });
  });

  describe("render method (via CompiledTemplate)", () => {
    it("should render template with variables", async () => {
      const input: PromptInput = {
        template: "Hello {name}, you are {age} years old!",
        variables: { name: "Alice", age: 25 },
      };

      const compileResult = engine.compile(input);
      expect(compileResult._tag).toBe("Right");

      if (compileResult._tag === "Right") {
        const renderResult = await compileResult.right.render({ name: "Bob", age: 30 });
        expect(renderResult._tag).toBe("Right");
        if (renderResult._tag === "Right") {
          expect(renderResult.right).toContain("Hello Bob");
          expect(renderResult.right).toContain("30 years old");
        }
      }
    });

    it("should handle auto-escaping when enabled", async () => {
      const escapingEngine = new QiTemplateEngine({ autoEscape: true });
      const input: PromptInput = {
        template: "Display {content}",
        variables: { content: "<script>alert('xss')</script>" },
      };

      const compileResult = escapingEngine.compile(input);
      expect(compileResult._tag).toBe("Right");

      if (compileResult._tag === "Right") {
        const renderResult = await compileResult.right.render({
          content: "<script>alert('xss')</script>",
        });
        expect(renderResult._tag).toBe("Right");
        if (renderResult._tag === "Right") {
          expect(renderResult.right).toContain("&lt;script&gt;");
          expect(renderResult.right).toContain("&lt;/script&gt;");
        }
      }
    });

    it("should skip auto-escaping when disabled", async () => {
      const nonEscapingEngine = new QiTemplateEngine({ autoEscape: false });
      const input: PromptInput = {
        template: "Display {content}",
        variables: { content: "<b>bold</b>" },
      };

      const compileResult = nonEscapingEngine.compile(input);
      expect(compileResult._tag).toBe("Right");

      if (compileResult._tag === "Right") {
        const renderResult = await compileResult.right.render({
          content: "<b>bold</b>",
        });
        expect(renderResult._tag).toBe("Right");
        if (renderResult._tag === "Right") {
          expect(renderResult.right).toContain("<b>bold</b>");
        }
      }
    });

    it("should post-process rendered templates", async () => {
      const input: PromptInput = {
        template: "Line 1\n\n\n\nLine 2   \n\n\nLine 3    ",
        variables: {},
      };

      const compileResult = engine.compile(input);
      expect(compileResult._tag).toBe("Right");

      if (compileResult._tag === "Right") {
        const renderResult = await compileResult.right.render({});
        expect(renderResult._tag).toBe("Right");
        if (renderResult._tag === "Right") {
          // Should clean up excessive newlines and trailing spaces
          expect(renderResult.right).not.toContain("\n\n\n");
          expect(renderResult.right).not.toMatch(/ {3}$/);
        }
      }
    });

    it("should handle rendering errors", async () => {
      const input: PromptInput = {
        template: "Test {variable}",
        variables: { variable: "value" },
      };

      const compileResult = engine.compile(input);
      expect(compileResult._tag).toBe("Right");

      if (compileResult._tag === "Right") {
        // Provide problematic variable that might cause errors
        const renderResult = await compileResult.right.render({
          variable: {
            toString: () => {
              throw new Error("Render error");
            },
          },
        });
        expect(renderResult._tag).toBe("Left");
        if (renderResult._tag === "Left") {
          expect(renderResult.left.code).toBe("TEMPLATE_RENDER_ERROR");
        }
      }
    });
  });

  describe("validate method (via CompiledTemplate)", () => {
    it("should validate correct variables", () => {
      const input: PromptInput = {
        template: "Process {data} with {config}",
        variables: { data: "test", config: "settings" },
      };

      const compileResult = engine.compile(input);
      expect(compileResult._tag).toBe("Right");

      if (compileResult._tag === "Right") {
        const validateResult = compileResult.right.validate({
          data: "valid data",
          config: "valid config",
        });
        expect(validateResult._tag).toBe("Right");
      }
    });

    it("should detect missing required variables", () => {
      const input: PromptInput = {
        template: "Hello {name} and {friend}",
        variables: { name: "Alice", friend: "Bob" },
      };

      const compileResult = engine.compile(input);
      expect(compileResult._tag).toBe("Right");

      if (compileResult._tag === "Right") {
        const validateResult = compileResult.right.validate({
          name: "Alice",
          // Missing 'friend'
        });
        expect(validateResult._tag).toBe("Left");
        if (validateResult._tag === "Left") {
          expect(validateResult.left.code).toBe("VARIABLE_VALIDATION_ERROR");
        }
      }
    });

    it("should validate variable types", () => {
      const input: PromptInput = {
        template: "Count {number} items",
        variables: { number: 42 },
      };

      const compileResult = engine.compile(input);
      expect(compileResult._tag).toBe("Right");

      if (compileResult._tag === "Right") {
        // Valid type
        const validResult = compileResult.right.validate({ number: 123 });
        expect(validResult._tag).toBe("Right");

        // Invalid type
        const invalidResult = compileResult.right.validate({ number: "not a number" });
        expect(invalidResult._tag).toBe("Left");
      }
    });

    it("should handle custom validation functions", () => {
      const input: PromptInput = {
        template: "Email {address}",
        variables: { address: "test@example.com" },
      };

      const compileResult = engine.compile(input);
      expect(compileResult._tag).toBe("Right");

      if (compileResult._tag === "Right") {
        // Manually add custom validation for testing
        const compiledTemplate = compileResult.right;
        const variables = [...compiledTemplate.variables];
        const emailVar = variables.find((v) => v.name === "address");
        if (emailVar) {
          emailVar.validation = (value: unknown) =>
            typeof value === "string" && value.includes("@");
        }

        const validResult = compiledTemplate.validate({ address: "valid@email.com" });
        expect(validResult._tag).toBe("Right");

        const invalidResult = compiledTemplate.validate({ address: "invalid-email" });
        expect(invalidResult._tag).toBe("Left");
      }
    });

    it("should handle regex validation", () => {
      const input: PromptInput = {
        template: "Code {pattern}",
        variables: { pattern: "ABC123" },
      };

      const compileResult = engine.compile(input);
      expect(compileResult._tag).toBe("Right");

      if (compileResult._tag === "Right") {
        // Manually add regex validation for testing
        const compiledTemplate = compileResult.right;
        const variables = [...compiledTemplate.variables];
        const patternVar = variables.find((v) => v.name === "pattern");
        if (patternVar) {
          patternVar.validation = /^[A-Z]{3}\d{3}$/;
        }

        const validResult = compiledTemplate.validate({ pattern: "XYZ789" });
        expect(validResult._tag).toBe("Right");

        const invalidResult = compiledTemplate.validate({ pattern: "invalid" });
        expect(invalidResult._tag).toBe("Left");
      }
    });
  });

  describe("Private helper methods coverage", () => {
    it("should cover generateTemplateId", () => {
      const input1: PromptInput = {
        template: "Test {A}",
        variables: { A: "1" },
      };
      const input2: PromptInput = {
        template: "Test {B}",
        variables: { B: "2" },
      };

      const result1 = engine.compile(input1);
      const result2 = engine.compile(input2);

      expect(result1._tag).toBe("Right");
      expect(result2._tag).toBe("Right");
      if (result1._tag === "Right" && result2._tag === "Right") {
        expect(result1.right.id).not.toBe(result2.right.id);
      }
    });

    it("should cover simpleHash function", () => {
      const input1: PromptInput = {
        template: "Hash test 1",
        variables: {},
      };
      const input2: PromptInput = {
        template: "Hash test 2",
        variables: {},
      };

      const result1 = engine.compile(input1);
      const result2 = engine.compile(input2);

      expect(result1._tag).toBe("Right");
      expect(result2._tag).toBe("Right");
      if (result1._tag === "Right" && result2._tag === "Right") {
        expect(result1.right.id).not.toBe(result2.right.id);
      }
    });

    it("should cover getVariablePattern for different syntaxes", () => {
      const mustacheEngine = new QiTemplateEngine({ syntax: "mustache" });
      const handlebarsEngine = new QiTemplateEngine({ syntax: "handlebars" });

      const input: PromptInput = {
        template: "Test {variable}",
        variables: { variable: "value" },
      };

      const result1 = mustacheEngine.compile(input);
      const result2 = handlebarsEngine.compile(input);

      expect(result1._tag).toBe("Right");
      expect(result2._tag).toBe("Right");
    });

    it("should cover extractVariableName", () => {
      const input: PromptInput = {
        template: "Test {variable_name} and {another var}",
        variables: { variable_name: "test", "another var": "test2" },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const varNames = result.right.variables.map((v) => v.name);
        expect(varNames).toContain("variable_name");
        expect(varNames).toContain("another");
      }
    });

    it("should cover isOptionalVariable", () => {
      const input: PromptInput = {
        template: "Required {name} and optional {greeting?} with {default|fallback}",
        variables: { name: "test", greeting: "hello", default: "value" },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        const nameVar = result.right.variables.find((v) => v.name === "name");
        const greetingVar = result.right.variables.find((v) => v.name === "greeting");
        const defaultVar = result.right.variables.find((v) => v.name === "default");

        expect(nameVar?.required).toBe(true);
        expect(greetingVar?.required).toBe(false);
        expect(defaultVar?.required).toBe(false);
      }
    });

    it("should cover validateVariableType for all types", () => {
      const input: PromptInput = {
        template: "Multi-type: {str} {num} {bool} {arr} {obj} {null_val}",
        variables: {
          str: "string",
          num: 42,
          bool: true,
          arr: [1, 2, 3],
          obj: { key: "value" },
          null_val: null,
        },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");

      if (result._tag === "Right") {
        // Test string validation
        const strValidation = result.right.validate({
          str: "valid string",
          num: 123,
          bool: false,
          arr: [4, 5, 6],
          obj: { other: "object" },
          null_val: null,
        });
        expect(strValidation._tag).toBe("Right");

        // Test invalid number (NaN)
        const nanValidation = result.right.validate({
          str: "string",
          num: Number.NaN, // Invalid number
          bool: true,
          arr: [],
          obj: {},
          null_val: null,
        });
        expect(nanValidation._tag).toBe("Left");
      }
    });

    it("should cover preprocessVariables", async () => {
      const escapingEngine = new QiTemplateEngine({ autoEscape: true });
      const input: PromptInput = {
        template: "HTML content: {html}",
        variables: { html: "<div>test</div>" },
      };

      const result = escapingEngine.compile(input);
      expect(result._tag).toBe("Right");

      if (result._tag === "Right") {
        const renderResult = await result.right.render({ html: "<script>alert('test')</script>" });
        expect(renderResult._tag).toBe("Right");
        if (renderResult._tag === "Right") {
          expect(renderResult.right).toContain("&lt;script&gt;");
        }
      }
    });

    it("should cover escapeHtml function", async () => {
      const escapingEngine = new QiTemplateEngine({ autoEscape: true });
      const input: PromptInput = {
        template: "Special chars: {content}",
        variables: { content: `<>&"'` },
      };

      const result = escapingEngine.compile(input);
      expect(result._tag).toBe("Right");

      if (result._tag === "Right") {
        const renderResult = await result.right.render({ content: `<>&"'` });
        expect(renderResult._tag).toBe("Right");
        if (renderResult._tag === "Right") {
          expect(renderResult.right).toContain("&lt;");
          expect(renderResult.right).toContain("&gt;");
          expect(renderResult.right).toContain("&amp;");
          expect(renderResult.right).toContain("&quot;");
          expect(renderResult.right).toContain("&#039;");
        }
      }
    });

    it("should cover postprocessTemplate", async () => {
      const input: PromptInput = {
        template: "Line 1\n\n\n\nLine 2    \n  Line 3   ",
        variables: {},
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");

      if (result._tag === "Right") {
        const renderResult = await result.right.render({});
        expect(renderResult._tag).toBe("Right");
        if (renderResult._tag === "Right") {
          // Should clean up excessive newlines and trailing spaces
          expect(renderResult.right.split("\n\n\n").length).toBe(1);
          expect(renderResult.right.endsWith(" ")).toBe(false);
        }
      }
    });
  });

  describe("Cache management", () => {
    it("should clear cache", () => {
      const input: PromptInput = {
        template: "Cache test {value}",
        variables: { value: "test" },
      };

      const result1 = engine.compile(input);
      expect(result1._tag).toBe("Right");

      engine.clearCache();

      const result2 = engine.compile(input);
      expect(result2._tag).toBe("Right");

      // After clearing cache, should generate new template ID
      if (result1._tag === "Right" && result2._tag === "Right") {
        expect(result1.right.id).not.toBe(result2.right.id);
      }
    });

    it("should get cache statistics", () => {
      const input1: PromptInput = {
        template: "Cache stats test 1 {value}",
        variables: { value: "test1" },
      };
      const input2: PromptInput = {
        template: "Cache stats test 2 {value}",
        variables: { value: "test2" },
      };

      engine.compile(input1);
      engine.compile(input2);

      const stats = engine.getCacheStats();
      expect(stats.size).toBeGreaterThanOrEqual(2);
      expect(Array.isArray(stats.entries)).toBe(true);
      expect(stats.entries.length).toBe(stats.size);
    });
  });

  describe("Error handling and edge cases", () => {
    it("should handle empty templates", () => {
      const input: PromptInput = {
        template: "",
        variables: {},
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle templates with no variables", () => {
      const input: PromptInput = {
        template: "Static template with no variables",
        variables: {},
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
      if (result._tag === "Right") {
        expect(result.right.variables).toHaveLength(0);
      }
    });

    it("should handle malformed variable patterns", () => {
      const input: PromptInput = {
        template: "Malformed {unclosed and }unopened{ variables",
        variables: {},
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle very long variable names", () => {
      const longVarName = "very_long_variable_name_that_exceeds_normal_limits_".repeat(10);
      const input: PromptInput = {
        template: `Test {${longVarName}}`,
        variables: { [longVarName]: "value" },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle unicode variable names", () => {
      const input: PromptInput = {
        template: "Unicode variables: {变量} and {переменная}",
        variables: { 变量: "chinese", переменная: "russian" },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
    });

    it("should handle circular reference in template object", () => {
      const circular: any = { template: "Test {value}" };
      circular.self = circular;

      const input: PromptInput = {
        template: circular,
        variables: { value: "test" },
      };

      const result = engine.compile(input);
      expect(result._tag).toBe("Right");
    });
  });
});

describe("Utility Functions", () => {
  describe("createDefaultEngine", () => {
    it("should create engine with default settings", () => {
      const engine = createDefaultEngine();
      expect(engine).toBeInstanceOf(QiTemplateEngine);
    });
  });

  describe("createDevEngine", () => {
    it("should create engine with development settings", () => {
      const engine = createDevEngine();
      expect(engine).toBeInstanceOf(QiTemplateEngine);
    });
  });
});
