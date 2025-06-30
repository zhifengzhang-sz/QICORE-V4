/**
 * @fileoverview Comprehensive QiPrompt Test Suite
 * @purpose Test all prompt engineering functionality to 90%+ coverage
 */

import { describe, expect, it } from 'vitest';
import {
  type PromptConfig,
  QiPrompt,
  buildCompletePrompt,
  buildPromptWithContext,
  buildSystemPrompt,
  cleanGeneratedCode,
  createDefaultPromptConfig,
  extractCodeFromResponse,
  extractReasoning,
  validateInstructionForPrompt,
  validateModelForPrompt,
} from '../../src/modules/qiprompt';
import type { AIModel, InstructionSet } from '../../src/modules/types';

// ============================================================================
// TEST FIXTURES
// ============================================================================

const createValidModel = (): AIModel => ({
  id: 'claude-3-sonnet',
  name: 'Claude 3 Sonnet',
  provider: 'anthropic',
  modelName: 'claude-3-sonnet-20240229',
  temperature: 0.7,
  maxTokens: 4000,
});

const createValidInstruction = (): InstructionSet => ({
  id: 'test-instruction',
  name: 'Test Instruction',
  category: 'simple',
  content: 'Create a simple TypeScript function that adds two numbers',
  language: 'typescript',
  complexity: 'low',
  estimatedTokens: 100,
  tags: ['function', 'math', 'typescript'],
  version: '1.0.0',
});

const createInvalidModel = (): Partial<AIModel> => ({
  id: '',
  name: 'Invalid Model',
  provider: 'local',
  modelName: '',
  temperature: 3.0, // Invalid: > 2
});

const createInvalidInstruction = (): Partial<InstructionSet> => ({
  id: '',
  name: 'Invalid Instruction',
  category: 'simple',
  content: '',
  language: '',
  complexity: 'low',
  estimatedTokens: 0,
  tags: [],
  version: '1.0.0',
});

// ============================================================================
// PROMPT BUILDING TESTS
// ============================================================================

describe('QiPrompt - Prompt Building', () => {
  // biome-ignore lint/nursery/noSecrets: test function name
  describe('buildSystemPrompt', () => {
    it('should build system prompt with model and instruction details', () => {
      const model = createValidModel();
      const instruction = createValidInstruction();

      const result = buildSystemPrompt(model, instruction);

      expect(result).toContain('expert typescript programmer');
      expect(result).toContain('Claude 3 Sonnet');
      expect(result).toContain('anthropic');
      expect(result).toContain('typescript');
      expect(result).toContain('low');
      expect(result).toContain('function, math, typescript');
      expect(result).toContain('production-ready');
    });

    it('should handle model with minimal information', () => {
      const model: AIModel = {
        id: 'test-model',
        name: 'Test Model',
        provider: 'local',
        modelName: 'test-model-v1',
      };
      const instruction: InstructionSet = {
        id: 'test',
        name: 'Test',
        category: 'simple',
        content: 'Test content',
        language: 'javascript',
        complexity: 'high',
        estimatedTokens: 50,
        tags: ['test'],
        version: '1.0.0',
      };

      const result = buildSystemPrompt(model, instruction);

      expect(result).toContain('expert javascript programmer');
      expect(result).toContain('Test Model');
      expect(result).toContain('local');
      expect(result).toContain('high');
    });
  });

  // biome-ignore lint/nursery/noSecrets: test function name
  describe('buildPromptWithContext', () => {
    it('should build prompt with instruction content and requirements', () => {
      const instruction = createValidInstruction();

      const result = buildPromptWithContext(instruction);

      expect(result).toContain(instruction.content);
      expect(result).toContain('Please ensure your response:');
      expect(result).toContain('Contains only the requested code');
      expect(result).toContain('production-ready');
      expect(result).toContain('modern typescript best practices');
    });

    it('should handle different programming languages', () => {
      const instruction: InstructionSet = {
        id: 'python-test',
        name: 'Python Test',
        category: 'modern',
        content: 'Create a Python class',
        language: 'python',
        complexity: 'medium',
        estimatedTokens: 200,
        tags: ['class', 'oop'],
        version: '1.0.0',
      };

      const result = buildPromptWithContext(instruction);

      expect(result).toContain('Create a Python class');
      expect(result).toContain('modern python best practices');
    });
  });

  // biome-ignore lint/nursery/noSecrets: test function name
  describe('buildCompletePrompt', () => {
    it('should build complete prompt with system and user prompts', () => {
      const model = createValidModel();
      const instruction = createValidInstruction();

      const result = buildCompletePrompt(model, instruction);

      expect(result._tag).toBe('Right');
      if (result._tag === 'Right') {
        expect(result.right.systemPrompt).toBeDefined();
        expect(result.right.userPrompt).toBeDefined();
        expect(result.right.systemPrompt).toContain('expert typescript programmer');
        expect(result.right.userPrompt).toContain(instruction.content);
      }
    });

    it('should build prompt without system prompt when disabled', () => {
      const model = createValidModel();
      const instruction = createValidInstruction();
      const config: PromptConfig = {
        includeSystemPrompt: false,
        includeReasoningRequest: false,
        maxPromptLength: 8000,
        templateVariables: {},
      };

      const result = buildCompletePrompt(model, instruction, config);

      expect(result._tag).toBe('Right');
      if (result._tag === 'Right') {
        expect(result.right.systemPrompt).toBeUndefined();
        expect(result.right.userPrompt).toBeDefined();
      }
    });

    it('should fail with invalid model', () => {
      const model = createInvalidModel() as AIModel;
      const instruction = createValidInstruction();

      const result = buildCompletePrompt(model, instruction);

      expect(result._tag).toBe('Left');
      if (result._tag === 'Left') {
        expect(result.left.code).toBe('VALIDATION_ERROR');
        expect(result.left.message).toContain('Model');
      }
    });

    it('should fail with invalid instruction', () => {
      const model = createValidModel();
      const instruction = createInvalidInstruction() as InstructionSet;

      const result = buildCompletePrompt(model, instruction);

      expect(result._tag).toBe('Left');
      if (result._tag === 'Left') {
        expect(result.left.code).toBe('VALIDATION_ERROR');
        expect(result.left.message).toContain('Instruction');
      }
    });
  });
});

// ============================================================================
// CONTENT EXTRACTION TESTS
// ============================================================================

describe('QiPrompt - Content Extraction', () => {
  // biome-ignore lint/nursery/noSecrets: test function name
  describe('extractCodeFromResponse', () => {
    it('should extract code from markdown code blocks', () => {
      const response = `Here's the code:

\`\`\`typescript
function add(a: number, b: number): number {
  return a + b;
}
\`\`\`

This function adds two numbers.`;

      const result = extractCodeFromResponse(response);

      expect(result).toBe(`function add(a: number, b: number): number {
  return a + b;
}`);
    });

    it('should extract code from code block without language', () => {
      const response = `\`\`\`
const result = 42;
console.log(result);
\`\`\``;

      const result = extractCodeFromResponse(response);

      expect(result).toBe(`const result = 42;
console.log(result);`);
    });

    it('should handle multiple code blocks and take the first one', () => {
      const response = `\`\`\`typescript
const first = 1;
\`\`\`

\`\`\`typescript
const second = 2;
\`\`\``;

      const result = extractCodeFromResponse(response);

      expect(result).toBe('const first = 1;');
    });

    it('should return trimmed content when no code blocks exist', () => {
      const response = `   
      function add(a, b) {
        return a + b;
      }
      `;

      const result = extractCodeFromResponse(response);

      expect(result).toBe(`function add(a, b) {
        return a + b;
      }`);
    });

    it('should handle empty response', () => {
      const result = extractCodeFromResponse('');
      expect(result).toBe('');
    });

    it('should handle whitespace-only response', () => {
      const result = extractCodeFromResponse('   \n\t  ');
      expect(result).toBe('');
    });
  });

  describe('extractReasoning', () => {
    it('should extract reasoning from "thinking:" pattern', () => {
      const response = `Thinking: I need to create a function that adds two numbers.

\`\`\`typescript
function add(a: number, b: number): number {
  return a + b;
}
\`\`\``;

      const result = extractReasoning(response);

      expect(result).toBe('I need to create a function that adds two numbers.');
    });

    it('should extract reasoning from "my approach" pattern', () => {
      const response = `My approach will be to use TypeScript's type system to ensure type safety.

Here's the implementation:`;

      const result = extractReasoning(response);

      expect(result).toContain('approach will be to use TypeScript');
    });

    it('should extract reasoning from "let me" pattern', () => {
      const response = `Let me create a robust function with proper error handling.

The function will:`;

      const result = extractReasoning(response);

      expect(result).toContain('create a robust function');
    });

    it('should return empty string when no reasoning patterns found', () => {
      const response = `function add(a: number, b: number): number {
  return a + b;
}`;

      const result = extractReasoning(response);

      expect(result).toBe('');
    });

    it('should handle empty response', () => {
      const result = extractReasoning('');
      expect(result).toBe('');
    });
  });

  // biome-ignore lint/nursery/noSecrets: test function name
  describe('cleanGeneratedCode', () => {
    it('should remove leading and trailing newlines', () => {
      const code = '\n\n\nfunction test() {\n  return 42;\n}\n\n\n';

      const result = cleanGeneratedCode(code);

      expect(result).toBe('function test() {\n  return 42;\n}');
    });

    it('should convert tabs to spaces', () => {
      const code = 'function test() {\n\treturn 42;\n}';

      const result = cleanGeneratedCode(code);

      expect(result).toBe('function test() {\n  return 42;\n}');
    });

    it('should handle mixed whitespace', () => {
      const code = '\n\t\n\tfunction test() {\n\t\treturn 42;\n\t}\n\n\t';

      const result = cleanGeneratedCode(code);

      expect(result).toBe('function test() {\n    return 42;\n  }');
    });

    it('should handle empty code', () => {
      const result = cleanGeneratedCode('');
      expect(result).toBe('');
    });

    it('should handle whitespace-only code', () => {
      const result = cleanGeneratedCode('\n\n\t  \n\n');
      expect(result).toBe('');
    });
  });
});

// ============================================================================
// VALIDATION TESTS
// ============================================================================

describe('QiPrompt - Validation', () => {
  // biome-ignore lint/nursery/noSecrets: test function name
  describe('validateModelForPrompt', () => {
    it('should validate valid model', () => {
      const model = createValidModel();

      const result = validateModelForPrompt(model);

      expect(result._tag).toBe('Right');
      if (result._tag === 'Right') {
        expect(result.right).toEqual(model);
      }
    });

    it('should fail validation for empty model ID', () => {
      const model = { ...createValidModel(), id: '' };

      const result = validateModelForPrompt(model);

      expect(result._tag).toBe('Left');
      if (result._tag === 'Left') {
        expect(result.left.code).toBe('VALIDATION_ERROR');
        expect(result.left.message).toContain('Model ID must be non-empty');
        expect(result.left.context.get('field')).toBe('model.id');
      }
    });

    it('should fail validation for whitespace-only model ID', () => {
      const model = { ...createValidModel(), id: '   ' };

      const result = validateModelForPrompt(model);

      expect(result._tag).toBe('Left');
    });

    it('should fail validation for empty model name', () => {
      const model = { ...createValidModel(), modelName: '' };

      const result = validateModelForPrompt(model);

      expect(result._tag).toBe('Left');
      if (result._tag === 'Left') {
        expect(result.left.message).toContain('Model name must be non-empty');
        expect(result.left.context.get('field')).toBe('model.modelName');
      }
    });

    it('should fail validation for invalid temperature (too low)', () => {
      const model = { ...createValidModel(), temperature: -0.1 };

      const result = validateModelForPrompt(model);

      expect(result._tag).toBe('Left');
      if (result._tag === 'Left') {
        expect(result.left.message).toContain('Temperature must be between 0 and 2');
        expect(result.left.context.get('field')).toBe('model.temperature');
      }
    });

    it('should fail validation for invalid temperature (too high)', () => {
      const model = { ...createValidModel(), temperature: 2.1 };

      const result = validateModelForPrompt(model);

      expect(result._tag).toBe('Left');
    });

    it('should accept valid temperature bounds', () => {
      const model1 = { ...createValidModel(), temperature: 0 };
      const model2 = { ...createValidModel(), temperature: 2 };

      expect(validateModelForPrompt(model1)._tag).toBe('Right');
      expect(validateModelForPrompt(model2)._tag).toBe('Right');
    });

    it('should accept model without temperature', () => {
      const model = { ...createValidModel() };
      model.temperature = undefined;

      const result = validateModelForPrompt(model);

      expect(result._tag).toBe('Right');
    });
  });

  // biome-ignore lint/nursery/noSecrets: test function name
  describe('validateInstructionForPrompt', () => {
    it('should validate valid instruction', () => {
      const instruction = createValidInstruction();

      const result = validateInstructionForPrompt(instruction);

      expect(result._tag).toBe('Right');
      if (result._tag === 'Right') {
        expect(result.right).toEqual(instruction);
      }
    });

    it('should fail validation for empty instruction ID', () => {
      const instruction = { ...createValidInstruction(), id: '' };

      const result = validateInstructionForPrompt(instruction);

      expect(result._tag).toBe('Left');
      if (result._tag === 'Left') {
        expect(result.left.message).toContain('Instruction ID must be non-empty');
        expect(result.left.context.get('field')).toBe('instruction.id');
      }
    });

    it('should fail validation for empty instruction content', () => {
      const instruction = { ...createValidInstruction(), content: '' };

      const result = validateInstructionForPrompt(instruction);

      expect(result._tag).toBe('Left');
      if (result._tag === 'Left') {
        expect(result.left.message).toContain('Instruction content must be non-empty');
        expect(result.left.context.get('field')).toBe('instruction.content');
      }
    });

    it('should fail validation for empty instruction language', () => {
      const instruction = { ...createValidInstruction(), language: '' };

      const result = validateInstructionForPrompt(instruction);

      expect(result._tag).toBe('Left');
      if (result._tag === 'Left') {
        expect(result.left.message).toContain('Instruction language must be non-empty');
        expect(result.left.context.get('field')).toBe('instruction.language');
      }
    });

    it('should fail validation for whitespace-only fields', () => {
      const instruction = { ...createValidInstruction(), content: '   ', language: '\t\n' };

      const result = validateInstructionForPrompt(instruction);

      expect(result._tag).toBe('Left');
    });
  });
});

// ============================================================================
// CONFIGURATION TESTS
// ============================================================================

describe('QiPrompt - Configuration', () => {
  // biome-ignore lint/nursery/noSecrets: test function name
  describe('createDefaultPromptConfig', () => {
    it('should create default configuration', () => {
      const config = createDefaultPromptConfig();

      expect(config.includeSystemPrompt).toBe(true);
      expect(config.includeReasoningRequest).toBe(true);
      expect(config.maxPromptLength).toBe(8000);
      expect(config.templateVariables).toEqual({});
    });

    it('should create immutable configuration', () => {
      const config = createDefaultPromptConfig();

      // TypeScript should prevent this, but let's test runtime behavior
      expect(() => {
        // biome-ignore lint/suspicious/noExplicitAny: testing immutability
        (config as any).includeSystemPrompt = false;
      }).toThrow();
    });
  });
});

// ============================================================================
// QIPROMPT API TESTS
// ============================================================================

describe('QiPrompt - Complete API', () => {
  it('should expose all expected functions', () => {
    expect(QiPrompt.buildSystemPrompt).toBeDefined();
    expect(QiPrompt.buildPromptWithContext).toBeDefined();
    expect(QiPrompt.buildCompletePrompt).toBeDefined();
    expect(QiPrompt.extractCodeFromResponse).toBeDefined();
    expect(QiPrompt.extractReasoning).toBeDefined();
    expect(QiPrompt.cleanGeneratedCode).toBeDefined();
    expect(QiPrompt.validateModelForPrompt).toBeDefined();
    expect(QiPrompt.validateInstructionForPrompt).toBeDefined();
    expect(QiPrompt.createDefaultPromptConfig).toBeDefined();
  });

  it('should have all functions as readonly', () => {
    expect(() => {
      // biome-ignore lint/suspicious/noExplicitAny: testing immutability
      (QiPrompt as any).buildSystemPrompt = null;
    }).toThrow();
  });
});

// ============================================================================
// INTEGRATION TESTS
// ============================================================================

describe('QiPrompt - Integration', () => {
  it('should handle complete prompt generation workflow', () => {
    const model = createValidModel();
    const instruction = createValidInstruction();
    const config = createDefaultPromptConfig();

    // Build complete prompt
    const promptResult = buildCompletePrompt(model, instruction, config);
    expect(promptResult._tag).toBe('Right');

    if (promptResult._tag === 'Right') {
      const { systemPrompt, userPrompt } = promptResult.right;

      // Simulate AI response
      const aiResponse = `Thinking: I'll create a simple TypeScript function for adding numbers.

\`\`\`typescript
function add(a: number, b: number): number {
  return a + b;
}
\`\`\`

This function provides type safety and follows TypeScript best practices.`;

      // Extract content
      const code = extractCodeFromResponse(aiResponse);
      const reasoning = extractReasoning(aiResponse);
      const cleanCode = cleanGeneratedCode(code);

      expect(code).toContain('function add');
      expect(reasoning).toContain('simple TypeScript function');
      expect(cleanCode).toContain('function add');
      expect(systemPrompt).toContain('expert typescript programmer');
      expect(userPrompt).toContain(instruction.content);
    }
  });

  it('should handle error scenarios gracefully', () => {
    const invalidModel = createInvalidModel() as AIModel;
    const validInstruction = createValidInstruction();

    const result = buildCompletePrompt(invalidModel, validInstruction);

    expect(result._tag).toBe('Left');
    if (result._tag === 'Left') {
      expect(result.left.code).toBe('VALIDATION_ERROR');
      expect(result.left.category).toBe('VALIDATION');
    }
  });
});
