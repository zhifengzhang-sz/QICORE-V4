import { beforeEach, vi } from 'vitest';

// Mock console methods to reduce noise in tests unless explicitly needed
beforeEach(() => {
  // Preserve warn and error for important messages
  vi.spyOn(console, 'log').mockImplementation(() => {});
  vi.spyOn(console, 'info').mockImplementation(() => {});
  vi.spyOn(console, 'debug').mockImplementation(() => {});
});

// Global test utilities
global.testUtils = {
  createMockClaudeResponse: (overrides = {}) => ({
    success: true,
    content: 'Mock response content',
    error: null,
    metadata: {},
    ...overrides,
  }),
  
  createMockInstruction: (overrides = {}) => ({
    workingDir: '/test/directory',
    knowledgeUpdate: ['Test knowledge update'],
    actionInstruction: 'Test action instruction',
    qualityStandards: ['Test quality standard'],
    validationCriteria: ['Test validation criteria'],
    ...overrides,
  }),
  
  createMockExecutionResult: (overrides = {}) => ({
    instruction: global.testUtils.createMockInstruction(),
    response: global.testUtils.createMockClaudeResponse(),
    executionTime: 1000,
    timestamp: new Date().toISOString(),
    method: 'cli' as const,
    ...overrides,
  }),
};

// Extend global types for test utilities
declare global {
  var testUtils: {
    createMockClaudeResponse: (overrides?: any) => any;
    createMockInstruction: (overrides?: any) => any;
    createMockExecutionResult: (overrides?: any) => any;
  };
}