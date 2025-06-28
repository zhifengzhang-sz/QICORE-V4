import type { Database } from 'bun:sqlite';
import { afterEach, beforeEach, expect, vi } from 'vitest';

// Mock Bun's SQLite for testing
vi.mock('bun:sqlite', () => ({
  Database: vi.fn().mockImplementation(() => ({
    exec: vi.fn(),
    prepare: vi.fn().mockReturnValue({
      run: vi.fn(),
      all: vi.fn().mockReturnValue([]),
      get: vi.fn(),
    }),
    close: vi.fn(),
  })),
}));

// Mock console methods to prevent noise in tests unless explicitly needed
const originalConsole = {
  log: console.log,
  error: console.error,
  warn: console.warn,
  info: console.info,
};

beforeEach(() => {
  // Reset all mocks before each test
  vi.clearAllMocks();

  // Mock console methods to prevent output during tests
  console.log = vi.fn();
  console.error = vi.fn();
  console.warn = vi.fn();
  console.info = vi.fn();
});

afterEach(() => {
  // Clean up after each test
  vi.restoreAllMocks();

  // Restore console methods
  console.log = originalConsole.log;
  console.error = originalConsole.error;
  console.warn = originalConsole.warn;
  console.info = originalConsole.info;
});

// Global test utilities
globalThis.createMockDatabase = (): Database => {
  const mockDb = {
    exec: vi.fn(),
    prepare: vi.fn().mockReturnValue({
      run: vi.fn(),
      all: vi.fn().mockReturnValue([]),
      get: vi.fn(),
    }),
    close: vi.fn(),
  } as unknown as Database;

  return mockDb;
};

// Add custom matchers for better assertions
expect.extend({
  toBeValidStudyId(received: string) {
    const isValid = typeof received === 'string' && received.length > 0;
    return {
      message: () => `expected ${received} to be a valid study ID`,
      pass: isValid,
    };
  },

  toBeValidScore(received: number) {
    const isValid = typeof received === 'number' && received >= 0 && received <= 100;
    return {
      message: () => `expected ${received} to be a valid score between 0 and 100`,
      pass: isValid,
    };
  },

  toBeValidTimestamp(received: Date) {
    const isValid = received instanceof Date && !Number.isNaN(received.getTime());
    return {
      message: () => `expected ${received} to be a valid Date`,
      pass: isValid,
    };
  },
});

// Declare custom matchers for TypeScript
declare module 'vitest' {
  interface CustomMatchers<R = unknown> {
    toBeValidStudyId(): R;
    toBeValidScore(): R;
    toBeValidTimestamp(): R;
  }
}

declare global {
  function createMockDatabase(): Database;
}
