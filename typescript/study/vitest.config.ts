import tsconfigPaths from 'vite-tsconfig-paths';
import { defineConfig } from 'vitest/config';

export default defineConfig({
  plugins: [tsconfigPaths()],
  test: {
    // Enable globals for easier testing (like Jest)
    globals: true,

    // Use Node environment for our CLI/server application
    environment: 'node',

    // Test file patterns
    include: ['tests/**/*.{test,spec}.{js,ts}'],
    exclude: ['node_modules/**', 'dist/**', 'haskell/**'],

    // Setup files
    setupFiles: ['tests/setup.ts'],

    // Coverage configuration with v8 provider (fastest)
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      reportsDirectory: './coverage',
      include: ['src/**/*.{js,ts}'],
      exclude: [
        'src/index.ts', // CLI entry point
        'src/**/*.d.ts',
        'src/**/*.config.ts',
        'tests/**',
        'node_modules/**',
        'dist/**',
      ],
      // Coverage thresholds for production quality
      thresholds: {
        lines: 85,
        functions: 85,
        branches: 80,
        statements: 85,
      },
    },

    // Reporter configuration for better output
    reporters: ['verbose', 'json', 'html'],
    outputFile: {
      json: './test-results.json',
      html: './test-results.html',
    },

    // Timeouts
    testTimeout: 10000,
    hookTimeout: 10000,

    // Performance options
    pool: 'forks', // Best for Node.js applications
    poolOptions: {
      forks: {
        minForks: 1,
        maxForks: 4,
      },
    },

    // Enable watch mode by default in development
    watch: true,

    // Enable file parallelism for faster execution
    fileParallelism: true,

    // Provide context for dependency injection
    provide: {
      testEnvironment: 'vitest',
    },
  },

  // Let tsconfigPaths plugin handle all path resolution
  // No manual aliases needed - plugin reads from tsconfig.json
});
