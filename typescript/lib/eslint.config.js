// @ts-check
import eslint from '@eslint/js';
import tseslint from 'typescript-eslint';

export default tseslint.config(
  {
    ignores: ['dist/', 'node_modules/', '**/*.d.ts', 'coverage/', 'bun.lock'],
  },
  eslint.configs.recommended,
  ...tseslint.configs.recommended,
  {
    files: ['**/*.ts', '**/*.tsx'],
    languageOptions: {
      parser: tseslint.parser,
      parserOptions: {
        ecmaVersion: 'latest',
        sourceType: 'module',
        project: true,
      },
    },
    plugins: {
      '@typescript-eslint': tseslint.plugin,
    },
    rules: {
      // Allow any types for flexibility during development
      '@typescript-eslint/no-explicit-any': 'off',
      // Allow unused vars with underscore prefix
      '@typescript-eslint/no-unused-vars': ['error', { 'argsIgnorePattern': '^_' }],
      // Allow empty interfaces
      '@typescript-eslint/no-empty-interface': 'off',
      // Allow require statements
      '@typescript-eslint/no-var-requires': 'off',
      // Allow non-null assertions
      '@typescript-eslint/no-non-null-assertion': 'off',
      // Allow namespaces for structured exports
      '@typescript-eslint/no-namespace': 'off',
    },
  },
  {
    files: ['**/*.test.ts', '**/*.spec.ts'],
    rules: {
      // More relaxed rules for test files
      '@typescript-eslint/no-explicit-any': 'off',
      '@typescript-eslint/no-unused-vars': 'off',
    },
  }
);