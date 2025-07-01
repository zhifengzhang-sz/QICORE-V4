You are a senior TypeScript developer implementing components for a production system.

REQUIREMENTS:
- Use TypeScript with strict mode
- Provide complete, working implementation
- Include all necessary imports and dependencies
- Write clean, maintainable code
- Follow TypeScript best practices

TARGET COMPONENTS:
Implement the 5 QiCore v4 components as specified in the natural language specification.

REQUIRED READING:
You MUST read and follow the specification in these files:
- `../inputs/qi.v4.class.contracts.md` - Detailed behavioral contracts for all classes and functions
- `../inputs/qi.v4.component.contracts.md` - Component architecture and organization

Focus on correctness and usability. Create a complete TypeScript implementation that satisfies all the contracts described in both specification files.

OUTPUT STRUCTURE:
Create the following directory structure in `$EXPERIMENT_OUTPUT` (if not set, use `./output`):

```
src/
├── result.ts           # Result<T> component
├── qierror.ts          # QiError component  
├── configuration.ts    # Configuration component
├── logger.ts           # Logger component
├── cache.ts            # Cache component
├── index.ts            # Main exports
└── types.ts            # Shared types
tests/
├── result.test.ts      # Result<T> tests
├── qierror.test.ts     # QiError tests
├── configuration.test.ts # Configuration tests
├── logger.test.ts      # Logger tests
├── cache.test.ts       # Cache tests
└── integration.test.ts # Integration tests
package.json            # Dependencies and scripts
tsconfig.json           # TypeScript configuration
README.md               # Usage examples and documentation
```

DELIVERABLES:
1. Complete TypeScript implementation of all 5 components in src/ directory
2. Comprehensive test suite in tests/ directory
3. Package.json with all necessary dependencies
4. TypeScript configuration (tsconfig.json)
5. README.md with usage examples and design decisions 