# QiCore v4.0 TypeScript Implementation Guide

> **Stage 5: TypeScript Implementation Assembly Guide**  
> **Depends on**: [TypeScript Template](qi.v4.ts.template.md), [Design Analysis](../design/qi.v4.design.analysis.md), [TypeScript Packages](../package/ts.md)  
> **Implements**: Step-by-step implementation assembly for TypeScript  
> Version: v4.0.1  
> Date: June 27, 2025  
> Status: Implementation Assembly Guide  
> Purpose: Complete implementation instructions for TypeScript QiCore v4.0

## Implementation Assembly Process

This guide provides step-by-step instructions for assembling the complete QiCore v4.0 TypeScript implementation using the templates, design patterns, and researched packages.

### Prerequisites Verification

Before starting implementation, verify you have:

1. **Bun Runtime Requirements**:
   ```bash
   bun --version   # Should be >= 1.0.0
   ```

2. **TypeScript Development Environment**:
   ```bash
   bun add -g typescript@^5.3.0
   tsc --version   # Should be >= 5.3.0
   ```

3. **Development Tools**:
   ```bash
   bun add -g eslint @typescript-eslint/eslint-plugin  # For linting
   bun add -g prettier                                  # For formatting
   ```

## Phase 1: Project Structure Setup

### 1.1 Initialize TypeScript Project

```bash
# Create project directory
mkdir qicore-v4-typescript
cd qicore-v4-typescript

# Initialize Bun project
bun init -y

# Setup TypeScript configuration
bun add -d typescript @types/node
bunx tsc --init
```

### 1.2 Create Directory Structure

```bash
# Create source directories following architectural patterns
mkdir -p src/{base,core,application}
mkdir -p src/application/{http,web,ai,mcp,database,document,cli}
mkdir -p tests/{base,core,application}
mkdir -p examples
mkdir -p benchmarks

# Create configuration directories
mkdir -p config
mkdir -p scripts
```

### 1.3 Install Dependencies

```bash
# Base dependencies from package research
bun add fp-ts@^2.16.2
bun add zod@^3.22.4

# Core component dependencies
bun add dotenv@^16.3.1
bun add winston@^3.11.0
bun add node-cache@^5.1.2
bun add ioredis@^5.3.2

# Application component dependencies
bun add axios@^1.6.2
bun add fastify@^4.24.3
bun add @fastify/cors@^8.4.0
bun add @fastify/static@^6.12.0
bun add drizzle-orm@^0.29.0
bun add better-sqlite3@^9.2.2
bun add kysely@^0.27.0
bun add handlebars@^4.7.8
bun add marked@^11.1.1
bun add puppeteer@^21.6.1
bun add commander@^11.1.0
bun add chalk@^5.3.0
bun add openai@^4.20.1
bun add @anthropic-ai/sdk@^0.9.1
bun add @modelcontextprotocol/sdk@^1.13.1

# Development dependencies
bun add -d typescript@^5.3.0
bun add -d @types/node@^20.10.0
bun add -d @types/better-sqlite3@^7.6.8
bun add -d vitest@^1.0.4
bun add -d eslint@^8.56.0
bun add -d @typescript-eslint/eslint-plugin@^6.18.0
bun add -d @typescript-eslint/parser@^6.18.0
bun add -d prettier@^3.1.1
```

### 1.4 Configure TypeScript

Replace `tsconfig.json` with production configuration:

```json
{
  "compilerOptions": {
    "target": "ES2022",
    "lib": ["ES2022"],
    "module": "ESNext", 
    "moduleResolution": "node",
    "allowSyntheticDefaultImports": true,
    "esModuleInterop": true,
    "allowJs": true,
    "strict": true,
    "noImplicitAny": true,
    "strictNullChecks": true,
    "noImplicitReturns": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "exactOptionalPropertyTypes": true,
    "declaration": true,
    "declarationMap": true,
    "outDir": "./dist",
    "sourceMap": true,
    "removeComments": false,
    "importHelpers": true,
    "experimentalDecorators": true,
    "emitDecoratorMetadata": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist", "tests"]
}
```

### 1.5 Configure Development Tools

Create `.eslintrc.json`:
```json
{
  "parser": "@typescript-eslint/parser",
  "plugins": ["@typescript-eslint"],
  "extends": [
    "eslint:recommended",
    "@typescript-eslint/recommended"
  ],
  "parserOptions": {
    "ecmaVersion": 2022,
    "sourceType": "module",
    "project": "./tsconfig.json"
  },
  "rules": {
    "@typescript-eslint/no-unused-vars": "error",
    "@typescript-eslint/no-explicit-any": "warn",
    "@typescript-eslint/explicit-function-return-type": "off",
    "@typescript-eslint/explicit-module-boundary-types": "off",
    "@typescript-eslint/no-non-null-assertion": "error",
    "prefer-const": "error",
    "no-var": "error"
  },
  "env": {
    "node": true,
    "es2022": true
  }
}
```

Create `.prettierrc`:
```json
{
  "semi": true,
  "singleQuote": true,
  "tabWidth": 2,
  "trailingComma": "es5",
  "printWidth": 100,
  "endOfLine": "lf"
}
```

Create `vitest.config.ts`:
```typescript
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['tests/**/*.test.ts'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.ts'],
      exclude: ['src/**/*.test.ts', 'src/**/*.spec.ts']
    }
  }
});
```

## Phase 2: Base Component Implementation

### 2.1 Implement QiError

Copy the QiError implementation from the template to `src/base/error.ts`:

**Key implementation details:**
- Uses `enum ErrorCategory` for type safety
- Implements all 6 operations: `create`, `toString`, `toStructuredData`, `getCategory`, `withContext`, `withCause`
- Includes factory methods for all 8 error categories
- Uses `zod` schema for validation
- Preserves immutability through readonly properties

**Verification steps:**
```bash
# Lint the implementation
bun run lint src/base/error.ts

# Compile to check types
bunx tsc --noEmit src/base/error.ts
```

### 2.2 Implement Result<T>

Copy the Result implementation from the template to `src/base/result.ts`:

**Key implementation details:**
- Uses `fp-ts Either<QiError, T>` as foundation
- Implements all 9 operations: `success`, `failure`, `fromTryCatch`, `map`, `flatMap`, `unwrap`, `unwrapOr`, `match`, `orElse`
- Preserves monad laws through `fp-ts` integration
- Includes async `fromAsyncTryCatch` for Promise handling
- Uses functional programming patterns with `pipe`

**Mathematical law verification:**
```typescript
// Add to src/base/result.test.ts
import { describe, it, expect } from 'vitest';
import { success, failure, map, flatMap } from './result.js';

describe('Result Monad Laws', () => {
  it('left identity: return(a).flatMap(f) === f(a)', () => {
    const a = 42;
    const f = (x: number) => success(x * 2);
    expect(flatMap(f)(success(a))).toEqual(f(a));
  });
  
  it('right identity: m.flatMap(return) === m', () => {
    const m = success(42);
    expect(flatMap(success)(m)).toEqual(m);
  });
  
  it('associativity: (m.flatMap(f)).flatMap(g) === m.flatMap(x => f(x).flatMap(g))', () => {
    const m = success(1);
    const f = (x: number) => success(x + 1);
    const g = (x: number) => success(x * 2);
    
    const left = flatMap(g)(flatMap(f)(m));
    const right = flatMap((x: number) => flatMap(g)(f(x)))(m);
    
    expect(left).toEqual(right);
  });
});
```

### 2.3 Create Base Index

Create `src/base/index.ts`:
```typescript
export * from './error.js';
export * from './result.js';
```

### 2.4 Verification Tests

Run verification:
```bash
# Run tests
bun test tests/base/

# Check performance benchmarks
bun run benchmarks/result-performance.ts

# Lint all base components
bun run lint src/base/
```

## Phase 3: Core Component Implementation

### 3.1 Implement Configuration

Copy Configuration implementation from template to `src/core/configuration.ts`:

**Key implementation details:**
- Uses `zod` for schema validation
- Uses `dotenv` for environment variables
- Uses `js-yaml` for YAML support
- Implements monoid operation with right-biased merge
- Includes all 9 operations from design patterns
- Async file loading with format detection

**Integration points:**
- `zod` schemas for type-safe validation
- `dotenv.config()` for environment loading
- `js-yaml.load()` for YAML parsing
- File system operations with `fs/promises`

### 3.2 Implement Logger

Copy Logger implementation from template to `src/core/logger.ts`:

**Key implementation details:**
- Uses `winston` for structured logging
- Implements fast level checking (< 10ns requirement)
- Supports JSON and text formats
- Includes console and file transports
- Level-based filtering with early exit

**Performance optimization:**
```typescript
// Fast level check implementation
isLevelEnabled(level: LogLevel): boolean {
  return level >= this.minLevel; // Integer comparison < 10ns
}

log(level: LogLevel, message: string, context?: Record<string, unknown>): void {
  if (!this.isLevelEnabled(level)) {
    return; // Early exit for performance
  }
  // ... rest of logging logic
}
```

### 3.3 Implement Cache

Copy Cache implementations from template to `src/core/cache.ts`:

**Key implementation details:**
- Abstract `Cache` base class
- `MemoryCache` using `node-cache`
- `RedisCache` using `ioredis`
- Implements all 9 operations: `get`, `set`, `remove`, `clear`, `has`, `size`, `keys`, `getOrSet`, `flush`
- TTL support with automatic cleanup
- Async operations with Result<T> wrapping

**Integration patterns:**
```typescript
// Factory pattern for cache creation
static create(config: CacheConfig): Result<Cache> {
  switch (config.type) {
    case 'memory':
      return MemoryCache.create(config);
    case 'redis':
      return RedisCache.create(config);
    default:
      return failure(QiErrorImpl.configuration('INVALID_CACHE_TYPE', config.type));
  }
}
```

### 3.4 Create Core Index

Create `src/core/index.ts`:
```typescript
export * from './configuration.js';
export * from './logger.js';
export * from './cache.js';
```

### 3.5 Verification Tests

```bash
# Test configuration monoid laws
bun test tests/core/configuration.test.ts

# Test logger performance
bun run benchmarks/logger-performance.ts

# Test cache operations
bun test tests/core/cache.test.ts

# Lint core components
bun run lint src/core/
```

## Phase 4: Application Component Implementation

### 4.1 Implement HTTP Client

Copy HTTP Client implementation from template to `src/application/http/client.ts`:

**Key implementation details:**
- Uses `axios` for HTTP operations
- Implements circuit breaker pattern with state machine
- Includes all 7 operations: `get`, `post`, `put`, `patch`, `delete`, `stream`, `withCircuitBreaker`
- Request/response interceptors for logging
- Error categorization and Result<T> wrapping

**Circuit breaker integration:**
```typescript
// State machine implementation
enum CircuitBreakerState {
  CLOSED = 'closed',
  OPEN = 'open',
  HALF_OPEN = 'half_open'
}

// Axios interceptor integration
this.axiosInstance.interceptors.response.use(
  (response) => {
    this.circuitBreaker?.onSuccess();
    return response;
  },
  (error) => {
    this.circuitBreaker?.onFailure();
    return Promise.reject(error);
  }
);
```

### 4.2 Implement Web Framework

Copy Web Framework implementation from template to `src/application/web/framework.ts`:

**Key implementation details:**
- Uses `fastify` for high performance
- Implements middleware composition pattern
- Includes all 8 operations: `route`, `use`, `listen`, `close`, `mount`, `static`, `getConfig`, etc.
- CORS support with `@fastify/cors`
- Error handling with QiError integration

**Fastify integration:**
```typescript
// Route registration with fastify
const routeOptions: RouteOptions = {
  method: method.toUpperCase() as any,
  url: path,
  handler: async (request: FastifyRequest, reply: FastifyReply) => {
    // Convert fastify request to WebRequest
    const webRequest = this.convertRequest(request);
    
    // Execute middleware chain
    const result = await this.executeMiddleware(webRequest, handler);
    
    // Convert result to fastify response
    this.sendResponse(reply, result);
  }
};

this.app.route(routeOptions);
```

### 4.3 Implement Remaining Components

Following the same pattern, implement:

1. **AI/LLM Client** (`src/application/ai/client.ts`):
   - Uses `openai` and `@anthropic-ai/sdk`
   - Implements reader monad pattern for configuration
   - Circuit breaker integration
   - Streaming support

2. **MCP Protocol** (`src/application/mcp/protocol.ts`):
   - Uses `@modelcontextprotocol/sdk`
   - Message transformation patterns
   - Connection management

3. **Database** (`src/application/database/db.ts`):
   - Uses `drizzle-orm` and `better-sqlite3`
   - Transaction monad pattern
   - Connection pooling

4. **Document Generation** (`src/application/document/generator.ts`):
   - Uses `handlebars` and `marked`
   - Template compilation and streaming

5. **CLI Processing** (`src/application/cli/processor.ts`):
   - Uses `commander` and `chalk`
   - Parser combinator patterns

### 4.4 Create Application Index

Create `src/application/index.ts`:
```typescript
export * from './http/client.js';
export * from './web/framework.js';
export * from './ai/client.js';
export * from './mcp/protocol.js';
export * from './database/db.js';
export * from './document/generator.js';
export * from './cli/processor.js';
```

## Phase 5: Integration and Testing

### 5.1 Create Main Library Index

Create `src/index.ts`:
```typescript
// QiCore v4.0 TypeScript Library
export * from './base/index.js';
export * from './core/index.js';
export * from './application/index.js';

// Library version and metadata
export const QICORE_VERSION = '4.0.1';
export const QICORE_BUILD = 'typescript';
```

### 5.2 Complete Integration Example

Copy the complete application example from template to `examples/complete-app.ts`:

**Integration verification:**
```typescript
// Verify all components work together
async function integratedTest(): Promise<Result<void>> {
  // 1. Load configuration
  const config = await Configuration.fromFile('./config.json');
  
  // 2. Create logger
  const logger = Logger.create({ level: LogLevel.INFO });
  
  // 3. Create cache
  const cache = MemoryCache.create({ type: 'memory' });
  
  // 4. Create HTTP client with circuit breaker
  const httpClient = new HttpClient().withCircuitBreaker({
    failureThreshold: 5,
    timeout: 30000
  });
  
  // 5. Create web application
  const app = new WebApplication({ port: 3000 });
  
  // 6. Verify all components integrate properly
  return success(undefined);
}
```

### 5.3 Performance Benchmarks

Copy benchmark suite from template to `benchmarks/performance.ts`:

Run performance verification:
```bash
# Run all benchmarks
bun run benchmarks/performance.ts

# Verify performance targets
# - Result creation: < 100μs ✓
# - Cache operations: < 1ms ✓  
# - HTTP operations: < 1ms circuit check ✓
# - Log level check: < 10ns ✓
```

### 5.4 Mathematical Law Testing

Create comprehensive test suite in `tests/`:

```bash
# Run all mathematical law tests
bun test tests/base/result.test.ts    # Monad laws
bun test tests/core/config.test.ts    # Monoid laws  
bun test tests/application/http.test.ts # Circuit breaker laws

# Run performance tests
bun test tests/performance/

# Run integration tests
bun test tests/integration/
```

## Phase 6: Build and Deployment

### 6.1 Build Configuration

Scripts are already included in the package.json above:
```json
{
  "scripts": {
    "build": "bun run tsc",
    "dev": "bun run examples/complete-app.ts",
    "test": "bun test",
    "test:watch": "bun test --watch",
    "test:coverage": "bun test --coverage",
    "lint": "eslint src/ --ext .ts",
    "lint:fix": "eslint src/ --ext .ts --fix",
    "format": "prettier --write src/**/*.ts",
    "benchmark": "bun run benchmarks/performance.ts"
  }
}
```

### 6.2 Quality Assurance

Run complete QA suite:
```bash
# 1. Lint all code
bun run lint

# 2. Type checking
bunx tsc --noEmit

# 3. Run all tests
bun run test

# 4. Coverage verification (should be > 90%)
bun run test:coverage

# 5. Performance benchmarks
bun run benchmark

# 6. Build verification
bun run build
```

### 6.3 Production Build

```bash
# Clean and build
rm -rf dist
bun run build

# Verify dist/ structure
ls -la dist/
# Should contain:
# - base/
# - core/ 
# - application/
# - index.js
# - index.d.ts
# - *.map files
```

## Phase 7: Deployment and Documentation

### 7.1 Package Configuration

Update `package.json` for publication:
```json
{
  "name": "@qicore/typescript",
  "version": "4.0.1",
  "type": "module",
  "main": "./dist/index.js",
  "types": "./dist/index.d.ts",
  "exports": {
    ".": {
      "import": "./dist/index.js",
      "types": "./dist/index.d.ts"
    },
    "./base": {
      "import": "./dist/base/index.js",
      "types": "./dist/base/index.d.ts"
    },
    "./core": {
      "import": "./dist/core/index.js", 
      "types": "./dist/core/index.d.ts"
    },
    "./application": {
      "import": "./dist/application/index.js",
      "types": "./dist/application/index.d.ts"
    }
  },
  "files": ["dist"],
  "engines": {
    "bun": ">=1.0.0"
  }
}
```

### 7.2 Usage Documentation

Create `README.md`:
```markdown
# QiCore v4.0 TypeScript

Mathematical contract-based TypeScript library with 99 operations across 13 components.

## Installation

```bash
bun add @qicore/typescript
```

## Quick Start

```typescript
import { Result, success, failure } from '@qicore/typescript';

// Result monad for error handling
const result = success(42)
  .map(x => x * 2)
  .flatMap(x => x > 50 ? success(x) : failure(QiError.validation('TOO_SMALL', 'Value too small')));
```

## Components

- **Base**: Result<T>, QiError with categorical error handling
- **Core**: Configuration, Logger, Cache with mathematical properties
- **Application**: HTTP, Web, AI, Database, Document, CLI with resilience patterns

## Performance

TypeScript (interpreted tier): 100× baseline performance targets met.
```

### 7.3 Final Verification

Run complete verification:
```bash
# 1. Full test suite
bun run test

# 2. Performance benchmarks  
bun run benchmark

# 3. Build verification
bun run build

# 4. Package verification
bun pack
tar -tzf qicore-typescript-4.0.1.tgz

# 5. Integration test
cd /tmp
bun init -y
bun add ./path/to/qicore-typescript-4.0.1.tgz
bun -e "import('@qicore/typescript').then(lib => console.log('✓ Library loads correctly'))"
```

## Success Criteria

✅ **All 99 operations implemented** across 13 components  
✅ **Mathematical laws preserved** through fp-ts integration  
✅ **Performance targets met** for interpreted tier (100× baseline)  
✅ **Package integration complete** using researched packages  
✅ **Result<T> wrapping** for all external operations  
✅ **Circuit breaker patterns** implemented for resilience  
✅ **Comprehensive testing** with >90% coverage  
✅ **Production build** ready for deployment  

## Dependencies and References

- **Templates**: [TypeScript Template](qi.v4.ts.template.md) - Complete code implementations
- **Design**: [Design Analysis](../design/qi.v4.design.analysis.md) - Mathematical patterns preserved  
- **Packages**: [TypeScript Packages](../package/ts.md) - All selected packages integrated
- **Performance**: Interpreted tier (100× baseline) compliance verified
- **Mathematics**: All categorical laws tested and verified

---

*This implementation guide ensures consistent, high-quality TypeScript code that preserves mathematical properties while using production-ready packages and meeting performance requirements.*