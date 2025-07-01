# QiCore v4.0 - TypeScript Implementation

> **Mathematical Contract-Based TypeScript Library**  
> **Modern, High-Performance, Type-Safe**

QiCore v4.0 is a comprehensive TypeScript library providing 13 components with 99+ operations based on mathematical contracts and functional programming principles.

## Features

### **Modern Development Stack (2024-2025)**
- **🚀 Bun Runtime**: Ultra-fast JavaScript runtime and package manager
- **🔒 Full Type Safety**: Strict TypeScript with comprehensive type coverage
- **🛠️ Modern Tooling**: Biome linting/formatting, Bun test runner
- **📦 Standards Compliant**: ESM modules with modern TypeScript configuration

### **13 Core Components**

#### **Base Components (2)**
- **Result<T>**: Monad for error handling (8 operations)
- **QiError**: Structured error system with 8 categories (6 operations)

#### **Core Components (3)**  
- **Configuration**: Type-safe config with Zod validation (9 operations)
- **StructuredLogger**: High-performance logging with Pino (7 operations)
- **Cache**: Thread-safe caching with TTL/LRU eviction (9 operations)

#### **Application Components (8)**
- **HTTPClient**: HTTP client with circuit breaker (7 operations)
- **Database**: Database operations with transactions (5 operations)
- **WebApplication**: Framework integration (8 operations) - *planned*
- **CLIApplication**: Command-line processing (5 operations) - *planned*
- **AIClient**: Unified LLM client integration (5 operations) - *planned*
- **DocumentGenerator**: Template/PDF generation (6 operations) - *planned*
- **ASGIServer**: Server integration (5 operations) - *planned*
- **MCPClient**: Model Context Protocol (6 operations) - *planned*

## Installation

### **Using Bun (Recommended)**
```bash
# Install the package
bun add qicore

# Or install from source
git clone <repository>
cd qicore-v4-typescript
bun install
```

### **Using npm/yarn**
```bash
npm install qicore
# or
yarn add qicore
```

## Quick Start

```typescript
import { Result, QiError, Configuration, StructuredLogger, Cache } from "qicore";
import { z } from "zod";

// 1. Define configuration schema with Zod
const AppConfigSchema = z.object({
  appName: z.string().default("My App"),
  cacheSize: z.number().default(1000),
  debug: z.boolean().default(false),
});

// 2. Setup configuration
const config = new Configuration(AppConfigSchema);
config.loadFromObject({
  appName: "QiCore Demo",
  cacheSize: 500,
  debug: true,
});

// 3. Setup logging
const logger = new StructuredLogger({
  level: "info",
  component: "demo",
  prettyPrint: true,
});

// 4. Create cache
const cache = new Cache<string, string>({ maxSize: 1000 });

// 5. Use Result monad for error handling
async function demo() {
  const result = await Result.tryCatchAsync(async () => {
    // Cache operations
    await cache.set("user:123", "john_doe");
    const user = await cache.get("user:123");
    
    if (user.isSuccess()) {
      logger.info("User retrieved from cache", {
        userId: "123",
        username: user.unwrap(),
        cacheStats: cache.getStats(),
      });
      return user.unwrap();
    } else {
      throw new Error("User not found");
    }
  });

  return result.match({
    success: (username) => `Hello, ${username}!`,
    failure: (error) => `Error: ${error.message}`,
  });
}

// Run the demo
console.log(await demo());
```

## Testing

### **Run Tests**
```bash
# All tests
bun test

# Watch mode
bun test --watch

# Coverage
bun test --coverage

# Specific test file
bun test tests/base/result.test.ts
```

### **Test Categories**
- **Unit Tests**: Individual component testing
- **Integration Tests**: Cross-component functionality
- **Property Tests**: Mathematical law verification
- **Performance Tests**: Benchmarking and optimization

## Development

### **Setup Development Environment**
```bash
# Clone and setup
git clone <repository>
cd qicore-v4-typescript
bun install

# Run linting and formatting
bun run lint
bun run format

# Type checking
bun run typecheck

# Build
bun run build

# Development mode
bun run dev
```

### **Project Structure**
```
typescript/
├── src/
│   └── qicore/
│       ├── base/          # Components 1-2: Result<T>, QiError
│       ├── core/          # Components 3-5: Config, Logger, Cache  
│       └── application/   # Components 6-13: HTTP, DB, Web, etc.
├── tests/                 # Comprehensive test suite
├── examples/              # Usage examples
└── docs/                  # Documentation
```

## Mathematical Contracts

QiCore implements rigorous mathematical laws:

### **Monad Laws (Result<T>)**
```typescript
// Left Identity: return a >>= f ≡ f a
Result.success(a).flatMap(f) === f(a)

// Right Identity: m >>= return ≡ m  
m.flatMap(x => Result.success(x)) === m

// Associativity: (m >>= f) >>= g ≡ m >>= (x => f(x) >>= g)
m.flatMap(f).flatMap(g) === m.flatMap(x => f(x).flatMap(g))
```

### **Functor Laws (Result<T>)**
```typescript
// Identity: map(id) ≡ id
result.map(x => x) === result

// Composition: map(g ∘ f) ≡ map(g) ∘ map(f)
result.map(x => g(f(x))) === result.map(f).map(g)
```

## Status

✅ **Ready for Development** - Core components implemented and tested  
✅ **Mathematical Laws Verified** - Monad, functor, and monoid laws tested  
✅ **Modern TypeScript** - Full type safety with latest standards  
✅ **Bun Integration** - Optimized for modern JavaScript runtime  
⚠️ **Application Layer** - Some components planned for future implementation  

## Requirements

- **Bun**: 1.1.0+ (recommended) or Node.js 20.0.0+
- **TypeScript**: 5.5.0+
- **Dependencies**: Modern 2024-2025 package versions

## Recent Updates

- **2025-06-26**: Initial TypeScript implementation
  - Implemented Base components: Result<T> monad and QiError system
  - Implemented Core components: Configuration, Logger, Cache
  - Added HTTP client and Database with mathematical contracts
  - Created comprehensive test suite with Bun test runner
  - Set up modern TypeScript tooling with Biome

---

**QiCore v4.0** - Mathematical contracts meet modern TypeScript development.