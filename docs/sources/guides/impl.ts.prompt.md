# Stage 3: TypeScript-Specific Implementation Prompt

> **AI Prompt for generating TypeScript templates from design patterns**  
> **Based on**: Design patterns from Stage 2 and package research  
> **Generates**: `impl/qi.v4.typescript.template.md` with complete package integration  
> Version: v4.0.1  
> Date: December 26, 2024  
> Status: TypeScript Implementation Prompt  
> Purpose: Generate production-ready TypeScript templates using researched packages

## Context

You are creating TypeScript-specific code templates that implement the design patterns from Stage 2 using the exact packages identified in our comprehensive package research. Every template must use the specific packages chosen and wrap them with QICORE-V4 Result<T> patterns.

**Required Reading**:
- `design/qi.v4.design.analysis.md` - Design patterns to implement
- `guides/common.md` - Mathematical foundations and performance targets
- Package research results with selected packages

## Package Research Integration

**CRITICAL: Use these exact packages identified through comprehensive research:**

### Base Component Packages
```typescript
// Result<T> and QiError - fp-ts for categorical abstractions
import { Either, left, right } from 'fp-ts/Either'
import { Option, none, some } from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'
import { TaskEither } from 'fp-ts/TaskEither'

// Type utilities
type Result<T> = Either<QiError, T>
interface QiError {
  code: string
  message: string
  category: ErrorCategory
  context?: Record<string, unknown>
  cause?: QiError
  timestamp: number
}
```

### Core Component Packages
```typescript
// Configuration - zod for schema validation + dotenv
import { z } from 'zod'
import * as dotenv from 'dotenv'
import * as fs from 'fs/promises'
import * as yaml from 'js-yaml'

// Logging - winston or pino (structured logging)
import { createLogger, format, transports } from 'winston'
// OR: import pino from 'pino'

// Cache - ioredis (mature Redis client for Node.js)
import Redis from 'ioredis'
import { Cluster } from 'ioredis'
```

### Application Component Packages
```typescript
// HTTP Client - axios (comprehensive ecosystem) or fetch API
import axios, { AxiosInstance, AxiosResponse, AxiosError } from 'axios'
// OR: Built-in fetch with ponyfills for Node.js compatibility

// Web Framework - express (mature ecosystem) or fastify (performance)
import express, { Express, Request, Response, NextFunction } from 'express'
// OR: import fastify, { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify'

// Server - Node.js built-in HTTP/HTTPS
import { createServer, Server } from 'http'
import { createServer as createHttpsServer } from 'https'

// AI/LLM Client - openai SDK (comprehensive) + ollama client
import OpenAI from 'openai'
import { Ollama } from 'ollama'

// MCP Protocol - @modelcontextprotocol/sdk (official)
import { Client } from '@modelcontextprotocol/sdk/client/index.js'
import { StdioClientTransport } from '@modelcontextprotocol/sdk/client/stdio.js'

// Database - better-sqlite3 (synchronous performance) or pg (PostgreSQL)
import Database from 'better-sqlite3'
// OR: import { Pool, Client } from 'pg'

// Document Generation - handlebars (mature) or mustache
import Handlebars from 'handlebars'
// OR: import * as Mustache from 'mustache'
```

## TypeScript-Specific Implementation Guidelines

### Performance Tier Targets (TypeScript = Interpreted Tier)
```typescript
// Performance targets from common.md (Interpreted tier = 100× baseline):
// - Result creation: < 100μs
// - Log level check: < 10ns (same across all tiers)
// - Cache get: < 1ms
// - HTTP circuit check: < 1ms
// - Config validation: < 10ms
// - Web request handling: < 10ms
// - AI/LLM API call: < 200ms
// - Database query: < 100ms
```

### TypeScript Patterns and Optimizations
```typescript
// Use strict type checking
interface StrictConfig {
  compilerOptions: {
    strict: true
    noImplicitAny: true
    strictNullChecks: true
    noImplicitReturns: true
  }
}

// Use branded types for type safety
type UserId = string & { readonly brand: unique symbol }
type DatabaseId = number & { readonly brand: unique symbol }

// Use readonly for immutability
interface ImmutableConfig {
  readonly database: {
    readonly host: string
    readonly port: number
  }
}

// Use fp-ts for functional programming patterns
import { pipe } from 'fp-ts/function'
import * as TE from 'fp-ts/TaskEither'
import * as E from 'fp-ts/Either'
```

## Contract Implementation Specifications

### Base Component Implementation

#### Result<T> Contract
```typescript
// Package: fp-ts for functional abstractions
// Pattern: Railway-Oriented Programming from design analysis
// Performance: < 100μs creation (interpreted tier)

[Language-specific Result implementation template]
// Must implement ALL 8 operations:
// - success, failure, fromTryCatch (factory patterns)
// - map (functor pattern)  
// - flatMap (monadic bind pattern)
// - unwrap, unwrapOr (elimination patterns)
// - match (pattern matching)
// - orElse (error recovery pattern)

// Key TypeScript features:
// - Use fp-ts Either as foundation
// - Branded types for type safety
// - Strict null checks
// - Union types for error categories
```

#### QiError Contract
```typescript
// Package: Native TypeScript with strict typing
// Pattern: Structured Error Pattern from design analysis
// Performance: < 100μs creation (interpreted tier)

[Language-specific QiError implementation template]
// Must implement ALL 6 operations + 8 error categories:
// - create, toString, toStructuredData, getCategory, withContext, withCause
// - VALIDATION, NETWORK, FILESYSTEM, CONFIGURATION, CACHE, TIMEOUT, PERMISSION, UNKNOWN

// Key TypeScript features:
// - Discriminated unions for error categories
// - Readonly interfaces for immutability
// - JSON serialization with type safety
// - Optional chaining for context access
```

### Core Component Implementation

#### Configuration Contract
```typescript
// Packages: zod>=3.22.0 + dotenv>=16.3.0 + js-yaml>=4.1.0
// Pattern: Layered Configuration Pattern (monoid merge) from design analysis
// Performance: < 10ms validation (interpreted tier)

[Language-specific Configuration implementation template]
// Must implement ALL 9 operations:
// - fromFile, fromObject, fromString, fromEnvironment (loading patterns)
// - merge (monoid operation)
// - validate, validateRequired, validateType, validateCustom (validation patterns)

// Key integrations:
// - zod schemas for type-safe validation
// - dotenv for environment variable loading
// - js-yaml for YAML file support
// - Right-biased merge using object spread
```

#### Logger Contract
```typescript
// Package: winston>=3.11.0 or pino>=8.15.0
// Pattern: Level-Based Filtering Pattern from design analysis  
// Performance: < 10ns level check (same across all tiers)

[Language-specific Logger implementation template]
// Must implement ALL 7 operations:
// - create (factory pattern)
// - debug, info, warn, error, fatal (level patterns)
// - isLevelEnabled (performance pattern)

// Key winston/pino features:
// - Structured logging with metadata
// - Performance-optimized level checking
// - JSON output with custom formatting
// - Stream-based transports
```

#### Cache Contract
```typescript
// Package: ioredis>=5.3.0
// Pattern: State Management Pattern from design analysis
// Performance: < 1ms get (interpreted tier)

[Language-specific Cache implementation template]
// Must implement ALL 9 operations:
// - createMemory, createPersistent (factory patterns)
// - get, set, remove, clear, has (state operations)
// - getOrSet, keys (atomic/utility patterns)

// Key ioredis features:
// - Promise-based async operations
// - Connection pooling and clustering
// - TypeScript-friendly API
// - Built-in TTL support
// - Pipeline for batch operations
```

### Application Component Implementation

#### HTTP Client Contract
```typescript
// Package: axios>=1.6.0 or native fetch
// Pattern: Circuit Breaker State Machine from design analysis
// Performance: < 1ms circuit check (interpreted tier)

[Language-specific HTTP Client implementation template]
// Must implement ALL 7 operations:
// - get, post, put, delete, patch (basic HTTP patterns)
// - stream (streaming pattern)
// - withCircuitBreaker (circuit breaker pattern)

// Key axios features:
// - Request/response interceptors
// - Automatic JSON handling
// - Request/response transformation
// - Timeout configuration
// - Circuit breaker integration
```

#### Web Framework Contract
```typescript
// Package: express>=4.18.0 or fastify>=4.24.0
// Pattern: Request/Response Pipeline Pattern from design analysis
// Performance: < 10ms request handling (interpreted tier)

[Language-specific Web Framework implementation template]
// Must implement ALL 8 operations:
// - route, mount, group, param (routing patterns)
// - use, compose (middleware patterns)  
// - static, errorHandler (static/error patterns)

// Key express/fastify features:
// - Middleware composition via use()
// - Type-safe request/response handling
// - Route parameter extraction
// - Static file serving
// - Error handling middleware
```

#### Server Contract (Node.js HTTP)
```typescript
// Package: Node.js built-in http/https
// Pattern: Server Lifecycle Pattern from design analysis  
// Performance: < 1ms connection handling (interpreted tier)

[Language-specific Server implementation template]
// Must implement ALL 6 operations:
// - start, shutdown (lifecycle patterns)
// - accept, reject (connection patterns)
// - workers, health (worker/monitor patterns)

// Key Node.js features:
// - Built-in HTTP/HTTPS server
// - Graceful shutdown handling
// - Cluster module for workers
// - Health check endpoints
// - Signal handling for lifecycle
```

#### AI/LLM Client Contract
```typescript
// Package: openai>=4.20.0 + ollama>=0.5.0
// Pattern: Configuration Reader Pattern from design analysis
// Performance: < 200ms API call (interpreted tier)

[Language-specific AI/LLM Client implementation template]
// Must implement ALL 7 operations:
// - chat, chatStream, generate (chat patterns)
// - embedding, withConfig (embedding/config patterns)
// - withCircuitBreaker, streamGenerate (circuit breaker integration)

// Key OpenAI/Ollama features:
// - Streaming response support
// - Type-safe API interfaces
// - Configuration management
// - Error handling and retries
// - Circuit breaker for resilience
```

#### MCP Protocol Contract
```typescript
// Package: @modelcontextprotocol/sdk>=1.0.0
// Pattern: Message Transformation Pattern from design analysis
// Performance: < 1ms message parsing (interpreted tier)

[Language-specific MCP Protocol implementation template]
// Must implement ALL 6 operations:
// - connect, disconnect (connection patterns)
// - send, receive (messaging patterns)
// - listResources, callTool (resource/tool patterns)

// Key MCP SDK features:
// - Type-safe protocol implementation
// - Transport abstraction (stdio, websocket)
// - Request/response correlation
// - Resource and tool management
// - Protocol versioning support
```

#### Database Contract
```typescript
// Package: better-sqlite3>=8.7.0 or pg>=8.11.0
// Pattern: Transaction Composition Pattern from design analysis
// Performance: < 100ms query (interpreted tier)

[Language-specific Database implementation template]
// Must implement ALL 8 operations:
// - create, read, update, delete (CRUD patterns)
// - begin, commit (transaction patterns)
// - migrate, pool (migration/pool patterns)

// Key better-sqlite3/pg features:
// - Synchronous API (better-sqlite3) or async (pg)
// - Prepared statements for performance
// - Transaction support
// - Connection pooling (pg.Pool)
// - Migration management
```

#### Document Generation Contract
```typescript
// Package: handlebars>=4.7.0 or mustache>=4.2.0
// Pattern: Template Evaluation Pattern from design analysis
// Performance: < 100ms compilation (interpreted tier)

[Language-specific Document Generation implementation template]
// Must implement ALL 6 operations:
// - generate, generateFromFile, generateFromString (generation patterns)
// - stream (streaming pattern)
// - batch, validate (batch/validation patterns)

// Key Handlebars features:
// - Template compilation and caching
// - Helper functions and partials
// - Safe string handling
// - Custom helpers for logic
// - Streaming for large documents
```

## QICORE-V4 Wrapper Integration

### Result<T> Wrapper Pattern
```typescript
// Every package operation must return Result<T>
// Example wrapper pattern using fp-ts:

import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

const wrappedAxiosGet = (url: string): TE.TaskEither<QiError, AxiosResponse> =>
  TE.tryCatch(
    () => axios.get(url),
    (error): QiError => {
      if (axios.isAxiosError(error)) {
        return QiError.network("HTTP_REQUEST_FAILED", error.message)
      }
      return QiError.unknown("UNEXPECTED_ERROR", String(error))
    }
  )
```

### Circuit Breaker Integration
```typescript
// All external calls must integrate circuit breaker
// Use state machine pattern from design analysis

enum CircuitBreakerState {
  CLOSED = "closed",
  OPEN = "open",
  HALF_OPEN = "half_open"
}

interface CircuitBreakerConfig {
  readonly failureThreshold: number
  readonly timeout: number
  readonly monitoringPeriod: number
}

// Integrate with axios, OpenAI, database calls
```

### Performance Optimization Patterns
```typescript
// TypeScript-specific optimizations for interpreted tier:

// 1. Use object pooling for frequent allocations
class ObjectPool<T> {
  private readonly pool: T[] = []
  
  constructor(private readonly factory: () => T) {}
  
  acquire(): T {
    return this.pool.pop() ?? this.factory()
  }
  
  release(obj: T): void {
    this.pool.push(obj)
  }
}

// 2. Use Map for frequent lookups instead of objects
const fastLookup = new Map<string, ConfigValue>()

// 3. Use async/await with proper error handling
const optimizedDatabaseCall = async (): Promise<Result<Data>> => {
  try {
    const result = await db.query('SELECT * FROM users')
    return Result.success(result)
  } catch (error) {
    return Result.failure(QiError.database("QUERY_FAILED", String(error)))
  }
}

// 4. Use WeakMap for memory-efficient caching
const templateCache = new WeakMap<TemplateSource, CompiledTemplate>()
```

## Template Generation Requirements

### Generated File Structure
```markdown
# Generated: impl/qi.v4.typescript.template.md

## TypeScript Dependencies
[Complete package.json with all researched packages]
[tsconfig.json with strict type checking]

## Base Component Templates
[Result<T> and QiError implementations with fp-ts integration]

## Core Component Templates  
[Configuration, Logger, Cache with package integrations]

## Application Component Templates
[HTTP, Web, Server, AI/LLM, MCP, Database, Document with full implementations]

## Integration Examples
[Complete working examples showing all patterns together]

## Performance Benchmarks
[Code to verify tier-appropriate performance targets]

## Testing Templates
[Unit tests verifying mathematical laws and performance]
```

## Success Criteria

The generated TypeScript template must:

1. **Package Integration**: Use ALL researched packages correctly
2. **Pattern Fidelity**: Implement ALL design patterns from Stage 2
3. **Performance Compliance**: Meet interpreted tier targets from common.md
4. **Result<T> Wrapping**: Wrap ALL package operations in Result<T>
5. **Circuit Breaker Integration**: Include resilience patterns for external calls
6. **Complete Coverage**: Implement ALL 99 operations across all contracts
7. **TypeScript Optimization**: Use TypeScript-specific performance patterns
8. **Working Examples**: Show complete integration scenarios

## Package Research Summary

### Final Package Selection (from comprehensive research):
- **MCP Protocol**: `@modelcontextprotocol/sdk>=1.0.0` (official SDK)
- **Web Framework**: `express>=4.18.0` or `fastify>=4.24.0` (ecosystem vs performance)
- **HTTP Client**: `axios>=1.6.0` (comprehensive ecosystem)
- **AI/LLM**: `openai>=4.20.0` + `ollama>=0.5.0` (comprehensive + local)
- **Cache**: `ioredis>=5.3.0` (mature Redis client)
- **Database**: `better-sqlite3>=8.7.0` or `pg>=8.11.0` (performance vs features)
- **Config**: `zod>=3.22.0` + `dotenv>=16.3.0` (type-safe validation)
- **Logging**: `winston>=3.11.0` or `pino>=8.15.0` (features vs performance)
- **Templates**: `handlebars>=4.7.0` (mature with helpers)
- **Functional**: `fp-ts>=2.16.0` (categorical abstractions)

This research-driven approach ensures we use the best packages for each contract while maintaining QICORE-V4's mathematical correctness and performance targets. 