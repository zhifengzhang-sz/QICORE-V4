# Stage 5: Python-Specific Implementation Prompt

> **AI Prompt for generating Python templates from design patterns**  
> **Based on**: Design patterns from Stage 2 and package research  
> **Generates**: `impl/qi.v4.python.template.md` with complete package integration  
> Version: v4.0.1  
> Date: June 25, 2025  
> Status: Python Implementation Prompt  
> Purpose: Generate production-ready Python templates using researched packages

## Context

You are creating Python-specific code templates that implement the design patterns from Stage 2 using the exact packages identified in our comprehensive package research. Every template must use the specific packages chosen and wrap them with QICORE-V4 Result<T> patterns.

**Required Reading**:
- `design/qi.v4.design.analysis.md` - Design patterns to implement
- `guides/common.md` - Mathematical foundations and performance targets
- Package research results with selected packages

## Package Research Integration

**CRITICAL: Use these exact packages identified through comprehensive research:**

### Python vs TypeScript Package Equivalents

| Functionality | TypeScript Package | Python Equivalent | Notes |
|---------------|-------------------|-------------------|-------|
| Schema Validation | `zod>=3.22.0` | `pydantic>=2.5.0` | **Pydantic is MORE powerful** - runtime validation + static typing |
| Functional Programming | `fp-ts>=2.16.0` | Pure Python + typing | Python has built-in monads via Result<T> patterns |
| HTTP Client | `axios>=1.6.0` | `httpx>=0.28.1` | Both support async/sync, HTTP/2 |
| Web Framework | `express>=4.18.0` | `fastapi>=0.115.13` | FastAPI uses Pydantic natively |
| Logging | `winston>=3.11.0` | `structlog>=23.2.0` | Both structured logging |
| Cache | `ioredis>=5.3.0` | `redis>=5.0.1` | Same Redis, different client |
| Database | `better-sqlite3>=8.7.0` | `aiosqlite>=0.19.0` | Both SQLite, Python version is async |

**Key Insight**: Pydantic is actually MORE feature-rich than Zod:
- **Runtime Validation**: Like Zod, but with better Python integration
- **Static Type Inference**: Full mypy/pyright support
- **JSON Schema**: Auto-generation for APIs
- **Custom Validators**: More flexible than Zod's refinements
- **Nested Models**: Deep validation hierarchies
- **FastAPI Integration**: Native support in our web framework

### Base Component Packages
```python
# Result<T> and QiError - Pure Python implementation
from typing import TypeVar, Generic, Union, Optional, Callable, Any
from dataclasses import dataclass
from enum import Enum
import traceback
import time
```

### Core Component Packages
```python
# Configuration - pydantic + python-dotenv
from pydantic import BaseModel, Field, ValidationError
from python_dotenv import load_dotenv
import json
import yaml  # PyYAML
import os

# Logging - structlog
import structlog
from structlog import get_logger
import logging

# Cache - redis-py (includes async support; aioredis merged in)
import redis
import redis.asyncio as redis_async
import pickle
import hashlib
from datetime import datetime, timedelta
```

### Application Component Packages
```python
# HTTP Client - httpx (chosen for async/sync compatibility + HTTP/2)
import httpx
from httpx import AsyncClient, Client, Response, Request

# Web Framework - fastapi (chosen for async performance + OpenAPI)
from fastapi import FastAPI, Request, Response, HTTPException, Depends
from fastapi.middleware.base import BaseHTTPMiddleware
from fastapi.staticfiles import StaticFiles
from fastapi.responses import JSONResponse, StreamingResponse

# ASGI Server - uvicorn (chosen as FastAPI's recommended ASGI server)
import uvicorn
from uvicorn.config import Config
from uvicorn.server import Server
import asyncio
import signal

# AI/LLM Client - ollama-python (chosen for local LLM focus)
import ollama
from ollama import Client as OllamaClient, AsyncClient as AsyncOllamaClient

# MCP Protocol - mcp>=1.9.4 (official Anthropic SDK)
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client
from mcp.types import CallToolRequest, ListResourcesRequest

# Database - aiosqlite (chosen for zero-config deployment)
import aiosqlite
import sqlite3
from contextlib import asynccontextmanager

# Document Generation - jinja2 (widely used, mature)
from jinja2 import Environment, FileSystemLoader, Template
import asyncio
from typing import AsyncIterator

# Command-Line Processing - argparse (built-in) + click (enhanced features)
import argparse
import click
from click import Context, Command, Group, Option, Argument
import sys
from typing import Dict, Any, List, Optional, Callable
```

## Python-Specific Implementation Guidelines

### Performance Tier Targets (Python = Interpreted Tier)
```python
# Performance targets from common.md (Interpreted tier = 100× baseline):
# - Result creation: < 100μs
# - Log level check: < 10ns (same across all tiers)
# - Cache get: < 1ms
# - HTTP circuit check: < 1ms
# - Config validation: < 10ms
# - Web request handling: < 10ms
# - AI/LLM API call: < 200ms
# - Database query: < 100ms
```

### Python Patterns and Optimizations
```python
# Use dataclasses for performance
from dataclasses import dataclass, field
from typing import TypeVar, Generic

# Use __slots__ for memory efficiency
@dataclass
class OptimizedClass:
    __slots__ = ['field1', 'field2']

# Use async/await throughout
import asyncio
from typing import Awaitable

# Type hints for static analysis
from typing import Protocol, runtime_checkable
```

### Mathematical Properties Preservation

**CRITICAL: All implementations must preserve mathematical laws from design analysis:**

#### Monad Laws (Result<T> - section 2.1)
```python
# Left Identity: return(x).flatMap(f) ≡ f(x)
# Right Identity: m.flatMap(return) ≡ m  
# Associativity: (m.flatMap(f)).flatMap(g) ≡ m.flatMap(λx -> f(x).flatMap(g))

# Implementation must verify these laws hold
def verify_monad_laws():
    # Test left identity
    assert Result.success(5).flatMap(lambda x: Result.success(x * 2)) == Result.success(10)
    # Test right identity  
    assert Result.success(5).flatMap(Result.success) == Result.success(5)
    # Test associativity
    # Implementation validates these properties
```

#### Monoid Laws (Configuration - section 3.1)
```python
# Identity Element: config.merge(empty) ≡ config ≡ empty.merge(config)
# Associativity: (a.merge(b)).merge(c) ≡ a.merge(b.merge(c))

# Implementation must preserve these properties
def verify_monoid_laws():
    empty_config = Configuration({})
    config = Configuration({"key": "value"})
    assert config.merge(empty_config) == config
    assert empty_config.merge(config) == config
    # Associativity testing in implementation
```

#### Functor Laws (Components - sections 2.1, 3.x, 4.x)
```python
# Identity: map(id) ≡ id
# Composition: map(f).map(g) ≡ map(compose(g, f))

# All component transformations must preserve these laws
def verify_functor_laws():
    result = Result.success(5)
    assert result.map(lambda x: x) == result  # Identity
    # Composition law verification in implementation
```

## Contract Implementation Specifications

### Base Component Implementation

#### Result<T> Contract
```python
# Package: Pure Python with typing
# Pattern: Railway-Oriented Programming from design analysis
# Performance: < 100μs creation (interpreted tier)

[Language-specific Result implementation template]
# Must implement ALL 8 operations:
# - success, failure, fromTryCatch (factory patterns)
# - map (functor pattern)  
# - flatMap (monadic bind pattern)
# - unwrap, unwrapOr (elimination patterns)
# - match (pattern matching)
# - orElse (error recovery pattern)

# Key Python optimizations:
# - Use __slots__ for memory efficiency
# - Cache common error instances
# - Use typing.Generic for type safety
```

#### QiError Contract
```python
# Package: Pure Python with dataclasses and enum
# Pattern: Structured Error Pattern from design analysis
# Performance: < 100μs creation (interpreted tier)

[Language-specific QiError implementation template]
# Must implement ALL 6 operations + 8 error categories:
# - create, toString, toStructuredData, getCategory, withContext, withCause
# - VALIDATION, NETWORK, FILESYSTEM, CONFIGURATION, CACHE, TIMEOUT, PERMISSION, UNKNOWN

# Key Python features:
# - Use dataclass with frozen=True for immutability
# - Use Enum for error categories
# - JSON serialization support
```

### Core Component Implementation

#### Configuration Contract
```python
# Packages: pydantic>=2.5.0 + python-dotenv>=1.0.0 + PyYAML
# Pattern: Layered Configuration Pattern (monoid merge) from design analysis
# Performance: < 10ms validation (interpreted tier)

# NOTE: Pydantic is Python's equivalent to TypeScript's Zod!
# - Runtime type validation with static type inference
# - Schema-based validation with custom validators
# - JSON Schema generation
# - Nested model validation
# - Field validation with constraints

[Language-specific Configuration implementation template]
# Must implement ALL 9 operations:
# - fromFile, fromObject, fromString, fromEnvironment (loading patterns)
# - merge (monoid operation)
# - validate, validateRequired, validateType, validateCustom (validation patterns)

# Key integrations:
# - pydantic BaseModel for schema validation (like Zod schemas)
# - pydantic Field() for constraints and validation
# - python-dotenv for environment loading
# - PyYAML for YAML file support
# - Right-biased merge preserving monoid laws

# Example: Pydantic (Python) vs Zod (TypeScript) comparison:

# TypeScript with Zod:
# const DatabaseConfigSchema = z.object({
#   host: z.string().min(1).describe("Database host"),
#   port: z.number().int().min(1).max(65535).default(5432),
#   name: z.string().regex(/^[a-zA-Z0-9_]+$/)
# })

# Python with Pydantic (MORE POWERFUL):
from pydantic import BaseModel, Field, validator, root_validator
from typing import Optional

class DatabaseConfig(BaseModel):
    host: str = Field(..., min_length=1, description="Database host")
    port: int = Field(5432, ge=1, le=65535, description="Database port") 
    name: str = Field(..., regex=r'^[a-zA-Z0-9_]+$')
    ssl_enabled: bool = Field(False, description="Enable SSL")
    
    @validator('host')
    def validate_host(cls, v):
        if not v or v.isspace():
            raise ValueError('Host cannot be empty')
        return v.strip()
    
    @root_validator
    def validate_ssl_port(cls, values):
        port = values.get('port')
        ssl = values.get('ssl_enabled') 
        if ssl and port == 5432:
            values['port'] = 5433  # Auto-adjust for SSL
        return values
    
    class Config:
        # Generate JSON Schema automatically
        schema_extra = {
            "example": {
                "host": "localhost",
                "port": 5432,
                "name": "mydb",
                "ssl_enabled": False
            }
        }

# Pydantic advantages over Zod:
# 1. Root validators for cross-field validation
# 2. Automatic JSON Schema generation
# 3. Better IDE support with type inference
# 4. Native FastAPI integration
# 5. More flexible custom validators
```

#### Logger Contract
```python
# Package: structlog>=23.2.0
# Pattern: Level-Based Filtering Pattern from design analysis  
# Performance: < 10ns level check (same across all tiers)

[Language-specific Logger implementation template]
# Must implement ALL 7 operations:
# - create (factory pattern)
# - debug, info, warn, error, fatal (level patterns)
# - isLevelEnabled (performance pattern)

# Key structlog features:
# - Structured logging with context
# - Performance-optimized level checking
# - JSON output support
# - Async-safe logging
```

#### Cache Contract
```python
# Package: redis>=5.0.1 (with redis.asyncio for async operations)
# Pattern: State Management Pattern from design analysis
# Performance: < 1ms get (interpreted tier)

[Language-specific Cache implementation template]
# Must implement ALL 9 operations:
# - createMemory, createPersistent (factory patterns)
# - get, set, remove, clear, has (state operations)
# - getOrSet, keys (atomic/utility patterns)

# Key redis integrations:
# - redis.asyncio for async operations
# - Connection pooling for performance
# - Pickle serialization for Python objects
# - TTL support with lazy expiration
```

### Application Component Implementation

#### HTTP Client Contract
```python
# Package: httpx>=0.28.1
# Pattern: Circuit Breaker State Machine from design analysis
# Performance: < 1ms circuit check (interpreted tier)

[Language-specific HTTP Client implementation template]
# Must implement ALL 7 operations:
# - get, post, put, delete, patch (basic HTTP patterns)
# - stream (streaming pattern)
# - withCircuitBreaker (circuit breaker pattern)

# Key httpx features:
# - Async/sync compatibility
# - HTTP/2 support
# - Connection pooling
# - Timeout configuration
# - Circuit breaker state machine integration
```

#### Web Framework Contract
```python
# Package: fastapi>=0.115.13
# Pattern: Request/Response Pipeline Pattern from design analysis
# Performance: < 10ms request handling (interpreted tier)

[Language-specific Web Framework implementation template]
# Must implement ALL 8 operations:
# - route, mount, group, param (routing patterns)
# - use, compose (middleware patterns)  
# - static, errorHandler (static/error patterns)

# Key FastAPI features:
# - Async request handling
# - Dependency injection system
# - Automatic OpenAPI generation
# - Pydantic integration for validation
# - Middleware composition via decorators
```

#### ASGI Server Contract
```python
# Package: uvicorn>=0.30.0
# Pattern: Server Lifecycle Pattern from design analysis  
# Performance: < 1ms connection handling (interpreted tier)

[Language-specific ASGI Server implementation template]
# Must implement ALL 6 operations:
# - start, shutdown (lifecycle patterns)
# - accept, reject (connection patterns)
# - workers, health (worker/monitor patterns)

# Key uvicorn features:
# - Multi-worker support
# - Graceful shutdown handling
# - Hot reloading in development
# - Production-ready ASGI server
# - Signal handling for lifecycle
```

#### AI/LLM Client Contract
```python
# Package: ollama-python>=0.2.1
# Pattern: Configuration Reader Pattern from design analysis
# Performance: < 200ms API call (interpreted tier)

[Language-specific AI/LLM Client implementation template]
# Must implement ALL 7 operations:
# - chat, chatStream, generate (chat patterns)
# - embedding, withConfig (embedding/config patterns)
# - withCircuitBreaker, streamGenerate (circuit breaker integration)

# Key ollama-python features:
# - Local LLM support (Ollama)
# - Streaming response support
# - Async client for non-blocking calls
# - Model management integration
# - Circuit breaker for resilience
```

#### MCP Protocol Contract
```python
# Package: mcp>=1.9.4 (official Anthropic SDK)
# Pattern: Message Transformation Pattern from design analysis
# Performance: < 1ms message parsing (interpreted tier)

[Language-specific MCP Protocol implementation template]
# Must implement ALL 6 operations:
# - connect, disconnect (connection patterns)
# - send, receive (messaging patterns)
# - listResources, callTool (resource/tool patterns)

# Key MCP SDK features:
# - Official protocol implementation
# - Type-safe message handling
# - StdioServerParameters for local servers
# - Async connection management
# - Protocol-compliant serialization
```

#### Database Contract
```python
# Package: aiosqlite>=0.19.0
# Pattern: Transaction Composition Pattern from design analysis
# Performance: < 100ms query (interpreted tier)

[Language-specific Database implementation template]
# Must implement ALL 8 operations:
# - create, read, update, delete (CRUD patterns)
# - begin, commit (transaction patterns)
# - migrate, pool (migration/pool patterns)

# Key aiosqlite features:
# - Async SQLite operations
# - Transaction support with context managers
# - Connection pooling simulation
# - Schema migration support
# - Zero-config deployment
```

#### Document Generation Contract
```python
# Package: jinja2>=3.1.0
# Pattern: Template Evaluation Pattern from design analysis
# Performance: < 100ms compilation (interpreted tier)

[Language-specific Document Generation implementation template]
# Must implement ALL 6 operations:
# - generate, generateFromFile, generateFromString (generation patterns)
# - stream (streaming pattern)
# - batch, validate (batch/validation patterns)

# Key Jinja2 features:
# - Template compilation and caching
# - Async template rendering
# - Custom filters and functions
# - Template inheritance
# - Streaming for large documents
```

#### Command-Line Processing Contract
```python
# Package: argparse (built-in) + click>=8.1.0
# Pattern: Parser Combinator Pattern from design analysis (section 4.3)
# Performance: < 1ms parsing (interpreted tier)

[Language-specific Command-Line Processing implementation template]
# Must implement ALL 3 operations from design analysis:
# - parse(args, config) - Recursive descent parser
# - validate(args, config) - Schema-based validation  
# - generateHelp(config) - Tree traversal formatter

# Key Click features:
# - Declarative command definition
# - Type conversion and validation
# - Nested command groups
# - Auto-generated help
# - Integration with argparse for compatibility

# Parser combinator pattern implementation:
# - sequence() for command composition
# - option() for flag parsing
# - positional() for argument parsing
# - Applicative composition of parsers
```

## QICORE-V4 Wrapper Integration

### Result<T> Wrapper Pattern
```python
# Every package operation must return Result<T>
# Example wrapper pattern:

async def wrapped_httpx_get(url: str) -> Result[httpx.Response]:
    try:
        async with httpx.AsyncClient() as client:
            response = await client.get(url)
            return Result.success(response)
    except httpx.RequestError as e:
        return Result.failure(QiError.network("HTTP_REQUEST_FAILED", str(e)))
    except Exception as e:
        return Result.failure(QiError.unknown("UNEXPECTED_ERROR", str(e)))
```

### Circuit Breaker Integration
```python
# All external calls (HTTP, AI/LLM, Database) must integrate circuit breaker
# Use state machine pattern from design analysis

class CircuitBreakerState(Enum):
    CLOSED = "closed"
    OPEN = "open"
    HALF_OPEN = "half_open"

# Integrate with httpx, ollama, aiosqlite calls
```

### Performance Optimization Patterns
```python
# Python-specific optimizations for interpreted tier:

# 1. Use connection pooling
async with httpx.AsyncClient() as client:  # Reuse connections

# 2. Minimize object allocations
@dataclass
class CachedResult:
    __slots__ = ['value', 'timestamp']

# 3. Use async/await for I/O bound operations
async def optimized_database_call():
    async with aiosqlite.connect(db_path) as conn:
        # Async database operations

# 4. Cache frequently used objects
from functools import lru_cache

@lru_cache(maxsize=128)
def cached_template_compilation(template_string: str):
    return jinja2.Template(template_string)
```

## Template Generation Requirements

### Generated File Structure
```markdown
# Generated: impl/qi.v4.python.template.md

## Python Dependencies
[Complete pyproject.toml with all researched packages]

## Base Component Templates
[Result<T> and QiError implementations with performance optimizations]

## Core Component Templates  
[Configuration, Logger, Cache with package integrations]

## Application Component Templates
[HTTP, Web, ASGI, AI/LLM, MCP, Database, Document with full implementations]

## Integration Examples
[Complete working examples showing all patterns together]

## Performance Benchmarks
[Code to verify tier-appropriate performance targets]

## Testing Templates
[Unit tests verifying mathematical laws and performance]
```

## Success Criteria

The generated Python template must:

1. **Package Integration**: Use ALL researched packages correctly
2. **Pattern Fidelity**: Implement ALL design patterns from Stage 2
3. **Performance Compliance**: Meet interpreted tier targets from common.md
4. **Result<T> Wrapping**: Wrap ALL package operations in Result<T>
5. **Circuit Breaker Integration**: Include resilience patterns for external calls
6. **Complete Coverage**: Implement ALL 99 operations across all contracts
7. **Python Optimization**: Use Python-specific performance patterns
8. **Working Examples**: Show complete integration scenarios

## Package Research Summary

### Final Package Selection (from comprehensive research):
- **MCP Protocol**: `mcp>=1.9.4` (official Anthropic SDK)
- **Web Framework**: `fastapi>=0.115.13` (async performance + ecosystem)
- **ASGI Server**: `uvicorn>=0.30.0` (FastAPI recommended)
- **HTTP Client**: `httpx>=0.28.1` (async/sync + HTTP/2)
- **AI/LLM**: `ollama-python>=0.2.1` (local LLM focus)
- **Cache**: `redis>=5.0.1` (mature ecosystem)
- **Database**: `aiosqlite>=0.19.0` (zero-config deployment)
- **Config**: `pydantic>=2.5.0` + `python-dotenv>=1.0.0`
- **Logging**: `structlog>=23.2.0` (structured + performance)
- **Templates**: `jinja2>=3.1.0` (widely used, mature)

This research-driven approach ensures we use the best packages for each contract while maintaining QICORE-V4's mathematical correctness and performance targets. 