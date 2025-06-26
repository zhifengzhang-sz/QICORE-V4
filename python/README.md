# QiCore v4.0 - Python Implementation

> **Mathematical Contract-Based Python Library**  
> **Modern, High-Performance, Type-Safe**

QiCore v4.0 is a comprehensive Python library providing 13 components with 99+ operations based on mathematical contracts and functional programming principles.

## Features

### **Modern Development Stack (2024-2025)**
- **uv Package Manager**: 10-100x faster than pip/poetry
- **Full Type Safety**: Strict mypy compliance with comprehensive type hints
- **Modern Tooling**: Ruff linting, pytest testing, rich CLI output
- **Standards Compliant**: Modern pyproject.toml with hatchling backend

### **13 Core Components**

#### **Base Components (2)**
- **Result<T>**: Monad for error handling (8 operations)
- **QiError**: Structured error system with 8 categories (6 operations)

#### **Core Components (3)**  
- **Configuration**: Type-safe config with monoid merge (9 operations)
- **StructuredLogger**: High-performance logging with context (7 operations)
- **Cache**: Thread-safe caching with TTL/LRU eviction (9 operations)

#### **Application Components (8)**
- **HTTPClient**: HTTP client with circuit breaker (7 operations)
- **WebApplication**: FastAPI integration with Result patterns (8 operations)
- **CLIApplication**: Click-based CLI with rich output (5 operations)
- **Database**: Async SQLite with transaction support (5 operations)
- **AIClient**: Unified LLM client (OpenAI/Anthropic/Ollama) (5 operations)
- **DocumentGenerator**: Template/PDF generation with streaming (6 operations)
- **ASGIServer**: Uvicorn integration with SSL support (5 operations)
- **MCPClient**: Model Context Protocol implementation (6 operations)

## Installation

### **Using uv (Recommended)**
```bash
# Install the package
uv add qicore

# Or install from source
git clone <repository>
cd qicore-v4-python
uv sync --dev
```

### **Using pip**
```bash
pip install qicore
```

## Quick Start

```python
import asyncio
from qicore.base import Result, QiError
from qicore.core import Configuration, StructuredLogger, Cache, configure_logging
from qicore.application import HTTPClient, Database
from pydantic import BaseModel

class AppConfig(BaseModel):
    app_name: str = "My App"
    cache_size: int = 1000
    debug: bool = False

async def main():
    # 1. Setup logging
    await configure_logging(level="INFO", format="json")
    logger = StructuredLogger("myapp")
    
    # 2. Load configuration  
    config = Configuration(AppConfig)
    config.load_from_env()  # Loads from environment variables
    
    # 3. Create cache
    cache = Cache[str, str](max_size=1000)
    await cache.set("user:123", "john_doe")
    
    # 4. Use Result monad for error handling
    result = Result.success(42).map(lambda x: x * 2)
    if result.is_success():
        logger.info(f"Calculation result: {result.unwrap()}")
    
    # 5. Database operations
    db = Database("app.db")
    await db.connect()
    
    # All operations return Result<T> for consistent error handling
    query_result = await db.execute("SELECT * FROM users WHERE id = ?", (123,))
    if query_result.is_success():
        logger.info("Query successful")
    
    await db.close()

if __name__ == "__main__":
    asyncio.run(main())
```

## Testing

### **Run All Tests**
```bash
# Unit tests
uv run pytest tests/unit/

# Integration tests  
uv run pytest tests/integration/

# Property-based tests (mathematical laws)
uv run pytest tests/property/

# Performance benchmarks
uv run pytest tests/benchmarks/ --benchmark-only

# All tests with coverage
uv run pytest tests/ --cov=qicore --cov-report=html
```

## Development

### **Setup Development Environment**
```bash
# Clone and setup
git clone <repository>
cd qicore-v4-python
uv sync --dev

# Run linting and type checking
uv run ruff check .
uv run mypy src/

# Run tests
uv run pytest

# Build package
uv build
```

## Requirements

- **Python**: 3.13+ (3.13.5 tested)
- **Dependencies**: Modern 2024-2025 package versions
- **Optional**: Various external services (databases, AI providers, etc.)

## Status

✅ **Production Ready** - All 68 tests passing (100% success rate)  
✅ **Mathematical Laws Verified** - Monad, functor, and monoid laws tested  
✅ **Performance Benchmarks Passed** - High-performance implementation  
✅ **Comprehensive Test Coverage** - Unit, integration, property-based, and performance tests  
✅ **Modern Architecture** - Independent submodule with clean separation  

## Recent Updates

- **2025-06-26**: Complete restructuring and test fixes
  - Fixed database transaction management
  - Corrected configuration monoid test semantics  
  - Updated error chaining behavior tests
  - Added comprehensive .gitignore for Python development
  - All 68 tests now passing with full functionality verified

---

**QiCore v4.0** - Mathematical contracts meet modern Python development.