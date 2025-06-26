# QiCore v4.0 Environment Management

This directory contains scripts for managing the QiCore v4.0 development environment with mathematical contract support.

## 🚀 Quick Start

```bash
# Complete setup (auto-detects location)
./env/setup.sh

# Activate environment for development
source env/activate.sh

# Verify environment is working
./env/verify.sh
```

## 📁 Files

| File | Purpose | Usage |
|------|---------|--------|
| `setup.sh` | Complete environment setup | `./env/setup.sh [china\|global]` |
| `activate.sh` | Environment activation | `source env/activate.sh` |
| `mirrors.sh` | Mirror management | `source env/mirrors.sh [china\|global\|show\|test]` |
| `verify.sh` | Environment verification | `./env/verify.sh [--comprehensive\|--contracts\|--performance]` |
| `README.md` | This documentation | - |

## 🔧 Setup Script (`setup.sh`)

Complete development environment setup with mathematical contract support.

### Features
- ✅ Python 3.11+ environment creation
- ✅ Mathematical dependency installation
- ✅ Mirror optimization (China/Global)
- ✅ Contract verification tools
- ✅ Performance benchmark setup

### Usage
```bash
./env/setup.sh           # Auto-detect location
./env/setup.sh china     # Force China mirrors
./env/setup.sh global    # Force global mirrors
./env/setup.sh --help    # Show help
```

### Dependencies Installed
- **Mathematical Foundation**: returns, cytoolz, pydantic
- **Web Framework**: FastAPI, httpx, uvicorn
- **Development Tools**: pytest, mypy, ruff, black
- **Specialized**: redis, aiosqlite, jinja2, click

## 🔄 Mirror Management (`mirrors.sh`)

Session-scoped mirror configuration for optimal download speeds.

### Commands
```bash
source env/mirrors.sh china    # China mirrors (Tsinghua, Douban)
source env/mirrors.sh global   # Global mirrors (PyPI)
source env/mirrors.sh show     # Show current configuration
source env/mirrors.sh test     # Test mirror connectivity
source env/mirrors.sh auto     # Auto-detect optimal mirrors
```

### Mirror Configurations

#### China Mirrors
- Primary: `https://pypi.tuna.tsinghua.edu.cn/simple/`
- Secondary: `https://pypi.douban.com/simple/`
- Backup: `https://mirrors.aliyun.com/pypi/simple/`

#### Global Mirrors
- Primary: `https://pypi.org/simple/`
- Secondary: `https://pypi.python.org/simple/`

## 🏃‍♂️ Environment Activation (`activate.sh`)

Activate the QiCore v4.0 development environment.

### Features
- ✅ Virtual environment activation
- ✅ Environment status display
- ✅ Available command overview
- ✅ Current configuration summary

### Usage
```bash
source env/activate.sh
```

### Post-Activation Commands
```bash
python verify_contracts.py     # Verify mathematical contracts
pytest tests/ -v              # Run tests (when available)
uv add package-name           # Add new dependencies
source env/mirrors.sh china   # Switch to China mirrors
```

## 🔬 Verification Script (`verify.sh`)

Comprehensive verification of the development environment and mathematical contracts.

### Verification Areas
- **Environment**: Python version, uv, virtual environment
- **Packages**: All dependency imports and integration
- **Contracts**: Monad, monoid, and functor laws
- **Performance**: Interpreted tier compliance (100× baseline)

### Usage
```bash
./env/verify.sh                    # Quick verification
./env/verify.sh --comprehensive    # Full verification suite
./env/verify.sh --contracts        # Mathematical contracts only
./env/verify.sh --performance      # Performance targets only
./env/verify.sh --packages         # Package integration only
```

### Performance Targets
- **Result<T> operations**: < 100μs (interpreted tier)
- **Configuration validation**: < 10ms
- **Logger level check**: < 10ns (same across all tiers)

## 🔍 Mathematical Contract Verification

### Monad Laws (Result<T>)
```python
# Left Identity: return(a).flatMap(f) ≡ f(a)
# Right Identity: m.flatMap(return) ≡ m
# Associativity: (m.flatMap(f)).flatMap(g) ≡ m.flatMap(λx. f(x).flatMap(g))
```

### Monoid Laws (Configuration)
```python
# Identity: config ⊕ ∅ = config = ∅ ⊕ config
# Associativity: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
```

### Functor Laws (Components)
```python
# Identity: map(id) ≡ id
# Composition: map(f).map(g) ≡ map(compose(g, f))
```

## 🛠️ Development Workflow

### Initial Setup
```bash
# 1. Clone repository
git clone <repository-url>
cd qicore-v4

# 2. Setup environment
./env/setup.sh

# 3. Verify setup
./env/verify.sh
```

### Daily Development
```bash
# 1. Activate environment
source env/activate.sh

# 2. Verify contracts (if changed)
python verify_contracts.py

# 3. Develop...

# 4. Test changes
pytest tests/ -v

# 5. Check types and format
mypy src/
black src/ && ruff check src/
```

### Adding Dependencies
```bash
# Activate environment
source env/activate.sh

# Add new package
uv add package-name

# Update contracts verification if needed
# Run verification
./env/verify.sh --packages
```

### Mirror Switching
```bash
# Check current mirrors
source env/mirrors.sh show

# Test connectivity
source env/mirrors.sh test

# Switch if needed
source env/mirrors.sh china   # or global

# Re-sync packages
uv sync
```

## 🌍 Location-Specific Optimizations

### China Users
- **Mirrors**: Tsinghua, Douban, Aliyun
- **Setup**: `./env/setup.sh china`
- **Benefits**: 5-10x faster downloads
- **Fallback**: Auto-switches to global if China mirrors fail

### Global Users
- **Mirrors**: PyPI, Python.org
- **Setup**: `./env/setup.sh global` or `./env/setup.sh`
- **Benefits**: Standard, reliable access
- **Coverage**: Worldwide availability

### Auto-Detection
- **Logic**: Tests connectivity and speed to both mirror sets
- **Selection**: Chooses faster option automatically
- **Fallback**: Defaults to global if detection fails

## 🐛 Troubleshooting

### Common Issues

#### Environment Setup Fails
```bash
# Check prerequisites
python3 --version  # Should be 3.11+
curl --version     # Should be available

# Re-run setup
./env/setup.sh --help
```

#### Package Installation Fails
```bash
# Check mirrors
source env/mirrors.sh test

# Switch mirrors if needed
source env/mirrors.sh china  # or global

# Clear cache and retry
uv cache clean
./env/setup.sh
```

#### Mathematical Contract Failures
```bash
# Run detailed verification
./env/verify.sh --contracts

# Check specific laws
python -c "
from returns.result import Result
# Test monad laws manually...
"
```

#### Performance Issues
```bash
# Check system load
htop

# Run performance verification
./env/verify.sh --performance

# Check Python version optimization
python3 --version  # Should be 3.11+ for best performance
```

### Environment Reset
```bash
# Complete environment reset
deactivate  # If currently in environment
rm -rf .venv
./env/setup.sh
```

### Package Issues
```bash
# Check package integrity
uv pip list
uv pip check

# Reinstall specific package
uv remove package-name
uv add package-name

# Full package reinstall
rm -rf .venv
./env/setup.sh
```

## 📊 Environment Monitoring

### Health Check Commands
```bash
# Quick health check
./env/verify.sh

# Detailed verification
./env/verify.sh --comprehensive

# Contract-specific check
./env/verify.sh --contracts

# Performance monitoring
./env/verify.sh --performance
```

### Resource Monitoring
```bash
# Memory usage
python -c "
import psutil, os
print(f'Memory: {psutil.Process(os.getpid()).memory_info().rss / 1024 / 1024:.1f} MB')
"

# Package sizes
uv pip list | head -20

# Environment size
du -sh .venv/
```

## 🔐 Security Considerations

### Environment Isolation
- ✅ Session-scoped environment variables
- ✅ Virtual environment isolation
- ✅ No global system modifications
- ✅ Clean deactivation

### Mirror Security
- ✅ HTTPS-only mirrors
- ✅ Trusted mirror sources
- ✅ Fallback mechanisms
- ✅ Connectivity testing

### Package Integrity
- ✅ Locked dependency versions (uv.lock)
- ✅ Cryptographic hash verification
- ✅ Trusted package sources
- ✅ Mathematical contract validation

---

**QiCore v4.0 environment management provides a secure, reproducible, and mathematically sound development experience!**