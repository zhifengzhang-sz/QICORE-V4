# QiCore v4.0 Development Setup Guide

## 🎯 Quick Start (Recommended)

**For Mathematical Contract-Based Library Development:**

```bash
# Navigate to QiCore v4.0 directory
cd qicore-v4

# One-command setup (auto-detects your location)
./env/setup.sh
```

**That's it!** This gives you:
- ✅ Complete Python development environment
- ✅ Mathematical contract verification tools
- ✅ All dependencies for 13-component architecture
- ✅ Session-scoped environment management
- ✅ Optimized China/Global mirror configuration

## 📖 What You Get

### Core Development Environment
- **Python 3.11+**: With async/await support and modern type hints
- **Mathematical Libraries**: Returns, cytoolz, and functional programming tools
- **Web Framework**: FastAPI with async performance optimization
- **Testing Framework**: pytest with property-based testing for mathematical laws
- **Performance Tools**: Benchmarking for interpreted tier compliance (100× baseline)

### QiCore v4.0 Specific Tools
- **5-Stage Development Process**: Natural Language → Mathematical → Design → Template → Implementation
- **Contract Verification**: Automated verification of monad, monoid, and functor laws
- **Package Integration**: All researched packages (pydantic, httpx, structlog, etc.)
- **Performance Validation**: Tier-specific performance requirement validation

### Production Readiness
- **Type Safety**: Full mypy support with generic types
- **Code Quality**: ruff, black, and comprehensive linting
- **Documentation**: Automatic API documentation generation
- **Distribution**: Ready for PyPI publication

## 🔧 Detailed Setup Options

### For Most Users (Automatic Location Detection)
```bash
cd qicore-v4
./env/setup.sh
```

### China Users (Faster Downloads)
```bash
cd qicore-v4
./env/setup.sh china
```

### Global Users (Standard Mirrors)
```bash
cd qicore-v4
./env/setup.sh global
```

## 📚 Next Steps

After setup completes:

1. **Understand the Architecture**: [5-Stage Development Process](../sources/guides/guide.md)
2. **Review the Contracts**: [Mathematical Foundations](../sources/guides/common.md)
3. **Study the Implementation**: [Python Stage 5](../build/impl/qi.v4.py.impl.md)
4. **Generate Code**: Follow the implementation guide to create working Python library

## 🏗️ Development Workflows

### Mathematical Contract Development
```bash
# Enter development environment
./env/activate.sh

# Verify mathematical contracts
python -m qicore.verification.contracts

# Run property-based tests
pytest tests/ -v --property-based

# Benchmark performance (interpreted tier)
pytest tests/benchmarks/ --benchmark-only
```

### Package Research and Integration
```bash
# Research new packages for components
python -m qicore.research.packages --component=cache --language=python

# Verify package integration
python -m qicore.verification.packages

# Update Stage 4 package selections
python -m qicore.update.packages
```

### Implementation Generation
```bash
# Generate Stage 5 Python implementation
python -m qicore.generate.stage5 --language=python

# Verify implementation completeness
python -m qicore.verification.implementation --language=python

# Generate source code from templates
python -m qicore.generate.source --from=stage5 --target=src/
```

## 🆘 Troubleshooting

### Common Issues
- **Missing Prerequisites**: The setup script will check and guide you through installing required tools (Python 3.11+, uv, curl)
- **Network Issues**: Use location-specific setup (`./env/setup.sh china` or `./env/setup.sh global`)
- **Permission Issues**: Ensure you have write access to the qicore-v4 directory
- **Mathematical Verification Failures**: Check [Contract Verification Guide](verification-guide.md)

### Mathematical Contract Issues
- **Monad Law Failures**: See [Monad Implementation Guide](mathematical-contracts.md#monad-laws)
- **Performance Target Misses**: Check [Performance Optimization Guide](performance-optimization.md)
- **Package Integration Conflicts**: Review [Package Selection Methodology](../sources/guides/package-research-methodology.md)

### Development Environment Issues
- **Python Version Conflicts**: QiCore v4.0 requires Python 3.11+ for optimal type hints
- **Package Installation Failures**: Try switching mirrors with `./env/mirrors.sh china` or `./env/mirrors.sh global`
- **Mathematical Library Issues**: Verify returns library version >= 0.22.0

### Getting Help
- **Environment Issues**: Check [Python Development Environment](python-environment.md)
- **Mathematical Questions**: See [Mathematical Foundations](../sources/guides/common.md)
- **Stage Process Issues**: Review [5-Stage Development Guide](../sources/guides/guide.md)
- **Implementation Questions**: Study [Python Implementation Guide](../build/impl/qi.v4.py.impl.md)
- **Technical Details**: Browse [QiCore v4.0 Documentation](../README.md)

## 🚀 Success Indicators

After successful setup, you should be able to:
- ✅ Run mathematical contract verification tests
- ✅ Generate Stage 5 Python implementation from templates
- ✅ Execute property-based tests for monad/monoid laws
- ✅ Benchmark performance against interpreted tier targets
- ✅ Switch between China/Global mirrors seamlessly
- ✅ Access complete 13-component architecture documentation

## 🔬 Verification Commands

### Quick Health Check
```bash
# Verify entire development environment
./env/verify.sh

# Check mathematical contract compliance
python -m qicore.verify --contracts

# Validate performance targets
python -m qicore.verify --performance

# Test package integration
python -m qicore.verify --packages
```

### Deep Verification
```bash
# Run complete verification suite
./env/verify.sh --comprehensive

# Generate verification report
python -m qicore.verification.report --output=docs/reports/

# Verify against formal specification
python -m qicore.verify --formal-spec
```

---

**Welcome to QiCore v4.0 Development!**  
*Building mathematical contract-based libraries with 5-stage systematic transformation*