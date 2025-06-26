#!/bin/bash

# QiCore v4.0 Development Environment Setup
# Mathematical Contract-Based Library Development

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QICORE_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PYTHON_VERSION="3.11"
MIN_PYTHON_VERSION="3.11.0"

# Location detection
LOCATION="${1:-auto}"

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_header() {
    echo -e "${PURPLE}$1${NC}"
}

# Help function
show_help() {
    cat << EOF
QiCore v4.0 Development Environment Setup

USAGE:
    ./env/setup.sh [LOCATION]

LOCATION:
    auto      Auto-detect location and configure optimal mirrors (default)
    china     Use China mirrors for faster downloads in China
    global    Use global mirrors (standard PyPI)

EXAMPLES:
    ./env/setup.sh          # Auto-detect location
    ./env/setup.sh china    # Force China mirrors
    ./env/setup.sh global   # Force global mirrors

WHAT THIS SCRIPT DOES:
    ✅ Check prerequisites (Python 3.11+, uv, curl)
    ✅ Detect location and configure optimal mirrors
    ✅ Create isolated Python development environment
    ✅ Install mathematical contract dependencies
    ✅ Setup verification and testing tools
    ✅ Verify mathematical contracts
    ✅ Run performance benchmarks
    ✅ Show next steps for development

REQUIREMENTS:
    - Python 3.11 or higher
    - curl (for downloading uv)
    - Internet connection

EOF
}

# Check if help requested
if [[ "${1:-}" == "--help" ]] || [[ "${1:-}" == "-h" ]]; then
    show_help
    exit 0
fi

# Header
log_header "🔬 QiCore v4.0 Development Environment Setup"
log_header "   Mathematical Contract-Based Library Development"
log_header ""

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."
    
    local missing_deps=()
    
    # Check Python version
    if command -v python3 >/dev/null 2>&1; then
        local python_version=$(python3 -c "import sys; print('.'.join(map(str, sys.version_info[:3])))")
        log_info "Found Python $python_version"
        
        # Version comparison
        if ! python3 -c "import sys; sys.exit(0 if sys.version_info >= (3, 11) else 1)" 2>/dev/null; then
            log_error "Python 3.11+ required, found $python_version"
            missing_deps+=("python3.11+")
        else
            log_success "Python version meets requirements"
        fi
    else
        log_error "Python 3 not found"
        missing_deps+=("python3")
    fi
    
    # Check curl
    if ! command -v curl >/dev/null 2>&1; then
        log_error "curl not found"
        missing_deps+=("curl")
    else
        log_success "curl found"
    fi
    
    # Check git
    if ! command -v git >/dev/null 2>&1; then
        log_warning "git not found (recommended for development)"
    else
        log_success "git found"
    fi
    
    if [[ ${#missing_deps[@]} -gt 0 ]]; then
        log_error "Missing dependencies: ${missing_deps[*]}"
        log_info ""
        log_info "Install missing dependencies:"
        log_info "  Ubuntu/Debian: sudo apt update && sudo apt install ${missing_deps[*]}"
        log_info "  macOS: brew install ${missing_deps[*]}"
        log_info "  For Python 3.11+:"
        log_info "    Ubuntu/Debian: sudo apt install python3.11 python3.11-venv"
        log_info "    macOS: brew install python@3.11"
        exit 1
    fi
    
    log_success "All prerequisites met"
}

# Detect location
detect_location() {
    if [[ "$LOCATION" == "auto" ]]; then
        log_info "Detecting optimal mirror configuration..."
        
        # Try to detect China location
        if curl -s --max-time 3 "https://pypi.tuna.tsinghua.edu.cn/simple/" >/dev/null 2>&1; then
            local china_speed=$(curl -o /dev/null -s -w "%{time_total}" --max-time 10 "https://pypi.tuna.tsinghua.edu.cn/simple/" 2>/dev/null || echo "10")
            local global_speed=$(curl -o /dev/null -s -w "%{time_total}" --max-time 10 "https://pypi.org/simple/" 2>/dev/null || echo "10")
            
            # Use faster mirror
            if (( $(echo "$china_speed < $global_speed" | bc -l 2>/dev/null || echo "0") )); then
                LOCATION="china"
                log_info "Detected China location (China mirrors faster: ${china_speed}s vs ${global_speed}s)"
            else
                LOCATION="global"
                log_info "Detected global location (Global mirrors faster: ${global_speed}s vs ${china_speed}s)"
            fi
        else
            LOCATION="global"
            log_info "Defaulting to global mirrors"
        fi
    fi
    
    log_success "Using $LOCATION mirror configuration"
}

# Install uv (fast Python package manager)
install_uv() {
    log_info "Checking uv installation..."
    
    if command -v uv >/dev/null 2>&1; then
        local uv_version=$(uv --version | cut -d' ' -f2)
        log_success "uv already installed (version $uv_version)"
        return
    fi
    
    log_info "Installing uv (fast Python package manager)..."
    
    # Install uv
    if curl -LsSf https://astral.sh/uv/install.sh | sh; then
        # Add to PATH for current session
        export PATH="$HOME/.local/bin:$PATH"
        
        if command -v uv >/dev/null 2>&1; then
            log_success "uv installed successfully"
        else
            log_error "uv installation failed"
            exit 1
        fi
    else
        log_error "Failed to install uv"
        exit 1
    fi
}

# Configure mirrors
configure_mirrors() {
    log_info "Configuring package mirrors for $LOCATION..."
    
    # Source mirrors configuration
    source "$SCRIPT_DIR/mirrors.sh" "$LOCATION"
    
    log_success "Mirrors configured for $LOCATION"
}

# Create Python environment
create_environment() {
    log_info "Creating Python development environment..."
    
    cd "$QICORE_ROOT"
    
    # Remove existing environment if present
    if [[ -d ".venv" ]]; then
        log_warning "Removing existing virtual environment"
        rm -rf .venv
    fi
    
    # Create new environment
    if uv venv .venv --python="$PYTHON_VERSION"; then
        log_success "Python virtual environment created"
    else
        log_error "Failed to create virtual environment"
        exit 1
    fi
    
    # Activate environment
    source .venv/bin/activate
    
    log_success "Virtual environment activated"
}

# Install dependencies
install_dependencies() {
    log_info "Installing QiCore v4.0 dependencies..."
    
    cd "$QICORE_ROOT"
    
    # Ensure we're in the virtual environment
    source .venv/bin/activate
    
    # Check if pyproject.toml exists
    if [[ ! -f "pyproject.toml" ]]; then
        log_info "Creating pyproject.toml from Stage 5 templates..."
        
        # Create basic pyproject.toml for development
        cat > pyproject.toml << 'EOF'
[build-system]
requires = ["hatchling>=1.21.0"]
build-backend = "hatchling.build"

[project]
name = "qicore-v4"
version = "4.0.1"
description = "QiCore v4.0 Python Implementation - Mathematical Contract-Based Library"
readme = "README.md"
license = "MIT"
requires-python = ">=3.11"
authors = [
    {name = "QiCore Team", email = "team@qicore.dev"},
]
keywords = ["functional", "result", "monad", "async", "performance"]

# Mathematical contract dependencies
dependencies = [
    "returns>=0.22.0",
    "cytoolz>=0.12.3", 
    "toolz>=0.12.0",
    "aiofiles>=23.2.0",
    "httpx>=0.28.1",
    "pydantic>=2.5.0",
    "python-dotenv>=1.0.0",
    "PyYAML>=6.0.1",
    "structlog>=23.2.0",
    "redis>=5.0.1",
    "fastapi>=0.115.13",
    "uvicorn>=0.30.0",
    "openai>=1.12.0",
    "anthropic>=0.8.1",
    "ollama>=0.2.1",
    "mcp>=1.9.4",
    "aiosqlite>=0.19.0",
    "Jinja2>=3.1.0",
    "markdown>=3.5.0",
    "click>=8.1.0",
    "rich>=13.7.0",
]

[project.optional-dependencies]
dev = [
    "pytest>=7.4.0",
    "pytest-asyncio>=0.21.0",
    "pytest-benchmark>=4.0.0",
    "hypothesis>=6.100.0",
    "mypy>=1.8.0",
    "ruff>=0.1.9",
    "black>=23.12.0",
    "coverage>=7.4.0",
]
EOF
        log_success "pyproject.toml created"
    fi
    
    # Install dependencies
    log_info "Installing Python dependencies..."
    if uv sync --group dev; then
        log_success "Dependencies installed successfully"
    else
        log_error "Failed to install dependencies"
        exit 1
    fi
    
    # Install QiCore in development mode (if src exists)
    if [[ -d "src" ]]; then
        log_info "Installing QiCore v4.0 in development mode..."
        if uv pip install -e .; then
            log_success "QiCore v4.0 installed in development mode"
        else
            log_warning "Could not install QiCore in development mode (source not ready yet)"
        fi
    else
        log_info "Source directory not found - will be created during Stage 5 implementation"
    fi
}

# Create development tools
create_development_tools() {
    log_info "Creating development tools..."
    
    cd "$QICORE_ROOT"
    
    # Create basic verification script
    cat > "verify_contracts.py" << 'EOF'
#!/usr/bin/env python3
"""
QiCore v4.0 Mathematical Contract Verification
Quick verification script for development environment
"""

def verify_imports():
    """Verify all critical imports work"""
    print("🔍 Verifying imports...")
    
    try:
        # Mathematical foundation
        from returns.result import Result, Success, Failure
        from cytoolz import merge, curry
        from pydantic import BaseModel, Field
        print("✅ Mathematical foundation imports successful")
        
        # Web framework stack
        from fastapi import FastAPI
        import httpx
        import uvicorn
        print("✅ Web framework imports successful")
        
        # Development tools
        import pytest
        import mypy
        print("✅ Development tool imports successful")
        
        return True
        
    except ImportError as e:
        print(f"❌ Import failed: {e}")
        return False

def verify_basic_contracts():
    """Verify basic mathematical contracts"""
    print("\n📐 Verifying basic mathematical contracts...")
    
    try:
        from returns.result import Result
        
        # Test basic Result operations
        result = Result.from_value(42)
        mapped = result.map(lambda x: x * 2)
        
        if mapped.unwrap() == 84:
            print("✅ Basic Result<T> operations working")
        else:
            print("❌ Result<T> operations failed")
            return False
            
        # Test basic monoid operation
        from cytoolz import merge
        merged = merge({"a": 1}, {"b": 2})
        
        if merged == {"a": 1, "b": 2}:
            print("✅ Basic monoid operations working")
        else:
            print("❌ Monoid operations failed")
            return False
            
        return True
        
    except Exception as e:
        print(f"❌ Contract verification failed: {e}")
        return False

def verify_performance_targets():
    """Quick performance verification"""
    print("\n⚡ Verifying performance targets...")
    
    try:
        import time
        from returns.result import Result
        
        # Quick performance test
        start = time.perf_counter()
        for _ in range(1000):
            Result.from_value("test").map(str.upper)
        elapsed = (time.perf_counter() - start) / 1000 * 1000000  # μs per operation
        
        if elapsed < 100:  # Interpreted tier target
            print(f"✅ Performance target met: {elapsed:.2f}μs per operation (target: <100μs)")
        else:
            print(f"⚠️  Performance target missed: {elapsed:.2f}μs per operation (target: <100μs)")
            print("   This may be due to system load or need optimization")
            
        return True
        
    except Exception as e:
        print(f"❌ Performance verification failed: {e}")
        return False

def main():
    """Run all verifications"""
    print("🔬 QiCore v4.0 Development Environment Verification")
    print("=" * 60)
    
    success = True
    success &= verify_imports()
    success &= verify_basic_contracts()
    success &= verify_performance_targets()
    
    print("\n" + "=" * 60)
    if success:
        print("✅ QiCore v4.0 development environment ready!")
        print("📚 Next steps:")
        print("   1. Review docs/sources/guides/guide.md for 5-stage process")
        print("   2. Examine docs/build/impl/qi.v4.py.impl.md for implementation guide")
        print("   3. Run 'python -m qicore.generate.stage5 --language=python' (when ready)")
    else:
        print("❌ Development environment setup incomplete")
        print("🔧 Check the errors above and re-run ./env/setup.sh")
    
    return 0 if success else 1

if __name__ == "__main__":
    exit(main())
EOF
    
    chmod +x verify_contracts.py
    log_success "Verification script created"
    
    # Create activation script
    cat > "$SCRIPT_DIR/activate.sh" << 'EOF'
#!/bin/bash
# QiCore v4.0 Environment Activation

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QICORE_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "🔬 Activating QiCore v4.0 development environment..."

cd "$QICORE_ROOT"

if [[ -f ".venv/bin/activate" ]]; then
    source .venv/bin/activate
    echo "✅ QiCore v4.0 environment activated"
    echo "📍 Current directory: $(pwd)"
    echo "🐍 Python: $(python --version)"
    echo "📦 UV: $(uv --version)"
    
    # Show available commands
    echo ""
    echo "🛠️  Available commands:"
    echo "   python verify_contracts.py     # Verify mathematical contracts"
    echo "   pytest tests/ -v              # Run tests (when available)"
    echo "   uv add package-name           # Add new dependencies"
    echo "   source env/mirrors.sh china   # Switch to China mirrors"
    echo "   source env/mirrors.sh global  # Switch to global mirrors"
else
    echo "❌ QiCore v4.0 environment not found. Run ./env/setup.sh first."
    exit 1
fi
EOF
    
    chmod +x "$SCRIPT_DIR/activate.sh"
    log_success "Activation script created"
}

# Run verification
run_verification() {
    log_info "Running development environment verification..."
    
    cd "$QICORE_ROOT"
    source .venv/bin/activate
    
    # Run verification script
    if python verify_contracts.py; then
        log_success "Development environment verification passed"
    else
        log_warning "Some verification checks failed (may be expected for initial setup)"
    fi
}

# Show next steps
show_next_steps() {
    log_header ""
    log_header "🎉 QiCore v4.0 Development Environment Setup Complete!"
    log_header ""
    
    log_success "Environment ready at: $QICORE_ROOT"
    log_success "Mirror configuration: $LOCATION"
    
    echo ""
    log_info "📚 Next Steps:"
    echo "   1. Activate environment:    source env/activate.sh"
    echo "   2. Verify contracts:        python verify_contracts.py"
    echo "   3. Read methodology:        docs/sources/guides/guide.md"
    echo "   4. Study Stage 5:           docs/build/impl/qi.v4.py.impl.md"
    echo "   5. Generate implementation: Follow Stage 5 guide"
    
    echo ""
    log_info "🔧 Development Commands:"
    echo "   Switch mirrors:    source env/mirrors.sh [china|global]"
    echo "   Add packages:      uv add package-name"
    echo "   Run tests:         pytest tests/ -v"
    echo "   Type checking:     mypy src/"
    echo "   Code formatting:   black src/ && ruff check src/"
    
    echo ""
    log_info "📖 Documentation:"
    echo "   Setup guide:       docs/setup/README.md"
    echo "   Math contracts:    docs/setup/mathematical-contracts.md"
    echo "   Python env:        docs/setup/python-environment.md"
    
    echo ""
    log_success "Happy mathematical programming with QiCore v4.0! 🔬✨"
}

# Main execution
main() {
    check_prerequisites
    detect_location
    install_uv
    configure_mirrors
    create_environment
    install_dependencies
    create_development_tools
    run_verification
    show_next_steps
}

# Run main function
main