#!/bin/bash

# QiCore v4.0 Mirror Management
# Session-scoped mirror configuration for optimal download speeds

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Mirror configurations
CHINA_MIRRORS=(
    "https://pypi.tuna.tsinghua.edu.cn/simple/"
    "https://pypi.douban.com/simple/"
    "https://mirrors.aliyun.com/pypi/simple/"
)

GLOBAL_MIRRORS=(
    "https://pypi.org/simple/"
    "https://pypi.python.org/simple/"
)

# Logging functions
log_info() {
    echo -e "${BLUE}[MIRRORS]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[MIRRORS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[MIRRORS]${NC} $1"
}

log_error() {
    echo -e "${RED}[MIRRORS]${NC} $1"
}

# Help function
show_help() {
    cat << EOF
QiCore v4.0 Mirror Management

USAGE:
    source env/mirrors.sh [COMMAND]

COMMANDS:
    china     Configure China mirrors (faster in China)
    global    Configure global mirrors (standard PyPI)
    show      Show current mirror configuration
    test      Test mirror connectivity and speed
    help      Show this help message

EXAMPLES:
    source env/mirrors.sh china    # Switch to China mirrors
    source env/mirrors.sh global   # Switch to global mirrors
    source env/mirrors.sh show     # Show current configuration
    source env/mirrors.sh test     # Test all mirrors

NOTE: This script must be sourced (not executed) to set environment variables
      in the current shell session.

EOF
}

# Configure China mirrors
configure_china_mirrors() {
    log_info "Configuring China mirrors..."
    
    export UV_INDEX_URL="${CHINA_MIRRORS[0]}"
    export UV_EXTRA_INDEX_URL="${CHINA_MIRRORS[1]}"
    
    # Also set pip mirrors as fallback
    export PIP_INDEX_URL="${CHINA_MIRRORS[0]}"
    export PIP_EXTRA_INDEX_URL="${CHINA_MIRRORS[1]}"
    export PIP_TRUSTED_HOST="pypi.tuna.tsinghua.edu.cn pypi.douban.com mirrors.aliyun.com"
    
    log_success "China mirrors configured"
    log_info "Primary: ${CHINA_MIRRORS[0]}"
    log_info "Secondary: ${CHINA_MIRRORS[1]}"
}

# Configure global mirrors
configure_global_mirrors() {
    log_info "Configuring global mirrors..."
    
    export UV_INDEX_URL="${GLOBAL_MIRRORS[0]}"
    unset UV_EXTRA_INDEX_URL 2>/dev/null || true
    
    # Also set pip mirrors as fallback
    export PIP_INDEX_URL="${GLOBAL_MIRRORS[0]}"
    unset PIP_EXTRA_INDEX_URL 2>/dev/null || true
    unset PIP_TRUSTED_HOST 2>/dev/null || true
    
    log_success "Global mirrors configured"
    log_info "Primary: ${GLOBAL_MIRRORS[0]}"
}

# Show current configuration
show_configuration() {
    log_info "Current mirror configuration:"
    echo ""
    
    echo -e "  ${CYAN}UV Configuration:${NC}"
    echo "    UV_INDEX_URL=${UV_INDEX_URL:-"(not set)"}"
    echo "    UV_EXTRA_INDEX_URL=${UV_EXTRA_INDEX_URL:-"(not set)"}"
    
    echo ""
    echo -e "  ${CYAN}Pip Configuration:${NC}"
    echo "    PIP_INDEX_URL=${PIP_INDEX_URL:-"(not set)"}"
    echo "    PIP_EXTRA_INDEX_URL=${PIP_EXTRA_INDEX_URL:-"(not set)"}"
    echo "    PIP_TRUSTED_HOST=${PIP_TRUSTED_HOST:-"(not set)"}"
    
    echo ""
    
    # Detect current configuration type
    if [[ "${UV_INDEX_URL:-}" == *"tuna.tsinghua.edu.cn"* ]]; then
        log_success "Currently using China mirrors"
    elif [[ "${UV_INDEX_URL:-}" == *"pypi.org"* ]]; then
        log_success "Currently using global mirrors"
    else
        log_warning "Mirror configuration unclear or custom"
    fi
}

# Test mirror connectivity and speed
test_mirrors() {
    log_info "Testing mirror connectivity and speed..."
    echo ""
    
    # Test China mirrors
    echo -e "${CYAN}China Mirrors:${NC}"
    for mirror in "${CHINA_MIRRORS[@]}"; do
        echo -n "  Testing $mirror... "
        
        if timeout 10 curl -s --max-time 5 "$mirror" >/dev/null 2>&1; then
            # Measure response time
            local response_time=$(timeout 10 curl -o /dev/null -s -w "%{time_total}" --max-time 5 "$mirror" 2>/dev/null || echo "timeout")
            
            if [[ "$response_time" != "timeout" ]]; then
                echo -e "${GREEN}✅ ${response_time}s${NC}"
            else
                echo -e "${YELLOW}⚠️  timeout${NC}"
            fi
        else
            echo -e "${RED}❌ failed${NC}"
        fi
    done
    
    echo ""
    
    # Test global mirrors
    echo -e "${CYAN}Global Mirrors:${NC}"
    for mirror in "${GLOBAL_MIRRORS[@]}"; do
        echo -n "  Testing $mirror... "
        
        if timeout 10 curl -s --max-time 5 "$mirror" >/dev/null 2>&1; then
            # Measure response time
            local response_time=$(timeout 10 curl -o /dev/null -s -w "%{time_total}" --max-time 5 "$mirror" 2>/dev/null || echo "timeout")
            
            if [[ "$response_time" != "timeout" ]]; then
                echo -e "${GREEN}✅ ${response_time}s${NC}"
            else
                echo -e "${YELLOW}⚠️  timeout${NC}"
            fi
        else
            echo -e "${RED}❌ failed${NC}"
        fi
    done
    
    echo ""
    log_info "Test complete. Use the fastest working mirrors for your location."
}

# Test package installation speed
test_package_speed() {
    log_info "Testing package installation speed..."
    
    if ! command -v uv >/dev/null 2>&1; then
        log_warning "uv not found - cannot test package installation speed"
        return
    fi
    
    # Test with a small package
    local test_package="requests"
    echo ""
    log_info "Testing installation speed with package: $test_package"
    
    # Save current configuration
    local saved_uv_index="${UV_INDEX_URL:-}"
    local saved_uv_extra="${UV_EXTRA_INDEX_URL:-}"
    
    # Test China mirrors
    echo -e "${CYAN}Testing China mirrors:${NC}"
    configure_china_mirrors >/dev/null 2>&1
    
    local start_time=$(date +%s.%N)
    if uv pip install --dry-run "$test_package" >/dev/null 2>&1; then
        local end_time=$(date +%s.%N)
        local china_time=$(echo "$end_time - $start_time" | bc -l 2>/dev/null || echo "unknown")
        echo "  China mirrors: ${china_time}s"
    else
        echo "  China mirrors: failed"
        local china_time="failed"
    fi
    
    # Test global mirrors
    echo -e "${CYAN}Testing global mirrors:${NC}"
    configure_global_mirrors >/dev/null 2>&1
    
    start_time=$(date +%s.%N)
    if uv pip install --dry-run "$test_package" >/dev/null 2>&1; then
        end_time=$(date +%s.%N)
        local global_time=$(echo "$end_time - $start_time" | bc -l 2>/dev/null || echo "unknown")
        echo "  Global mirrors: ${global_time}s"
    else
        echo "  Global mirrors: failed"
        local global_time="failed"
    fi
    
    # Restore original configuration
    if [[ -n "$saved_uv_index" ]]; then
        export UV_INDEX_URL="$saved_uv_index"
    else
        unset UV_INDEX_URL 2>/dev/null || true
    fi
    
    if [[ -n "$saved_uv_extra" ]]; then
        export UV_EXTRA_INDEX_URL="$saved_uv_extra"
    else
        unset UV_EXTRA_INDEX_URL 2>/dev/null || true
    fi
    
    echo ""
    log_info "Package installation speed test complete"
}

# Auto-detect optimal mirrors
auto_detect_mirrors() {
    log_info "Auto-detecting optimal mirrors..."
    
    # Test connectivity to both mirror types
    local china_available=false
    local global_available=false
    local china_speed="10"
    local global_speed="10"
    
    # Test China mirrors
    if timeout 5 curl -s --max-time 3 "${CHINA_MIRRORS[0]}" >/dev/null 2>&1; then
        china_available=true
        china_speed=$(timeout 5 curl -o /dev/null -s -w "%{time_total}" --max-time 3 "${CHINA_MIRRORS[0]}" 2>/dev/null || echo "10")
    fi
    
    # Test global mirrors
    if timeout 5 curl -s --max-time 3 "${GLOBAL_MIRRORS[0]}" >/dev/null 2>&1; then
        global_available=true
        global_speed=$(timeout 5 curl -o /dev/null -s -w "%{time_total}" --max-time 3 "${GLOBAL_MIRRORS[0]}" 2>/dev/null || echo "10")
    fi
    
    # Choose faster option
    if [[ "$china_available" == true ]] && [[ "$global_available" == true ]]; then
        # Compare speeds (use bc if available, otherwise simple comparison)
        if command -v bc >/dev/null 2>&1; then
            if (( $(echo "$china_speed < $global_speed" | bc -l) )); then
                log_info "China mirrors faster ($china_speed s vs $global_speed s)"
                configure_china_mirrors
            else
                log_info "Global mirrors faster ($global_speed s vs $china_speed s)"
                configure_global_mirrors
            fi
        else
            # Fallback: use China if both work (likely in China)
            log_info "Both mirrors available, defaulting to China mirrors"
            configure_china_mirrors
        fi
    elif [[ "$china_available" == true ]]; then
        log_info "Only China mirrors available"
        configure_china_mirrors
    elif [[ "$global_available" == true ]]; then
        log_info "Only global mirrors available"
        configure_global_mirrors
    else
        log_warning "No mirrors available - check internet connection"
        return 1
    fi
}

# Main function
main() {
    local command="${1:-show}"
    
    case "$command" in
        "china")
            configure_china_mirrors
            ;;
        "global")
            configure_global_mirrors
            ;;
        "show")
            show_configuration
            ;;
        "test")
            test_mirrors
            ;;
        "speed")
            test_package_speed
            ;;
        "auto")
            auto_detect_mirrors
            ;;
        "help"|"--help"|"-h")
            show_help
            ;;
        *)
            log_error "Unknown command: $command"
            echo ""
            show_help
            return 1
            ;;
    esac
}

# Check if script is being sourced
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    log_error "This script must be sourced, not executed directly"
    log_info "Usage: source env/mirrors.sh [COMMAND]"
    exit 1
fi

# Run main function with all arguments
main "$@"