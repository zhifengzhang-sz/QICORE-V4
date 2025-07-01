#!/bin/bash

# MathForge Experiment Runner
# Sets up environment variables and runs experiments

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXPERIMENT_BASE="${SCRIPT_DIR}/results"

# Create results directory if it doesn't exist
mkdir -p "$EXPERIMENT_BASE"

# Function to run an approach
run_approach() {
    local approach=$1
    local output_dir="${EXPERIMENT_BASE}/${approach}"
    
    echo "🚀 Running Approach ${approach^^}"
    echo "📁 Output directory: $output_dir"
    
    # Set environment variable
    export EXPERIMENT_OUTPUT="$output_dir"
    
    # Create output directory
    mkdir -p "$output_dir"
    
    # Change to approach directory
    cd "${SCRIPT_DIR}/${approach}"
    
    echo "📋 Instruction file: $(pwd)/instruction.*"
    echo "🎯 Output will be created in: $EXPERIMENT_OUTPUT"
    echo ""
    echo "To run multiple experiments manually:"
    echo "  cd $(pwd)"
    echo "  # Run Claude with instruction file (creates ./output)"
    echo "  # mv output e1"
    echo "  # Run Claude again (creates ./output)"  
    echo "  # mv output e2"
    echo "  # Run Claude again (creates ./output)"
    echo "  # mv output e3"
    echo ""
    
    # Optional: Auto-open instruction file
    if command -v code >/dev/null 2>&1; then
        echo "💡 Opening instruction file in VS Code..."
        code instruction.* 2>/dev/null || true
    fi
}

# Function to show usage
show_usage() {
    echo "Usage: $0 [approach1|approach2|approach3|all]"
    echo ""
    echo "Examples:"
    echo "  $0 approach1    # Set up for Approach 1 (Natural Language)"
    echo "  $0 approach2    # Set up for Approach 2 (Structured Guidance)"
    echo "  $0 approach3    # Set up for Approach 3 (Formal Verification)"
    echo "  $0 all          # Set up all approaches"
    echo ""
    echo "Environment Variables:"
    echo "  EXPERIMENT_OUTPUT - Directory where generated code will be placed"
    echo "                      Default: ./output (then mv output e1, mv output e2, etc.)"
    echo ""
    echo "Consistency Testing:"
    echo "  Run each approach multiple times (e1, e2, e3...) to test consistency"
    echo "  Compare results within each approach to measure consistency improvements"
}

# Main execution
case "${1:-}" in
    approach1)
        run_approach "approach1"
        ;;
    approach2)
        run_approach "approach2"
        ;;
    approach3)
        run_approach "approach3"
        ;;
    all)
        run_approach "approach1"
        echo "----------------------------------------"
        run_approach "approach2"
        echo "----------------------------------------"
        run_approach "approach3"
        ;;
    ""|help|--help|-h)
        show_usage
        ;;
    *)
        echo "❌ Unknown approach: $1"
        echo ""
        show_usage
        exit 1
        ;;
esac

echo "✅ Experiment setup complete!"
echo ""
echo "📊 Results will be stored in:"
echo "   - Approach 1: ${EXPERIMENT_BASE}/approach1/"
echo "   - Approach 2: ${EXPERIMENT_BASE}/approach2/"
echo "   - Approach 3: ${EXPERIMENT_BASE}/approach3/" 