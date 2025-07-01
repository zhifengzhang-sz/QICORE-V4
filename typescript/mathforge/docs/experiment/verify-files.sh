#!/bin/bash

# Verify all referenced files exist for the experiment

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MISSING_FILES=()

echo "🔍 Verifying experiment files..."

# Check inputs directory files
INPUT_FILES=(
    "qi.v4.component.contracts.md"
    "patterns.md"
    "component-implementation.md"
    "testing-strategy.md"
    "integration-patterns.md"
    "function-signatures.md"
    "usage-examples.md"
    "performance-specifications.md"
)

echo "📁 Checking inputs/ directory..."
for file in "${INPUT_FILES[@]}"; do
    if [ -f "${SCRIPT_DIR}/inputs/${file}" ]; then
        echo "  ✅ inputs/${file}"
    else
        echo "  ❌ inputs/${file} - MISSING"
        MISSING_FILES+=("inputs/${file}")
    fi
done

# Check approach-specific files
echo "📁 Checking approach1/ directory..."
if [ -f "${SCRIPT_DIR}/approach1/instruction.md" ]; then
    echo "  ✅ approach1/instruction.md"
else
    echo "  ❌ approach1/instruction.md - MISSING"
    MISSING_FILES+=("approach1/instruction.md")
fi

# approach1 now reads from shared inputs directory - no local files needed

echo "📁 Checking approach2/ directory..."
if [ -f "${SCRIPT_DIR}/approach2/instruction.yaml" ]; then
    echo "  ✅ approach2/instruction.yaml"
else
    echo "  ❌ approach2/instruction.yaml - MISSING"
    MISSING_FILES+=("approach2/instruction.yaml")
fi

echo "📁 Checking approach3/ directory..."
if [ -f "${SCRIPT_DIR}/approach3/instruction.yaml" ]; then
    echo "  ✅ approach3/instruction.yaml"
else
    echo "  ❌ approach3/instruction.yaml - MISSING"
    MISSING_FILES+=("approach3/instruction.yaml")
fi

if [ -f "${SCRIPT_DIR}/approach3/property-specifications.md" ]; then
    echo "  ✅ approach3/property-specifications.md"
else
    echo "  ❌ approach3/property-specifications.md - MISSING"
    MISSING_FILES+=("approach3/property-specifications.md")
fi

# Report results
echo ""
if [ ${#MISSING_FILES[@]} -eq 0 ]; then
    echo "✅ All experiment files are present!"
    echo ""
    echo "📊 File Summary:"
    echo "  - ${#INPUT_FILES[@]} shared input files"
    echo "  - 3 approach instruction files"
    echo "  - 1 property specification (approach3)"
    echo ""
    echo "🚀 Experiment is ready to run!"
else
    echo "❌ Missing ${#MISSING_FILES[@]} files:"
    for file in "${MISSING_FILES[@]}"; do
        echo "  - ${file}"
    done
    echo ""
    echo "Please create the missing files before running experiments."
    exit 1
fi 