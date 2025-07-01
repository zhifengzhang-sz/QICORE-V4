#!/bin/bash

# Archive Claude Code Experiment Results
# Usage: ./archive-experiment.sh [experiment_name]
# If no name provided, uses ex1, ex2, ex3, etc.

set -e

EXPERIMENT_DIR="/home/zzhang/dev/qi/github/mcp-server/qicore-v4/claude-code-experiment"
cd "$EXPERIMENT_DIR"

# Determine experiment name
if [ -n "$1" ]; then
    EXPERIMENT_NAME="$1"
else
    # Find next available ex number
    COUNTER=1
    while [ -d "ex$COUNTER" ]; do
        COUNTER=$((COUNTER + 1))
    done
    EXPERIMENT_NAME="ex$COUNTER"
fi

ARCHIVE_DIR="$EXPERIMENT_NAME"

echo "🗂️  Archiving experiment results to: $ARCHIVE_DIR"

# Check if src/ exists (has results to archive)
if [ ! -d "src" ]; then
    echo "❌ No src/ directory found. Nothing to archive."
    exit 1
fi

# Create archive directory
mkdir -p "$ARCHIVE_DIR"

# Archive implementation files
echo "📁 Moving implementation files..."
if [ -d "src" ]; then
    mv src "$ARCHIVE_DIR/"
    echo "   ✅ src/ → $ARCHIVE_DIR/src/"
fi

if [ -d "tests" ]; then
    mv tests "$ARCHIVE_DIR/"
    echo "   ✅ tests/ → $ARCHIVE_DIR/tests/"
fi

# Archive project configuration files
echo "📋 Moving project files..."
PROJECT_FILES=(
    "tsconfig.json"
    "vitest.config.ts"
    "vitest.config.js"
    ".gitignore"
    "README.md"
)

for file in "${PROJECT_FILES[@]}"; do
    if [ -f "$file" ]; then
        mv "$file" "$ARCHIVE_DIR/"
        echo "   ✅ $file → $ARCHIVE_DIR/$file"
    fi
done

# Archive generated directories
echo "🔨 Moving generated files..."
GENERATED_DIRS=(
    "dist"
    "coverage" 
    ".nyc_output"
)

for dir in "${GENERATED_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        mv "$dir" "$ARCHIVE_DIR/"
        echo "   ✅ $dir/ → $ARCHIVE_DIR/$dir/"
    fi
done

# Create experiment metadata
echo "📊 Creating experiment metadata..."
cat > "$ARCHIVE_DIR/EXPERIMENT_INFO.md" << EOF
# Experiment: $EXPERIMENT_NAME

**Date**: $(date '+%Y-%m-%d %H:%M:%S')
**Archive Script**: archive-experiment.sh
**Working Directory**: $EXPERIMENT_DIR

## Archived Contents

### Implementation Files
- src/ - Source code implementation
- tests/ - Test files

### Project Configuration
$(for file in "${PROJECT_FILES[@]}"; do
    if [ -f "$ARCHIVE_DIR/$file" ]; then
        echo "- $file"
    fi
done)

### Generated Files
$(for dir in "${GENERATED_DIRS[@]}"; do
    if [ -d "$ARCHIVE_DIR/$dir" ]; then
        echo "- $dir/"
    fi
done)

## Analysis Commands

\`\`\`bash
# Navigate to experiment
cd $EXPERIMENT_NAME

# Check test coverage
npm test -- --coverage

# Run performance benchmarks  
npm run test:performance

# Count test files
find tests -name "*.test.ts" | wc -l

# Count source files
find src -name "*.ts" | wc -l

# Check linting
npm run lint
\`\`\`

## Quick Stats

\`\`\`bash
# Generate stats for this experiment
echo "Source files: \$(find src -name '*.ts' | wc -l)"
echo "Test files: \$(find tests -name '*.test.ts' | wc -l)"
echo "Total lines: \$(find src tests -name '*.ts' | xargs wc -l | tail -1)"
\`\`\`
EOF

# Reset project to clean state
echo "🧹 Resetting project to clean state..."

# Keep only essential template files
KEEP_FILES=(
    "docs/"
    "EXPERIMENT_TASK.md" 
    "archive-experiment.sh"
    "reset-project.sh"
    "package.json"
    "package-lock.json"
    "node_modules/"
)

echo "   📂 Keeping essential files:"
for item in "${KEEP_FILES[@]}"; do
    if [ -e "$item" ]; then
        echo "     ✅ $item"
    fi
done

# package.json and node_modules are kept (shared across experiments)

# Create fresh tsconfig.json
echo "⚙️  Creating fresh tsconfig.json..."
cat > tsconfig.json << 'EOF'
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ESNext", 
    "moduleResolution": "bundler",
    "allowSyntheticDefaultImports": true,
    "esModuleInterop": true,
    "allowJs": true,
    "strict": true,
    "forceConsistentCasingInFileNames": true,
    "skipLibCheck": true,
    "declaration": true,
    "outDir": "./dist",
    "types": ["vitest/globals", "node"]
  },
  "include": ["src/**/*", "tests/**/*"],
  "exclude": ["node_modules", "dist", "ex*"]
}
EOF

# Summary
echo ""
echo "✅ Experiment archived successfully!"
echo ""
echo "📊 Experiment Summary:"
echo "   📁 Archive: $ARCHIVE_DIR/"
echo "   📋 Metadata: $ARCHIVE_DIR/EXPERIMENT_INFO.md"
echo "   🧹 Project reset to clean state"
echo ""
echo "🚀 Ready for next experiment!"
echo ""
echo "💡 Next steps:"
echo "   1. Run: npm install"
echo "   2. Start Claude Code agent experiment" 
echo "   3. After completion, run: ./archive-experiment.sh"
echo ""
EOF