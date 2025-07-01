#!/bin/bash

# Reset Claude Code Experiment Project to Clean State
# Usage: ./reset-project.sh

set -e

EXPERIMENT_DIR="/home/zzhang/dev/qi/github/mcp-server/qicore-v4/claude-code-experiment"
cd "$EXPERIMENT_DIR"

echo "🧹 Resetting Claude Code experiment project..."

# Remove implementation files
echo "🗑️  Removing implementation files..."
REMOVE_ITEMS=(
    "src/"
    "tests/"
    "dist/"
    "coverage/"
    ".nyc_output/"
    "bun.lock"
)

for item in "${REMOVE_ITEMS[@]}"; do
    if [ -e "$item" ]; then
        rm -rf "$item"
        echo "   ✅ Removed $item"
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

# Create vitest config
echo "🧪 Creating vitest.config.ts..."
cat > vitest.config.ts << 'EOF'
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'coverage/**',
        'dist/**',
        'ex*/**',
        '**/*.d.ts',
        '**/*.config.*',
        '**/node_modules/**'
      ]
    }
  }
});
EOF

echo ""
echo "✅ Project reset complete!"
echo ""
echo "🚀 Ready for next Claude Code experiment!"
echo ""
echo "💡 Next steps:"
echo "   1. Dependencies already installed (shared node_modules)"
echo "   2. Start Claude Code agent with task:"
echo "      - Implement QiCore v4.0 TypeScript components"
echo "      - Follow docs/qi/core/ specifications"
echo "      - Create comprehensive tests for ALL 6 components"
echo "   3. After completion, run: ./archive-experiment.sh"
echo ""
EOF