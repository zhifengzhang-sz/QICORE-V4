# Experiment: ex12

**Date**: 2025-06-30 07:19:02
**Archive Script**: archive-experiment.sh
**Working Directory**: /home/zzhang/dev/qi/github/mcp-server/qicore-v4/claude-code-experiment

## Archived Contents

### Implementation Files
- src/ - Source code implementation
- tests/ - Test files

### Project Configuration
- tsconfig.json
- vitest.config.ts

### Generated Files
- coverage/

## Analysis Commands

```bash
# Navigate to experiment
cd ex12

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
```

## Quick Stats

```bash
# Generate stats for this experiment
echo "Source files: $(find src -name '*.ts' | wc -l)"
echo "Test files: $(find tests -name '*.test.ts' | wc -l)"
echo "Total lines: $(find src tests -name '*.ts' | xargs wc -l | tail -1)"
```
