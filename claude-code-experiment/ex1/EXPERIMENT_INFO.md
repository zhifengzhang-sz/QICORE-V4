# Experiment: ex1

**Date**: 2025-06-29 22:05:24
**Archive Script**: archive-experiment.sh
**Working Directory**: /home/zzhang/dev/qi/github/mcp-server/qicore-v4/claude-code-experiment

## Archived Contents

### Implementation Files
- src/ - Source code implementation
- tests/ - Test files

### Project Configuration
- package.json
- package-lock.json
- tsconfig.json
- bun.lock

### Generated Files
- node_modules/

## Analysis Commands

```bash
# Navigate to experiment
cd ex1

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
