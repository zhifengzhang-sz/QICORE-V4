# Experiment Runner Guide

> **Purpose**: Practical instructions for conducting the empirical study  
> **Audience**: Researchers running the AI code generation comparison  
> **Goal**: Consistent, reproducible experimental methodology  

## Experiment Setup

### Prerequisites

**Environment Requirements**:
- Node.js 18+ with TypeScript support
- Access to AI model (Claude, GPT-4, or local LLM)
- Git for version control of generated code
- Testing framework (Vitest) for verification
- Property-based testing library (fast-check)

**Directory Structure**:
```
experiments/
├── approach1/
│   ├── results/
│   │   ├── run1/
│   │   ├── run2/
│   │   └── ...
│   └── logs/
├── approach2/
│   ├── results/
│   └── logs/
├── approach3/
│   ├── results/
│   └── logs/
└── analysis/
    ├── metrics/
    ├── comparisons/
    └── reports/
```

## Experimental Protocol

### Phase 1: Approach 1 Experiments

**For Each Component (Result, Logger, Cache, Config)**:

1. **Setup Session**:
   ```bash
   mkdir -p experiments/approach1/results/result-run1
   cd experiments/approach1/results/result-run1
   ```

2. **AI Prompt Template**:
   ```
   I need you to implement a [COMPONENT] component in TypeScript based on this specification:
   
   [PASTE SPECIFICATION FROM approach1/[component]-spec.md]
   
   Please provide a complete implementation with:
   - TypeScript interfaces and types
   - Implementation code
   - Basic usage examples
   - Any necessary dependencies in package.json
   
   Focus on making it work correctly and be usable.
   ```

3. **Data Collection**:
   - Save AI response as `implementation.ts`
   - Save conversation log as `conversation.md`
   - Record packages chosen as `packages.json`
   - Note any questions AI asked as `ai-questions.md`
   - Time the generation process

4. **Repeat 5 Times**:
   - Use fresh AI session for each run
   - Same specification, no additional guidance
   - Save each run in separate directory

### Phase 2: Approach 2 Experiments

**Enhanced Prompt Template**:
```
I need you to implement a [COMPONENT] component following these comprehensive guidelines:

SPECIFICATION:
[PASTE FROM approach2/[component]-guided-spec.md]

DESIGN PATTERNS:
[PASTE FROM approach2/design-patterns.md]

PACKAGE RECOMMENDATIONS:
[PASTE FROM approach2/package-recommendations.md]

Please provide:
1. Implementation following the specified patterns
2. Justification for package choices
3. MAX-MIN analysis (what you used packages for vs. custom implementation)
4. Integration plan with other QiCore components
5. Quality checklist verification

Focus on following the guidance while producing high-quality, consistent code.
```

**Additional Data Collection**:
- Save guidance adherence analysis as `adherence-analysis.md`
- Record MAX-MIN ratio as `max-min-metrics.json`
- Document package justifications as `package-rationale.md`

### Phase 3: Approach 3 Experiments

**Verification-Enhanced Process**:

1. **Initial Generation** (same as Approach 2)
2. **Verification Run**:
   ```bash
   npm test -- --run verification-suite
   ```
3. **Feedback Loop**:
   ```
   Your implementation failed these verification checks:
   
   [PASTE VERIFICATION FAILURES]
   
   Please refine your implementation to pass all verification tests.
   Explain your changes and how they address the failures.
   ```
4. **Iteration Until Success**
5. **Document Refinement Process**

**Verification-Specific Data**:
- Save verification results as `verification-results.json`
- Record refinement cycles as `refinement-log.md`
- Document final implementation as `final-implementation.ts`

## Data Analysis Framework

### Quantitative Metrics Collection

**Consistency Scoring**:
```typescript
interface ConsistencyMetrics {
  componentName: string;
  approach: 1 | 2 | 3;
  runs: Array<{
    runId: string;
    linesOfCode: number;
    packagesUsed: string[];
    customCodeRatio: number;
    testCoverage: number;
    codeQualityScore: number;
  }>;
  consistencyScore: number; // 0-100
  averageMetrics: {
    loc: number;
    packageCount: number;
    customRatio: number;
    quality: number;
  };
}
```

**Package Usage Analysis**:
```typescript
interface PackageUsageMetrics {
  approach: 1 | 2 | 3;
  totalPackagesUsed: Set<string>;
  commonPackages: string[]; // Used in >50% of runs
  packageConsistency: number; // 0-100
  maxMinCompliance: number; // 0-100 (approaches 2&3 only)
}
```

### Qualitative Analysis Framework

**Code Quality Assessment**:
- **Readability**: Clear variable names, good structure
- **Maintainability**: Separation of concerns, modularity
- **Best Practices**: TypeScript usage, error handling
- **Architecture**: Pattern adherence, design principles

**AI Behavior Analysis**:
- **Decision Making**: How AI chose between options
- **Guidance Following**: Adherence to specifications
- **Learning**: Improvement across refinement cycles
- **Reasoning**: Quality of explanations and justifications

## Automated Analysis Tools

### Metrics Collection Script

```typescript
// scripts/collect-metrics.ts
interface ExperimentResults {
  approach: number;
  component: string;
  runs: RunMetrics[];
}

function analyzeConsistency(runs: RunMetrics[]): number {
  // Calculate similarity between implementations
  // Return consistency score 0-100
}

function calculateMaxMinRatio(code: string, packages: string[]): number {
  // Analyze package usage vs custom implementation
  // Return ratio of package code to total code
}
```

### Comparison Dashboard

```typescript
// Generate comparison reports
function generateComparisonReport(
  approach1: ExperimentResults,
  approach2: ExperimentResults,
  approach3: ExperimentResults
): ComparisonReport {
  return {
    consistencyComparison: compareConsistency([approach1, approach2, approach3]),
    qualityComparison: compareQuality([approach1, approach2, approach3]),
    packageUsageComparison: comparePackageUsage([approach1, approach2, approach3]),
    insights: extractInsights([approach1, approach2, approach3])
  };
}
```

## Expected Timeline

### Phase Schedule
- **Phase 1** (Approach 1): 2-3 days
  - 4 components × 5 runs × 30 minutes = ~10 hours
- **Phase 2** (Approach 2): 3-4 days  
  - More complex guidance, longer sessions
- **Phase 3** (Approach 3): 4-5 days
  - Includes verification cycles and refinement
- **Analysis Phase**: 2-3 days
  - Data processing, comparison, report generation

### Milestones
1. **Baseline Established**: Approach 1 complete
2. **Guidance Impact Measured**: Approach 2 complete
3. **Verification Benefits Assessed**: Approach 3 complete
4. **Research Insights Documented**: Analysis complete

## Success Indicators

### Quantitative Success
- **Statistical Significance**: Clear differences between approaches
- **Consistency Improvement**: Measurable increase from Approach 1→2→3
- **Quality Metrics**: Higher scores with more guidance
- **Package Usage**: Better MAX-MIN compliance in guided approaches

### Qualitative Success
- **Clear Patterns**: Identifiable differences in AI behavior
- **Actionable Insights**: Practical recommendations for AI collaboration
- **Process Understanding**: Clear understanding of when/how guidance helps
- **Reproducible Results**: Consistent findings across multiple runs

## Troubleshooting

### Common Issues
- **AI Inconsistency**: Use same model/temperature across all runs
- **Specification Ambiguity**: Refine specs if AI asks same questions repeatedly
- **Verification Failures**: Ensure verification tests are correct and fair
- **Data Collection**: Standardize data formats for analysis

### Quality Assurance
- **Peer Review**: Have specifications reviewed before experiments
- **Pilot Runs**: Test methodology with small sample first
- **Documentation**: Maintain detailed logs for reproducibility
- **Version Control**: Track all generated code and analysis scripts

This experimental framework provides a systematic approach to studying AI code generation consistency and the impact of different guidance strategies. 