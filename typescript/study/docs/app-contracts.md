# App Functionality Interface Contracts

**Complete specification of all application-layer functionality for the AI Consistency Study Platform**

## 📋 Overview

This document defines **interface contracts** for all app-layer components, providing clear specifications for:
- Input/output contracts
- Error handling requirements  
- Performance expectations
- Data validation rules
- Integration patterns

## 🎯 Design Principles

### **Contract-Driven Development**
- **Clear interfaces**: Every function has explicit input/output contracts
- **Error boundaries**: Well-defined error conditions and responses
- **Type safety**: Complete TypeScript type coverage
- **Immutability**: Readonly data structures throughout
- **Composability**: Functions designed for easy composition

### **Architectural Constraints**
- **App uses modules**: No circular dependencies with modules
- **Pure functions preferred**: Mark impure functions explicitly
- **Result<T> error handling**: Consistent error propagation
- **Validation at boundaries**: Input validation for all public APIs

---

## 📊 1. Study Orchestrator Contracts

**Location**: `src/app/study-orchestrator.ts`

### **Primary Interface**
```typescript
interface StudyExecutor {
  // Core execution
  executeStudy(config: StudyConfig): AsyncResult<StudyResult>;
  
  // Control operations
  pauseStudy(studyId: string): AsyncResult<void>;
  resumeStudy(studyId: string): AsyncResult<void>;
  cancelStudy(studyId: string): AsyncResult<void>;
  
  // Progress monitoring
  getProgress(studyId: string): AsyncResult<StudyProgress>;
  getStatus(studyId: string): AsyncResult<StudyStatus>;
}
```

### **Configuration Contracts**
```typescript
interface AppConfig {
  readonly codeGeneration: CodeGeneratorConfig;
  readonly concurrency: ConcurrencyConfig;
  readonly persistence: PersistenceConfig;
  readonly monitoring: MonitoringConfig;
}

interface CodeGeneratorConfig {
  readonly timeout: number;              // Milliseconds (1-300000)
  readonly retryAttempts: number;        // 0-5
  readonly rateLimitDelay: number;       // Milliseconds (0-10000)
  readonly maxPromptLength: number;      // Characters (1-50000)
}

interface ConcurrencyConfig {
  readonly maxConcurrentGenerations: number;  // 1-10
  readonly batchSize: number;                  // 1-100
  readonly delayBetweenBatches: number;       // Milliseconds (0-60000)
}
```

### **Execution Context Contracts**
```typescript
interface StudyExecutionContext {
  readonly studyId: string;              // Non-empty UUID
  readonly config: StudyConfig;          // Validated configuration
  readonly appConfig: AppConfig;         // Application settings
  readonly startTime: Date;              // Execution start timestamp
  readonly generations: GenerationResult[];  // Completed generations
  readonly errors: AppError[];           // Accumulated errors
}

interface StudyProgress {
  readonly totalCombinations: number;     // > 0
  readonly completedCombinations: number; // 0 <= completed <= total
  readonly successfulGenerations: number; // 0 <= successful <= completed
  readonly failedGenerations: number;     // 0 <= failed <= completed
  readonly estimatedTimeRemaining: number; // Milliseconds (0-∞)
}
```

### **Error Conditions**
- **INVALID_CONFIG**: Configuration validation failed
- **STUDY_NOT_FOUND**: Study ID does not exist
- **STUDY_ALREADY_RUNNING**: Cannot start already running study
- **GENERATION_TIMEOUT**: Generation exceeded timeout limit
- **CONCURRENT_LIMIT_EXCEEDED**: Too many concurrent operations

---

## 🗄️ 2. Database Layer Contracts

**Location**: `src/app/database/`

### **Primary Interface**
```typescript
interface StudyDataStore {
  // Study lifecycle
  createStudy(id: string, name: string, description: string): AsyncResult<void>;
  updateStudyStatus(studyId: string, status: StudyStatus): AsyncResult<void>;
  deleteStudy(studyId: string): AsyncResult<void>;
  
  // Generation data
  insertGenerationResult(result: GenerationResult): AsyncResult<void>;
  getGenerationResults(studyId: string): AsyncResult<GenerationResult[]>;
  getGenerationsByModel(studyId: string, modelId: string): AsyncResult<GenerationResult[]>;
  getGenerationsByInstruction(studyId: string, instructionId: string): AsyncResult<GenerationResult[]>;
  
  // Analytics queries
  getModelComparisons(studyId: string): AsyncResult<ModelComparison[]>;
  getInstructionComparisons(studyId: string): AsyncResult<InstructionComparison[]>;
  getStudyStatistics(studyId: string): AsyncResult<StudyStatistics>;
  
  // Cleanup and maintenance
  cleanupOldStudies(olderThanDays: number): AsyncResult<number>;
  optimizeDatabase(): AsyncResult<void>;
  close(): AsyncResult<void>;
}
```

### **Database Schema Contracts**
```typescript
interface DatabaseSchema {
  studies: {
    id: string;                    // Primary key, UUID format
    name: string;                  // 1-200 characters
    description: string;           // 0-2000 characters  
    created_at: Date;              // ISO timestamp
    completed_at: Date | null;     // ISO timestamp or null
    status: StudyStatus;           // 'running' | 'completed' | 'failed' | 'cancelled'
  };
  
  generations: {
    id: string;                    // Primary key, UUID format
    study_id: string;              // Foreign key to studies.id
    model_id: string;              // 1-100 characters
    instruction_id: string;        // 1-100 characters
    run_number: number;            // 1-1000
    timestamp: Date;               // ISO timestamp
    generated_code: string;        // 0-100000 characters
    compilation_success: boolean;   // true/false
    compilation_errors: string;     // JSON array of errors
    compilation_warnings: string;   // JSON array of warnings
    execution_time: number;        // Milliseconds (0-300000)
    // Score fields (0-100)
    syntactic_score: number;
    semantic_score: number;
    modern_score: number;
    completeness_score: number;
    documentation_score: number;
    performance_score: number;
    overall_score: number;
    // Performance metrics
    tokens_used: number;           // 0-200000
    response_time: number;         // Milliseconds (0-300000)
    prompt_tokens: number;         // 0-100000
    completion_tokens: number;     // 0-100000
    embedding_vector: string | null; // JSON array or null
  };
}
```

### **Query Performance Contracts**
- **Individual record operations**: < 10ms
- **Bulk inserts (100 records)**: < 500ms
- **Analytics queries**: < 2000ms
- **Database size limit**: < 1GB for optimal performance

### **Error Conditions**
- **DATABASE_CONNECTION_FAILED**: Cannot connect to database
- **CONSTRAINT_VIOLATION**: Foreign key or unique constraint violated
- **QUERY_TIMEOUT**: Query exceeded time limit
- **STORAGE_FULL**: Insufficient disk space
- **CORRUPTION_DETECTED**: Database integrity check failed

---

## 🎯 3. Evaluation System Contracts

**Location**: `src/app/evaluation/`

### **Primary Interface**
```typescript
interface CodeEvaluator {
  // Main scoring function
  scoreImplementation(code: GeneratedCode): AsyncResult<ImplementationScore>;
  
  // Individual scoring components
  scoreContractCompliance(code: string): number;        // 0-100
  scoreModernity(code: string): number;                 // 0-100
  scoreCompleteness(code: string): number;              // 0-100
  scoreQuality(code: string): number;                   // 0-100
  
  // Validation and compilation
  validateSyntax(code: string, language: string): AsyncResult<boolean>;
  checkCompilation(code: string): AsyncResult<CompilationResult>;
  
  // Quality assessment
  detectPatterns(code: string, patterns: RegExp[]): PatternMatch[];
  analyzeComplexity(code: string): ComplexityMetrics;
}
```

### **Scoring Methodology Contracts**
```typescript
interface ScoringRules {
  contractCompliance: {
    weight: 0.4;  // 40% of overall score
    criteria: {
      resultTypeDefinition: { points: 20; pattern: /data\s+Result\s+\w+/i };
      qiErrorIntegration: { points: 20; pattern: /data\s+QiError/i };
      successFunction: { points: 15; pattern: /success\s*::|success\s*=/i };
      failureFunction: { points: 15; pattern: /failure\s*::|failure\s*=/i };
      mapOperation: { points: 10; pattern: /map\s*::|map\s*=|fmap/i };
      flatMapOperation: { points: 10; pattern: /flatMap|bind|>>=|=<</i };
      unwrapOperation: { points: 5; pattern: /unwrap\s*::|unwrap\s*=/i };
      unwrapOrOperation: { points: 5; pattern: /unwrapOr|fromMaybe/i };
    };
  };
  
  modernity: {
    weight: 0.2;  // 20% of overall score
    criteria: {
      ghc2024Pragma: { points: 15; pattern: /{-#\s*LANGUAGE\s+GHC2024\s*#-}/i };
      strictFields: { points: 10; pattern: /!\w+/g };
      derivingStock: { points: 8; pattern: /deriving\s+stock/i };
      lambdaCase: { points: 7; pattern: /\\case/i };
      modernImports: { points: 5; pattern: /import\s+qualified.*as/i };
    };
  };
  
  // ... similar for completeness and quality
}
```

### **Performance Contracts**
- **Single code scoring**: < 100ms
- **Batch scoring (100 items)**: < 5 seconds
- **Pattern matching**: < 10ms per pattern
- **Memory usage**: < 50MB per evaluation

### **Error Conditions**
- **INVALID_CODE_FORMAT**: Code is not parseable
- **EVALUATION_TIMEOUT**: Scoring exceeded time limit
- **PATTERN_COMPILATION_FAILED**: Regex pattern is invalid
- **LANGUAGE_NOT_SUPPORTED**: Evaluation not available for language

---

## 📈 4. Analysis Engine Contracts

**Location**: `src/app/analysis/`

### **Primary Interface**
```typescript
interface StatisticalAnalyzer {
  // Core statistics
  calculateConsistency(scores: number[]): number;
  calculateMean(values: number[]): number;
  calculateStandardDeviation(values: number[]): number;
  calculateCoefficientOfVariation(values: number[]): number;
  
  // Comparative analysis
  compareModels(results: GenerationResult[]): ModelComparison[];
  compareInstructions(results: GenerationResult[]): InstructionComparison[];
  
  // Statistical significance
  calculateSignificance(groupA: number[], groupB: number[]): SignificanceTest;
  performANOVA(groups: number[][]): ANOVAResult;
  
  // Report generation
  generateReport(studyResult: StudyResult): AsyncResult<StudyReport>;
  generateSummary(studyResult: StudyResult): StudySummary;
}
```

### **Statistical Analysis Contracts**
```typescript
interface ModelComparison {
  readonly modelId: string;              // Non-empty identifier
  readonly meanScore: number;            // 0-100
  readonly consistency: number;          // 0-1 (lower = more consistent)
  readonly rank: number;                 // 1-N ranking
  readonly strengths: readonly string[]; // Top performing areas
  readonly weaknesses: readonly string[]; // Areas for improvement
  readonly sampleSize: number;           // Number of generations
  readonly confidenceInterval: [number, number]; // 95% CI for mean
}

interface SignificanceTest {
  readonly pValue: number;               // 0-1
  readonly isSignificant: boolean;       // p < 0.05
  readonly testStatistic: number;        // Test-specific statistic
  readonly method: 'ttest' | 'mannwhitney' | 'chi2';
  readonly effectSize: number;           // Cohen's d or equivalent
}

interface StudyStatistics {
  readonly totalGenerations: number;     // > 0
  readonly successRate: number;          // 0-1
  readonly meanScore: number;            // 0-100
  readonly standardDeviation: number;    // ≥ 0
  readonly coefficientOfVariation: number; // ≥ 0
  readonly consistencyRank: 'high' | 'medium' | 'low';
  readonly modelComparisons: readonly ModelComparison[];
  readonly instructionComparisons: readonly InstructionComparison[];
}
```

### **Performance Contracts**
- **Basic statistics (1000 values)**: < 50ms
- **Model comparison analysis**: < 500ms
- **Report generation**: < 2 seconds
- **Statistical tests**: < 200ms

### **Error Conditions**
- **INSUFFICIENT_DATA**: Not enough data points for analysis
- **INVALID_DISTRIBUTION**: Data doesn't meet test assumptions
- **CALCULATION_OVERFLOW**: Numerical computation overflow
- **ANALYSIS_TIMEOUT**: Analysis exceeded time limit

---

## ⚙️ 5. Configuration Management Contracts

**Location**: `src/app/config/`

### **Primary Interface**
```typescript
interface StudyConfigManager {
  // Configuration loading
  loadConfig(path: string): AsyncResult<StudyConfig>;
  loadConfigFromString(json: string): AsyncResult<StudyConfig>;
  
  // Configuration validation
  validateConfig(config: StudyConfig): AsyncResult<ValidationResult>;
  validateModel(model: AIModel): AsyncResult<ValidationResult>;
  validateInstruction(instruction: InstructionSet): AsyncResult<ValidationResult>;
  
  // Configuration creation
  createDefaultConfig(): StudyConfig;
  createTemplateConfig(template: ConfigTemplate): StudyConfig;
  
  // Configuration utilities
  mergeConfigs(base: StudyConfig, override: Partial<StudyConfig>): StudyConfig;
  exportConfig(config: StudyConfig): string;
}
```

### **Configuration Schema Contracts**
```typescript
interface StudyConfig {
  readonly id: string;                   // UUID format, non-empty
  readonly name: string;                 // 1-200 characters
  readonly description: string;          // 0-2000 characters
  readonly models: readonly AIModel[];   // 1-20 models
  readonly instructions: readonly InstructionSet[]; // 1-50 instructions
  readonly execution: ExecutionConfig;
  readonly output: OutputConfig;
}

interface ExecutionConfig {
  readonly runsPerCombination: number;   // 1-100
  readonly timeout: number;              // 1000-300000 ms
  readonly maxConcurrency: number;       // 1-10
  readonly retryAttempts: number;        // 0-5
  readonly delayBetweenRuns: number;     // 0-60000 ms
}

interface ValidationResult {
  readonly isValid: boolean;
  readonly errors: readonly ValidationError[];
  readonly warnings: readonly ValidationWarning[];
}

interface ValidationError {
  readonly field: string;                // Field path (e.g., 'models[0].temperature')
  readonly message: string;              // Human-readable error
  readonly code: string;                 // Machine-readable error code
  readonly value: unknown;               // Invalid value
}
```

### **Validation Rules**
```typescript
interface ValidationRules {
  studyConfig: {
    id: { required: true; format: 'uuid' };
    name: { required: true; minLength: 1; maxLength: 200 };
    models: { required: true; minItems: 1; maxItems: 20 };
    instructions: { required: true; minItems: 1; maxItems: 50 };
  };
  
  aiModel: {
    id: { required: true; pattern: /^[a-zA-Z0-9\-_]+$/ };
    name: { required: true; minLength: 1; maxLength: 100 };
    provider: { required: true; enum: ['anthropic', 'openai', 'local'] };
    temperature: { optional: true; min: 0; max: 2 };
    maxTokens: { optional: true; min: 1; max: 200000 };
  };
}
```

### **Performance Contracts**
- **Configuration loading**: < 100ms
- **Validation**: < 50ms
- **Configuration merging**: < 10ms
- **Export/serialization**: < 50ms

### **Error Conditions**
- **CONFIG_FILE_NOT_FOUND**: Configuration file doesn't exist
- **INVALID_JSON_FORMAT**: Configuration is not valid JSON
- **VALIDATION_FAILED**: Configuration doesn't meet schema requirements
- **CIRCULAR_DEPENDENCY**: Circular references in configuration

---

## 🚀 6. Code Generation Contracts

**Location**: `src/app/generators/`

### **Primary Interface**
```typescript
interface CodeGenerator {
  // Main generation function
  generateCode(model: AIModel, instruction: InstructionSet): AsyncResult<GeneratedCode>;
  
  // Batch operations
  generateBatch(requests: GenerationRequest[]): AsyncResult<GeneratedCode[]>;
  
  // Provider-specific generators
  createClaudeGenerator(config: ClaudeConfig): AsyncResult<CodeGenerator>;
  createOpenAIGenerator(config: OpenAIConfig): AsyncResult<CodeGenerator>;
  createLocalGenerator(config: LocalConfig): AsyncResult<CodeGenerator>;
  
  // Validation and testing
  validateConnection(): AsyncResult<boolean>;
  testGeneration(simplePrompt: string): AsyncResult<string>;
}
```

### **Generation Request Contracts**
```typescript
interface GenerationRequest {
  readonly model: AIModel;               // Validated model configuration
  readonly instruction: InstructionSet; // Validated instruction
  readonly systemPrompt?: string;        // Optional system context
  readonly userPrompt: string;           // 1-50000 characters
  readonly metadata: RequestMetadata;
}

interface RequestMetadata {
  readonly requestId: string;            // UUID for tracking
  readonly timestamp: Date;              // Request creation time
  readonly timeout: number;              // Milliseconds (1000-300000)
  readonly retryAttempts: number;        // 0-5
  readonly priority: 'low' | 'normal' | 'high';
}

interface GeneratedCode {
  readonly id: string;                   // UUID
  readonly code: string;                 // Generated source code
  readonly model: AIModel;               // Model used for generation
  readonly instruction: InstructionSet;  // Instruction used
  readonly generation: GenerationMetadata;
  readonly quality: QualityMetrics;
}
```

### **Performance Contracts**
- **Single generation**: < 30 seconds (configurable timeout)
- **Batch processing**: Parallel execution up to concurrency limit
- **Connection validation**: < 5 seconds
- **Memory usage**: < 100MB per generation

### **Error Conditions**
- **API_KEY_INVALID**: Authentication failed
- **RATE_LIMIT_EXCEEDED**: Too many requests
- **MODEL_NOT_AVAILABLE**: Requested model is not accessible
- **GENERATION_TIMEOUT**: Generation exceeded time limit
- **CONTENT_FILTERED**: Generated content was filtered

---

## 🏃 7. Study Execution Contracts

**Location**: `src/app/runners/`

### **Primary Interface**
```typescript
interface StudyRunner {
  // Execution control
  startStudy(config: StudyConfig): AsyncResult<StudyExecution>;
  pauseStudy(executionId: string): AsyncResult<void>;
  resumeStudy(executionId: string): AsyncResult<void>;
  cancelStudy(executionId: string): AsyncResult<void>;
  
  // Progress monitoring
  getProgress(executionId: string): AsyncResult<ExecutionProgress>;
  getMetrics(executionId: string): AsyncResult<ExecutionMetrics>;
  
  // Result handling
  saveResults(results: StudyResult): AsyncResult<void>;
  loadResults(studyId: string): AsyncResult<StudyResult>;
}
```

### **Execution State Contracts**
```typescript
interface StudyExecution {
  readonly id: string;                   // UUID
  readonly studyId: string;              // Reference to study configuration
  readonly status: ExecutionStatus;      // Current execution state
  readonly startTime: Date;              // Execution start
  readonly endTime?: Date;               // Execution end (if completed)
  readonly progress: ExecutionProgress;   // Current progress
  readonly context: ExecutionContext;    // Execution environment
}

interface ExecutionProgress {
  readonly totalTasks: number;           // Total number of generation tasks
  readonly completedTasks: number;       // Completed tasks
  readonly successfulTasks: number;      // Successful completions
  readonly failedTasks: number;          // Failed tasks
  readonly currentBatch: number;         // Current batch being processed
  readonly estimatedTimeRemaining: number; // Milliseconds
}

interface ExecutionMetrics {
  readonly averageGenerationTime: number; // Milliseconds
  readonly successRate: number;           // 0-1
  readonly tokensPerMinute: number;       // Processing rate
  readonly errorRate: number;             // 0-1
  readonly peakMemoryUsage: number;       // MB
}
```

### **Performance Contracts**
- **Study startup**: < 5 seconds
- **Progress updates**: < 100ms
- **Graceful shutdown**: < 10 seconds
- **Resource cleanup**: Complete cleanup on termination

### **Error Conditions**
- **EXECUTION_NOT_FOUND**: Execution ID doesn't exist
- **INVALID_STATE_TRANSITION**: Cannot transition from current state
- **RESOURCE_EXHAUSTED**: Insufficient system resources
- **EXECUTION_FAILED**: Unrecoverable execution error

---

## 📊 8. Integration Contracts

### **Cross-Component Communication**
```typescript
interface ComponentIntegration {
  // Event-driven communication
  emit(event: string, data: unknown): void;
  on(event: string, handler: (data: unknown) => void): void;
  
  // Service discovery
  getService<T>(name: string): AsyncResult<T>;
  registerService<T>(name: string, service: T): void;
  
  // Health monitoring
  checkHealth(): AsyncResult<HealthStatus>;
  getMetrics(): SystemMetrics;
}

interface HealthStatus {
  readonly status: 'healthy' | 'degraded' | 'unhealthy';
  readonly components: Record<string, ComponentHealth>;
  readonly timestamp: Date;
}

interface ComponentHealth {
  readonly status: 'up' | 'down' | 'degraded';
  readonly responseTime: number;        // Milliseconds
  readonly errorRate: number;           // 0-1
  readonly lastCheck: Date;
}
```

### **Data Flow Contracts**
```typescript
// Standard data flow: Config → Generation → Evaluation → Analysis → Storage
interface DataFlow {
  configValidation: StudyConfig → Result<StudyConfig>;
  codeGeneration: (AIModel, InstructionSet) → Result<GeneratedCode>;
  qualityEvaluation: GeneratedCode → Result<QualityMetrics>;
  statisticalAnalysis: GenerationResult[] → Result<StudyStatistics>;
  dataStorage: StudyResult → Result<void>;
}
```

---

## ✅ Validation and Testing Contracts

### **Test Coverage Requirements**
- **Unit tests**: 90%+ coverage for all public functions
- **Integration tests**: All component interactions tested
- **Property tests**: Mathematical properties verified
- **Performance tests**: All performance contracts validated

### **Quality Gates**
```typescript
interface QualityGates {
  // Code quality
  linting: 'biome check --no-errors';
  typeChecking: 'tsc --noEmit';
  testing: 'bun test --coverage > 90%';
  
  // Performance
  responseTime: 'All endpoints < specified limits';
  memoryUsage: 'No memory leaks detected';
  
  // Security
  dependencies: 'No high-severity vulnerabilities';
  configuration: 'No secrets in configuration files';
}
```

---

## 🔧 Implementation Guidelines

### **1. Contract Implementation**
- Every interface must be implemented completely
- All error conditions must be handled explicitly
- Performance contracts must be measured and verified

### **2. Type Safety**
- Use TypeScript strict mode
- No `any` types in public interfaces
- Complete type annotations for all parameters

### **3. Error Handling**
- Use `Result<T>` for all operations that can fail
- Provide specific error codes and messages
- Include context information in errors

### **4. Testing Strategy**
- Test all public interface contracts
- Mock external dependencies
- Verify error conditions and edge cases

**Status**: Complete Specification  
**Version**: 1.0.0  
**Next Steps**: Implementation following these contracts  