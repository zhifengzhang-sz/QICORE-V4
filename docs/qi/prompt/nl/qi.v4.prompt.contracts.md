# QiPrompt v4.0 Natural Language Interface Contracts

## Overview

QiPrompt provides natural language interface contracts for sophisticated prompt engineering and template processing. This specification defines behavioral contracts using mathematical foundations while maintaining practical usability.

## Component Architecture

### Prompt Construction Pipeline
```
Input Text/Template → Processing → Validation → Enhancement → Output
```

**Natural Language Contract**: "QiPrompt transforms raw input text and templates into sophisticated, contextually-aware prompts through a validated processing pipeline that ensures consistency and quality."

### Core Components

#### 1. PromptTemplate Component

**Purpose**: Template management and variable substitution
**Mathematical Foundation**: String interpolation with validation monoid

**Natural Language Contracts**:

- **Template Creation**: "I can create a template from a string with placeholders marked by {{variable}} syntax, validating that all placeholders are properly formatted"
- **Variable Substitution**: "I can substitute variables in templates with provided values, ensuring type safety and reporting missing variables"
- **Template Composition**: "I can compose multiple templates together while maintaining variable scoping and avoiding conflicts"

**Interface Contract**:
```typescript
interface PromptTemplate {
  // Template creation and validation
  create(template: string): Result<PromptTemplate>
  validate(): Result<ValidationResult>
  
  // Variable management  
  getVariables(): string[]
  substitute(variables: Record<string, unknown>): Result<string>
  
  // Template composition
  compose(other: PromptTemplate): Result<PromptTemplate>
  include(partial: PromptTemplate, name: string): Result<PromptTemplate>
}
```

#### 2. ContentProcessor Component

**Purpose**: Advanced text processing and enhancement
**Mathematical Foundation**: Transformation pipeline with composition laws

**Natural Language Contracts**:

- **Text Enhancement**: "I can enhance raw text by applying formatting, style improvements, and contextual enrichments while preserving meaning"
- **Content Validation**: "I can validate content against quality criteria, checking for completeness, clarity, and adherence to style guidelines"
- **Format Conversion**: "I can convert content between different formats (markdown, plain text, structured) while maintaining semantic integrity"

**Interface Contract**:
```typescript
interface ContentProcessor {
  // Text processing pipeline
  enhance(content: string, options?: EnhanceOptions): Result<string>
  validate(content: string, criteria: ValidationCriteria): Result<ValidationResult>
  
  // Format handling
  convertFormat(content: string, from: Format, to: Format): Result<string>
  extractMetadata(content: string): Result<ContentMetadata>
  
  // Processing composition
  pipe(...processors: ContentProcessor[]): ContentProcessor
}
```

#### 3. ContextManager Component

**Purpose**: Context awareness and prompt contextualization
**Mathematical Foundation**: Context algebra with merge operations

**Natural Language Contracts**:

- **Context Tracking**: "I can track and maintain contextual information across prompt interactions, building cumulative understanding"
- **Context Injection**: "I can inject relevant context into prompts based on the current situation and historical patterns"
- **Context Scoping**: "I can manage context scopes, ensuring appropriate information is available while preventing information leakage"

**Interface Contract**:
```typescript
interface ContextManager {
  // Context lifecycle
  createContext(initial?: ContextData): Result<Context>
  updateContext(context: Context, updates: ContextData): Result<Context>
  
  // Context operations
  injectContext(prompt: string, context: Context): Result<string>
  extractContext(response: string): Result<ContextData>
  
  // Context scoping
  scopeContext(context: Context, scope: string): Result<Context>
  mergeContexts(contexts: Context[]): Result<Context>
}
```

#### 4. QualityAssurance Component

**Purpose**: Prompt quality validation and improvement
**Mathematical Foundation**: Quality metrics with validation algebra

**Natural Language Contracts**:

- **Quality Assessment**: "I can assess prompt quality across multiple dimensions including clarity, specificity, and effectiveness"
- **Improvement Suggestions**: "I can suggest specific improvements to prompts based on quality analysis and best practices"
- **Quality Tracking**: "I can track quality metrics over time, identifying patterns and improvement opportunities"

**Interface Contract**:
```typescript
interface QualityAssurance {
  // Quality assessment
  assessQuality(prompt: string): Result<QualityReport>
  validateCriteria(prompt: string, criteria: QualityCriteria): Result<ValidationResult>
  
  // Improvement suggestions
  suggestImprovements(prompt: string): Result<ImprovementSuggestion[]>
  applyImprovements(prompt: string, suggestions: ImprovementSuggestion[]): Result<string>
  
  // Quality tracking
  trackQuality(prompt: string, results: QualityMetrics): Result<void>
  getQualityTrends(): Result<QualityTrends>
}
```

## Mathematical Properties

### Template Composition Laws
```typescript
// Associativity: (a compose b) compose c = a compose (b compose c)
compose(compose(templateA, templateB), templateC) === 
compose(templateA, compose(templateB, templateC))

// Identity: template compose empty = template
compose(template, emptyTemplate) === template

// Variable preservation: composed templates preserve all variables
getVariables(compose(a, b)) === union(getVariables(a), getVariables(b))
```

### Context Merge Properties
```typescript
// Commutativity: merge(a, b) = merge(b, a) for non-conflicting keys
// Associativity: merge(merge(a, b), c) = merge(a, merge(b, c))
// Identity: merge(context, emptyContext) = context
```

### Quality Validation Monoid
```typescript
// Associativity: validate(validate(a, b), c) = validate(a, validate(b, c))
// Identity: validate(quality, neutral) = quality
// Consistency: validate(prompt, criteria) produces deterministic results
```

## Error Handling Patterns

### Template Errors
- **TEMPLATE_INVALID**: Malformed template syntax
- **VARIABLE_MISSING**: Required variables not provided
- **COMPOSITION_CONFLICT**: Variable name conflicts in composition

### Processing Errors  
- **ENHANCEMENT_FAILED**: Content enhancement processing failed
- **FORMAT_UNSUPPORTED**: Requested format conversion not supported
- **VALIDATION_FAILED**: Content validation against criteria failed

### Context Errors
- **CONTEXT_SCOPE_VIOLATION**: Attempted access outside context scope
- **CONTEXT_MERGE_CONFLICT**: Conflicting context data in merge operation
- **CONTEXT_INJECTION_FAILED**: Failed to inject context into prompt

### Quality Errors
- **QUALITY_ASSESSMENT_FAILED**: Quality assessment could not be completed
- **CRITERIA_INVALID**: Quality criteria specification is invalid
- **IMPROVEMENT_APPLICATION_FAILED**: Failed to apply suggested improvements

## Integration Patterns

### With QiCore Components
```typescript
// Integrate with QiCore Result types
const processPrompt = (template: string, variables: Variables): Result<ProcessedPrompt> =>
  createTemplate(template)
    .flatMap(t => t.substitute(variables))
    .flatMap(prompt => enhanceContent(prompt))
    .flatMap(enhanced => assessQuality(enhanced))
    .map(quality => ({ prompt: enhanced, quality }))
```

### With QiAgent Components  
```typescript
// Integrate with agent generation pipeline
const generateContextualPrompt = (context: AgentContext): Result<string> =>
  extractContextData(context)
    .flatMap(data => injectContext(baseTemplate, data))
    .flatMap(prompt => validateQuality(prompt))
    .flatMap(validated => applyImprovements(validated))
```

## Performance Guarantees

### Template Operations
- **Template Creation**: O(n) where n is template length
- **Variable Substitution**: O(m) where m is number of variables
- **Template Composition**: O(n + m) where n,m are template sizes

### Content Processing
- **Text Enhancement**: O(n) linear processing
- **Format Conversion**: O(n) with format-specific optimizations
- **Quality Assessment**: O(n) with configurable depth

### Context Management
- **Context Creation**: O(1) constant time
- **Context Injection**: O(n + c) where n is prompt size, c is context size
- **Context Merging**: O(c₁ + c₂) where c₁,c₂ are context sizes

## Usage Examples

### Basic Template Processing
```typescript
const template = createTemplate("Hello {{name}}, your task is: {{task}}")
  .flatMap(t => t.substitute({ name: "Alice", task: "code review" }))
  .flatMap(prompt => enhanceContent(prompt))
  .flatMap(enhanced => assessQuality(enhanced))
```

### Advanced Context Management
```typescript
const contextualPrompt = createContext({ domain: "software", level: "expert" })
  .flatMap(ctx => injectContext(template, ctx))
  .flatMap(prompt => validateCriteria(prompt, expertLevelCriteria))
  .flatMap(validated => suggestImprovements(validated))
```

### Quality-Driven Processing
```typescript
const qualityPrompt = processTemplate(input)
  .flatMap(prompt => assessQuality(prompt))
  .flatMap(quality => quality.score > 0.8 
    ? success(prompt) 
    : suggestImprovements(prompt).flatMap(improvements => 
        applyImprovements(prompt, improvements)))
```

---

**Version**: 4.0  
**Date**: June 30, 2025  
**Status**: Production Specification  
**Dependencies**: QiCore Base (Result<T>, QiError), Mathematical Foundations 