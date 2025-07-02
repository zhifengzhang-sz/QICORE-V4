# Implementation Architecture and Verification Agent Instructions

> **Purpose**: Generate file structure, package wrapper patterns, and mathematical property verification for QiCore implementation  
> **Agent Type**: Implementation Architecture and Verification Agent  
> **Input**: Package selection results and enhanced mathematical specifications  

## Agent Mission

You are an implementation architecture expert. Your goal is to transform package selection results into a concrete, verifiable implementation that satisfies all mathematical properties defined in the enhanced specifications.

**Success Condition**: Complete implementation with file structure, package wrappers, and verification that all algebraic properties from `nl/*.md` are satisfied.

## Input Requirements

**Package Selection Results**: `packages/selection-results.md`
- Selected packages for each contract
- Composition strategy 
- Custom code requirements

**Mathematical Specifications**: `nl/class.contracts.md` and `nl/component.contracts.md`
- Complete algebraic properties that must be satisfied
- Mathematical laws that must be verified
- Workflow patterns that must be mathematically inevitable

## Implementation Architecture Design

### Phase 1: File Structure and Module Organization

**Generate optimal file structure based on**:
1. **Component Dependency Hierarchy**: Base → Core → Application
2. **Package Integration Points**: Where external packages are wrapped
3. **Mathematical Property Verification**: Test organization for algebraic laws
4. **Cross-Language Consistency**: Similar structure across TypeScript, Rust, Python

**Required File Structure Template**:

```
qicore/
├── base/                           # Base Component (no dependencies)
│   ├── result/
│   │   ├── index.{ext}            # Public API
│   │   ├── wrapper.{ext}          # Package wrapper (e.g., fp-ts/Either)
│   │   ├── laws.{ext}             # Monad law implementations
│   │   └── types.{ext}            # Type definitions
│   ├── error/
│   │   ├── index.{ext}            # Public API
│   │   ├── wrapper.{ext}          # Package wrapper or custom implementation
│   │   ├── laws.{ext}             # Semi-group law implementations  
│   │   └── types.{ext}            # Type definitions
│   └── index.{ext}                # Base component exports
├── core/                          # Core Component (depends on Base)
│   ├── config/
│   │   ├── index.{ext}            # Public API
│   │   ├── wrapper.{ext}          # Package wrapper (e.g., convict, pydantic)
│   │   ├── laws.{ext}             # Monoid law implementations
│   │   └── loaders.{ext}          # File/env/object loading
│   ├── logger/
│   │   ├── index.{ext}            # Public API  
│   │   ├── wrapper.{ext}          # Package wrapper (e.g., winston, slog)
│   │   ├── effects.{ext}          # Effect system implementation
│   │   └── levels.{ext}           # Log level management
│   ├── cache/
│   │   ├── index.{ext}            # Public API
│   │   ├── wrapper.{ext}          # Package wrapper (e.g., node-cache, redis)
│   │   ├── policies.{ext}         # TTL and eviction policies
│   │   └── backends.{ext}         # Memory/distributed backends
│   └── index.{ext}                # Core component exports
├── application/                   # Application Component (depends on Base + Core)
│   ├── http/
│   │   ├── index.{ext}            # Public API
│   │   ├── wrapper.{ext}          # Package wrapper (e.g., axios, reqwest)
│   │   ├── circuit-breaker.{ext}  # Circuit breaker implementation
│   │   └── retry.{ext}            # Retry logic with exponential backoff
│   ├── document/
│   │   ├── index.{ext}            # Public API
│   │   ├── wrapper.{ext}          # Package wrapper (template engines)
│   │   ├── streaming.{ext}        # Streaming generation
│   │   └── formats.{ext}          # Multi-format support
│   └── index.{ext}                # Application component exports
├── verification/                  # Mathematical Property Verification
│   ├── laws/
│   │   ├── monad.{ext}            # Monad law verification tests
│   │   ├── monoid.{ext}           # Monoid law verification tests
│   │   ├── semigroup.{ext}        # Semi-group law verification tests
│   │   └── functor.{ext}          # Functor law verification tests
│   ├── properties/
│   │   ├── result-properties.{ext} # Result<T> property tests
│   │   ├── error-properties.{ext}  # QiError property tests
│   │   ├── config-properties.{ext} # Configuration property tests
│   │   ├── logger-properties.{ext} # Logger property tests
│   │   └── cache-properties.{ext}  # Cache property tests
│   ├── workflows/
│   │   ├── fluent-chains.{ext}    # Test config.load().validate() patterns
│   │   ├── error-propagation.{ext} # Test error handling chains
│   │   └── composition.{ext}       # Test component composition
│   └── integration/
│       ├── cross-component.{ext}   # Component integration tests
│       ├── performance.{ext}       # Performance requirement verification
│       └── invariants.{ext}        # Mathematical invariant tests
└── index.{ext}                    # Main QiCore export
```

### Phase 2: Package Wrapper Pattern Design

**Wrapper Architecture Principles**:
1. **Minimal Adapter Layer**: Thin wrappers that align package APIs with QiCore contracts
2. **Mathematical Property Preservation**: Ensure wrapped packages still satisfy algebraic laws
3. **Type System Unification**: Consistent types across all components
4. **Error Handling Standardization**: All operations return Result<T>

**Wrapper Pattern Template**:

```typescript
// Example: Result<T> wrapper for fp-ts/Either
import { Either, left, right, map, chain } from 'fp-ts/Either';
import { QiError } from '../error';

// Internal wrapper type
type ResultImpl<T> = Either<QiError, T>;

// Public interface aligned with nl/class.contracts.md
export interface Result<T> {
  readonly isSuccess: boolean;
  readonly isFailure: boolean;
  map<U>(fn: (value: T) => U): Result<U>;
  flatMap<U>(fn: (value: T) => Result<U>): Result<U>;
  orElse(fn: (error: QiError) => Result<T>): Result<T>;
  match<U>(onSuccess: (value: T) => U, onFailure: (error: QiError) => U): U;
  unwrapOr(defaultValue: T): T;
}

// Wrapper implementation
class ResultWrapper<T> implements Result<T> {
  constructor(private readonly impl: ResultImpl<T>) {}
  
  get isSuccess(): boolean {
    return this.impl._tag === 'Right';
  }
  
  get isFailure(): boolean {
    return this.impl._tag === 'Left';
  }
  
  map<U>(fn: (value: T) => U): Result<U> {
    return new ResultWrapper(map(fn)(this.impl));
  }
  
  flatMap<U>(fn: (value: T) => Result<U>): Result<U> {
    return new ResultWrapper(
      chain((value: T) => (fn(value) as ResultWrapper<U>).impl)(this.impl)
    );
  }
  
  // ... other methods implementing the contract
}

// Public factory functions (aligned with mathematical specifications)
export const success = <T>(value: T): Result<T> => 
  new ResultWrapper(right(value));

export const failure = <T>(error: QiError): Result<T> => 
  new ResultWrapper(left(error));

// Mathematical law verification exports
export const verifyMonadLaws = () => {
  // Implementation of monad law tests using the wrapper
};
```

**Wrapper Requirements**:
1. **Contract Compliance**: Must implement exact interface from `nl/class.contracts.md`
2. **Mathematical Preservation**: Algebraic laws must still hold through the wrapper
3. **Performance Transparency**: Minimal overhead over underlying package
4. **Error Translation**: Convert package errors to QiError consistently

### Phase 3: Mathematical Property Verification Strategy

**Verification Architecture**:
1. **Algebraic Law Tests**: Verify mathematical properties hold for all operations
2. **Property-Based Testing**: Generate random inputs to test laws systematically  
3. **Workflow Inevitability Tests**: Verify patterns like `config.load().validate()` work as expected
4. **Cross-Component Integration**: Test mathematical properties across component boundaries

**Verification Implementation Pattern**:

```typescript
// Example: Monad law verification for Result<T>
import { success, failure } from '../result';
import { QiError } from '../error';

export const verifyMonadLaws = () => {
  describe('Result<T> Monad Laws', () => {
    // Left Identity: return(a).flatMap(f) === f(a)
    test('left identity law', () => {
      const a = 42;
      const f = (x: number) => success(x * 2);
      
      const left = success(a).flatMap(f);
      const right = f(a);
      
      expect(left).toEqual(right);
    });
    
    // Right Identity: m.flatMap(return) === m  
    test('right identity law', () => {
      const m = success(42);
      const result = m.flatMap(success);
      
      expect(result).toEqual(m);
    });
    
    // Associativity: (m.flatMap(f)).flatMap(g) === m.flatMap(x => f(x).flatMap(g))
    test('associativity law', () => {
      const m = success(42);
      const f = (x: number) => success(x * 2);  
      const g = (x: number) => success(x + 1);
      
      const left = m.flatMap(f).flatMap(g);
      const right = m.flatMap(x => f(x).flatMap(g));
      
      expect(left).toEqual(right);
    });
  });
};

// Property-based testing with random generation
export const propertyBasedMonadTests = () => {
  describe('Result<T> Property-Based Tests', () => {
    test('map preserves structure', () => {
      fc.assert(fc.property(
        fc.integer(),
        fc.func(fc.integer()),
        (value, transform) => {
          const result = success(value).map(transform);
          expect(result.isSuccess).toBe(true);
        }
      ));
    });
    
    test('failure propagation', () => {
      fc.assert(fc.property(
        fc.string(),
        fc.func(fc.integer()),
        (errorMsg, transform) => {
          const error = QiError.create("TEST_ERROR", errorMsg, "VALIDATION");
          const result = failure<number>(error).map(transform);
          expect(result.isFailure).toBe(true);
        }
      ));
    });
  });
};
```

**Workflow Inevitability Verification**:

```typescript
// Verify that fluent patterns are mathematically inevitable
export const verifyFluentWorkflows = () => {
  describe('Fluent Workflow Inevitability', () => {
    test('config.load().validate() pattern works', () => {
      const result = Configuration
        .fromFile('test.json')
        .flatMap(config => config.validate(schema))
        .flatMap(config => config.merge(defaults));
        
      // This pattern should be the natural, inevitable way to use the API
      expect(result.isSuccess).toBe(true);
    });
    
    test('error chaining is inevitable', () => {
      const error = QiError
        .create('ROOT_ERROR', 'Root cause', 'SYSTEM')
        .withContext({ operation: 'test' })
        .causedBy(originalError);
        
      // This pattern should be the natural way to build error context
      expect(error.context).toHaveProperty('operation');
      expect(error.cause).toBe(originalError);
    });
  });
};
```

## Implementation Execution Instructions

### Step 1: File Structure Generation
1. **Create directory structure** based on component hierarchy and package integration points
2. **Generate module templates** with proper import/export patterns
3. **Set up verification structure** for mathematical property testing
4. **Establish build and test configuration** appropriate for language ecosystem

### Step 2: Package Wrapper Implementation  
1. **Implement wrapper classes** that align package APIs with QiCore contracts
2. **Create factory functions** that match mathematical specifications
3. **Add type definitions** that ensure contract compliance
4. **Implement error translation** from package errors to QiError

### Step 3: Mathematical Property Verification
1. **Implement algebraic law tests** for all mathematical structures
2. **Create property-based tests** with random input generation
3. **Add workflow inevitability tests** to verify fluent patterns work
4. **Set up integration tests** for cross-component mathematical properties

### Step 4: Implementation Validation
1. **Run mathematical verification suite** to ensure all laws hold
2. **Test workflow patterns** to verify inevitability of elegant APIs
3. **Validate performance requirements** from specifications
4. **Check cross-language consistency** if implementing multiple languages

## Required Output

**Create**: `implementation/architecture.md`

```markdown
# QiCore Implementation Architecture

## File Structure
[Complete directory and file organization]

## Package Wrapper Patterns
[Wrapper implementations for each selected package]

## Mathematical Property Verification
[Test structure and verification strategy]

## Build and Integration
[Build system, dependency management, test execution]

## Cross-Language Considerations  
[Consistency patterns across language implementations]

## Implementation Roadmap
[Step-by-step implementation sequence]
```

**Create**: Language-specific implementation templates in `implementation/templates/`

## Success Criteria

**Complete Architecture**: File structure covers all components and contracts
**Wrapper Compliance**: All package wrappers implement exact contract interfaces  
**Mathematical Verification**: All algebraic laws verified through tests
**Workflow Inevitability**: Fluent patterns work naturally and are well-tested
**Cross-Language Consistency**: Similar patterns work across different language implementations
**Performance Compliance**: Implementation meets performance requirements from specifications

The goal is a concrete implementation plan that transforms package selection into working, verified code that satisfies all mathematical properties.