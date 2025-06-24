# QiCore v4.0 Mathematical Model Contracts

> **Interface Contract Layer**  
> **Purpose**: Abstract mathematical models that bridge formal specifications and design patterns  
> **Extracted from**: `sources/guides/common.md` categorical structures  
> **Used by**: Stage 2 design process for deriving concrete patterns  
> Version: v4.0.1  
> Date: June 23, 2025

## Overview

This document defines the **abstract mathematical model contracts** that form the interface layer between:
- **Input**: Concrete mathematical models from `build/objective/formal.spec.md`
- **Output**: Design patterns that derive from these abstract contracts

These contracts serve as **verification points** and **design constraints** for cross-language implementations.

## Contract Usage Flow

```
Stage 1: sources/nl + sources/guides/formal.prompt.md + sources/guides/common.md 
         → build/objective/formal.spec.md (concrete mathematical models)

Stage 2: sources/guides/design.prompt.md + sources/guides/common.md + build/guides/mathematical-contracts.md + build/objective/formal.spec.md
         → build/design/*.md (design patterns derived from both abstract contracts and concrete models)

Stage 3+: Design patterns → Implementation
```

**Key Point**: `mathematical-contracts.md` is a **source file** (not build output) that provides abstract mathematical interfaces for the design stage to reference.

## Abstract Mathematical Models

### 1. Monad Contract

**Abstract Interface**:
```haskell
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

-- Laws that ALL monadic implementations must satisfy
-- Left Identity: return a >>= f ≡ f a
-- Right Identity: m >>= return ≡ m  
-- Associativity: (m >>= f) >>= g ≡ m >>= (λx → f x >>= g)
```

**Design Stage Derivations**:
- **Result Monad**: Error handling with short-circuiting
- **IO Monad**: Side effects with sequencing (Web Framework)
- **State Monad**: Stateful computations (Cache)
- **Reader Monad**: Dependency injection (Config, AI Client)
- **Transaction Monad**: Database operations with ACID properties
- **Continuation Monad**: Async/await patterns (ASGI Server)

### 2. Functor Contract

**Abstract Interface**:
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Laws
-- Identity: fmap id ≡ id
-- Composition: fmap (f . g) ≡ fmap f . fmap g
```

**Design Stage Derivations**:
- **Result Functor**: Data transformations with error preservation
- **Stream Functor**: Lazy data transformation pipelines
- **Protocol Functor**: Message transformation (MCP Protocol)
- **Component Functor**: Cross-component data boundaries

### 3. Monoid Contract

**Abstract Interface**:
```haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a

-- Laws
-- Left Identity: mempty `mappend` x ≡ x
-- Right Identity: x `mappend` mempty ≡ x
-- Associativity: (x `mappend` y) `mappend` z ≡ x `mappend` (y `mappend` z)
```

**Design Stage Derivations**:
- **Configuration Monoid**: Right-biased merge for settings
- **Log Monoid**: Append-only log aggregation
- **Error Monoid**: Error context accumulation

### 4. Effect Interface Contract

**Abstract Interface**:
```haskell
-- Simple effect interface (NOT free monads)
type Effect m = {
  perform :: EffectOperation -> m ()
  handle :: EffectError -> m ()
}
```

**Design Stage Derivations**:
- **Logging Effect**: Structured logging with levels
- **Cache Effect**: Side-effecting cache operations
- **IO Effect**: File system and network operations

### 5. State Machine Contract

**Abstract Interface**:
```haskell
type StateMachine s e = {
  transition :: s -> e -> s
  isValid :: s -> Bool
  terminal :: s -> Bool
}
```

**Design Stage Derivations**:
- **Circuit Breaker**: CLOSED → OPEN → HALF_OPEN states
- **Connection Management**: Connection lifecycle states
- **Cache Eviction**: LRU state transitions

### 6. Stream Coalgebra Contract

**Abstract Interface**:
```haskell
type Stream a = {
  next :: Stream a -> Maybe (a, Stream a)
  isEmpty :: Stream a -> Bool
  finalize :: Stream a -> IO ()
}
```

**Design Stage Derivations**:
- **HTTP Response Streaming**: Large response handling
- **Document Generation Streaming**: Incremental document creation
- **AI Response Streaming**: Real-time LLM responses
- **Database Result Streaming**: Large query results

## Performance Contract Constraints

All derived implementations must respect tier-based performance:

**Tier Multipliers** (from `common.md`):
- **Native** (Rust, C++): 1× baseline
- **VM-based** (Go, Java): 10× baseline  
- **Functional** (Haskell): 50× baseline
- **Interpreted** (Python, JavaScript): 100× baseline

**Contract Performance Requirements**:
- **Monad bind operation**: < tier_multiplier × 1μs
- **Functor map operation**: < tier_multiplier × 0.5μs
- **Monoid append operation**: < tier_multiplier × 0.1μs
- **Effect perform operation**: < tier_multiplier × 10μs
- **State transition**: < tier_multiplier × 0.1μs
- **Stream next operation**: < tier_multiplier × 1μs

## Cross-Language Verification Requirements

### 1. Law Compliance Testing

Each language implementation must provide property-based tests:

```typescript
// Example: Result monad left identity law
test('Result monad left identity', () => {
  const a = 5;
  const f = (x: number) => Result.success(x * 2);
  expect(Result.success(a).flatMap(f)).toEqual(f(a));
});
```

### 2. Performance Verification

Each implementation must include benchmarks demonstrating tier compliance:

```python
# Example: Performance within tier constraints
def test_result_bind_performance():
    # Python (interpreted tier): 100× baseline = 100μs max
    assert benchmark_result_bind() < 100e-6  # 100 microseconds
```

### 3. Contract Inheritance Verification

Design patterns must explicitly derive from abstract contracts:

```rust
// Example: Explicit contract derivation
impl Monad for Result<T, QiError> {
    fn return_(value: T) -> Self { Result::Success(value) }
    fn bind<U>(self, f: impl Fn(T) -> Result<U, QiError>) -> Result<U, QiError> {
        match self {
            Success(x) => f(x),
            Failure(e) => Failure(e),
        }
    }
}
```

## Integration with Design Stage

### Input to design.prompt.md

The design stage should use:
1. **Concrete models** from `build/objective/formal.spec.md`
2. **Abstract contracts** from this document
3. **Derivation strategy**: Show how concrete models derive from abstract contracts

### Output from design stage

Design patterns should explicitly state:
1. **Contract basis**: Which abstract contract the pattern implements
2. **Derivation rationale**: Why this derivation serves the concrete use case
3. **Law preservation**: How the pattern maintains contract laws
4. **Performance profile**: Tier-specific performance characteristics

## Usage Instructions

### For Design Stage (Stage 2)

When creating design patterns, always:
1. **Reference abstract contract**: Start with the appropriate contract from this document
2. **Show derivation**: Explain how the concrete model specializes the abstract contract
3. **Verify laws**: Ensure the derived pattern preserves contract laws
4. **Specify performance**: Include tier-based performance expectations

### For Implementation Stage (Stage 5)

When implementing language-specific code, always:
1. **Import contracts**: Reference both design patterns and this contract document
2. **Test laws**: Include property-based tests for contract law compliance
3. **Benchmark performance**: Verify tier-appropriate performance
4. **Document derivation**: Show the inheritance chain from abstract to concrete

## Benefits

1. **Clear Verification Points**: Explicit laws and properties to test
2. **Design Guidance**: Abstract contracts constrain design choices appropriately
3. **Cross-Language Consistency**: Same mathematical foundation across languages
4. **Performance Predictability**: Tier-based constraints ensure realistic expectations
5. **Formal Verification Ready**: Contracts can be proven in formal systems (Haskell/Coq/Agda)

## Dependencies and References

- **Extracts from**: [Common Mathematical Foundations](common.md)
- **Used by**: [Design Prompt](design.prompt.md) - Stage 2 design derivation
- **Implements**: Abstract interface layer for mathematical models
- **Enables**: Systematic derivation of design patterns from mathematical contracts
- **Integration**: Bridge between concrete formal models and practical design patterns 