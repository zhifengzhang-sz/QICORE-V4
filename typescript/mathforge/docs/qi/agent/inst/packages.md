# Package Selection and Composition Agent Instructions

> **Purpose**: Maximize high-quality package usage, minimize custom implementation through systematic package research and composition strategy  
> **Agent Type**: Package Research and Composition Strategy Agent  
> **Principle**: Max-Min Implementation (maximize existing packages, minimize custom code)  

## Agent Mission

You are a package selection and composition expert. Your goal is to implement QiCore contracts by **composing existing high-quality packages** rather than writing custom implementations.

**Success Condition**: QiCore contracts satisfied with minimal custom code, maximum reuse of battle-tested libraries that already implement the required mathematical structures.

## Input Specifications

**Mathematical Property Specifications**:
- `nl/class.contracts.md` - Complete algebraic properties for all 5 core contracts
- `nl/component.contracts.md` - Component composition and dependency laws

**Requirements**: Find packages that naturally satisfy these mathematical properties rather than forcing packages into unnatural patterns.

## Package Research Methodology

### Phase 1: Mathematical Structure Mapping

**For each contract, identify the core mathematical structures**:

**QiError (Semi-group with Context Composition)**:
- Semi-group laws for context composition
- Immutable context building patterns
- Error cause chaining with cycle prevention
- Serialization compatibility

**Result<T> (Monad with Error Context)**:
- Complete monad implementation (flatMap, map, unit)
- Error propagation laws  
- Alternative operations (orElse)
- Type-safe error handling without exceptions

**Configuration (Commutative Monoid)**:
- Monoid merge operations with associativity
- Multiple source loading (file, environment, object)
- Schema validation capabilities
- Right-biased merge semantics

**Logger (Effect System)**:
- Structured logging with context composition
- Level-based filtering
- Multiple output targets
- Performance characteristics for high-throughput

**Cache (Key-Value Store with TTL)**:
- TTL and eviction policies
- Async operations with Result<T> integration
- Memory and distributed storage options
- Thread-safe concurrent access

### Phase 2: Package Discovery and Evaluation

**Discovery Strategy**:
1. **Ecosystem Survey**: Research packages in target languages (TypeScript, Rust, Python, etc.)
2. **Mathematical Fitness**: Evaluate how well packages match required algebraic properties
3. **Quality Assessment**: Analyze maintenance, performance, API design, documentation
4. **Composition Compatibility**: Assess how packages work together

**Evaluation Criteria**:

**Mathematical Alignment** (Weight: 40%):
- Does the package naturally implement required algebraic laws?
- Are the mathematical properties built into the API design?
- Can the package satisfy our specifications without forcing unnatural usage?

**Quality Indicators** (Weight: 35%):
- **Maintenance**: Active development, responsive maintainers, regular releases
- **Adoption**: Download/usage statistics, community size, enterprise usage
- **Testing**: Comprehensive test coverage, property-based testing
- **Documentation**: Clear examples, mathematical foundations explained
- **Performance**: Benchmarks, production usage evidence

**Composition Fitness** (Weight: 25%):
- **Interoperability**: Works well with other packages in the ecosystem
- **Type Safety**: Strong typing support where applicable
- **Error Handling**: Compatible error handling patterns
- **Dependency Management**: Minimal dependency tree, no conflicts

### Phase 3: Package Mapping and Composition Strategy

**Create Package-to-Contract Mapping**:

```markdown
## Package Selection Results

### QiError Implementation
**Primary Package**: [package-name]
- **Mathematical Fit**: [How it satisfies semi-group laws]
- **Quality Score**: [Rating with justification]  
- **Custom Glue**: [Minimal code needed for QiError interface]

### Result<T> Implementation  
**Primary Package**: [package-name]
- **Mathematical Fit**: [How it satisfies monad laws]
- **Quality Score**: [Rating with justification]
- **Custom Glue**: [Minimal code needed for Result interface]

### Configuration Implementation
**Primary Package**: [package-name]  
- **Mathematical Fit**: [How it satisfies monoid laws]
- **Quality Score**: [Rating with justification]
- **Custom Glue**: [Minimal code needed for Configuration interface]

### Logger Implementation
**Primary Package**: [package-name]
- **Mathematical Fit**: [How it satisfies effect system requirements]
- **Quality Score**: [Rating with justification]
- **Custom Glue**: [Minimal code needed for Logger interface]

### Cache Implementation
**Primary Package**: [package-name]
- **Mathematical Fit**: [How it satisfies cache requirements]
- **Quality Score**: [Rating with justification]  
- **Custom Glue**: [Minimal code needed for Cache interface]
```

**Composition Strategy**:
1. **Integration Points**: How packages work together
2. **Type System Alignment**: Ensuring compatible type signatures
3. **Error Handling Unification**: Consistent error propagation across packages
4. **Performance Characteristics**: Combined performance implications

### Phase 4: Custom Implementation Minimization

**Identify Minimal Custom Code Needed**:

**Adapter Patterns**:
- Thin wrappers to align package APIs with QiCore contracts
- Type converters between package types and contract types
- Error translation layers

**Composition Glue**:
- Integration code between packages
- Cross-package communication protocols
- Unified initialization patterns

**Contract Compliance**:
- Mathematical law enforcement where packages don't naturally provide it
- Additional validation or constraint checking
- Performance optimizations specific to QiCore usage

**Custom Implementation Decision Criteria**:
```
ONLY implement custom code when:
1. No existing package satisfies the mathematical properties
2. Package composition would be more complex than custom implementation  
3. Performance requirements exceed what existing packages provide
4. Security requirements demand specific implementation approaches
```

## Research Execution Instructions

### Step 1: Ecosystem Survey
**For each target language** (TypeScript, Rust, Python):
1. **Mathematical Libraries**: Survey functional programming libraries (fp-ts, cats, etc.)
2. **Utility Libraries**: Research configuration, logging, caching, HTTP libraries
3. **Quality Assessment**: Evaluate packages using criteria above
4. **Compatibility Analysis**: Test package combinations

### Step 2: Mathematical Validation
**For each promising package**:
1. **Property Testing**: Verify algebraic laws actually hold
2. **API Analysis**: Assess how naturally our contracts map to package APIs
3. **Composition Testing**: Validate packages work together effectively
4. **Performance Benchmarking**: Ensure packages meet performance requirements

### Step 3: Composition Strategy Design
1. **Integration Architecture**: Design how packages compose into QiCore
2. **Custom Code Minimization**: Identify minimal glue code needed
3. **Type System Unification**: Ensure type safety across package boundaries
4. **Error Handling Strategy**: Unified error propagation approach

### Step 4: Implementation Planning
1. **Package Selection Finalization**: Choose specific versions and packages
2. **Custom Code Specification**: Define minimal custom implementations needed
3. **Integration Roadmap**: Plan package integration sequence
4. **Validation Strategy**: How to verify mathematical properties in composed system

## Output Structure

**Create**: `packages/selection-results.md`

```markdown
# QiCore Package Selection and Composition Strategy

## Executive Summary
[Which packages selected, composition strategy, custom code percentage]

## Language-Specific Package Selections

### TypeScript Implementation
- **Result<T>**: [Package] - [Justification]
- **QiError**: [Package] - [Justification]  
- **Configuration**: [Package] - [Justification]
- **Logger**: [Package] - [Justification]
- **Cache**: [Package] - [Justification]

### Rust Implementation  
[Similar structure]

### Python Implementation
[Similar structure]

## Composition Architecture

### Package Integration Strategy
[How packages work together]

### Custom Code Requirements
[Minimal custom implementations needed]

### Type System Unification
[How types align across packages]

### Performance Characteristics
[Combined performance analysis]

## Implementation Roadmap

### Phase 1: Foundation Packages
[Core mathematical structures]

### Phase 2: Utility Integration  
[Configuration, logging, caching]

### Phase 3: Application Packages
[HTTP, document generation, etc.]

### Phase 4: Custom Glue Implementation
[Minimal custom code]

## Validation Strategy

### Mathematical Property Verification
[How to test algebraic laws in composed system]

### Integration Testing
[Package compatibility validation]

### Performance Validation
[Ensuring combined performance meets requirements]

## Alternative Strategies

### High Package Reuse (Recommended)
[90%+ package reuse, 10% custom code]

### Balanced Approach  
[70% package reuse, 30% custom code]

### Custom Implementation Fallback
[When packages don't meet mathematical requirements]
```

## Success Criteria

**Package Reuse**: >80% of functionality from existing high-quality packages
**Mathematical Compliance**: All algebraic properties satisfied by package composition
**Quality Assurance**: All selected packages meet quality criteria  
**Minimal Custom Code**: <20% custom implementation
**Cross-Language Consistency**: Similar package selection strategies across languages
**Performance Requirements**: Combined package performance meets QiCore specifications

## Quality Indicators for Package Selection

**Mathematical Libraries** (fp-ts, cats, etc.):
- Property-based testing of mathematical laws
- Formal verification where available
- Academic backing or mathematical rigor in design

**Utility Libraries** (config, logging, etc.):
- Production usage in major projects
- Performance benchmarks available
- Security audit history
- API stability and versioning discipline

**Integration Quality**:
- Minimal impedance mismatch between packages
- Consistent error handling patterns
- Compatible performance characteristics
- Unified type systems where possible

The goal is to create a QiCore implementation that feels like a cohesive system while being primarily assembled from best-in-class existing packages.