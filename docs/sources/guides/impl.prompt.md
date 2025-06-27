# Stage 3: Design to Language-Agnostic Implementation Templates

## Objective

Transform design patterns into language-agnostic implementation templates that:
1. **Build upon the mathematical contracts** from Stage 1
2. Preserve mathematical properties across all language targets
3. Define clear integration points for external packages
4. Maintain cross-language behavioral consistency

## Critical Dependency

**THIS IS ESSENTIAL**: All implementation templates MUST be built on top of the mathematical contracts defined in `build/guides/mathematical-contracts.md`. The concrete implementations are instances of these abstract contracts.

## Input Documents

1. **Primary Input**: `build/design/qi.v4.design.analysis.md`
   - Design patterns derived from formal specification
   - Component boundaries and interactions
   - Performance requirements

2. **Mathematical Foundation**: `build/guides/mathematical-contracts.md`
   - Abstract mathematical contracts that ALL implementations must satisfy
   - These are the "interface laws" that ensure correctness
   - Every concrete implementation MUST be an instance of these contracts

3. **Shared Foundations**: `sources/guides/common.md`
   - Mathematical pattern definitions
   - Cross-language mappings

## Output Requirements

Generate `build/impl/qi.v4.impl.template.md` containing:

### 1. Contract Dependency Declaration

```markdown
## Mathematical Contract Dependencies

This implementation template instantiates the following mathematical contracts:
- Result<T> contract: Monad laws from mathematical-contracts.md
- Configuration contract: Monoid laws from mathematical-contracts.md
- Logger contract: Effect interface from mathematical-contracts.md
- Cache contract: State consistency from mathematical-contracts.md
[... all other contracts ...]

Every implementation MUST satisfy these contracts to be considered correct.
```

### 2. Language-Agnostic Templates

For each component, create templates that:

```markdown
## Component: Result<T>

### Mathematical Contract
References: mathematical-contracts.md#result-monad-contract

### Abstract Template
```pseudo
type Result<T> implements MonadContract<Result> {
    // Contract requirement: Monad laws
    static success<T>(value: T): Result<T>
    static failure<T>(error: Error): Result<T>
    
    // Contract requirement: Functor law
    map<U>(fn: T → U): Result<U>
    
    // Contract requirement: Monad bind law  
    flatMap<U>(fn: T → Result<U>): Result<U>
    
    // Contract requirement: Error handling
    recover(fn: Error → T): Result<T>
}
```

### Contract Verification Requirements
- Left identity: Result.success(a).flatMap(f) ≡ f(a)
- Right identity: m.flatMap(Result.success) ≡ m
- Associativity: m.flatMap(f).flatMap(g) ≡ m.flatMap(x => f(x).flatMap(g))
```

### 3. Package Integration Points

Mark where external packages should fulfill contracts:

```markdown
## Package Integration Strategy

### Result<T> Implementation
- **Contract**: Monad with error handling
- **Package Integration Point**: Use language's Result/Either/Option type
- **Wrapper Requirement**: Must adapt to satisfy our mathematical contract

### Configuration Implementation  
- **Contract**: Monoid with deep merge
- **Package Integration Point**: Configuration parsing library
- **Wrapper Requirement**: Must implement monoid merge operation

[... continue for all components ...]
```

### 4. Cross-Language Consistency Rules

```markdown
## Cross-Language Behavioral Consistency

All implementations across languages MUST:
1. Satisfy the same mathematical contracts
2. Exhibit identical error handling behavior
3. Maintain referential transparency where specified
4. Preserve performance characteristics within language tier
```

## Transformation Process

1. **Load Mathematical Contracts First**
   ```
   contracts = load("build/guides/mathematical-contracts.md")
   design = load("build/design/qi.v4.design.analysis.md")
   ```

2. **For Each Design Pattern**
   - Identify which mathematical contract it implements
   - Create abstract template that satisfies the contract
   - Define verification requirements from the contract
   - Mark package integration points

3. **Verify Contract Coverage**
   - Ensure every mathematical contract has a corresponding template
   - Verify all contract laws are preserved in template structure
   - Confirm package integration preserves contracts

## Template Structure

Each component template must include:

1. **Contract Reference**: Which mathematical contract it implements
2. **Abstract Operations**: Language-agnostic operation signatures
3. **Contract Laws**: Mathematical properties that must hold
4. **Integration Points**: Where packages can be used
5. **Verification Strategy**: How to test contract compliance

## Example: Configuration Component

```markdown
## Configuration Component Template

### Mathematical Contract
References: mathematical-contracts.md#configuration-monoid-contract

### Contract Laws
- Identity: config.merge(empty) ≡ config
- Associativity: (a.merge(b)).merge(c) ≡ a.merge(b.merge(c))
- Commutativity: a.merge(b) ≡ b.merge(a) (for our use case)

### Abstract Template
```pseudo
type Configuration<T> implements MonoidContract<Configuration<T>> {
    // Monoid identity
    static empty<T>(): Configuration<T>
    
    // Monoid operation  
    merge(other: Configuration<T>): Configuration<T>
    
    // Additional operations
    load(source: ConfigSource): Result<Configuration<T>>
    validate(): Result<boolean>
    get<K>(key: K): Result<T[K]>
}
```

### Package Integration Points
- Config parsing: YAML/JSON/TOML parsers
- Schema validation: Type validation libraries
- Merge operation: Deep merge utilities

### Contract Verification
```pseudo
// Verify monoid laws
assert(config.merge(empty()) == config)
assert(a.merge(b.merge(c)) == (a.merge(b)).merge(c))
```
```

## Critical Success Factors

1. **Contract Dependency**: Every template explicitly references its mathematical contract
2. **Law Preservation**: Template structure ensures mathematical laws are maintained
3. **Package Readiness**: Clear integration points for Stage 4 package selection
4. **Verification Path**: Each template includes how to verify contract compliance

## Common Pitfalls to Avoid

1. **Creating templates without contract references** - Every template MUST reference its contract
2. **Ignoring mathematical laws** - Laws must be preserved in template structure
3. **Language bias** - Templates must remain truly language-agnostic
4. **Missing verification** - Every contract requirement needs verification strategy

## Output Validation

The generated `qi.v4.impl.template.md` is valid if:
- [ ] Every mathematical contract has a corresponding template
- [ ] All templates explicitly reference their contracts
- [ ] Contract laws are preserved in template structure
- [ ] Package integration points are clearly marked
- [ ] Verification strategies are defined for each contract
- [ ] Cross-language consistency is maintained