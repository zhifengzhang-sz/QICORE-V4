# QiCore v4.0 Development Guide

> **Simple 5-Stage Transformation Process**  
> **Purpose**: Transform natural language specifications into production-ready code  
> **From**: Human-written contracts **To**: Working implementations

## The 5-Stage Process

### **Stage 1: Natural Language → Mathematical Formalization**
- **Use**: `sources/guides/formal.prompt.md` + `sources/guides/common.md`
- **Input**: 
  - `sources/nl/qi.v4.class.contracts.md` 
  - `sources/nl/qi.v4.component.contracts.md`
- **Output**: 
  - `build/objective/formal/qi.v4.formal.spec.md` (concrete mathematical models)
  - `build/guides/mathematical-contracts.md` (abstract interface contracts)
- **Purpose**: Transform contracts into formal mathematical models and extract their abstract mathematical patterns
- **Automation**: `sources/agent/build/inst.formal.yaml`

### **Stage 2: Mathematical → Design Patterns**  
- **Use**: `sources/guides/design.prompt.md` + `sources/guides/common.md` + `build/guides/mathematical-contracts.md`
- **Input**: 
  - `build/objective/formal/qi.v4.formal.spec.md` (concrete models)
  - `build/guides/mathematical-contracts.md` (abstract contracts)
- **Output**: `build/design/qi.v4.design.analysis.md`
- **Purpose**: Derive design patterns that preserve mathematical properties
- **Automation**: `sources/agent/build/inst.design.yaml`

### **Stage 3: Design → Language-Agnostic Implementation**
- **Use**: `sources/guides/impl.prompt.md` + `sources/guides/common.md` + `build/guides/mathematical-contracts.md`
- **Input**: `build/design/qi.v4.design.analysis.md`
- **Output**: `build/impl/qi.v4.impl.template.md`
- **Purpose**: Create language-agnostic implementation templates
- **Automation**: `sources/agent/build/inst.impl.yaml` *(TODO: Create this file)*

### **Stage 4: Implementation → Package Research**
- **Use**: `sources/guides/package-research-methodology.md` + `sources/guides/common.md`
- **Input**: `build/impl/qi.v4.impl.template.md` + Target Language
- **Output**: 
  - `build/package/py.md` (Python packages - selection only)
  - `build/package/ts.md` (TypeScript packages - selection only)
  - `build/package/hs.md` (Haskell packages - selection only)
- **Purpose**: Research and select packages that satisfy mathematical contracts (NO implementation code)
- **Scope**: Currently supporting Python, TypeScript, and Haskell ecosystems
- **Automation**: `sources/agent/build/inst.package.yaml`

### **Stage 5: Templates + Packages → Language-Specific Implementation**
- **Use**: Language-specific implementation guides:
  - TypeScript: `sources/guides/impl.ts.prompt.md`
  - Python: `sources/guides/impl.py.prompt.md`
  - Haskell: `sources/guides/impl.hs.prompt.md` *(TODO: Create this file)*
- **Input**: 
  - `build/impl/qi.v4.impl.template.md` (language-agnostic templates)
  - `build/package/[LANG].md` (selected packages from Stage 4)
  - `build/guides/mathematical-contracts.md` (mathematical contracts)
- **Output**: 
  - `build/impl/qi.v4.[LANG].template.md` (language-specific templates)
  - `build/impl/qi.v4.[LANG].impl.md` (wrapper implementations fulfilling contracts)
- **Purpose**: Create wrappers around selected packages to fulfill mathematical contracts + implementation guides
- **Scope**: Currently supporting Python, TypeScript, and Haskell implementations
- **Key Objective**: Build the wrapper layer that bridges selected packages to contract requirements
- **Automation**: `sources/agent/build/inst.impl.[LANG].yaml`

## File Structure

```
docs/
├── sources/                      # Source files that drive the design process
│   ├── guides/                   # Core methodologies
│   │   ├── formal.prompt.md              # Stage 1 methodology
│   │   ├── design.prompt.md              # Stage 2 methodology
│   │   ├── impl.prompt.md                # Stage 3 methodology
│   │   ├── package-research-methodology.md # Stage 4 methodology
│   │   ├── impl.ts.prompt.md             # Stage 5 TypeScript
│   │   ├── impl.py.prompt.md             # Stage 5 Python
│   │   ├── impl.rs.prompt.md             # Stage 5 Rust (TODO)
│   │   ├── impl.hs.prompt.md             # Stage 5 Haskell (TODO)
│   │   ├── impl.go.prompt.md             # Stage 5 Go (TODO)
│   │   └── common.md                     # Shared foundations
│   ├── nl/                       # Natural language contracts (input)
│   │   ├── qi.v4.class.contracts.md
│   │   └── qi.v4.component.contracts.md
│   └── agent/                    # Workflow automation files
│       └── build/                # YAML workflow definitions
│           ├── inst.formal.yaml          # Stage 1 automation
│           ├── inst.design.yaml          # Stage 2 automation
│           ├── inst.impl.yaml            # Stage 3 automation (TODO)
│           ├── inst.package.yaml         # Stage 4 automation (TODO)
│           ├── inst.impl.ts.yaml         # Stage 5 TypeScript
│           ├── inst.impl.py.yaml         # Stage 5 Python
│           ├── inst.impl.rs.yaml         # Stage 5 Rust
│           ├── inst.impl.hs.yaml         # Stage 5 Haskell
│           └── inst.impl.go.yaml         # Stage 5 Go
└── build/                        # Generated outputs from design process
    ├── objective/formal/         # Stage 1 output
    │   └── qi.v4.formal.spec.md
    ├── guides/                   # Stage 1 abstract contracts
    │   └── mathematical-contracts.md
    ├── design/                   # Stage 2 output
    │   └── qi.v4.design.analysis.md
    ├── impl/                     # Stage 3 & 5 outputs
    │   ├── qi.v4.impl.template.md        # Stage 3: Language-agnostic
    │   ├── qi.v4.ts.template.md          # Stage 5: TypeScript code
    │   ├── qi.v4.ts.impl.md              # Stage 5: TypeScript guide
    │   ├── qi.v4.py.template.md          # Stage 5: Python code
    │   ├── qi.v4.py.impl.md              # Stage 5: Python guide
    │   └── [other languages...]
    └── package/                  # Stage 4 output
        ├── ts.md                 # TypeScript packages
        ├── py.md                 # Python packages
        └── hs.md                 # Haskell packages
```

## Missing Files Summary

### High Priority (Core Process)
1. **Stage 1 Output**: `build/guides/mathematical-contracts.md` - Abstract mathematical interface contracts  
2. **Stage 3 Output**: `build/impl/qi.v4.impl.template.md` - Language-agnostic implementation template
3. **Stage 3 Workflow**: `sources/agent/build/inst.impl.yaml` - Automation for Stage 3
4. **Fix Stage 4**: Restructure `build/package/[LANG].md` files to contain ONLY package selection (no implementation code)

### Medium Priority (Language Support)
4. **Rust Guide**: `sources/guides/impl.rs.prompt.md` - Rust implementation methodology
5. **Haskell Guide**: `sources/guides/impl.hs.prompt.md` - Haskell implementation methodology
6. **Go Guide**: `sources/guides/impl.go.prompt.md` - Go implementation methodology

### Already Exist (Verified)
- ✅ All Stage 1 & 2 files
- ✅ TypeScript and Python implementation guides
- ✅ All workflow files for Stage 5 (all languages)

## Usage Instructions

### Manual Process
1. **Stage 1**: Transform natural language to mathematical specification
   ```bash
   # Input: sources/nl/*.md + guides
   # Output: build/objective/formal/qi.v4.formal.spec.md + build/guides/mathematical-contracts.md
   ```

2. **Stage 2**: Derive design patterns from mathematical specification
   ```bash
   # Input: build/objective/formal/qi.v4.formal.spec.md + build/guides/mathematical-contracts.md + guides
   # Output: build/design/qi.v4.design.analysis.md
   ```

3. **Stage 3**: Create language-agnostic implementation template
   ```bash
   # Input: build/design/qi.v4.design.analysis.md + guides
   # Output: build/impl/qi.v4.impl.template.md
   ```

4. **Stage 4**: Research packages for target language (SELECTION ONLY)
   ```bash
   # Input: build/impl/qi.v4.impl.template.md + target language
   # Output: build/package/[LANG].md (package selection rationale, NO code)
   ```

5. **Stage 5**: Generate language-specific implementation (WITH integration examples)
   ```bash
   # Input: build/impl/qi.v4.impl.template.md + build/package/[LANG].md
   # Output: build/impl/qi.v4.[LANG].template.md (HOW to use packages) + qi.v4.[LANG].impl.md (complete implementation)
   ```

### Automated Process
```bash
# Stage 1: Mathematical formalization
run sources/agent/build/inst.formal.yaml

# Stage 2: Design patterns
run sources/agent/build/inst.design.yaml

# Stage 3: Language-agnostic template (manual - workflow TODO)
manually create build/impl/qi.v4.impl.template.md

# Stage 4: Package research (SELECTION ONLY)
run sources/agent/build/inst.package.yaml

# Stage 5: Language-specific implementation (WITH integration examples)
run sources/agent/build/inst.impl.[LANG].yaml
```

## Language Codes
- `ts` = TypeScript
- `py` = Python  
- `rs` = Rust
- `hs` = Haskell
- `go` = Go

## Success Criteria

### Process Integrity
- ✅ Natural language specifications transform systematically into working code
- ✅ Mathematical properties preserved through all stages
- ✅ Cross-language behavioral consistency achieved

### Quality Assurance  
- ✅ Evidence-based package selection with mathematical contract compliance
- ✅ Production-ready implementations built on proven packages
- ✅ All operations implemented with complete coverage
- ✅ Component architecture maintained throughout

## Current Project Scope (Updated)

### **Supported Languages (3)**:
- **Python**: Interpreted tier (100× baseline) - Comprehensive ecosystem
- **TypeScript**: Interpreted tier (100× baseline) - Strong type safety
- **Haskell**: Functional tier (50× baseline) - Mathematical correctness

### **Stage Boundary Clarifications**:

**Stage 4 (Package Research)**:
- ✅ Research and select packages that satisfy mathematical contracts
- ✅ Document selection rationale and performance characteristics
- ❌ NO implementation code or wrappers

**Stage 5 (Implementation)**:
- ✅ Create language-specific templates using selected packages
- ✅ Build wrapper implementations that fulfill mathematical contracts
- ✅ Generate implementation guides with code examples
- ✅ Bridge the gap between packages and contract requirements

### **Recently Completed**:
- ✅ Updated package research with 2024-2025 current data
- ✅ Expanded Haskell coverage from 54% to 85% through systematic research
- ✅ Enhanced TypeScript research with performance-focused selections
- ✅ Corrected documentation to reflect current scope

## Next Steps

1. **Immediate**: Complete Stage 5 for current languages (Python, TypeScript, Haskell)
2. **Short-term**: Generate wrapper implementations and integration guides
3. **Long-term**: Consider adding other languages based on demand

## Dependencies and References

- **Input**: [Class Contracts](../nl/qi.v4.class.contracts.md), [Component Contracts](../nl/qi.v4.component.contracts.md)
- **Mathematics**: [Mathematical Contracts](../build/guides/mathematical-contracts.md), [Common Foundations](common.md)
- **Methodology**: [Package Research](package-research-methodology.md)
- **Process**: [Workflow YAML Files](../agent/build/)
- **Used By**: Any project requiring systematic specification-to-implementation transformation