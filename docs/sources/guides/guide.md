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
- **Automation**: `sources/agent/build/inst.impl.yaml`

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
  - Haskell: `sources/guides/impl.hs.prompt.md`
- **Input**: 
  - `build/impl/qi.v4.impl.template.md` (language-agnostic templates)
  - `build/package/[LANG].md` (selected packages from Stage 4)
  - `build/guides/mathematical-contracts.md` (mathematical contracts)
- **Output**: 
  - `build/impl/qi.v4.[LANG].template.md` (language-specific code templates)
  - `build/impl/qi.v4.[LANG].impl.md` (implementation guide/driver)
- **Purpose**: Generate BOTH ready-to-use code templates AND implementation assembly guides
- **Scope**: Currently supporting Python, TypeScript, and Haskell implementations
- **Key Objective**: Bridge the gap between selected packages and mathematical contracts
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
│   │   ├── impl.hs.prompt.md             # Stage 5 Haskell
│   │   └── common.md                     # Shared foundations
│   ├── nl/                       # Natural language contracts (input)
│   │   ├── qi.v4.class.contracts.md
│   │   └── qi.v4.component.contracts.md
│   └── agent/                    # Workflow automation files
│       └── build/                # YAML workflow definitions
│           ├── inst.formal.yaml          # Stage 1 automation
│           ├── inst.design.yaml          # Stage 2 automation
│           ├── inst.impl.yaml            # Stage 3 automation
│           ├── inst.package.yaml         # Stage 4 automation
│           ├── inst.impl.ts.yaml         # Stage 5 TypeScript
│           ├── inst.impl.py.yaml         # Stage 5 Python
│           └── inst.impl.hs.yaml         # Stage 5 Haskell
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
    │   ├── qi.v4.hs.template.md          # Stage 5: Haskell code
    │   └── qi.v4.hs.impl.md              # Stage 5: Haskell guide
    └── package/                  # Stage 4 output
        ├── ts.md                 # TypeScript packages
        ├── py.md                 # Python packages
        └── hs.md                 # Haskell packages
```

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

4. **Stage 4**: Research packages for target language
   ```bash
   # Input: build/impl/qi.v4.impl.template.md + target language
   # Output: build/package/[LANG].md (package selection rationale only)
   ```

5. **Stage 5**: Generate language-specific implementation
   ```bash
   # Input: build/impl/qi.v4.impl.template.md + build/package/[LANG].md + guides
   # Output: build/impl/qi.v4.[LANG].template.md + qi.v4.[LANG].impl.md
   ```

### Automated Process
```bash
# Stage 1: Mathematical formalization
run sources/agent/build/inst.formal.yaml

# Stage 2: Design patterns
run sources/agent/build/inst.design.yaml

# Stage 3: Language-agnostic template
run sources/agent/build/inst.impl.yaml

# Stage 4: Package research
run sources/agent/build/inst.package.yaml

# Stage 5: Language-specific implementation
run sources/agent/build/inst.impl.[LANG].yaml
```

## Language Codes
- `ts` = TypeScript
- `py` = Python  
- `hs` = Haskell

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

## Current Project Scope

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
- ✅ Create language-specific code templates using selected packages
- ✅ Create implementation guides showing how to assemble final library
- ✅ Bridge selected packages to mathematical contracts with wrapper code
- ✅ Ensure mathematical laws are preserved in language-specific idioms

## Next Steps

1. **Use Stage 1-3 outputs**: Work with existing formal spec, design, and templates
2. **Complete Stage 5**: Generate TypeScript and Haskell implementations
3. **Verify implementations**: Use verification workflows to ensure correctness
4. **Deploy**: Create production-ready libraries for each language