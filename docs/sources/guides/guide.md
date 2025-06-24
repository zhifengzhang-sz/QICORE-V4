# QiCore v4.0 Development Guide

> **Simple 5-Stage Transformation Process**  
> **Purpose**: Transform natural language specifications into production-ready code  
> **From**: Human-written contracts **To**: Working implementations

## The 5-Stage Process (Enhanced)

### **Stage 1: Natural Language → Mathematical Formalization**
- **Use**: `sources/guides/formal.prompt.md` + `sources/guides/common.md` + `build/guides/mathematical-contracts.md`
- **Input**: 
  - `sources/nl/qi.v4.class.contracts.md` 
  - `sources/nl/qi.v4.component.contracts.md`
- **Output**: `build/objective/formal/qi.v4.formal.spec.md`
- **Purpose**: Transform contracts into formal mathematical models that satisfy abstract contracts
- **Optional**: `sources/agent/build/inst.formal.yaml` (workflow automation)

### **Stage 2: Mathematical → Design Patterns**  
- **Use**: `sources/guides/design.prompt.md` + `sources/guides/common.md` + `build/guides/mathematical-contracts.md`
- **Input**: `build/objective/formal/qi.v4.formal.spec.md`
- **Mathematical Contracts**: Uses abstract mathematical contracts as design constraints
- **Output**: `build/design/qi.v4.design.analysis.md`
- **Purpose**: Derive design patterns that preserve mathematical properties
- **Optional**: `docs/sources/agent/build/inst.design.yaml` (workflow automation)

### **Stage 3: Design → Language-Agnostic Implementation**
- **Use**: `sources/guides/impl.prompt.md` + `sources/guides/common.md` + `build/guides/mathematical-contracts.md`
- **Input**: `build/design/qi.v4.design.analysis.md`
- **Output**: `build/impl/qi.v4.impl.template.md`
- **Purpose**: Create language-agnostic templates that satisfy mathematical contracts
- **Optional**: Manual process (no workflow automation)

### **Stage 4: Package Research → Wrapper Design**
- **Use**: `sources/guides/package-research-methodology.md` + `sources/guides/common.md` + `build/guides/mathematical-contracts.md`
- **Input**: `build/impl/qi.v4.impl.template.md` + Target Language
- **Process**: Research packages that can implement required mathematical contracts
- **Output**: `build/research/qi.v4.[LANG].packages.md` + `build/research/qi.v4.[LANG].wrappers.md`
- **Purpose**: Find packages that can satisfy mathematical contracts and design wrappers
- **Optional**: Manual process (no workflow automation)

### **Stage 5: Templates + Research → Language-Specific Code**
- **Use**: Language-specific implementation guides (only TypeScript and Python currently available):
  - TypeScript: `sources/guides/impl.ts.prompt.md`
  - Python: `sources/guides/impl.py.prompt.md`
  - **TODO**: Implementation guides needed for Rust, Haskell, and Go
- **Input**: 
  - `build/impl/qi.v4.impl.template.md`
  - `build/research/qi.v4.[LANG].packages.md`
  - `build/research/qi.v4.[LANG].wrappers.md`
  - `build/guides/mathematical-contracts.md`
- **Target Language**: Specify one of: `ts` (TypeScript), `py` (Python), `rs` (Rust), `hs` (Haskell), `go` (Go)
- **Output**: `build/impl/qi.v4.[LANG].impl.md`
- **Purpose**: Implement concrete types that satisfy mathematical contracts using chosen packages
- **Optional**: `docs/sources/agent/build/inst.impl.[LANG].yaml` (workflow automation)

## Essential Files

**Transformation Methodologies:**
- `build/guides/mathematical-contracts.md` - Abstract mathematical contracts used in ALL stages
- `sources/guides/formal.prompt.md` - Stage 1 transformation methodology
- `sources/guides/design.prompt.md` - Stage 2 transformation methodology
- `sources/guides/impl.prompt.md` - Stage 3 language-agnostic implementation
- `sources/guides/package-research-methodology.md` - Stage 4 package research methodology
- Language-specific implementation guides (Stage 5):
  - `sources/guides/impl.ts.prompt.md` - TypeScript implementation
  - `sources/guides/impl.py.prompt.md` - Python implementation
  - **TODO**: Missing implementation guides for Rust, Haskell, and Go
- `sources/guides/common.md` - Shared mathematical foundations

**Workflow Automation (Optional):**
- `sources/agent/build/inst.formal.yaml` - Stage 1 workflow
- `sources/agent/build/inst.design.yaml` - Stage 2 workflow
- `sources/agent/build/inst.impl.[LANG].yaml` - Stage 5 implementation workflow for available languages

## Usage Instructions

### Manual Process
1. **Stage 1**: Use `sources/guides/formal.prompt.md` + `sources/guides/common.md` to transform your NL contracts to mathematical spec
2. **Stage 2**: Use `sources/guides/design.prompt.md` + `sources/guides/common.md` + `build/guides/mathematical-contracts.md` + formal spec to create design patterns
3. **Stage 3**: Use `sources/guides/impl.prompt.md` + `sources/guides/common.md` to create language-agnostic implementation templates
4. **Stage 4**: Use `sources/guides/package-research-methodology.md` + `sources/guides/common.md` to research packages and design wrappers
5. **Stage 5**: Use `sources/guides/impl.[LANG].prompt.md` with package research outputs and your target language code (`ts`, `py`, `rs`, `hs`, or `go`)

### Automated Process (Optional)
1. **Stage 1**: Run `sources/agent/build/inst.formal.yaml` workflow
2. **Stage 2**: Run `sources/agent/build/inst.design.yaml` workflow (uses mathematical-contracts.md)
3. **Stage 3**: Follow manual process using `sources/guides/impl.prompt.md`
4. **Stage 4**: Follow manual process using `sources/guides/package-research-methodology.md`
5. **Stage 5**: Run `sources/agent/build/inst.impl.[LANG].yaml` workflow (e.g., `inst.impl.ts.yaml` for TypeScript)

### Language Codes
- `ts` = TypeScript
- `py` = Python  
- `rs` = Rust
- `hs` = Haskell
- `go` = Go

## File Structure

```
docs/
├── sources/                   # Source files that drive the design process
│   ├── guides/               # Core methodologies
│   │   ├── formal.prompt.md
│   │   ├── design.prompt.md
│   │   ├── package-research-methodology.md  # Stage 4 methodology
│   │   ├── impl.py.prompt.md    # Language-specific implementation
│   │   ├── impl.ts.prompt.md    # Language-specific implementation
│   │   └── common.md
│   ├── nl/                   # Natural language contracts (input)
│   │   ├── qi.v4.class.contracts.md
│   │   └── qi.v4.component.contracts.md
│   └── agent/               # Workflow automation files
│       └── build/          # YAML workflow definitions
│           ├── inst.formal.yaml
│           ├── inst.design.yaml
│           ├── inst.impl.py.yaml
│           ├── inst.impl.ts.yaml
│           ├── inst.impl.rs.yaml
│           ├── inst.impl.hs.yaml
│           └── inst.impl.go.yaml
└── build/                     # Generated outputs from design process
    ├── guides/               # Core mathematical contracts
    │   └── mathematical-contracts.md    # Mathematical contracts for all stages
    ├── package/              # Language-specific package research
    │   ├── py.md
    │   ├── ts.md
    │   └── hs.md
    ├── objective/formal/      # Stage 1 output: Mathematical specs
    │   └── qi.v4.formal.spec.md
    ├── design/               # Stage 2 output: Design patterns
    │   └── qi.v4.design.analysis.md
    ├── research/             # Stage 4 output: Package research
    │   ├── qi.v4.py.packages.md
    │   ├── qi.v4.py.wrappers.md
    │   ├── qi.v4.ts.packages.md
    │   └── qi.v4.ts.wrappers.md
    └── impl/                 # Stage 5 output: Implementation templates
        ├── qi.v4.ts.template.md
        ├── qi.v4.ts.impl.md
        └── [other languages...]
```

## Success Criteria

- ✅ Natural language specifications transform systematically into working code
- ✅ Mathematical properties preserved through all stages  
- ✅ **NEW**: Evidence-based package selection with wrapper design
- ✅ **NEW**: Production-ready implementations built on proven packages
- ✅ Cross-language behavioral consistency achieved
- ✅ All operations implemented with complete coverage
- ✅ Component architecture maintained throughout

## Next Steps

1. Start with your natural language contracts in `sources/nl/`
2. Run Stage 1 and 2 to get design patterns
3. **NEW**: Run Stage 3 to research packages and design wrappers for your target language
4. **Choose your target language**: `ts`, `py`, `rs`, `hs`, or `go`
5. Run Stage 4 with your chosen language code and package research results
6. Get language-specific templates: `build/impl/qi.v4.[LANG].template.md` + `build/impl/qi.v4.[LANG].impl.md`

## Example: Python Workflow (Enhanced)

1. **Stage 1**: Generate `build/objective/formal/qi.v4.formal.spec.md`
2. **Stage 2**: Generate `build/design/qi.v4.design.analysis.md`  
3. **Stage 3**: Research Python packages → Generate `build/research/qi.v4.py.packages.md` + `build/research/qi.v4.py.wrappers.md`
4. **Stage 4**: Use research results → Get `build/impl/qi.v4.py.template.md` + `build/impl/qi.v4.py.impl.md`

## Previously: The Missing Integration

The `package-research-methodology.md` exists but was not integrated into the main workflow, creating a gap between theoretical contracts and practical implementation.

## Now: Integrated Package Research as Stage 4

**Stage 4 (Package Research)** now explicitly bridges:
- **Input**: Language-agnostic implementation templates
- **Process**: Evidence-based package selection using existing methodology
- **Output**: Language-specific package selections and wrapper specifications
- **Integration**: Feeds directly into language-specific implementation prompts

This makes package selection a **visible, mandatory step** rather than an implicit assumption.

## Dependencies and References

- **Input**: [Class Contracts](../nl/qi.v4.class.contracts.md), [Component Contracts](../nl/qi.v4.component.contracts.md)
- **Methodology**: [Common Foundations](common.md), [Package Research](package-research-methodology.md)
- **Mathematics**: [Common Foundations](common.md)
- **Process**: [Workflow YAML Files](../agent/build/)
- **Used By**: Any project requiring systematic specification-to-implementation transformation with production-ready packages 