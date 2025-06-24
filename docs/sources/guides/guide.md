# QiCore v4.0 Development Guide

> **Simple 4-Stage Transformation Process**  
> **Purpose**: Transform natural language specifications into production-ready code  
> **From**: Human-written contracts **To**: Working implementations

## The 5-Stage Process (Enhanced)

### **Stage 1: Natural Language → Mathematical Formalization**
- **Use**: `sources/guides/formal.prompt.md` + `sources/guides/common.md`
- **Input**: 
  - `sources/nl/qi.v4.class.contracts.md` 
  - `sources/nl/qi.v4.component.contracts.md`
- **Output**: `build/objective/formal/qi.v4.formal.spec.md`
- **Optional**: `agent/build/inst.formal.yaml` (workflow automation)

### **Stage 2: Mathematical → Design Patterns**  
- **Use**: `sources/guides/design.prompt.md` + `sources/guides/common.md` + `sources/guides/mathematical-contracts.md`
- **Input**: `build/objective/formal/qi.v4.formal.spec.md`
- **Mathematical Contracts**: Uses abstract mathematical contracts from `sources/guides/mathematical-contracts.md` 
- **Output**: `build/design/qi.v4.design.analysis.md`
- **Optional**: `docs/agent/inst.stage2.design.yaml` (workflow automation)

### **Stage 3: Design → Language-Agnostic Implementation**
- **Use**: `sources/guides/impl.prompt.md` + `sources/guides/common.md`
- **Input**: `build/design/qi.v4.design.analysis.md`
- **Output**: `build/impl/qi.v4.impl.template.md`
- **Optional**: `agent/build/inst.impl.yaml` (workflow automation)

### **Stage 4: Package Research → Wrapper Design (INTEGRATE EXISTING)**
- **Use**: `sources/guides/package-research-methodology.md` + `sources/guides/common.md`
- **Input**: `build/impl/qi.v4.impl.template.md` + Target Language
- **Process**: Comprehensive package research → Evidence-based selection → Wrapper specification
- **Output**: `build/research/qi.v4.[LANG].packages.md` + `build/research/qi.v4.[LANG].wrappers.md`
- **Purpose**: Bridge mathematical contracts to production-ready packages

### **Stage 5: Templates + Research → Language-Specific Code**
- **Use**: `sources/guides/impl.[LANG].prompt.md` + package research outputs
- **Input**: 
  - `build/impl/qi.v4.impl.template.md`
  - `build/research/qi.v4.[LANG].packages.md`
  - `build/research/qi.v4.[LANG].wrappers.md`
- **Target Language**: Specify one of: `ts` (TypeScript), `py` (Python), `rs` (Rust), `hs` (Haskell), `go` (Go)
- **Output**: `build/impl/qi.v4.[LANG].impl.md`
- **Example**: For TypeScript → `build/impl/qi.v4.ts.impl.md`
- **Optional**: `agent/build/inst.impl.[LANG].yaml` (workflow automation)

## Essential Files

**Transformation Methodologies:**
- `sources/guides/formal.prompt.md` - Stage 1 transformation methodology
- `sources/guides/design.prompt.md` - Stage 2 transformation methodology
- `sources/guides/impl.prompt.md` - Stage 3 language-agnostic implementation
- `sources/guides/package-research-methodology.md` - Stage 4 package research methodology (INTEGRATED)
- `sources/guides/impl.[LANG].prompt.md` - Stage 5 language-specific implementation (e.g., `impl.py.prompt.md`)
- `sources/guides/common.md` - Shared mathematical foundations
- `sources/guides/mathematical-contracts.md` - Abstract mathematical contracts for Stage 2

**Workflow Automation (Optional):**
- `docs/agent/inst.formal.yaml` - Stage 1 workflow
- `docs/agent/inst.stage2.design.yaml` - Stage 2 workflow (uses mathematical-contracts.md)
- `docs/agent/inst.research.[LANG].yaml` - Stage 4 package research workflow (NEW)
- `docs/agent/inst.impl.[LANG].yaml` - Stage 5 implementation workflow

## Usage Instructions

### Manual Process
1. **Stage 1**: Use `sources/guides/formal.prompt.md` + `sources/guides/common.md` to transform your NL contracts to mathematical spec
2. **Stage 2**: Use `sources/guides/design.prompt.md` + `sources/guides/common.md` + `sources/guides/mathematical-contracts.md` + formal spec to create design patterns
3. **Stage 3**: Use `sources/guides/impl.prompt.md` + `sources/guides/common.md` to create language-agnostic implementation templates
4. **Stage 4**: Use `sources/guides/package-research-methodology.md` + `sources/guides/common.md` to research packages and design wrappers
5. **Stage 5**: Use `sources/guides/impl.[LANG].prompt.md` with package research outputs and your target language code (`ts`, `py`, `rs`, `hs`, or `go`)

### Automated Process (Optional)
1. **Stage 1**: Run `docs/agent/inst.formal.yaml` workflow
2. **Stage 2**: Run `docs/agent/inst.stage2.design.yaml` workflow (uses mathematical-contracts.md)
3. **Stage 3**: Run `docs/agent/inst.impl.yaml` workflow  
4. **Stage 4**: Run `docs/agent/inst.research.[LANG].yaml` workflow (NEW)
5. **Stage 5**: Run `docs/agent/inst.impl.[LANG].yaml` workflow (e.g., `inst.impl.ts.yaml` for TypeScript)

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
│   │   ├── mathematical-contracts.md    # Mathematical contracts for Stage 2
│   │   ├── package-research-methodology.md  # Stage 4 methodology
│   │   ├── impl.py.prompt.md    # Language-specific implementation
│   │   ├── impl.ts.prompt.md    # Language-specific implementation
│   │   └── common.md
│   ├── nl/                   # Natural language contracts (input)
│   │   ├── qi.v4.class.contracts.md
│   │   └── qi.v4.component.contracts.md
│   └── agent/               # Optional workflow automation (see docs/agent/)
│       └── README.md
├── agent/                     # Workflow automation files
│   ├── inst.formal.yaml
│   ├── inst.stage2.design.yaml      # Uses mathematical-contracts.md
│   ├── inst.research.[LANG].yaml   # NEW: Package research workflows  
│   └── inst.impl.[LANG].yaml
└── build/                     # Generated outputs from design process
    ├── objective/formal/      # Stage 1 output: Mathematical specs
    │   └── qi.v4.formal.spec.md
    ├── design/               # Stage 2 output: Design patterns
    │   └── qi.v4.design.analysis.md
    ├── research/             # Stage 4 output: Package research (NEW)
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