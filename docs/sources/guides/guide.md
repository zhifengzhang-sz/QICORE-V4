# QiCore v4.0 Development Guide

> **Simple 4-Stage Transformation Process**  
> **Purpose**: Transform natural language specifications into production-ready code  
> **From**: Human-written contracts **To**: Working implementations

## The 4-Stage Process (Enhanced)

### **Stage 1: Natural Language → Mathematical Formalization**
- **Use**: `guides/formal.prompt.md` + `guides/common.md`
- **Input**: 
  - `sources/nl/qi.v4.class.contracts.md` 
  - `sources/nl/qi.v4.component.contracts.md`
- **Output**: `build/objective/formal/qi.v4.formal.spec.md`
- **Optional**: `agent/build/inst.formal.yaml` (workflow automation)

### **Stage 2: Mathematical → Design Patterns**  
- **Use**: `guides/design.prompt.md` + `guides/common.md`
- **Input**: `build/objective/formal/qi.v4.formal.spec.md`
- **Output**: `build/design/qi.v4.design.analysis.md`
- **Optional**: `agent/build/inst.design.yaml` (workflow automation)

### **Stage 3: Package Research → Wrapper Design (NEW)**
- **Use**: `guides/package-research-methodology.md` + `guides/common.md`
- **Input**: `build/design/qi.v4.design.analysis.md` + Target Language
- **Process**: Comprehensive package research → Evidence-based selection → Wrapper specification
- **Output**: `build/research/qi.v4.[LANG].packages.md` + `build/research/qi.v4.[LANG].wrappers.md`
- **Purpose**: Bridge mathematical contracts to production-ready packages

### **Stage 4: Design + Research → Code Templates**
- **Use**: `guides/impl.[LANG].prompt.md` + `guides/common.md`
- **Input**: 
  - `build/design/qi.v4.design.analysis.md`
  - `build/research/qi.v4.[LANG].packages.md`
  - `build/research/qi.v4.[LANG].wrappers.md`
- **Target Language**: Specify one of: `ts` (TypeScript), `py` (Python), `rs` (Rust), `hs` (Haskell), `go` (Go)
- **Output**: 
  - `build/impl/qi.v4.[LANG].template.md` + `build/impl/qi.v4.[LANG].impl.md`
  - Example: For TypeScript → `build/impl/qi.v4.ts.template.md` + `build/impl/qi.v4.ts.impl.md`
- **Optional**: `agent/build/inst.impl.[LANG].yaml` (workflow automation)

## Essential Files

**Transformation Methodologies:**
- `guides/formal.prompt.md` - Stage 1 transformation methodology
- `guides/design.prompt.md` - Stage 2 transformation methodology
- `guides/package-research-methodology.md` - Stage 3 package research methodology (NEW)
- `guides/impl.[LANG].prompt.md` - Stage 4 language-specific implementation (e.g., `impl.py.prompt.md`)
- `guides/common.md` - Shared mathematical foundations

**Workflow Automation (Optional):**
- `agent/build/inst.formal.yaml` - Stage 1 workflow
- `agent/build/inst.design.yaml` - Stage 2 workflow
- `agent/build/inst.research.[LANG].yaml` - Stage 3 package research workflow (NEW)
- `agent/build/inst.impl.[LANG].yaml` - Stage 4 implementation workflow

## Usage Instructions

### Manual Process
1. **Stage 1**: Use `formal.prompt.md` to transform your NL contracts to mathematical spec
2. **Stage 2**: Use `design.prompt.md` to transform the formal spec to design patterns
3. **Stage 3**: Use `package-research-methodology.md` to research packages and design wrappers
4. **Stage 4**: Use `impl.[LANG].prompt.md` with your target language code (`ts`, `py`, `rs`, `hs`, or `go`)

### Automated Process (Optional)
1. **Stage 1**: Run `inst.formal.yaml` workflow
2. **Stage 2**: Run `inst.design.yaml` workflow
3. **Stage 3**: Run `inst.research.[LANG].yaml` workflow (NEW)
4. **Stage 4**: Run `inst.impl.[LANG].yaml` workflow (e.g., `inst.impl.ts.yaml` for TypeScript)

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
│   │   ├── package-research-methodology.md  # NEW: Stage 3 methodology
│   │   ├── impl.py.prompt.md    # Language-specific implementation
│   │   ├── impl.ts.prompt.md    # Language-specific implementation
│   │   └── common.md
│   ├── nl/                   # Natural language contracts (input)
│   │   ├── qi.v4.class.contracts.md
│   │   └── qi.v4.component.contracts.md
│   └── agent/build/          # Optional workflow automation
│       ├── inst.formal.yaml
│       ├── inst.design.yaml
│       ├── inst.research.py.yaml   # NEW: Package research workflows
│       ├── inst.research.ts.yaml   # NEW: Package research workflows
│       └── inst.impl.[LANG].yaml
└── build/                     # Generated outputs from design process
    ├── objective/formal/      # Stage 1 output: Mathematical specs
    │   └── qi.v4.formal.spec.md
    ├── design/               # Stage 2 output: Design patterns
    │   └── qi.v4.design.analysis.md
    ├── research/             # Stage 3 output: Package research (NEW)
    │   ├── qi.v4.py.packages.md
    │   ├── qi.v4.py.wrappers.md
    │   ├── qi.v4.ts.packages.md
    │   └── qi.v4.ts.wrappers.md
    └── impl/                 # Stage 4 output: Implementation templates
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

## The Missing Link: Stage 3 Package Research

**Previously Missing**: The systematic process for transforming mathematical contracts into language-specific implementations using proven packages.

**Now Included**: Comprehensive package research methodology that:
- **Researches** high-quality packages with evidence-based selection
- **Designs** QICORE-V4 wrappers around proven packages  
- **Bridges** mathematical contracts to practical implementation
- **Ensures** production-ready code built on solid foundations

This fills the critical gap between **theoretical contracts** and **practical implementation**.

## Dependencies and References

- **Input**: [Class Contracts](../nl/qi.v4.class.contracts.md), [Component Contracts](../nl/qi.v4.component.contracts.md)
- **Methodology**: [Common Foundations](common.md), [Package Research](package-research-methodology.md)
- **Mathematics**: [Common Foundations](common.md)
- **Process**: [Workflow YAML Files](../agent/build/)
- **Used By**: Any project requiring systematic specification-to-implementation transformation with production-ready packages 