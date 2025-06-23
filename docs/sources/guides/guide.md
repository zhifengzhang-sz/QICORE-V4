# QiCore v4.0 Development Guide

> **Simple 3-Stage Transformation Process**  
> **Purpose**: Transform natural language specifications into production-ready code  
> **From**: Human-written contracts **To**: Working implementations

## The 3-Stage Process

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

### **Stage 3: Design Patterns → Code Templates**
- **Use**: `guides/impl.prompt.md` + `guides/common.md`
- **Input**: `build/design/qi.v4.design.analysis.md` 
- **Target Language**: Specify one of: `ts` (TypeScript), `py` (Python), `rs` (Rust), `hs` (Haskell), `go` (Go)
- **Output**: 
  - `build/impl/qi.v4.[LANG].template.md` + `build/impl/qi.v4.[LANG].impl.md`
  - Example: For TypeScript → `build/impl/qi.v4.ts.template.md` + `build/impl/qi.v4.ts.impl.md`
- **Optional**: `agent/build/inst.impl.[LANG].yaml` (workflow automation)

## Essential Files

**Transformation Methodologies:**
- `guides/formal.prompt.md` - Stage 1 transformation methodology
- `guides/design.prompt.md` - Stage 2 transformation methodology  
- `guides/impl.prompt.md` - Stage 3 transformation methodology
- `guides/common.md` - Shared mathematical foundations

**Workflow Automation (Optional):**
- `agent/build/inst.formal.yaml` - Stage 1 workflow
- `agent/build/inst.design.yaml` - Stage 2 workflow
- `agent/build/inst.impl.ts.yaml` - Stage 3 TypeScript workflow

## Usage Instructions

### Manual Process
1. **Stage 1**: Use `formal.prompt.md` to transform your NL contracts to mathematical spec
2. **Stage 2**: Use `design.prompt.md` to transform the formal spec to design patterns
3. **Stage 3**: Use `impl.prompt.md` with your target language code (`ts`, `py`, `rs`, `hs`, or `go`)

### Automated Process (Optional)
1. **Stage 1**: Run `inst.formal.yaml` workflow
2. **Stage 2**: Run `inst.design.yaml` workflow  
3. **Stage 3**: Run `inst.impl.[LANG].yaml` workflow (e.g., `inst.impl.ts.yaml` for TypeScript)

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
│   │   ├── impl.prompt.md
│   │   └── common.md
│   ├── nl/                   # Natural language contracts (input)
│   │   ├── qi.v4.class.contracts.md
│   │   └── qi.v4.component.contracts.md
│   └── agent/build/          # Optional workflow automation
│       ├── inst.formal.yaml
│       ├── inst.design.yaml
│       └── inst.impl.ts.yaml
└── build/                     # Generated outputs from design process
    ├── objective/formal/      # Stage 1 output: Mathematical specs
    │   └── qi.v4.formal.spec.md
    ├── design/               # Stage 2 output: Design patterns
    │   └── qi.v4.design.analysis.md
    └── impl/                 # Stage 3 output: Implementation templates
        ├── qi.v4.ts.template.md
        ├── qi.v4.ts.impl.md
        └── [other languages...]
```

## Success Criteria

- ✅ Natural language specifications transform systematically into working code
- ✅ Mathematical properties preserved through all stages  
- ✅ Cross-language behavioral consistency achieved
- ✅ All operations implemented with complete coverage
- ✅ Component architecture maintained throughout

## Next Steps

1. Start with your natural language contracts in `sources/nl/`
2. Run Stage 1 and 2 to get design patterns
3. **Choose your target language**: `ts`, `py`, `rs`, `hs`, or `go`
4. Run Stage 3 with your chosen language code
5. Get language-specific templates: `build/impl/qi.v4.[LANG].template.md` + `build/impl/qi.v4.[LANG].impl.md`

## Example: TypeScript Workflow

1. **Stage 1**: Generate `build/objective/formal/qi.v4.formal.spec.md`
2. **Stage 2**: Generate `build/design/qi.v4.design.analysis.md`  
3. **Stage 3**: Specify `ts` → Get `build/impl/qi.v4.ts.template.md` + `build/impl/qi.v4.ts.impl.md`

## Dependencies and References

- **Input**: [Class Contracts](../nl/qi.v4.class.contracts.md), [Component Contracts](../nl/qi.v4.component.contracts.md)
- **Methodology**: [Common Foundations](common.md)
- **Mathematics**: [Common Foundations](common.md)
- **Process**: [Workflow YAML Files](../agent/build/)
- **Used By**: Any project requiring systematic specification-to-implementation transformation 