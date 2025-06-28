# Modern Nix + Haskell Development Setup Documentation

## Overview

This document describes the modern Nix-based Haskell development environment that was established for the QiCore project, representing 2024-2025 best practices for reproducible Haskell development.

## Knowledge Foundation (Critical First Step)

### Web Research Updates
Before any technical setup, comprehensive web research was conducted on:

1. **Nix Flakes Ecosystem (2024-2025)**
   - `haskell-flake` vs traditional nixpkgs approaches
   - `flake-parts` architecture for modular setups
   - Modern flake.nix patterns and best practices
   - GHCup vs Nix for Haskell toolchain management

2. **Haskell Infrastructure Evolution**
   - GHC2024 language edition (vs GHC2021, Haskell2010)
   - Modern extension recommendations
   - Tooling ecosystem: fourmolu, hlint, HLS integration
   - Performance optimization techniques

### Key Discovery: Paradigm Shift
The research revealed that modern Haskell + Nix development has moved from:
- **Old**: Manual tool management + channel-based Nix
- **New**: Flake-based reproducible environments + language editions

## Nix Environment Architecture

### Flake-Based Setup

**flake.nix Structure:**
```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  
  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      
      perSystem = { config, system, pkgs, ... }: {
        # Development shell with comprehensive tooling
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            fourmolu
            hlint
            apply-refact
            ghcid
            hoogle
          ];
        };
        
        # Package outputs using callCabal2nix
        packages.default = pkgs.haskellPackages.callCabal2nix "qicore-base" ./. {};
      };
    };
}
```

### Environment Benefits

1. **Reproducibility**: Exact same toolchain across all machines
2. **Isolation**: Project-specific environments without global pollution
3. **Automation**: `nix develop` provides complete setup instantly
4. **Cross-platform**: Works on Linux, macOS, Windows (WSL)

## Haskell Project Infrastructure

### Modern Cabal Configuration

**qicore-base.cabal:**
```cabal
cabal-version: 3.4
name: qicore-base
version: 4.0.0
language: GHC2021

common warnings
  ghc-options:
    -Wall -Wcompat -Widentities
    -Wincomplete-record-updates -Wincomplete-uni-patterns
    -Wmissing-export-lists -Wmissing-signatures
    -Wredundant-constraints -Wmissing-deriving-strategies

library
  import: warnings
  exposed-modules:
    QiCore.Base
    QiCore.Base.Error
    QiCore.Base.Result
  default-extensions:
    LambdaCase, MultiWayIf, ViewPatterns
    RecordWildCards, DerivingVia, DeriveAnyClass
    TypeFamilies, OverloadedStrings
```

### Key Configuration Decisions

1. **GHC2021 Language Edition**: Modern curated extension set
2. **Comprehensive Warnings**: Catch issues early in development
3. **Common Stanzas**: Reusable configuration blocks
4. **Modern Extensions**: 15+ extensions beyond base GHC2021

## Quality Infrastructure (Critical Success Factor)

### Code Formatting: Fourmolu

**fourmolu.yaml:**
```yaml
column-limit: 100
indentation: 2
comma-style: leading
diff-friendly: true
newlines-between-decls: 1
haddock-style: single-line
let-style: inline
in-style: right-align
single-constraint-parens: never
```

### Linting: HLint

**hlint.yaml:**
```yaml
- warn: {name: "Use lambda-case"}
- warn: {name: "Use DerivingVia"}
- warn: {name: "Avoid partial functions"}
- ignore: {name: "Redundant do"}
```

### Editor Integration

**Haskell Language Server (HLS):**
- Automatic via Nix development shell
- Real-time error checking and suggestions
- Code completion and refactoring
- Integrated with VS Code, Emacs, Vim

## Development Workflow

### Daily Commands
```bash
# Enter development environment
nix develop

# Format code
fourmolu --mode inplace .

# Lint code
hlint .

# Build project
cabal build

# Run tests
cabal test

# Generate documentation
cabal haddock
```

### Git Integration

**Pre-commit hooks:**
```bash
#!/bin/sh
fourmolu --mode check .
hlint .
cabal build
```

## Performance Optimization Setup

### GHC Options
- **Development**: `-O0 -Wall` (fast compilation)
- **Production**: `-O2 -funbox-strict-fields` (optimized runtime)

### Optimization Patterns
- Strict fields in data types (`!`)
- INLINE pragmas for hot paths
- Modern deriving strategies
- Efficient collection operations

## Documentation Generation

### Haddock Configuration
- Hyperlinked source code
- Mathematical notation support
- Example code blocks
- Cross-module references

### Documentation Standards
- Module-level overview
- Function examples
- Design rationale
- Performance characteristics

## Comparison: Before vs After

### Before (Traditional Setup)
- Manual tool installation (stack, cabal, ghc)
- Version conflicts between projects
- Platform-specific setup issues
- Inconsistent development environments

### After (Modern Nix Setup)
- Declarative tool specifications
- Isolated project environments
- Reproducible builds across platforms
- Automatic dependency management

## Lessons Learned

### Critical Success Factors

1. **Knowledge Update First**: Researching 2024-2025 practices was essential
2. **Quality Infrastructure**: Formatter + linter + comprehensive warnings
3. **Language Modernization**: GHC2021 vs Haskell2010 made huge difference
4. **Comprehensive Tooling**: HLS + fourmolu + hlint integration

### What Didn't Work

1. **Channel-based Nix**: Too unpredictable for serious development
2. **Manual tooling**: Version conflicts and setup complexity
3. **Basic cabal files**: Missing modern warnings and extensions
4. **Minimal approach**: Going beyond requirements produced better results

## Future Improvements

### Potential Enhancements
- **Direnv integration**: Automatic environment loading
- **GitHub Actions**: CI/CD with Nix
- **Docker images**: Containerized deployment
- **Horizon Haskell**: Alternative package sets

### Monitoring Ecosystem
- Watch GHC release schedule
- Follow Nix flakes evolution
- Monitor tooling improvements
- Track language edition updates

## Conclusion

The modern Nix + Haskell setup represents a paradigm shift toward:
- **Reproducible development environments**
- **Quality-first tooling integration** 
- **Modern language feature adoption**
- **Comprehensive developer experience**

This infrastructure setup enabled the dramatic improvements in the Haskell implementation quality, proving that excellent tooling foundation is essential for excellent results.

The time invested in researching and setting up modern infrastructure paid dividends throughout the development process, making this approach highly recommended for new Haskell projects in 2024-2025. 