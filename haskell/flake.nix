{
  description = "QiCore Haskell Project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      
      perSystem = { self', system, lib, config, pkgs, ... }: 
      let
        # Configure nixpkgs to allow broken packages and unfree software
        pkgs' = import inputs.nixpkgs {
          inherit system;
          config = {
            allowBroken = true;
            allowUnfree = true;
          };
        };
        
        # Define our Haskell environment with GHC 9.8
        hsPkgs = pkgs'.haskell.packages.ghc98.override {
          overrides = self: super: {
            # Add your project package
            qicore-base = self.callCabal2nix "qicore-base" ./. { };
          };
        };
      in
      {
        # Make the enhanced pkgs available  
        _module.args.pkgs = pkgs';
        _module.args.hsPkgs = hsPkgs;

        # Modern Haskell packages output
        packages = {
          # Main package from cabal file
          qicore-base = hsPkgs.qicore-base;
          default = hsPkgs.qicore-base;
        };

        # Modern apps output (if you have executables)
        apps = {
          # If your .cabal file defines executables, they'll be available here
          # qicore-base = {
          #   type = "app";
          #   program = "${config.packages.qicore-base}/bin/qicore-base";
          # };
        };

        # Modern development shell with comprehensive tooling
        devShells.default = hsPkgs.shellFor {
          packages = p: [ p.qicore-base ];
          
          # Enable Hoogle documentation
          withHoogle = true;
          
          # Comprehensive development tooling
          nativeBuildInputs = with pkgs'; [
            # Modern Haskell toolchain
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server
            hsPkgs.ghcid  # Fast recompilation
            
            # Code quality tools
            hsPkgs.fourmolu      # Modern formatter
            hsPkgs.hlint         # Linter
            hsPkgs.apply-refact  # Auto-apply hlint
            hsPkgs.retrie        # Refactoring tool
            
            # Documentation and exploration
            hsPkgs.hoogle        # Type search
            # hsPkgs.haddock     # Removed due to broken package
            
            # Cabal tooling
            hsPkgs.cabal-fmt     # Format .cabal files
            hsPkgs.cabal2nix     # Generate Nix expressions
            
            # Development utilities
            git
            curl
            jq
            
            # Nix tooling
            nil                    # Nix LSP
            nixpkgs-fmt           # Nix formatter
            
            # Shell
            zsh
            
            # Optional: Database tools (uncomment if needed)
            # postgresql
            # sqlite
            
            # Optional: Documentation (uncomment if needed)
            # pandoc
          ];

          shellHook = ''
            echo "🚀 QiCore Haskell Development Environment (Modern)"
            echo "📦 GHC: $(ghc --version)"
            echo "📦 Cabal: $(cabal --version)"
            echo "🔧 HLS: $(haskell-language-server --version 2>/dev/null || echo 'Available')"
            echo ""
            echo "🎯 Available commands:"
            echo "  cabal build           - Build the project"
            echo "  cabal test            - Run tests"
            echo "  cabal repl            - Start GHCi with project loaded"
            echo "  cabal run <exe>       - Run executable (if any)"
            echo ""
            echo "🔧 Code quality tools:"
            echo "  fourmolu -i .         - Format all Haskell files"
            echo "  hlint .               - Lint code for suggestions"
            echo "  apply-refact --inplace \$(hlint . --serialise)"
            echo "                        - Auto-apply hlint suggestions"
            echo "  ghcid                 - Fast rebuild on file changes"
            echo ""
            echo "📚 Documentation:"
            echo "  hoogle <query>        - Search for functions by type"
            echo "  cabal haddock         - Generate documentation"
            echo ""
            echo "🔧 Package management:"
            echo "  cabal-fmt -i *.cabal  - Format cabal files"
            echo ""
            echo "📝 To add dependencies: Edit qicore-base.cabal and run 'cabal build'"
          '';
        };

        # Minimal dev shell (fallback)
        devShells.minimal = pkgs'.mkShell {
          buildInputs = with pkgs'; [
            haskell.compiler.ghc98
            cabal-install
            git
          ];
          shellHook = ''
            echo "🚀 Minimal Haskell Environment"
            echo "📦 GHC: $(ghc --version)"
            echo "📦 Cabal: $(cabal --version)"
          '';
        };

        # HLS-only shell (for IDE integration)
        devShells.hls = hsPkgs.shellFor {
          packages = p: [ p.qicore-base ];
          nativeBuildInputs = with pkgs'; [
            hsPkgs.haskell-language-server
            git
          ];
          shellHook = ''
            echo "🔧 HLS-focused Environment"
            echo "🔧 HLS: $(haskell-language-server --version 2>/dev/null || echo 'Available')"
          '';
        };

        # Checks for CI/CD
        checks = {
          build = config.packages.qicore-base;
          # Add more checks as needed
          # test = hsPkgs.qicore-base.checks;
        };
      };
    };
}