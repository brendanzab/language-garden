{
  description = "Programming language garden";

  # Flake dependency specification
  #
  # To update all flake inputs:
  #
  #     $ nix flake update --commit-lockfile
  #
  # To update individual flake inputs:
  #
  #     $ nix flake lock --update-input <input> ... --commit-lockfile
  #
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    # Construct an output set that supports a number of default systems
    flake-utils.lib.eachDefaultSystem (system:
      let
        legacyPackages = nixpkgs.legacyPackages.${system};
        ocamlPackages = legacyPackages.ocamlPackages;
        lib = legacyPackages.lib;
      in
      {
        # Exposed packages that can be built or run with `nix build` or
        # `nix run` respectively:
        #
        #     $ nix build .#<name>
        #     $ nix run .#<name> -- <args?>
        #
        packages = {
          # Compilation

          compile-arith = ocamlPackages.buildDunePackage {
            pname = "compile-arith";
            version = "0";
            src = ./.;
            duneVersion = "3";

            nativeBuildInputs = [
              ocamlPackages.menhir
            ];

            buildInputs = [
              ocamlPackages.cmdliner
            ];

            nativeCheckInputs = [
              ocamlPackages.mdx.bin
            ];

            checkInputs = [
              ocamlPackages.alcotest
              ocamlPackages.mdx
              ocamlPackages.qcheck
              ocamlPackages.qcheck-core
              ocamlPackages.qcheck-alcotest
            ];
          };

          compile-arithcond = ocamlPackages.buildDunePackage {
            pname = "compile-arithcond";
            version = "0";
            src = ./.;
            duneVersion = "3";

            nativeBuildInputs = [
              ocamlPackages.menhir
            ];

            buildInputs = [
              ocamlPackages.cmdliner
            ];

            nativeCheckInputs = [
              ocamlPackages.mdx.bin
            ];

            checkInputs = [
              ocamlPackages.alcotest
              ocamlPackages.mdx
              ocamlPackages.qcheck
              ocamlPackages.qcheck-core
              ocamlPackages.qcheck-alcotest
            ];
          };

          compile-closure-conv = ocamlPackages.buildDunePackage {
            pname = "compile-closure-conv";
            version = "0";
            src = ./.;
            duneVersion = "3";

            nativeBuildInputs = [
              ocamlPackages.menhir
            ];

            buildInputs = [
              ocamlPackages.cmdliner
            ];

            nativeCheckInputs = [
              ocamlPackages.mdx.bin
            ];

            checkInputs = [
              ocamlPackages.mdx
            ];
          };

          # Elaboration

          elab-dependent = ocamlPackages.buildDunePackage {
            pname = "elab-dependent";
            version = "0";
            src = ./.;
            duneVersion = "3";

            nativeBuildInputs = [
              ocamlPackages.menhir
            ];

            buildInputs = [
              ocamlPackages.cmdliner
            ];

            nativeCheckInputs = [
              ocamlPackages.mdx.bin
            ];

            checkInputs = [
              ocamlPackages.mdx
            ];
          };

          elab-record-patching = ocamlPackages.buildDunePackage {
            pname = "elab-record-patching";
            version = "0";
            src = ./.;
            duneVersion = "3";

            nativeBuildInputs = [
              ocamlPackages.menhir
            ];

            buildInputs = [
              ocamlPackages.cmdliner
            ];
          };

          # Languages

          lang-fractal-growth = ocamlPackages.buildDunePackage {
            pname = "lang-fractal-growth";
            version = "0";
            src = ./.;
            duneVersion = "3";

            buildInputs = [
              ocamlPackages.cmdliner
            ];

            nativeCheckInputs = [
              ocamlPackages.mdx.bin
            ];

            checkInputs = [
              ocamlPackages.mdx
            ];
          };

          lang-shader-graphics = ocamlPackages.buildDunePackage {
            pname = "lang-shader-graphics";
            version = "0";
            src = ./.;
            duneVersion = "3";

            buildInputs = [
              ocamlPackages.cmdliner
            ];

            nativeCheckInputs = [
              ocamlPackages.mdx.bin
              # For `lang-shader-graphics/test/dune`
              legacyPackages.netpbm
            ];

            checkInputs = [
              ocamlPackages.mdx
            ];
          };

          # WIP projects

          wip-compile-stratify = ocamlPackages.buildDunePackage {
            pname = "wip-compile-stratify";
            version = "0";
            src = ./.;
            duneVersion = "3";
          };

          wip-compile-uncurry = ocamlPackages.buildDunePackage {
            pname = "wip-compile-uncurry";
            version = "0";
            src = ./.;
            duneVersion = "3";
          };

          wip-elab-builtins = ocamlPackages.buildDunePackage {
            pname = "wip-elab-builtins";
            version = "0";
            src = ./.;
            duneVersion = "3";

            nativeBuildInputs = [
              ocamlPackages.menhir
            ];

            buildInputs = [
              ocamlPackages.cmdliner
            ];

            nativeCheckInputs = [
              ocamlPackages.mdx.bin
            ];

            checkInputs = [
              ocamlPackages.mdx
            ];
          };
        };

        # Development shells
        #
        #    $ nix develop .#<name>
        #    $ nix develop .#<name> --command dune build @test
        #
        # [Direnv](https://direnv.net/) is recommended for automatically loading
        # development environments in your shell. For example:
        #
        #    $ echo "use flake" > .envrc && direnv allow
        #    $ dune build @test
        #
        devShells = {
          default = legacyPackages.mkShell {
            nativeBuildInputs = [
              # Language formatters
              legacyPackages.nixpkgs-fmt
              legacyPackages.ocamlformat
              # For `dune build --watch ...`
              legacyPackages.fswatch
              # Editor tools
              ocamlPackages.ocaml-lsp
              ocamlPackages.ocamlformat-rpc-lib
              # Fancy REPL thing
              ocamlPackages.utop
            ];

            inputsFrom =
              lib.lists.map
                (p: p.overrideAttrs (_: {
                  # Ensure that the `nativeCheckInputs` and `checkInputs` are
                  # included in the development shell
                  doCheck = true;
                }))
                (lib.attrValues self.packages.${system});
          };
        };

        # Flake checks
        #
        #     $ nix flake check
        #
        checks = {
          # Check Nix formatting
          nixpkgs-fmt = legacyPackages.runCommand "check-nixpkgs-fmt"
            { nativeBuildInputs = [ legacyPackages.nixpkgs-fmt ]; }
            ''
              mkdir $out
              nixpkgs-fmt --check ${./flake.nix}
            '';
        }
        # Dune package tests
        // lib.mapAttrs
          (name: package:
            package.overrideAttrs (oldAttrs: {
              name = "check-${oldAttrs.name}";
              doCheck = true;
            }))
          self.packages.${system};
      });
}
