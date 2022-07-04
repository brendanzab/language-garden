{
  description = "Programming language garden";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        legacyPackages = nixpkgs.legacyPackages.${system};
        ocamlPackages = legacyPackages.ocamlPackages;
        lib = legacyPackages.lib;
      in
      {
        # Executed by `nix build .#<name>`
        packages =
          let
            version = "0";
            duneVersion = "3";
            src = ./.;
          in
          {
            arith = ocamlPackages.buildDunePackage {
              pname = "arith";
              inherit version duneVersion src;

              nativeBuildInputs = [
                ocamlPackages.menhir
              ];

              buildInputs = [
                ocamlPackages.pp
              ];
            };

            mltt = ocamlPackages.buildDunePackage {
              pname = "mltt";
              inherit version duneVersion src;

              nativeBuildInputs = [
                ocamlPackages.menhir
                ocamlPackages.ppx_string # for `mltt/tests/dune_inc.ml`
              ];

              buildInputs = [
                ocamlPackages.pp
              ];
            };

            mltt-small = ocamlPackages.buildDunePackage {
              pname = "mltt-small";
              inherit version duneVersion src;
            };

            record-patching = ocamlPackages.buildDunePackage {
              pname = "record-patching";
              inherit version duneVersion src;
            };
          };

        # Executed by `nix run .#<name> <args?>`
        apps = lib.mapAttrs
          (name: package: {
            type = "app";
            program = "${package}/bin/${name}";
          })
          self.packages.${system};

        # Used by `nix develop .#<name>`
        devShells = {
          default = legacyPackages.mkShell {
            nativeBuildInputs = [
              # for `dune build --watch ...`
              legacyPackages.fswatch
              # Editor tools
              ocamlPackages.ocaml-lsp
              ocamlPackages.ocamlformat-rpc-lib
              # Fancy REPL thing
              ocamlPackages.utop
            ];

            inputsFrom =
              lib.attrValues self.packages.${system};
          };
        };

        # For backwards compatibility: remove later
        devShell = self.devShells.${system}.default;

        # Executed by `nix flake check`
        checks = {
          # Check Nix formatting
          nixpkgs-fmt = legacyPackages.runCommand "check-nixpkgs-fmt"
            { nativeBuildInputs = [ legacyPackages.nixpkgs-fmt ]; }
            ''
              mkdir $out
              nixpkgs-fmt --check ${./flake.nix}
            '';
        }
        // lib.mapAttrs
          (name: package:
            package.overrideAttrs (oldAttrs: {
              doCheck = true;
            }))
          self.packages.${system};
      });
}
