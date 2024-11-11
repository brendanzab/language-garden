{
  description = "A garden of toy programming language implementations";

  # Flake dependency specification
  #
  # To update all flake inputs:
  #
  #     $ nix flake update --commit-lock-file
  #
  # To update individual flake inputs:
  #
  #     $ nix flake lock --update-input <input> ... --commit-lock-file
  #
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

    # Externally extensible flake systems. See <https://github.com/nix-systems/nix-systems>.
    systems.url = "github:nix-systems/default";

    # Follow the `systems` input in flakes that still use `flake-utils`.
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    # Main package repository for opam, the package manager of OCaml.
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };

    # Create nix derivations from opam package definitions.
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.opam-repository.follows = "opam-repository";
    };
  };

  outputs = { systems, opam-nix, nixpkgs, ... }:
    let
      # Nixpkgs library functions.
      lib = nixpkgs.lib;

      # Iterate over each system, configured via the `systems` input.
      eachSystem = lib.genAttrs (import systems);

      # Local packages, detected from the package definition files in `./opam/`.
      localPackagesQuery = eachSystem (system:
        let
          opam-lib = opam-nix.lib.${system};
        in
        lib.mapAttrs (_: lib.last)
          (opam-lib.listRepo (opam-lib.makeOpamRepo ./.)));

      # Development package versions.
      devPackagesQuery = {
        ocaml-lsp-server = "*";
        ocamlformat = "*";
        utop = "*";
      };

      # Development package versions, along with the base compiler tools, used
      # when building the opam project with `opam-nix`.
      allPackagesQuery = devPackagesQuery // {
        # Force the ocaml compiler to be taken from opam-repository:
        ocaml-base-compiler = "5.1.1"; # Fix for opam-nix choosing 5.1.1~rc1 over 5.1.1
      };

      # Package-specific derivation overrides.
      overlay = eachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        final: prev: {
          lang-shader-graphics = prev.lang-shader-graphics.overrideAttrs (previousAttrs: {
            nativeCheckInputs = [
              # For `lang-shader-graphics/test/dune`
              pkgs.netpbm
            ];
          });
        });

      buildOpamProject = system: options:
        (opam-nix.lib.${system}.buildOpamProject' options ./. allPackagesQuery).overrideScope
          overlay.${system};

      legacyPackages = eachSystem (system:
        buildOpamProject system { });

      packages = eachSystem (system: lib.getAttrs
        (lib.attrNames localPackagesQuery.${system})
        legacyPackages.${system});

      devPackages = eachSystem (system: lib.getAttrs
        (lib.attrNames devPackagesQuery)
        legacyPackages.${system});
    in
    {
      # Exposed packages that can be built or run with `nix build` or
      # `nix run` respectively:
      #
      #     $ nix build .#<name>
      #     $ nix run .#<name>
      #     $ nix run .#<name> -- <args>
      #
      inherit packages;

      # Used for nixpkgs packages, also accessible via `nix build`:
      #
      #     $ nix build .#<name>
      #
      inherit legacyPackages;

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
      devShells = eachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # Packages with development dependencies enabled
          packages =
            lib.getAttrs
              (lib.attrNames localPackagesQuery.${system})
              (buildOpamProject system {
                resolveArgs.with-doc = true;
                resolveArgs.with-test = true;
              });
        in
        {
          default = pkgs.mkShell {
            inputsFrom = lib.attrValues packages;
            buildInputs = lib.attrValues devPackages.${system} ++ [
              # Packages from NixPkgs can be added here
              pkgs.nixpkgs-fmt
            ];
          };
        });
    };
}
