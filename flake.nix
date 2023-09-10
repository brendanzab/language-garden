{
  description = "A garden of toy programming language implementations";

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

  outputs = { self, systems, opam-nix, nixpkgs, ... }@inputs:
    let
      # Iterate over each system, configured via the `systems` input.
      eachSystem = nixpkgs.lib.genAttrs (import systems);

      # Local packages, detected from the package definition files in `./opam/`.
      localPackagesQuery = eachSystem (system:
        let
          opam-lib = opam-nix.lib.${system};
        in
        nixpkgs.lib.mapAttrs (_: nixpkgs.lib.last)
          (opam-lib.listRepo (opam-lib.makeOpamRepo ./.)));

      # Development package versions.
      devPackagesQuery = {
        ocaml-lsp-server = "*";
        # FIXME: ocamlformat 0.26.0 fails to build with:
        #
        #     File "lib/dune", line 39, characters 2-16:
        #     39 |   ocp-indent.lib
        #            ^^^^^^^^^^^^^^
        #     Error: Library "ocp-indent.lib" not found.
        #     -> required by library "ocamlformat-lib" in _build/default/lib
        #     -> required by _build/default/META.ocamlformat-lib
        #     -> required by _build/install/default/lib/ocamlformat-lib/META
        #     -> required by _build/default/ocamlformat-lib.install
        #     -> required by alias install
        ocamlformat = "0.25.1";
        utop = "*";
      };

      # Development package versions, along with the base compiler tools, used
      # when building the opam project with `opam-nix`.
      query = devPackagesQuery // {
        # Force the ocaml compiler to be taken from opam-repository:
        ocaml-base-compiler = "5.0.0"; # Fix for opam-nix choosing 5.0.0~rc1 over 5.0.0
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
        (opam-nix.lib.${system}.buildOpamProject' options ./. query).overrideScope'
          overlay.${system};

      legacyPackages = eachSystem (system:
        buildOpamProject system { });

      packages = eachSystem (system:
        nixpkgs.lib.getAttrs
          (nixpkgs.lib.attrNames localPackagesQuery.${system})
          legacyPackages.${system});

      devPackages = eachSystem (system:
        nixpkgs.lib.getAttrs
          (nixpkgs.lib.attrNames devPackagesQuery)
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
            nixpkgs.lib.getAttrs
              (nixpkgs.lib.attrNames localPackagesQuery.${system})
              (buildOpamProject system {
                resolveArgs.dev = true;
                resolveArgs.with-doc = true;
                resolveArgs.with-test = true;
              });
        in
        {
          default = pkgs.mkShell {
            inputsFrom = nixpkgs.lib.attrValues packages;
            buildInputs = nixpkgs.lib.attrValues devPackages.${system} ++ [
              # Packages from NixPkgs can be added here
              pkgs.nixpkgs-fmt
            ];
          };
        });
    };
}
