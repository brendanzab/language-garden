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

    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };

    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.opam-repository.follows = "opam-repository";
    };
  };

  outputs = { self, flake-utils, opam-nix, nixpkgs, ... }@inputs:
    # Construct an output set that supports a number of default systems
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        opam-lib = opam-nix.lib.${system};

        # Local packages, detected from the `*.opam` files found in `./opam/`.
        localPackagesQuery = builtins.mapAttrs (_: pkgs.lib.last)
          (opam-lib.listRepo (opam-lib.makeOpamRepo ./.));
        # Development packages, automatically added to the development shell.
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
        query = devPackagesQuery // {
          ## Force versions of certain packages here, e.g:
          ## - force the ocaml compiler to be taken from opam-repository:
          ocaml-base-compiler = "*";
          ## - or force the compiler to be taken from nixpkgs and be a certain version:
          # ocaml-system = "4.14.0";
          ## - or force ocamlfind to be a certain version:
          # ocamlfind = "1.9.2";
        };

        overlay = final: prev: {
          lang-shader-graphics = prev.lang-shader-graphics.overrideAttrs (previousAttrs: {
            nativeCheckInputs = [
              # For `lang-shader-graphics/test/dune`
              pkgs.netpbm
            ];
          });
        };

        buildOpamProject = options:
          (opam-lib.buildOpamProject' options ./. query).overrideScope' overlay;

        legacyPackages = buildOpamProject { };
        devPackages = pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) legacyPackages;
        packages = pkgs.lib.getAttrs (builtins.attrNames localPackagesQuery) legacyPackages;

        legacyTestPackages = buildOpamProject { resolveArgs.with-test = true; };
        testPackages = pkgs.lib.getAttrs (builtins.attrNames localPackagesQuery) legacyTestPackages;
      in
      {
        # Exposed packages that can be built or run with `nix build` or
        # `nix run` respectively:
        #
        #     $ nix build .#<name>
        #     $ nix run .#<name> -- <args?>
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
        devShells.default = pkgs.mkShell {
          inputsFrom = builtins.attrValues testPackages;
          buildInputs = builtins.attrValues devPackages ++ [
            # You can add packages from nixpkgs here
          ];
        };
      });
}
