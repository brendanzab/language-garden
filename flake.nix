{
  description = "Programming language garden";

  inputs = {
    # Utilities for writing flakes
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Legacy packages that have not been converted to flakes
        legacyPackages = nixpkgs.legacyPackages.${system};
        # OCaml packages available on nixpkgs
        ocamlPackages = legacyPackages.ocamlPackages;
      in
      {
        devShell = legacyPackages.mkShell {
          nativeBuildInputs = [
            ocamlPackages.dune_2
            ocamlPackages.menhir
            ocamlPackages.ocaml
            ocamlPackages.findlib

            # for editor support
            legacyPackages.fswatch # for `dune build --watch ...`
            ocamlPackages.ocamlformat-rpc-lib
            ocamlPackages.ocaml-lsp
          ];

          buildInputs = [
            ocamlPackages.angstrom
            ocamlPackages.pp
            ocamlPackages.stdio
          ];
        };
      });
}
