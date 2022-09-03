# Language garden 🌱

Some experiments in programming language implementation.

- [**compile-arith**](./compile/arith/): A compiler from tree-based arithmetic expressions to a stack based IR, implemented in OCaml.
- [**elab-dependant**](./elab/dependant/): An tiny elaborator for a small dependently typed language, implemented in OCaml.
- [**elab-record-patching**](./experiments/record-patching/): An elaborator of a dependently typed language with singletons and record patching, implemented in OCaml.

## Development setup

When using [Nix flakes](https://nixos.wiki/wiki/Flakes) and [nix-direnv](https://github.com/nix-community/nix-direnv/):

```sh
echo "use flake" > .envrc
direnv allow
```

## Some useful resources

- [AndrasKovacs/elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/): Some excellent,
  high quality examples of implementing elaborators for dependently typed programming languages,
  demonstrating bidirectional type checking and normalisation-by-evaluation, and extensions to this.
- [jozefg/nbe-for-mltt](https://github.com/jozefg/nbe-for-mltt): An implementation of bidirectional
  typechecking and normalisation-by-evaluation for dependent types.
