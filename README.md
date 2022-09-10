# Language garden 🌱

Some experiments in programming language implementation.

- [**compile-arith**](./compile-arith/): A compiler from tree-based arithmetic expressions to a stack machine.
- [**compile-arithcond**](./compile-arithcond/): A compiler from tree-based arithmetic and conditional expressions to a stack machine.
- [**elab-dependent**](./elab-dependent/): An tiny elaborator for a small dependently typed language.
- [**elab-record-patching**](./elab-record-patching/): An elaborator of a dependently typed language with singletons and record patching.

## Some useful resources

- [AndrasKovacs/elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/): Some excellent,
  high quality examples of implementing elaborators for dependently typed programming languages,
  demonstrating bidirectional type checking and normalisation-by-evaluation, and extensions to this.
- [jozefg/nbe-for-mltt](https://github.com/jozefg/nbe-for-mltt): An implementation of bidirectional
  typechecking and normalisation-by-evaluation for dependent types.

## Development setup

Using [Nix] is not required, but can be useful for setting up a development
shell with the packages and tools used in this project. With [Nix flakes]
enabled and [nix-direnv] installed, run:

```sh
echo "use flake" > .envrc
direnv allow
```

You’ll want to locally exclude the `.envrc`, or add it to your global gitignore.

[Nix]: https://nixos.org
[Nix flakes]: https://nixos.wiki/wiki/Flakes
[nix-direnv]: https://github.com/nix-community/nix-direnv
