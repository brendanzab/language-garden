# Language garden 🌱

Some experiments in programming language implementation.

- [**arith**](./arith/): A compiler from tree-based arithmetic expressions to a stack based IR, implemented in OCaml.
- [**mltt**](./mltt/): An elaborator for Martin-Löf type theory with dependent records, implemented in OCaml.
- [**mltt-small**](./mltt-mall/): An tiny elaborator for Martin-Löf type theory, implemented in OCaml.
- [**record-patching**](./record-patching/): An elaborator of an intensional type theory with singletons and record patching, implemented in OCaml.

## Development setup

When using [Nix flakes](https://nixos.wiki/wiki/Flakes) and [nix-direnv](https://github.com/nix-community/nix-direnv/):

```sh
echo "use flake" > .envrc
direnv allow
```
