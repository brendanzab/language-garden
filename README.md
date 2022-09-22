# Language garden 🌱

Some toy programming language implementations, mostly implemented in OCaml.

These projects are mostly my attempt to understand different techniques and
approaches to implementing programming languages. Perhaps from these seedlings
something new and interesting might germinate?

## Implementations

### Elaboration

Elaboration is an approach to implementing language front-ends where a complicated,
user friendly _surface language_ is type checked and lowered to a simpler, typed
_core language_. This approach to type checking is particularly popular and
useful for implementing dependently typed programming languages, but is more
widely applicable as well.

- [**elab-dependent**](./elab-dependent/): An elaborator for a small dependently typed lambda calculus.
- [**elab-record-patching**](./elab-record-patching/): An elaborator of a dependently typed lambda calculus with singletons and record patching.
- [**wip-elab-dependent**](./wip-elab-dependent): An older elaborator for dependent types, will probably be deleted in the future.

### Compilation

These are related to compilation. Mainly to stack-machines, but I’m interested
in exploring more approaches in the future, and other compilation passes
related to compiling functional programming languages.

- [**compile-arith**](./compile-arith/): A compiler from tree-based arithmetic expressions to a stack machine.
- [**compile-arithcond**](./compile-arithcond/): A compiler from tree-based arithmetic and conditional expressions to a stack machine.
- [**compile-arith-verified**](./compile-arith-verified/): A formally verified arithmetic expression compiler and decompiler in Lean 4.
- [**compile-sdf**](./compile-sdf/): An embedded DSL for building signed distance functions, compiling them to GLSL shaders.

## Development setup

Using [Nix] is not required, but can be useful for setting up a development
shell with the packages and tools used in this project. With [Nix flakes]
enabled and [nix-direnv] installed, run:

```sh
echo "use flake" > .envrc
direnv allow
```

You’ll want to locally exclude the `.envrc`, or add it to your global gitignore.

After that, dune can be used to build, test, and run the projects:

```sh
dune build
dune test
dune exec compile-arith <<< "1 + 2 * 27"
```

Alternatively, opam files are provided (but might be broken).

[Nix]: https://nixos.org
[Nix flakes]: https://nixos.wiki/wiki/Flakes
[nix-direnv]: https://github.com/nix-community/nix-direnv
