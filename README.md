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

- [**elab-dependent**](./elab-dependent/):
  An elaborator for a small dependently typed lambda calculus.
- [**elab-dependent-sugar**](./elab-dependent-sugar/):
  An elaborator for a small dependently typed lambda calculus with syntactic sugar.
- [**elab-record-patching**](./elab-record-patching/):
  An elaborator of a dependently typed lambda calculus with singletons and record patching.
- [**elab-stlc-abstract**](./elab-stlc-abstract):
  An LCF-style elaborator that moves the construction of well-typed terms behind
  a trusted interface.
- [**elab-stlc-unification**](./elab-stlc-unification):
  An elaborator for a simply typed lambda calculus where type annotations can be omitted.

### Compilation

These are related to compilation. Mainly to stack-machines, but I’m interested
in exploring more approaches in the future, and other compilation passes
related to compiling functional programming languages.

- [**compile-arith**](./compile-arith/):
  Compiling arithmetic expressions to stack machine instructions and A-Normal Form.
- [**compile-arithcond**](./compile-arithcond/):
  Compiling arithmetic and conditional expressions to stack machine instructions and A-Normal Form.
- [**compile-arith-verified**](./compile-arith-verified/):
  A formally verified arithmetic expression compiler and decompiler in Lean 4.
- [**compile-closure-conv**](./compile-closure-conv):
  Typed closure conversion and lambda lifting for a simply typed lambda calculus
  with booleans and integers.

### Languages

Miscellaneous programming language experiments.

- [**lang-datalog**](./lang-datalog/):
  A simple Datalog interpreter.
- [**lang-fractal-growth**](./lang-fractal-growth/):
  Experiments with using grammars and rewriting systems to model fractal growth.
- [**lang-lc-interpreters**](./lang-lc-interpreters/):
  A comparison of lambda calculus interpreters using different approaches to
  name binding.
- [**lang-shader-graphics**](./lang-shader-graphics/):
  An embedded DSL for describing procedural graphics, based on signed distance
  functions. These can be rendered on the CPU or compiled to GLSL shaders.

### Work in progress projects

While most of the above projects need more work put into them, the following
projects need more work put into them and a more incomplete in comparison.

- [**wip-elab-builtins**](./wip-elab-builtins/):
  An elaborator that supports built-in types and operations.
- [**wip-compile-stratify**](./wip-compile-stratify/):
  Compiling a dependently typed lambda calculus into a stratified intermediate
  language.
- [**wip-compile-uncurry**](./wip-compile-uncurry/):
  Compiling single-parameter functions to multiparameter functions.

## Development setup

### With Nix

Using [Nix] is not required, but can be useful for setting up a development
shell with the packages and tools used in this project. With [Nix flakes]
enabled:

```sh
nix run .#arith -- compile --target=anf <<< "1 + 2 * 27"
```

[nix-direnv] can be used to load development tools into your shell
automatically. Once it’s installed, run the following commands to enable it in
the project directory:

```sh
echo "use flake" > .envrc
direnv allow
```

You’ll want to locally exclude the `.envrc`, or add it to your global gitignore.

After that, [dune] can be used to build, test, and run the projects:

```sh
dune build
dune test
dune exec arith -- compile --target=anf <<< "1 + 2 * 27"
```

[dune]: https://dune.build
[Nix]: https://nixos.org
[Nix flakes]: https://nixos.wiki/wiki/Flakes
[nix-direnv]: https://github.com/nix-community/nix-direnv

### With opam

Alternatively, [opam] package definitions are provided in the [`./opam`](./opam)
directory. They drive the Nix flake, so _should_ be up to date. I don’t use opam
however, so I’m not sure what the workflow is.

[opam]: opam.ocaml.org
