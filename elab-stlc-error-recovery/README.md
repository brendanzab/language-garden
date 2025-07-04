# Elaboration with error recovery

- Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional) (+ error recovery)

---

This extends [elab-stlc-bidirectional](../elab-stlc-bidirectional) with support for
recovering from errors encountered during type checking. This is important for
allowing programmers to fix multiple errors before re-checking their program.

Rather than throwing exceptions (as has been done in previous projects) we
instead update a list of errors in the elaboration context, returning error
sentinels as stand-ins for types and terms as required.

<!-- $MDX file=examples/readme.txt -->
```
let x : Int := true;

let example : Foo -> Int :=
  fun x y => x + z;

x + false
```

<details open="true">
<summary>Elaboration output</summary>

<!-- $MDX file=examples/readme.stderr -->
```
error: mismatched types
  ┌─ <stdin>:1:15
  │
1 │ let x : Int := true;
  │                ^^^^
  = expected: Int
       found: Bool

error: unbound type `Foo`
  ┌─ <stdin>:3:14
  │
3 │ let example : Foo -> Int :=
  │               ^^^

error: unexpected parameter
  ┌─ <stdin>:4:8
  │
4 │   fun x y => x + z;
  │         ^

error: unbound name `z`
  ┌─ <stdin>:4:17
  │
4 │   fun x y => x + z;
  │                  ^

error: mismatched types
  ┌─ <stdin>:6:4
  │
6 │ x + false
  │     ^^^^^
  = expected: Int
       found: Bool
```

</details>

## Limitations and future work

This approach was inspired by [`rustc`’s error recovery](https://rustc-dev-guide.rust-lang.org/ty.html#type-errors),
and seems to work well enough in the testing I’ve done. That said, it’s an
engineering-driven solution, and it might be worth exploring [Hazel’s error recovery](https://doi.org/10.1145/3632910)
in the future, which seems to be more carefully considered from a formal perspective.
This would become more important if we decided to allow programmers to run
partially broken programs, e.g. in a similar way to GHC’s [deferred type errors](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/defer_type_errors.html).

## Project overview

| Module        | Description                             |
| ------------- | --------------------------------------- |
| [`Main`]      | Command line interface                  |
| [`Lexer`]     | Lexer for the surface language          |
| [`Parser`]    | Parser for the surface language         |
| [`Surface`]   | Surface language, including elaboration |
| [`Core`]      | Core language, including normalisation, and pretty printing |
| [`Prim`]      | Primitive operations                    |

[`Main`]: ./main.ml
[`Lexer`]: ./lexer.ml
[`Parser`]: ./parser.mly
[`Surface`]: ./surface.ml
[`Core`]: ./core.ml
[`Prim`]: ./prim.ml

## Resources

- [The `ty` module: representing types - Type errors](https://rustc-dev-guide.rust-lang.org/ty.html#type-errors),
  Rust Compiler Development Guide

- Eric Zhao et. al. [Total Type Error Localization and Recovery with Holes](https://doi.org/10.1145/3632910), 2024.

  This paper documents a similar approach that is used in the [Hazel](https://hazel.org/)
  language. I’ve adapted some of the ideas from the paper in this project, in
  particular around the elaboration of if expressions and functions, but I’ve
  not yet read it fully yet and there might be some differences. One major
  difference is that I currently elaborate some terms to `Reported_error`
  sentinels, as opposed to preserving erroneous terms inside holes.

- Vladimir Keleshev, [Advanced Error Handling in OCaml](https://keleshev.com/advanced-error-handling-in-ocaml)

  I tried using this approach, but while it’s nice that it doesn’t pollute the
  core language with error sentinels I feel like but it ends up being too
  conservative in practice: for instance if you find an error in the definition
  of a let binding you won’t be able to continue checking inside the body.

## Examples

More examples can be found in [`tests.t`](tests.t).
