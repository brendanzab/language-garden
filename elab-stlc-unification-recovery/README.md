# Elaboration with error recovery

- Extends [**elab-stlc-unification**](../elab-stlc-unification) (+ error recovery)

---

This extends [elab-stlc-unification](../elab-stlc-unification) with support for
recovering from errors encountered during type checking. This is important for
allowing programmers to fix multiple errors before re-checking their program.

To implement error recovery we add `Reported_error` sentinels to types and terms
in our core language which are inserted whenever we run into errors during
elaboration.

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
error: mismatched types:
  expected: Int
  found: Bool
  ┌─ <stdin>:1:15
  │
1 │ let x : Int := true;
  │                ^^^^

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

error: mismatched types:
  expected: Int
  found: Bool
  ┌─ <stdin>:6:4
  │
6 │ x + false
  │     ^^^^^
```

</details>

## Project overview

| Module        | Description                             |
| ------------- | --------------------------------------- |
| [`Main`]      | Command line interface                  |
| [`Lexer`]     | Lexer for the surface language          |
| [`Parser`]    | Parser for the surface language         |
| [`Surface`]   | Surface language, including elaboration |
| [`Core`]      | Core language, including normalisation, unification, and pretty printing |
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

- Vladimir Keleshev, [Advanced Error Handling in OCaml](https://keleshev.com/advanced-error-handling-in-ocaml)

  I tried using this approach, but while it’s nice that it doesn’t pollute the
  core language with error sentinels I feel like but it ends up being too
  conservative in practice: for instance if you find an error in the definition
  of a let binding you won’t be able to continue checking inside the body.

## Examples

More examples can be found in [`tests.t`](tests.t).
