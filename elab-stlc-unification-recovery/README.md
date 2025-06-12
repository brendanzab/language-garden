# Simply typed lambda calculus with unification

- Extends [**elab-stlc-unification**](../elab-stlc-unification) (+ error recovery)

---

This extends [elab-stlc-unification](../elab-stlc-unification) with support for
reporting multiple errors during type checking. This is important for real-world
programming language implementations, as we don’t want our type checkers to stop
on the first error, potentially hiding other errors.

<!-- $MDX file=examples/readme.txt -->
```
let x : Int := true;

let example-1 : Foo -> Int := 3;
let example-2 : Int -> Int :=
  fun x y x => x;

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
  ┌─ <stdin>:3:16
  │
3 │ let example-1 : Foo -> Int := 3;
  │                 ^^^
error: mismatched types:
  expected: ?0 -> Int
  found: Int
  ┌─ <stdin>:3:30
  │
3 │ let example-1 : Foo -> Int := 3;
  │                               ^
error: unexpected parameter
  ┌─ <stdin>:5:8
  │
5 │   fun x y x => x;
  │         ^
error: unexpected parameter
  ┌─ <stdin>:5:10
  │
5 │   fun x y x => x;
  │           ^
error: mismatched types:
  expected: Int
  found: Bool
  ┌─ <stdin>:7:4
  │
7 │ x + false
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

## Examples

More examples can be found in [`tests.t`](tests.t).
