# Simply typed lambda calculus with unification

- Extends [**elab-stlc-unification**](../elab-stlc-unification) (+ error recovery)

---

This extends [elab-stlc-unification](../elab-stlc-unification) with support for
reporting multiple errors during type checking. This is important for real-world
programming language implementations, as we don’t want our type checkers to stop
on the first error, potentially hiding other errors until the program is
re-checked.

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
