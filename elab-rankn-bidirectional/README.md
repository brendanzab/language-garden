# Bidirectional type checking for System F

Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional).

This implements a bidirectional elaborator for a rank-n polymorphic lambda calculus.

## Project overview

| Module        | Description                             |
| ------------- | --------------------------------------- |
| [`Main`]      | Command line interface                  |
| [`Lexer`]     | Lexer for the surface language          |
| [`Parser`]    | Parser for the surface language         |
| [`Surface`]   | Surface language, including elaboration |
| [`Core`]      | Core language, including normalisation, and pretty printing |
| [`Prim`]      | Primitive operations                    |

[`Main`]: ./Main.ml
[`Lexer`]: ./Lexer.ml
[`Parser`]: ./Parser.mly
[`Surface`]: ./Surface.ml
[`Core`]: ./Core.ml
[`Prim`]: ./Prim.ml

## Examples

```sh
$ rankn-bidirectional elab <<< "fun x => x + 2"
<input>:1:4: ambiguous parameter type
[1]
```

```sh
$ rankn-bidirectional elab <<< "fun (x : Int) => x + 2"
fun (x : Int) => #int-add -x 2 : Int -> Int
```

```sh
$ rankn-bidirectional elab <<< "(fun x f => f x * x) : Int -> (Int -> Int) -> Int"
fun (x : Int) => fun (f : Int -> Int) => #int-mul -(f x) x :
  Int -> (Int -> Int) -> Int
```

More examples can be found in [`tests.t`](tests.t).
