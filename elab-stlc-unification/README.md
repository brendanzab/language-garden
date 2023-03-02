# Simply typed lambda calculus with unification

This an elaborator for the simply typed lambda calculus that allows programmers
to omit type annotations. This is done by inserting _metavariables_ that
stand-in for unknown types during elaboration. These are updated later based
on how they are used in other parts of the program.

This approach is a stepping-stone to more powerful type checking algorithms,
such as those for Hindley-Milner type systems. Note that it’s not a highly
optimised implementation – the goal here is clarity.

This implementation was originally based on [Arad Arbel’s gist](https://gist.github.com/aradarbel10/837aa65d2f06ac6710c6fbe479909b4c).

## Project overview

| Module        | Description                             |
| ------------- | --------------------------------------- |
| [`Main`]      | Command line interface                  |
| [`Lexer`]     | Lexer for the surface language          |
| [`Parser`]    | Parser for the surface language         |
| [`Surface`]   | Surface language, including elaboration |
| [`Core`]      | Core language, including normalisation, unification, and pretty printing |

[`Main`]: ./Main.ml
[`Lexer`]: ./Lexer.mll
[`Parser`]: ./Parser.mly
[`Surface`]: ./Surface.ml
[`Core`]: ./Core.ml

## Examples

```sh
$ stlc-unification <<< "1 + 2"
1 + 2 : Int
```

```sh
$ stlc-unification <<< "fun x => x + 2"
fun (x : Int) => x + 2 : Int -> Int
```

```sh
$ stlc-unification <<< "fun x f => f x * x"
fun (x : Int) => fun (f : Int -> Int) => f x * x : Int -> (Int -> Int) -> Int
```

```sh
$ stlc-unification <<< "let f x := x; f 3"
let f : Int -> Int := fun (x : Int) => x; f 3 : Int
```

Some type errors:

```sh
$ stlc-unification <<< "let f x := x + 1; f f"
mismatched types:
  expected: (Int -> Int) -> ?1
  found: Int -> Int
[1]
```

```sh
$ stlc-unification <<< "fun f => f f"
infinite type:
  expected: ?0 -> ?1
  found: ?0
[1]
```
