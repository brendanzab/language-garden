# Bidirectional type checking and pattern unification for higher-rank polymorphism

Extends [**elab-system-f-bidirectional**](../elab-system-f-bidirectional).

An elaborator for a higher-rank polymorphic lambda calculus where explicit
type applications and type annotations can be omitted.

```text
let id [a] (x : a) := x;
let always [a] (x : a) [b] (y : b) := x;

always id 33
```

The approach to unification-with levels was mainly based on Mark Barbone’s
[implementation](https://gist.github.com/mb64/87ac275c327ea923a8d587df7863d8c7)
of higher-rank polymorphism. Note that we do not implement polymorphic subtyping
in this implementation.

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

In addition to the resources used in [elab-system-f-bidirectional](../elab-system-f-bidirectional):

- Mark Barbone, [mb64/tychk.ml](https://gist.github.com/mb64/87ac275c327ea923a8d587df7863d8c7):
  Bidirectional typechecking for higher-rank polymorphism in OCaml, without
  polymorphic subtyping
- Oleg Kiselyov, [Efficient and Insightful Generalization](https://okmij.org/ftp/ML/generalization.html)
- Richard A. Eisenberg, [Type inference in OCaml and GHC using Levels](https://www.youtube.com/watch?v=iFUrhTQi0-U)

## Examples

```sh
$ system-f-unification elab <<< "fun [a] (x : a) => x"
fun [a] => fun (x : a) => x : [a] -> a -> a
```

```sh
$ system-f-unification elab <<< "(fun [a] (x : a) => x) 3"
(fun [a] => fun (x : a) => x) [Int] 3 : Int
```

```sh
$ system-f-unification norm <<< "(fun [a] (x : a) [b] (y : b) => x) 3"
fun [b] => fun (y : b) => 3 : [b] -> b -> Int
```

More examples can be found in [`tests.t`](tests.t).
