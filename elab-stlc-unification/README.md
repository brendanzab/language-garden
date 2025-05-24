# Simply typed lambda calculus with unification

Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional).

This an elaborator for a simply typed lambda calculus (with booleans and integers)
that allows programmers to omit type annotations. This is done by inserting
_metavariables_ that stand-in for unknown types during elaboration. These are
later updated based on how they are used in other parts of the program.

<!-- $MDX file=examples/readme.txt -->
```
let foo x y z :=
  if x = 0 then y else z;

foo 3 true false
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/readme.stdout -->
```
let foo : Int -> Bool -> Bool -> Bool :=
  fun (x : Int) => fun (y : Bool) => fun (z : Bool) =>
    if #int-eq x 0 then y else z;
foo 3 true false : Bool
```

</details>

This approach is a stepping-stone to more powerful type checking algorithms,
such as those for Hindley-Milner type systems.

This implementation was originally based on [Arad Arbel’s gist](https://gist.github.com/aradarbel10/837aa65d2f06ac6710c6fbe479909b4c).

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

- Arad Arbel, [aradarbel10/main.ml](https://gist.github.com/aradarbel10/837aa65d2f06ac6710c6fbe479909b4c):
  minimal STLC type inference with mutable metavars

## Examples

```sh
$ stlc-unification elab <<< "fun x => x + 2"
fun (x : Int) => #int-add x 2 : Int -> Int
```

```sh
$ stlc-unification elab <<< "fun x f => f x * x"
fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
  Int -> (Int -> Int) -> Int
```

```sh
$ stlc-unification elab <<< "fun x y => if x = 0 then y else 3"
fun (x : Int) => fun (y : Int) => if #int-eq x 0 then y else 3 :
  Int -> Int -> Int
```

More examples can be found in [`tests.t`](tests.t).
