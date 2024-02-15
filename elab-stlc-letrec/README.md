# Simply typed lambda calculus with fixpoint

This an elaborator for a simply typed lambda calculus (with booleans and integers)
enriched with fixpoints for general recursion.

This is done by adding a `fix` primitive with typing rule
`fix : ((a -> b) -> a -> b) -> a -> b`
and reduction rule
`fix f x = f (fix f) x`

## Project overview

| Module      | Description                                                              |
| ----------- | ------------------------------------------------------------------------ |
| [`Main`]    | Command line interface                                                   |
| [`Lexer`]   | Lexer for the surface language                                           |
| [`Parser`]  | Parser for the surface language                                          |
| [`Surface`] | Surface language, including elaboration                                  |
| [`Core`]    | Core language, including normalisation, unification, and pretty printing |

[`Main`]: ./Main.ml
[`Lexer`]: ./Lexer.mll
[`Parser`]: ./Parser.mly
[`Surface`]: ./Surface.ml
[`Core`]: ./Core.ml

## Examples

```sh
$ stlc-letrec <<< "fun x => x + 2"
fun (x : Int) => x + 2 : Int -> Int
fun (x : Int) => x + 2
```

```sh
$ stlc-letrec <<< "fun x f => f x * x"
fun (x : Int) => fun (f : Int -> Int) => f x * x : Int -> (Int -> Int) -> Int
fun (x : Int) => fun (f : Int -> Int) => f x * x
```

```sh
$ stlc-letrec <<< "fun x y => if x = 0 then y else 3"
fun (x : Int) => fun (y : Int) => if x = 0 then y else 3 : Int -> Int -> Int
fun (x : Int) => fun (y : Int) => if x = 0 then y else 3
```

More examples can be found in [`tests.t`](tests.t).
