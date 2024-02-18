# Simply typed lambda calculus with fixpoint

This an elaborator for a simply typed lambda calculus (with boolean, integers and records)
enriched with fixpoints for general recursion.

This is done by adding `fix1` and `fix2` primitives:
`fix1 : ((a -> b) -> a -> b) -> a -> b`
`fix2 : ({ f1: a1 -> b1; f2:  a2 -> b2 } -> { f1: a1 -> b1; f2:  a2 -> b2 }) -> { f1: a1 -> b1; f2:  a2 -> b2 }`

`fix1 f x = f (fix1 f) x`
`(fix2 f).l x = (f (fix2 f)).l x`

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
$ stlc-fix-records <<< "fun x => x + 2"
fun (x : Int) => x + 2 : Int -> Int
fun (x : Int) => x + 2
```

```sh
$ stlc-fix-records <<< "fun x f => f x * x"
fun (x : Int) => fun (f : Int -> Int) => f x * x : Int -> (Int -> Int) -> Int
fun (x : Int) => fun (f : Int -> Int) => f x * x
```

```sh
$ stlc-fix-records <<< "fun x y => if x = 0 then y else 3"
fun (x : Int) => fun (y : Int) => if x = 0 then y else 3 : Int -> Int -> Int
fun (x : Int) => fun (y : Int) => if x = 0 then y else 3
```

More examples can be found in [`tests.t`](tests.t).
