# Simply typed lambda calculus with recursive let bindings

Extends [**elab-stlc-unification**](../elab-stlc-unification).

This is an implementation of recursive let bindings for the simply typed lambda
calculus. These are elaborated to applications of a primitive fixed-point
combinator in the core language.

Mutual recursion sill needs to be implemented.

Thanks goes to [Karl Meakin](https://github.com/Kmeakin) for help in trying out
different approaches when implementing this.

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

## Resources

- [Many faces of the fixed-point combinator](https://okmij.org/ftp/Computation/fixed-point-combinators.html)
  by Oleg Kiselyov
- [A simple type-theoretic language: Mini-TT](https://web.archive.org/web/20220208175952/https://www.cse.chalmers.se/~bengt/papers/GKminiTT.pdf)
  by Thierry Coquand et. al.
- [Fixed-point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator) on Wikipedia
- [Mutual recursion](https://en.wikipedia.org/wiki/Mutual_recursion) on Wikipedia

## Examples

<!-- $MDX file=examples/readme.txt -->
```
let rec fact n :=
  if n = 0 then 1 else n * fact (n - 1);

fact 5
```

Elaborated program:

<!-- $MDX file=examples/readme.stdout -->
```
let fact : Int -> Int :=
  #fix (fact : Int -> Int) =>
    fun (n : Int) => if n = 0 then 1 else n * fact (n - 1);
fact 5 : Int
```

More examples can be found in [`tests.t`](tests.t).
