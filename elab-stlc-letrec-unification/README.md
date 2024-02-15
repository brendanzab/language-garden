# Simply typed lambda calculus with recursive let bindings

Extends [**elab-stlc-unification**](../elab-stlc-unification).

This is an implementation of recursive let bindings for the simply typed lambda
calculus. These are elaborated to a primitive fixed-point combinator in the core
language. Normalisation-by-evaluation for fixed-points follows a similar
approach to the one described by Grégoire and Leroy in [A Compiled
Implementation of Strong Reduction](https://xavierleroy.org/publi/strong-reduction.pdf).

Thanks goes to [Karl Meakin](https://github.com/Kmeakin) for help in trying out
different approaches when implementing this.

> [!WARNING]
> It’s come to my attention (thanks Karl) that this approach breaks if you
> under-apply a recursive binding _before_ applying the parameter that guards
> the recursion. For example, the following program loops forever under the
> current implementation:
>
> ```
> let rec count-down x n :=
>   if n = 0 then x else count-down x (n - 1);
>
> count-down true
> ```
>
> It seems this is why they have a multiple arguments on fixed points in the
> strong reduction paper. This seems like it would require detecting the guard
> parameter during elaboration, or just conservatively binding all of the
> parameters in the elaborated function as part of fixed point elaboration.

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

## Todo list

- [x] singly recursive bindings
- [ ] mutually recursive bindings
- [ ] optional fuel/recursion limit

## Resources

- [Many faces of the fixed-point combinator](https://okmij.org/ftp/Computation/fixed-point-combinators.html)
  by Oleg Kiselyov.
- [A Compiled Implementation of Strong Reduction](https://xavierleroy.org/publi/strong-reduction.pdf)
  by Benjamin Grégoire and Xavier Leroy.
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
