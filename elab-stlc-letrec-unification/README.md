# Simply typed lambda calculus with recursive let bindings

- Extends [**elab-stlc-unification**](../elab-stlc-unification) (+ mutual recursion)

---

This is an implementation of recursive let bindings for the simply typed lambda
calculus. Singly recursive functions are elaborated to fixed-points in the core
language:

<!-- $MDX file=examples/factorial.txt -->
```
let rec fact n :=
  if n = 0 then 1 else n * fact (n - 1);

fact 5
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/factorial.stdout -->
```
let fact : Int -> Int :=
  #fix (fact : Int -> Int) =>
    fun (n : Int) =>
      if #int-eq n 0 then 1 else #int-mul n (fact (#int-sub n 1));
fact 5 : Int
```

</details>

Mutually recursive functions are elaborated to fixed-points over tuples of
functions:

<!-- $MDX file=examples/even-odd.txt -->
```
let rec is-even n :=
      if n = 0 then true else is-odd (n - 1);
    rec is-odd n :=
      if n = 0 then false else is-even (n - 1);

is-even 6
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/even-odd.stdout -->
```
let $mutual-0 : (Int -> Bool, Int -> Bool) :=
  #fix ($mutual-0 : (Int -> Bool, Int -> Bool)) =>
    (fun (n : Int) =>
       if #int-eq n 0 then true else $mutual-0.1 (#int-sub n 1),
    fun (n : Int) =>
      if #int-eq n 0 then false else $mutual-0.0 (#int-sub n 1));
$mutual-0.0 6 : Bool
```

</details>

Due to the introduction of general recursion to the language, care must be taken
when implementing quotation, as the naive approach will lead to infinite loops
when quoting under-applied recursive definitions. To avoid this, we disable the
unfolding of recursive definitions during quotation.

Thanks goes to [Karl Meakin](https://github.com/Kmeakin) for help in exploring
different approaches and pointing out bugs in my initial implementations.

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

## Todo list

- [x] singly recursive bindings
- [x] mutually recursive bindings
- [ ] optional fuel/recursion limit

## Resources

In addition to the resources used in [elab-stlc-unification](../elab-stlc-unification):

- [Many faces of the fixed-point combinator](https://okmij.org/ftp/Computation/fixed-point-combinators.html)
  by Oleg Kiselyov.
- [Fixed-point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator) on Wikipedia
- [Mutual recursion](https://en.wikipedia.org/wiki/Mutual_recursion) on Wikipedia

Some other approaches to combining fixed points with normalisation-by-evaluation
(assuming totality checking) can be found here:

- [A Compiled Implementation of Strong Reduction](https://xavierleroy.org/publi/strong-reduction.pdf)
  by Benjamin Grégoire and Xavier Leroy.
- [A simple type-theoretic language: Mini-TT](https://web.archive.org/web/20220208175952/https://www.cse.chalmers.se/~bengt/papers/GKminiTT.pdf)
  by Thierry Coquand et. al.

## Examples

More examples can be found in [`tests.t`](tests.t).
