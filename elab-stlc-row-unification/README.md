# Simply typed lambda calculus with structural record and variant types

- Extends [**elab-stlc-unification**](../elab-stlc-unification) (+ structural records, structural variants)

---

This elaborator introduces structural record and variant types to a simply
typed lambda calculus. In order to reduce the need for up-front type
annotations, we use _row metavariables_[^row] to accumulate maps of labelled
types during unification. This is similar to the “flexible records” approach
used in some implementations of Standard ML. We could extend this to support
[row polymorphism](https://en.wikipedia.org/wiki/Row_polymorphism) in a future
project, but for now this is limited to inferring monomorphic rows.

<!-- $MDX file=examples/readme.txt -->
```
let point x y :=
  { x := x; y := y };

let add p1 p2 := point (p1.x + p2.x) (p1.y + p2.y);
let sub p1 p2 := point (p1.x - p2.x) (p1.y - p2.y);

let _ :=
  add (point 1 2) (point 3 4);

let apply x :=
  match x with {
    | [incr := x] => x + 1
    | [decr := x] => x - 1
    | [square := x] => x * x
  };

apply [incr := 1]
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/readme.stdout -->
```
let point : Int -> Int -> { x : Int; y : Int } :=
  fun (x : Int) => fun (y : Int) => { x := x; y := y };
let add :
      { x : Int; y : Int } -> { x : Int; y : Int } -> { x : Int; y : Int }
:=
  fun (p1 : { x : Int; y : Int }) => fun (p2 : { x : Int; y : Int }) =>
    point (#int-add p1.x p2.x) (#int-add p1.y p2.y);
let sub :
      { x : Int; y : Int } -> { x : Int; y : Int } -> { x : Int; y : Int }
:=
  fun (p1 : { x : Int; y : Int }) => fun (p2 : { x : Int; y : Int }) =>
    point (#int-sub p1.x p2.x) (#int-sub p1.y p2.y);
let _ : { x : Int; y : Int } := add (point 1 2) (point 3 4);
let apply : [decr : Int | incr : Int | square : Int] -> Int :=
  fun (x : [decr : Int | incr : Int | square : Int]) =>
    match x with {
      | [decr := x] => #int-sub x 1
      | [incr := x] => #int-add x 1
      | [square := x] => #int-mul x x
    };
apply ([incr := 1] : [decr : Int | incr : Int | square : Int]) : Int
```

</details>

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

[^row]: The term “row” purportedly comes from [Algol 68](https://www.softwarepreservation.org/projects/ALGOL/report/Algol68_revised_report-AB.pdf):

    > Let $L$ be a fixed countable set of _labels_ $a_1, a_2, \dots$. If
    > $p: D \rarr X$, where $D$ is a finite subset of $L$, (that is a family of
    > elements of $X$ indexed by a finite subset of $L$) then we call $\rho$ a
    > _row_ of $X$’s (This terminology is stolen from Algol 68) p a row of X's.
    > If we fix an ordering on L, then any row has a canonical finite
    > representation $\langle (a_{i_1}, x_1), \dots, (a_{i_n}, x_n)\rangle$
    > where the $a_{i_k}$ are in increasing order.
    >
    > -- Mitchell Wand, [Complete Type Inference for Simple Objects](http://www.ccs.neu.edu/home/wand/papers/wand-lics-87.pdf), 1987

    [Apparently](https://rosettacode.org/wiki/Category:ALGOL_68-rows) Algol 68
    used the term “row” to refer to arrays.
