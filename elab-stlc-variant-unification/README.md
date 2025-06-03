# Simply typed lambda calculus with structural record and variant types

- Extends [**elab-stlc-unification**](../elab-stlc-unification) (+ structural records, structural variants)

---

This elaborator introduces structural record and variant types to a simply
typed lambda calculus. In order to allow programmers to write programs that use
records and variants without many up-front type annotations, we use row
metavariables to accumulate maps of labelled types during unification. This is
similar to the “flexible records” approach used in some implementations of
Standard ML. We could extend this to support [row polymorphism](https://en.wikipedia.org/wiki/Row_polymorphism)
in a future project, but for now this is limited to inferring monomorphic rows.

<!-- $MDX file=examples/readme.txt -->
```
let point x y :=
  { x := x; y := y };

let add p1 p2 := point (p1.x + p2.x) (p1.y + p2.y);
let sub p1 p2 := point (p1.x - p2.x) (p1.y - p2.y);

let _ :=
  add (point 1 2) (point 3 4);

let apply x :=
  match x with
  | [incr := x] => x + 1
  | [decr := x] => x - 1
  | [square := x] => x * x
  end;

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
    match x with
    | [decr := x] => #int-sub x 1
    | [incr := x] => #int-add x 1
    | [square := x] => #int-mul x x
    end;
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
