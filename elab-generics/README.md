# Generics

- Extends [**elab-stlc-unification**](../elab-stlc-unification) (+ type parameters, mutual recursion) (- normalisation-by-evaluation)
- Based on [**scraps/elab_poly_generics_letrec.ml**](../scraps/elab_poly_generics_letrec.ml)

---

An elaborator for a simple, polymorphic functional language. The goal of this
project is as a response to the common question “how do I implement generics?”

We support typechecking expressions like:

<!-- $MDX file=examples/combinators.txt -->
```
let id [A] (x : A) : A := x;
let const [A, B] (x : A) (y : B) : A := x;

let flip [A, B, C] (f : A -> B -> C) : B -> A -> C :=
  fun x y => f y x;

flip const true id 45
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/combinators.stdout -->
```
let id [A] : A -> A := fun (x : A) => x;
let const [A, B] : A -> B -> A := fun (x : A) => fun (y : B) => x;
let flip [A, B, C] : (A -> B -> C) -> B -> A -> C :=
  fun (f : A -> B -> C) => fun (x : B) => fun (y : A) => f y x;
flip [Int -> Int, Bool, Int -> Int] const [Int -> Int, Bool] true id [Int] 45
: Int
```

</details>

Recursive bindings are also supported:

<!-- $MDX file=examples/fix.txt -->
```
let rec fix [A, B] (f : (A -> B) -> A -> B) (x : A) : B :=
  f (fix f) x;

let fact :=
  fix (fun fact n =>
    if n = 0 then 1 else n * fact (n - 1));

let fib :=
  fix (fun fib n =>
    if n = 0 then 0 else
    if n = 1 then 1 else
      fib (n - 1) + fib (n - 2));

fact 5
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/fix.stdout -->
```
let rec {
  fix [A, B] : ((A -> B) -> A -> B) -> A -> B :=
    fun (f : (A -> B) -> A -> B) => fun (x : A) => f (fix [A, B] f) x;
};
let fact : Int -> Int :=
  fix
    [Int, Int]
    (fun (fact : Int -> Int) => fun (n : Int) =>
       if #int-eq n 0 then 1 else #int-mul n (fact (#int-sub n 1)));
let fib : Int -> Int :=
  fix
    [Int, Int]
    (fun (fib : Int -> Int) => fun (n : Int) =>
       if #int-eq n 0 then
         0
       else
         if #int-eq n 1 then
           1
         else
           #int-add (fib (#int-sub n 1)) (fib (#int-sub n 2)));
fact 5 : Int
```

</details>

The type parameters _must_ be explicitly bound, similar to Rust, Swift, Java,
etc. This is what I think most people are asking for when they ask about
implementing generics, as opposed to Hindley-Milner type systems (with
generalisation[^hm]).

A stripped-down version of this project can be found in [`scraps/check_poly_generics.ml`].

[^hm]: For examples of Hindley-Milner type system implementations that implement
  generalisation, see [`scraps/check_poly_algorithm_j.ml`] and
  [`scraps/elab_poly_algorithm_j.ml`]).

[`scraps/check_poly_algorithm_j.ml`]: ../scraps/check_poly_algorithm_j.ml
[`scraps/elab_poly_algorithm_j.ml`]: ../scraps/check_poly_algorithm_j.ml
[`scraps/check_poly_generics.ml`]: ../scraps/check_poly_generics.ml

## Project overview

| Module        | Description                             |
| ------------- | --------------------------------------- |
| [`Main`]      | Command line interface                  |
| [`Lexer`]     | Lexer for the surface language          |
| [`Parser`]    | Parser for the surface language         |
| [`Surface`]   | Surface language, including elaboration |
| [`Core`]      | Core language, including evaluation, unification, and pretty printing |
| [`Prim`]      | Primitive operations                    |

[`Main`]: ./main.ml
[`Lexer`]: ./lexer.ml
[`Parser`]: ./parser.mly
[`Surface`]: ./surface.ml
[`Core`]: ./core.ml
[`Prim`]: ./prim.ml

## Future work

- [x] Recursive definitions (see [elab-stlc-letrec-unification](../elab-stlc-letrec-unification))
- [ ] Local datatype definitions (see [misc_local_datatypes.ml](../scraps/misc_local_datatypes.ml)
  and [Practical Type Inference with Levels](https://doi.org/10.1145/3729338)
- [ ] Local type aliases

## Examples

More examples can be found in the [`examples`](./examples/) and
[`tests`](./tests/) directories.
