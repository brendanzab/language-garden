# Generics

- Extends [**elab-stlc-unification**](../elab-stlc-unification) (+ type parameters)

---

An elaborator for a simple, polymorphic functional language. The goal of this
project is as a response to the common question “how do I implement generics?”

We support typechecking expressions like:

<!-- $MDX file=examples/readme.txt -->
```
let id [A] (x : A) : A := x;
let const [A, B] (x : A) (y : B) : A := x;

const id true 45
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/readme.stdout -->
```
let id : A -> A [A] := fun (x : A) => x;
let const : A -> B -> A [A, B] := fun (x : A) => fun (y : B) => x;
const [Int -> Int, Bool] id [Int] true 45 : Int
```

</details>

The type parameters _must_ be explicitly bound, similar to Rust, Swift, Java,
etc. This is what I think most people are asking for when they ask about
implementing generics, as opposed to Hindley-Milner type systems (with
generalisation[^hm]).

A more stripped-down version of this project can be found in [`scraps/check_poly_generics.ml`].

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
| [`Core`]      | Core language, including normalisation, unification, and pretty printing |
| [`Prim`]      | Primitive operations                    |

[`Main`]: ./main.ml
[`Lexer`]: ./lexer.ml
[`Parser`]: ./parser.mly
[`Surface`]: ./surface.ml
[`Core`]: ./core.ml
[`Prim`]: ./prim.ml

## Future work

- Recursive definitions (see [elab-stlc-letrec-unification](../elab-stlc-letrec-unification))
- Local datatype definitions (see [misc_local_datatypes.ml](../scraps/misc_local_datatypes.ml)
  and [Practical Type Inference with Levels](https://doi.org/10.1145/3729338)
- Local type aliases

## Examples

More examples can be found in [`tests.t`](tests.t).
