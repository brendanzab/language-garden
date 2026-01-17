# Generics

- Extends [**elab-generics**](../elab-generics) (+ type aliases, tuples)
- Based on [**scraps/elab_poly_generics_tydefs.ml**](../scraps/elab_poly_generics_tydefs.ml)

---

An implementation of generics that supports local type definitions.

<!-- $MDX file=examples/points-and-vectors.txt -->
```
let type Point2 A := (A, A);
let type Vector2 A := (A, A);

-- Scale a vector by a scalar
let scale (v : Vector2 Int) (s : Int) : Vector2 Int :=
  (v.0 * s, v.1 * s);

-- Move a point by a displacement vector
let move (p : Point2 Int) (v : Vector2 Int) : Point2 Int :=
  (p.0 + v.0, p.1 + v.1);

()
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/points-and-vectors.stdout -->
```
let type Point2 A := (A, A);
let type Vector2 A := (A, A);
let scale : Vector2 Int -> Int -> Vector2 Int :=
  fun (v : Vector2 Int) => fun (s : Int) => (#int-mul v.0 s, #int-mul v.1 s);
let move : Point2 Int -> Vector2 Int -> Point2 Int :=
  fun (p : Point2 Int) => fun (v : Vector2 Int) =>
    (#int-add p.0 v.0, #int-add p.1 v.1);
() : ()
```

</details>

<!-- $MDX file=examples/pairs.txt -->
```
let type Pair A B := (A, B);

let fst [A, B] (p : Pair A B) : A := p.0;
let snd [A, B] (p : Pair A B) : B := p.1;
let elim [A, B, C] (p : Pair A B) (f : A -> B -> C) : C :=
  f p.0 p.1;

let curry [A, B, C] (f : Pair A B -> C) : A -> B -> C :=
  fun a b => f (a, b);

let uncurry [A, B, C] (f : A -> B -> C) : Pair A B -> C :=
  fun p => f p.0 p.1;

()
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/pairs.stdout -->
```
let type Pair A B := (A, B);
let fst [A, B] : Pair A B -> A := fun (p : Pair A B) => p.0;
let snd [A, B] : Pair A B -> B := fun (p : Pair A B) => p.1;
let elim [A, B, C] : Pair A B -> (A -> B -> C) -> C :=
  fun (p : Pair A B) => fun (f : A -> B -> C) => f p.0 p.1;
let curry [A, B, C] : (Pair A B -> C) -> A -> B -> C :=
  fun (f : Pair A B -> C) => fun (a : A) => fun (b : B) => f (a, b);
let uncurry [A, B, C] : (A -> B -> C) -> Pair A B -> C :=
  fun (f : A -> B -> C) => fun (p : (A, B)) => f p.0 p.1;
() : ()
```

</details>

## Implementation notes

Substitution in types is now handled with normalisation-by-evaluation as opposed
to rewriting in place like in [`elab-generics`]. I find that in the presence of
parameterised type definitions this helps to keep name binding manageable, and
scales up to fancier type systems (see [`elab-system-f-bidirectional`] and
[`elab-dependent`]).

[`elab-generics`]: ../elab-generics
[`elab-system-f-bidirectional`]: ../elab-system-f-bidirectional
[`elab-dependent`]: ../elab-dependent

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
- [ ] Local unfolding of type definitions. See [Efficient Elaboration with
      Controlled Definition Unfolding](https://andraskovacs.github.io/pdfs/wits24prez.pdf#page=19)
      by Andras Kovacs for more details.

## Examples

More examples can be found in the [`examples`](./examples/) and
[`tests`](./tests/) directories.
