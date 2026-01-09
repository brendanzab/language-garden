# Generics

- Extends [**elab-generics-tydefs**](../elab-generics-tydefs) (+ type aliases, tuples)
- Based on [**scraps/elab_poly_generics_tydefs.ml**](../scraps/elab_poly_generics_tydefs.ml)

---

An implementation of generics that supports local type definitions.

```
let type Point2 A := (A, A);
let type Vector2 A := (A, A);

let scale (v : Vector2 Int) (s : Int) : Vector2 Int :=
  (v.1 * s, v.2 * s)

let add (p : Point2 Int) (diff : Vector2 Int) : Point Int2 :=
  (p.1 + diff.1, p.2 + diff.2)
```

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

## Examples

More examples can be found in the [`examples`](./examples/) and
[`tests`](./tests/) directories.
