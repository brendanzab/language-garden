# Bidirectional typing for a simply typed lambda calculus

This project implements a bidirectional elaborator for a simply typed lambda
calculus with booleans and integers. We use bidirectional type checking during
elaboration to allow the programmer to omit some type annotations, without
the need for more advanced techniques like unification. The idea is that we
split type checking into into two mutually recursive functions, for “checking”
and “inference”:

<!-- $MDX skip -->
```ocaml
(** checks a term in the presence of a type annotation *)
val check_tm : context -> tm -> Core.ty -> Core.tm

(** infers a type from a term *)
val infer_tm : context -> tm -> Core.tm * Core.ty
```

The name “bidirectional” comes from how the type information flows up and down
the stack when evaluating the type checker - upward when we’re in checking mode,
and downward when we’re in inference mode (this corresponds to the information
flow on the proof tree).

Bidirectional typing comes in very handy for improving the locality of type
errors, and when implementing fancier type systems where full type inference
would be undecidable - for example when adding subtyping, dependent types, or
higher rank types. You can find it in use in the implementation of many
real world programming languages, for example in Idris, Agda, Haskell, Scala,
Typescript, etc.

## Project overview

| Module        | Description                             |
| ------------- | --------------------------------------- |
| [`Main`]      | Command line interface                  |
| [`Lexer`]     | Lexer for the surface language          |
| [`Parser`]    | Parser for the surface language         |
| [`Surface`]   | Surface language, including elaboration |
| [`Core`]      | Core language, including normalisation, and pretty printing |
| [`Prim`]      | Primitive operations                    |

[`Main`]: ./main.ml
[`Lexer`]: ./lexer.ml
[`Parser`]: ./parser.mly
[`Surface`]: ./surface.ml
[`Core`]: ./core.ml
[`Prim`]: ./prim.ml

## Resources

- David R. Christiansen, [Bidirectional Typing Rules: A Tutorial](https://davidchristiansen.dk/tutorials/bidirectional.pdf)
- David R. Christiansen, [Bidirectional Type Checking](https://www.youtube.com/watch?v=utyBNDj7s2w)
- Jana Dunfield and Neel Krishnaswami, [Bidirectional Typing](https://dl.acm.org/doi/10.1145/3450952)
- Frank Pfenning, [Lecture Notes on Bidirectional Type Checking](https://www.cs.cmu.edu/~fp/courses/15312-f04/handouts/15-bidirectional.pdf)
- Conor McBride, [Type Inference Needs a Revolution](https://www.youtube.com/watch?v=ad4BVmPni7A)

## Examples

```sh
$ stlc-bidirectional elab <<< "fun x => x + 2"
<input>:1:4: ambiguous parameter type
[1]
```

```sh
$ stlc-bidirectional elab <<< "fun (x : Int) => x + 2"
fun (x : Int) => #int-add x 2 : Int -> Int
```

```sh
$ stlc-bidirectional elab <<< "(fun x f => f x * x) : Int -> (Int -> Int) -> Int"
fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
  Int -> (Int -> Int) -> Int
```

More examples can be found in [`tests.t`](tests.t).
