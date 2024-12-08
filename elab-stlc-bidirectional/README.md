# Bidirectional type checking for a simply typed lambda calculus

Bidirectional typing is a technique for implementing type checking and
elaboration, allowing type annotations to be provided when checking terms to
help to disambiguate terms. It is used in the implementation of many dependent
and non-dependently typed programming languages, for example in Idris, Agda,
Haskell, Scala, Typescript, etc.

This project implements an elaborator for a simply typed lambda calculus with
booleans and integers. We use bidirectional type checking during elaboration to
allow the programmer to omit some type annotations, breaking elaboration into
two mutually recursive functions:

<!-- $MDX skip -->
```ocaml
val elab_check : context -> tm -> Core.ty -> Core.tm
val elab_infer : context -> tm -> Core.tm * Core.ty
```

A similar approach is used in later projects, like [**elab-dependent**](../elab-dependent)
and [**elab-record-patching**](../elab-record-patching) to implement more
complicated type systems with dependent types and subtyping.

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
fun (x : Int) => #int-add -x 2 : Int -> Int
```

```sh
$ stlc-bidirectional elab <<< "(fun x f => f x * x) : Int -> (Int -> Int) -> Int"
fun (x : Int) => fun (f : Int -> Int) => #int-mul -(f x) x :
  Int -> (Int -> Int) -> Int
```

More examples can be found in [`tests.t`](tests.t).
