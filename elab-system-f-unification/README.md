# Bidirectional typing with unification for higher-rank polymorphism

Extends [**elab-stlc-unification**](../elab-stlc-unification) and [**elab-system-f-bidirectional**](../elab-system-f-bidirectional).

This implements a bidirectional elaborator for a higher-rank polymorphic lambda
calculus where explicit type applications and type annotations can be omitted.

<!-- $MDX file=examples/readme.txt -->
```text
let id [a] (x : a) := x;
let always [a] (x : a) [b] (y : b) := x;

let _ := id 3;
let _ := id id;
let _ := always id 3;

-- Call a polymorphic function with two different types
let test (f : [a] -> a -> a) : Bool :=
  let _ := f 3;
  f true;

test (fun x => x)
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/readme.stdout -->
```text
let id : [a] -> a -> a := fun [a] => fun (x : a) => x;
let always : [a] -> a -> [b] -> b -> a :=
  fun [a] => fun (x : a) => fun [b] => fun (y : b) => x;
let _ : Int := id [Int] 3;
let _ : [a] -> a -> a := id [[a] -> a -> a] id;
let _ : [a] -> a -> a := always [[a] -> a -> a] id [Int] 3;
let test : ([a] -> a -> a) -> Bool :=
  fun (f : [a] -> a -> a) => let _ : Int := f [Int] 3;
                             f [Bool] true;
test (fun [$a] => fun (x : $a) => x) : Bool
```

</details>

The approach to unification-with levels was mainly based on Mark Barbone’s
[implementation](https://gist.github.com/mb64/87ac275c327ea923a8d587df7863d8c7)
of higher-rank polymorphism. Note that we do not implement polymorphic subtyping
in this implementation. This is left to a future subproject.

Thanks goes to Andras Kovacs for helping me iron out some bugs in my
implementation, and in helping to improve my understanding of the level-raising
approach to unification.

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

## Resources

In addition to the resources used in [elab-stlc-unification](../elab-stlc-unification)
and [elab-system-f-bidirectional](../elab-system-f-bidirectional):

- Mark Barbone, [mb64/tychk.ml](https://gist.github.com/mb64/87ac275c327ea923a8d587df7863d8c7):
  Bidirectional typechecking for higher-rank polymorphism in OCaml, with and
  without polymorphic subtyping.
- Oleg Kiselyov, [Efficient and Insightful Generalization](https://okmij.org/ftp/ML/generalization.html):
  Describes using levels for efficient generalisation in the implementation of
  Hindley-Milner typecheckers. Mark Barbone’s approach is based on this work.
- Richard A. Eisenberg, [Type inference in OCaml and GHC using Levels](https://www.youtube.com/watch?v=iFUrhTQi0-U)
- Jana Dunfield and Neelakantan Krishnaswami,
  [Complete and easy bidirectional typechecking for higher-rank polymorphism](https://dl.acm.org/doi/10.1145/2544174.2500582),
  [[DOI](https://doi.org/10.1145/2544174.2500582)]
  [[PDF](https://www.cl.cam.ac.uk/~nk480/bidir.pdf)]

## Examples

More examples can be found in [`tests.t`](tests.t).
