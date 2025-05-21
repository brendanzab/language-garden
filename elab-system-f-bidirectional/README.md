# Bidirectional typing for higher-rank polymorphism

Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional).

This implements a bidirectional elaborator for a
[higher-rank polymorphic](https://en.wikipedia.org/wiki/Parametric_polymorphism#Higher-rank_polymorphism)
lambda calculus (i.e. [System F](https://en.wikipedia.org/wiki/System_F)),
adding type functions to the simply typed lambda calculus.
This allows us to express polymorphic terms,
for example:

<!-- $MDX file=examples/readme.txt -->
```text
let id [a] (x : a) := x;
let always [a] (x : a) [b] (y : b) := x;

always [Int -> Int] (id [Int])
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/readme.stdout -->
```text
let id : [a] -> a -> a := fun [a] => fun (x : a) => x;
let always : [a] -> a -> [b] -> b -> a :=
  fun [a] => fun (x : a) => fun [b] => fun (y : b) => x;
always [Int -> Int] (id [Int]) : [b] -> b -> Int -> Int
```

</details>

Note that polymorphic terms _must_ be explicitly instantiated in this language,
as we do not implement metavariables and pattern unification.

The elaborator uses normalisation-by-evaluation (NbE) when comparing types,
similar to [elab-dependent](../elab-dependent/).
This handles the substitution of type variables,
and could be extended to support type-level functions in an implementation of System Fω.
Type values are used throughout the elaborator to avoid the error-prone shifting of de Bruijn indices.

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

In addition to the resources used in [elab-stlc-bidirectional](../elab-stlc-bidirectional):

- [fullpoly](https://github.com/mspertus/TAPL/blob/main/fullpoly) from
  Benjamin Pierce’s [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/).
  Uses de Bruijn index shifting heavily,
  which can be difficult to implement correctly without introducing bugs.
- [elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/):
  Examples of implementing elaborators for dependently typed programming languages.
  Similar techniques are used in these implementations (i.e. bidirectional elaboration and NbE),
  and examples of pattern unification implicit instantiation are also provided.
- [sfpl](https://github.com/balint99/sfpl/):
  A higher ranked functional language that uses NbE in types,
  and implements pattern unification for instantiating polymorphic types
  in a similar way to the elaboration-zoo.

## Examples

```sh
$ system-f-bidirectional elab <<< "fun [a] (x : a) => x"
fun [a] => fun (x : a) => x : [a] -> a -> a
```

```sh
$ system-f-bidirectional elab <<< "(fun [a] (x : a) => x) [Int] 3"
(fun [a] => fun (x : a) => x) [Int] 3 : Int
```

```sh
$ system-f-bidirectional norm <<< "(fun [a] (x : a) [b] (y : b) => x) [Int] 3 [Bool]"
fun (y : Bool) => 3 : Bool -> Int
```

More examples can be found in [`tests.t`](tests.t).
