# Bidirectional type checking for higher-rank polymorphism

Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional).

This implements a bidirectional elaborator for a
[higher-rank polymorphic](https://en.wikipedia.org/wiki/Parametric_polymorphism#Higher-rank_polymorphism)
lambda calculus (i.e. [System F](https://en.wikipedia.org/wiki/System_F)),
adding type functions to the simply typed lambda calculus.
This allows us to express polymorphic terms,
for example:

```text
let id [a] (x : a) := x;
let always [a] (x : a) [b] (y : b) := x;

always [Int -> Int] (id [Int])
```

Note that polymorphic terms _must_ be explicitly instantiated in this language,
as we do not implement metavariables and pattern unification.

The elaborator uses normalisation-by-evaluation (NbE) when comparing types,
similar to [elab-dependent](../elab-dependent/).
This handles the substitution of type variables,
and could be extended to support type-level functions in an implementation of System Fω.
Type values are used in the type environment and elaboration context
to avoid constantly shifting de Bruijn indices in an error-prone way.

## Project overview

| Module        | Description                             |
| ------------- | --------------------------------------- |
| [`Main`]      | Command line interface                  |
| [`Lexer`]     | Lexer for the surface language          |
| [`Parser`]    | Parser for the surface language         |
| [`Surface`]   | Surface language, including elaboration |
| [`Core`]      | Core language, including normalisation, and pretty printing |
| [`Prim`]      | Primitive operations                    |

[`Main`]: ./Main.ml
[`Lexer`]: ./Lexer.ml
[`Parser`]: ./Parser.mly
[`Surface`]: ./Surface.ml
[`Core`]: ./Core.ml
[`Prim`]: ./Prim.ml

## Resources

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
