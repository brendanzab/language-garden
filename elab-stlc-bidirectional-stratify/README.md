# Stratifying a surface language into simply typed terms

Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional).

Statically typed languages tend to evolve dependently typed features over time,
and if the surface syntax is not prepared to handle it, this can lead to awkward
compromises to avoid syntactic ambiguities. For examples of such workarounds,
see the braces in Rust’s generic constant arguments, first class modules in
OCaml, and the various challenges in preparing GHC to support dependent types.

This elaborator takes a surface language that appears to be a full-spectrum
dependently typed language and stratifies it into a simply typed lambda
calculus. The idea is that this could retain space in the surface language to
allow for dependently typed features to be added in the future.

We achieve this using the following GADTs:

<!-- $MDX file=Surface.ml,part=elab-types -->
```ocaml
  (* An elaborated type *)
  type _ elab_ty =
    | Univ1 : [`Univ1] elab_ty
    | Univ0 : [`Univ0] elab_ty
    | Type : Core.ty -> Core.ty elab_ty

  (* An elaborated term *)
  type _ elab_tm =
    | Univ0 : [`Univ1] elab_tm
    | Type : Core.ty -> [`Univ0] elab_tm
    | Expr : Core.expr -> Core.ty elab_tm

  type ann_tm =
    | AnnTm : 'ann elab_tm * 'ann elab_ty -> ann_tm
```

These types allow us to define a bidirectional type checking algorithm that
works over multiple levels of our core language. Universes only exist as part of
the elaboration process.

Some possible downsides to this approach are:

- It requires contextual information to decide if something is a type or not,
  which could make some language tooling more challenging to implement without
  also implementing a similar form of elaboration.
- Some programmers might be confused that types and expressions use the same
  syntax, disambiguated based on context (on the other hand, other programmers
  might find this insightful).
- A single namespace is used for types and expressions, so the same name can no
  longer be reused for both types and expressions.

## Project overview

| Module        | Description                             |
| ------------- | --------------------------------------- |
| [`Main`]      | Command line interface                  |
| [`Lexer`]     | Lexer for the surface language          |
| [`Parser`]    | Parser for the surface language         |
| [`Surface`]   | Surface language, including elaboration |
| [`Core`]      | Core language, including normalisation, and pretty printing |

[`Main`]: ./Main.ml
[`Lexer`]: ./Lexer.mll
[`Parser`]: ./Parser.mly
[`Surface`]: ./Surface.ml
[`Core`]: ./Core.ml

## Todo list

- [x] Local type bindings (e.g. `let Foo : Type := Int; 1 : Foo`)
- [ ] Type parameters (e.g. `let Id (A : Type) : Type := A; 1 : Id Int`)
- [ ] Metavariables and unification (e.g. `let id (A : _) (x : A) : A := x; id _ 1`)
- [ ] Implicit type parameters (e.g. `let id {A : Type} (x : A) : A := x; id 1`)

## Examples

```sh
$ stlc-bidirectional-stratify elab <<< "Int"
Int : Type
```

```sh
$ stlc-bidirectional-stratify elab <<< "Int -> Int -> Bool"
Int -> Int -> Bool : Type
```

```sh
$ stlc-bidirectional-stratify elab <<< "Int : Type"
Int : Type
```

```sh
$ stlc-bidirectional-stratify elab <<< "Type"
Type : Type 1
```

```sh
$ stlc-bidirectional-stratify elab <<< "let Kind := Type; let Number : Kind := Int; 1 + 2 : Number"
1 + 2 : Int
```

```sh
$ stlc-bidirectional-stratify elab <<< "fun (x : Int) => x + 2"
fun (x : Int) => x + 2 : Int -> Int
```

```sh
$ stlc-bidirectional-stratify elab <<< "(fun x f => f x * x) : Int -> (Int -> Int) -> Int"
fun (x : Int) => fun (f : Int -> Int) => f x * x : Int -> (Int -> Int) -> Int
```

More examples can be found in [`tests.t`](tests.t).
