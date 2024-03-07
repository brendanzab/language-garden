# Future proofing the simply typed lambda calculus for fancy types

Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional).

Statically typed languages tend to evolve dependently typed features over time,
and if the surface syntax is not prepared to handle it, this can lead to awkward
compromises to avoid syntactic ambiguities. For examples of such workarounds,
see the braces in Rust’s generic constant arguments, first class modules in
OCaml, and the various challenges in preparing GHC to support dependent types.

This elaborator takes a surface language that appears to be a full-spectrum
dependently typed language and stratifies it into a simply typed lambda
calculus. The idea is that this could retain syntactic room in the surface
language to allow language features like dependent types to be added later on.
It also has the benefit of using a consistent syntax for types and expressions,
which some programmers prefer.

For example the following program:

<!-- $MDX file=examples/elephant.txt -->
```
let Elephant : Type := Int;
let grow (e : Elephant) : Elephant := e + 1;
grow 4
```

Will be elaborated to:

<!-- $MDX file=examples/elephant.stdout -->
```
let grow : Int -> Int := fun (e : Int) => e + 1;
grow 4 : Int
```

Attempting to use features like type parameters will result in an elaboration
error, for example:

```sh
$ stlc-bidirectional-stratify elab <<< "fun (A : Type) (x : A) => x"
<input>:1:9: expected type, found universe
[1]
```

We achieve this using the following GADTs in our elaborator:

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
- We can no longer overload the syntax of types and terms as easily, for example
  when overloading the syntax for tuple types and tuple literals.
- When introducing fancier types we might want to make staging of types explicit
  for performance reasons, which could be a breaking change.

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
$ stlc-bidirectional-stratify elab <<< "let Univ := Type; let Number : Univ := Int; 1 + 2 : Number"
1 + 2 : Int
```

More examples can be found in [`tests.t`](tests.t).
