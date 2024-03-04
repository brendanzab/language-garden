# Stratifying a surface language into simply typed terms

Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional).

Statically typed languages tend to evolve dependently typed features over time,
and if the surface syntax is not prepared to handle it, this can lead to awkward
compromises to avoid syntactic ambiguities.

This elaborator takes a surface language that appears to be a full-spectrum
dependently typed language and stratifies it into a simply typed lambda
calculus. A more advanced version of this could be used to implement a 1ML-style
language that looks dependently typed, but in reality is not.

In the future it would be interesting try to extend this to support type
parameters and metavariable unification.

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

- [ ] Local type bindings
- [ ] Type parameters
- [ ] Metavariables and unification

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
$ stlc-bidirectional-stratify elab <<< "fun (x : Int) => x + 2"
fun (x : Int) => x + 2 : Int -> Int
```

```sh
$ stlc-bidirectional-stratify elab <<< "(fun x f => f x * x) : Int -> (Int -> Int) -> Int"
fun (x : Int) => fun (f : Int -> Int) => f x * x : Int -> (Int -> Int) -> Int
```

More examples can be found in [`tests.t`](tests.t).
