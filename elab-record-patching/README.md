# Elaboration with Record Patching and Singleton Types

- Extends [**elab-dependent-sugar**](../elab-dependent-sugar) (+ dependent record types, singleton types, record patching, coercive subtyping)

---

This is an implementation of a dependently typed language with dependent
record types, with some additional features intended to make it more
convenient to use records as first-class modules. It was originally ported
from [a gist by mb64](https://gist.github.com/mb64/04315edd1a8b1b2c2e5bd38071ff66b5),
which was inspired by the “record patching” feature implemented in [CoolTT](https://github.com/RedPRL/cooltt).

The type system is implemented in terms of an ‘elaborator’, which type
checks and tanslates a user-friendly surface language into a simpler and
more explicit core language that is more closely connected to type theory.

*This was originally posted at
[record-patching.ml](https://gist.github.com/brendanzab/3b27daf123209619e7abad5335ec4480).*

## Record patching

Record patching is a way to constrain the values of fields in a record type.
Given a record type `R`, a record patch can be applied using the syntax
`R [ l := t; ... ]`. For example:

```text
let Monoid := {
  T : Type;
  empty : T;
  append : T -> T -> T;
};

let string-monoid : Monoid [ T := String ] := {
  empty := "";
  append := string-append;
};
```

This is like Standard ML’s `where type` syntax for [type realisation](
https://smlfamily.github.io/sml97-defn.pdf#page=28), OCaml’s `with` operator for
[constraining module types](https://v2.ocaml.org/manual/modtypes.html#ss%3Amty-with),
and Rust’s `Iterator<Item = T>` shorthand syntax for [equality constraints](
https://rust-lang.github.io/rfcs/0195-associated-items.html#constraining-associated-types)
in type parameter bounds.

### Elaboration of patches to singleton types

Patches only exist as a feature of the surface language and are removed
during elaboration. The expression `Monoid [ T := String ]` in the example
above elaborates to a new record type, where the type of the `T` field is
constrained to `String` through the use of a singleton type.

We also derive the definitions of missing fields in record literals from
singletons in the expected type. This works nicely in combination with
record patching. Note in the previous example how we don't need to define
the field `T` in `string-monoid`.

With that in mind, the definition of `string-monoid` is elaborated to:

```text
let string-monoid : {
  T : Type [= String]; -- singleton type patched here
  empty : T;
  append : T -> T -> T;
} := {
  T := String; -- definition taken from the singleton
  empty := "";
  append := string-append;
};
```

## Future work

### Total space conversion

CoolTT implements ‘total space conversion’ which automatically converts
functions in the form `F : { l : T; ... } -> Type` to the record type
`{ l : T; ..., fibre : F { l := l; ... } }` where necessary. Apparently this
could help address the ‘bundling problem’, and reduce the need to implement
implicit function parameters.

### Opaque ascription

Adding a ‘sealing operator’ `e :> T` to the surface language would allow us to
opaquely ascribe a type `T` to an expression `e`. This would prevent the
contents of the expression from reducing definitionally, allowing us to define
abstract data types.

Opaque ascription is sometimes modelled using effects, as seen in the language
[1ML](https://people.mpi-sws.org/~rossberg/1ml/). The paper [“Logical Relations
as Types: Proof-Relevant Parametricity for Program Modules”](https://doi.org/10.1145/3474834)
describes an effectful approach based on call-by-push-value that could be useful
in the context of dependent types.

Apparently effects are only needed in the presence of mutable references,
however. If we didn’t need these, we might be able implement sealing by
hiding definitions behind function parameters. For example:

- `... (e :> T) ...` elaborates to `... (fun (x : T) := x ...) e`
- `... let x :> T := e; ...` elaborates to `... ((fun (x : T) := ...) e)`

### Metavariables and unification

Implicit function types and unification could be convenient. This could be
challenging to implement in the presence coercive subtyping, however.
Apparently total space conversion addresses some of the same pain points as
implicit parameters, but I'm still somewhat skeptical of this!

### Patches elaborate to large, unfolded terms

Each patch currently elaborates to a copy of the original record type. This
a problem for error messages, where the type ends up fully unfolded and to
understand, and it could become a performance issue down the line when
elaborating and compiling larger programs.

A distiller could attempt to convert singletons back to patches for better
error messages, but to really address the usability and performance issues
we might ultimately need to add patches to the core language and control the
level of unfolding with glued evaluation.

### Use patch syntax for record literal updates

The same syntax used by patches could be used as a way to update the fields
of record literals.

## Related work

This implementation is heavily based on [mb64’s sketch implementation in
Haskell](https://gist.github.com/mb64/04315edd1a8b1b2c2e5bd38071ff66b5) but
contains various bug fixes, alterations, and extensions.

Record patching was originally proposed and implemented for CoolTT in the
setting of cubical type theory:

- [Record patching (like SML `where type`](https://github.com/RedPRL/cooltt/issues/266)
- [Support for auto-converting between fibered and parameterized type families](https://github.com/RedPRL/cooltt/issues/267)

Reed Mullanix's presentation from WITS’22, [Setting the Record Straight with
Singletons](https://www.youtube.com/watch?v=1_ZJIYu2BRk) ([slides](https://cofree.coffee/~totbwf/slides/WITS-2022.pdf))
provides a good description of the approach taken in CoolTT, which continues to
be developed and improved.

Elaborating record patches to singleton types is similar to approaches
developed for formalising and implementing type realisation in Standard ML,
for example in [“Extensional equivalence and singleton types”](https://doi.org/10.1145/1183278.1183281).
Unlike this work, we avoid defining singletons in terms of extensional equality,
which makes it much easier to maintain decideable type checking.
