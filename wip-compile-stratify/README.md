# Stratifying a dependently typed lambda calculus

> NOTE: A work in progress!

This aims to demonstrate how to compile a dependently typed language with first
class types into a language that is stratified into terms, types, and kinds.
This could be useful as part of compiling a dependently typed language into a
language that lacks dependent types.

In the elaborator of dependently typed programming languages it is common to
work with a single AST for terms in the core language, stratified using universe
levels. This is convenient for implementing elaborators, but in order to make
compilation easier it can be beneficial to “stratify” these layers explicitly in
the syntax.

The stratified language is pretty close to the _CC_ language that is used as a
source language in [“Singleton types here, singleton types there, singleton types
everywhere”](https://doi.org/10.1145/1707790.1707792) by Monnier and Haguenauer,
but omitting the `(Kscm, Kind)` rule from the pure type system,  along with the
`Πk:u.κ` production from the stratified language (in order to avoid introducing
impredicativity).

## Todo list

- [ ] surface language
  - [ ] parser
  - [ ] elaborator
- [ ] stratify connectives
  - [x] dependent function types
  - [ ] dependent record types
  - [ ] primitive numbers
- [ ] phase separation translation
