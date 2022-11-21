# Stratifying a dependently typed lambda calculus

> NOTE: A work in progress!

This aims to demonstrate how to compile a dependently typed language with first
class types into a language that is stratified into terms, types, and kinds.
This could be useful as part of compiling a dependently typed language into a
language that lacks dependent types.

The stratified language is pretty close to the _CC_ language used in [“Singleton
types here, singleton types there, singleton types everywhere”](https://doi.org/10.1145/1707790.1707792)
by Monnier and Haguenauer.

## Todo list

- [ ] surface language
  - [ ] parser
  - [ ] elaborator
- [ ] connectives
  - [x] dependent function types
  - [ ] dependent record types
  - [ ] primitive numbers
