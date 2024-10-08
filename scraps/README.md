# Miscellaneous scraps

I’m not really ready to add these to the top-level directory. They also lack
build system support and are untested for now.

- [**compile-arith-rust**](./compile-arith-rust/):
  Arithmetic expression evaluators and compilers in Rust.
- [**compile-arith-verified**](./compile-arith-verified/):
  A formally verified arithmetic expression compiler and decompiler in Lean 4.
- [**check-stlc-bidir.rs**](./check-stlc-bidir.rs):
  Bidirectional type checker for a simple functional language
- [**check-stlc-inference-rules.ml**](./check-stlc-inference-rules.ml):
  A demo of translating inference rules for the STLC into a type inference algorithm
- [**check-dependent.pl**](./check-dependent.pl):
  A small dependent type system, implemented in SWI-Prolog using normalisation-by-evaluation.
- [**elab-stlc-bidir.rs**](./elab-stlc-bidir.rs):
  Bidirectional elaborator for a simple functional language
  (compare with [check-stlc-bidir.rs](./check-stlc-bidir.rs)).
- [**eval-cek**](./eval-cek.ml): A tree-walking interpreter for the lambda
  calculus, refactored into continuation-passing-style in the style of the
  CEK machine.
- [**eval-extensible**](./eval-extensible.ml): Extensible interpreters for
  lambda calculus and arithmetic expressions.
- [**eval-landins-knot**](./eval-landins-knot.ml):
  Demonstration of Landin’s Knot, an approach to encoding general recursion
  using higher-order references and backpatching.
