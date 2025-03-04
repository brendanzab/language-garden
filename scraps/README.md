# Miscellaneous scraps

I’m not really ready to add these to the top-level directory. They also lack
build system support and are untested for now.

- [**compile-arith-rust**](./compile-arith-rust/):
  Arithmetic expression evaluators and compilers in Rust.
- [**compile-arith-verified**](./compile-arith-verified/):
  A formally verified arithmetic expression compiler and decompiler in Lean 4.
- [**check_stlc_bidir.rs**](./check_stlc_bidir.rs):
  Bidirectional type checker for a simple functional language
- [**check_stlc_inference_rules.ml**](./check_stlc_inference_rules.ml):
  A demo of translating inference rules for the STLC into a type inference algorithm
- [**check_dependent.pl**](./check_dependent.pl):
  A small dependent type system, implemented in SWI-Prolog using normalisation-by-evaluation.
- [**elab_stlc_bidir.rs**](./elab_stlc_bidir.rs):
  Bidirectional elaborator for a simple functional language
  (compare with [check_stlc_bidir.rs](./check_stlc_bidir.rs)).
- [**eval_cek.ml**](./eval_cek.ml): A tree-walking interpreter for the lambda
  calculus, refactored into continuation-passing-style in the style of the
  CEK machine.
- [**eval_control_flow_cps.ml**](./eval_control_flow_cps.ml):
  An evaluator for imperative control flow (loop, break, continue) implemented
  using continuation passing style
- [**eval_extensible.ml**](./eval_extensible.ml): Extensible interpreters for
  lambda calculus and arithmetic expressions.
- [**eval_landins_knot.ml**](./eval_landins_knot.ml):
  Demonstration of Landin’s Knot, an approach to encoding general recursion
  using higher-order references and backpatching.
- [**eval_triple_store.ml**](./eval_triple_store.ml):
  Example of inferring facts from a triple store.
- [**lang_ast_submodules.ml**](./lang_ast_submodules.ml):
  A pattern for nesting mutually recursive datatypes in submodules without
  duplicating the datatype definitions.
- [**lang_build_systems.ml**](./lang_build_systems.ml):
  Memoized build system using OCaml 5’s algebraic effects and handlers.
