# Miscellaneous scraps

I’m not really ready to add these to the top-level directory. They also might
lack build system support and are untested for now.

Parsing:

- [**parse_sexpr.ml**](./parse_sexpr.ml): Recursive descent parser for S-expressions.

Evaluation:

- [**eval_cek.ml**](./eval_cek.ml): A tree-walking interpreter for the lambda
  calculus, refactored into continuation-passing-style in the style of the
  CEK machine.
- [**eval_control_flow_cps.ml**](./eval_control_flow_cps.ml):
  An evaluator for imperative control flow (loop, break, continue) implemented
  using continuation passing style
- [**eval_extensible.ml**](./eval_extensible.ml): Extensible interpreters for
  lambda calculus and arithmetic expressions.
- [**eval_imp.ml**](./eval_imp.ml):
  A simple imperative language.
- [**eval_landins_knot.ml**](./eval_landins_knot.ml):
  Demonstration of Landin’s Knot, an approach to encoding general recursion
  using higher-order references and backpatching.
- [**eval_stlc_gadt.ml**](./eval_stlc_gadt.ml):
  A well-typed lambda calculus evaluator using GADTs.
- [**eval_stlc_gadt_values_hoas.ml**](./eval_stlc_gadt_values_hoas.ml):
  A well-typed lambda calculus evaluator, with a separate value datatype.
- [**eval_stlc_gadt_values_closures.ml**](./eval_stlc_gadt_values_closures.ml):
  A well-typed lambda calculus evaluator, with a separate value datatype, this
  time using defunctionalised closures in the semantic domain.
- [**eval_stlc_gadt_globals.ml**](./eval_stlc_gadt_globals.ml):
  A well-typed lambda calculus evaluator, extended with some global definitions.
- [**eval_triple_store.ml**](./eval_triple_store.ml):
  Example of inferring facts from a triple store.
- [**eval_unsure_calculator**](./eval_unsure_calculator.ml):
  Probabilistic calculator based on Filip Hracek’s _Unsure Calculator_.

Type checking:

- [**check_stlc_bidir.rs**](./check_stlc_bidir.rs):
  Bidirectional type checker for a simple functional language
- [**check_stlc_inference_rules.ml**](./check_stlc_inference_rules.ml):
  A demonstration of translating inference rules for the STLC into a type
  inference algorithm.
- [**check_stlc_inference_rules_bidir.ml**](./check_stlc_inference_rules_bidir.ml):
  A demonstration of translating bidirectional inference rules for the STLC into
  a type inference algorithm.
- [**check_dependent.pl**](./check_dependent.pl):
  A small dependent type system, implemented in SWI-Prolog using normalisation-by-evaluation.

Elaboration:

- [**elab_stlc_bidir.rs**](./elab_stlc_bidir.rs):
  Bidirectional elaborator for a simple functional language
  (compare with [check_stlc_bidir.rs](./check_stlc_bidir.rs)).
- [**elab_stlc_gadt.ml**](./elab_stlc_gadt.ml):
  An elaborator from an untyped surface language into a well-typed core language.
- [**elab_stlc_gadt_bidir.ml**](./elab_stlc_gadt_bidir.ml):
  An elaborator from an untyped surface language into a well-typed core language,
  allowing more type annotations to be omitted in the surface language.

Compilation:

- [**compile-arith-rust**](./compile-arith-rust/):
  Arithmetic expression evaluators and compilers in Rust.
- [**compile-arith-verified**](./compile-arith-verified/):
  A formally verified arithmetic expression compiler and decompiler in Lean 4.

Miscellaneous:

- [**misc_adt_properties.ml**](./misc_adt_properties.ml):
  Some examples of abstract data types with their associated properties.
- [**misc_ast_folds.ml**](./misc_ast_folds.ml):
  Folding over an AST using recursion schemes.
- [**misc_ast_submodules.ml**](./misc_ast_submodules.ml):
  A pattern for nesting mutually recursive datatypes in submodules without
  duplicating the datatype definitions.
- [**misc_effects_build_system.ml**](./misc_effects_build_system.ml):
  Memoized build system using OCaml 5’s algebraic effects and handlers.
- [**misc_effects_come_from.ml**](./misc_effects_come_from.ml):
  COMEFROM in OCaml, implemented with algebraic effects and handlers.
- [**misc_effects_state.ml**](./misc_effects_state.ml):
  State effect, implemented with algebraic effects and handlers.
- [**misc_isorecursion_vs_equirecursion.ml**](./misc_isorecursion_vs_equirecursion.ml):
  Comparing isorecursive variant types with equirecursive variant types in OCaml.
- [**misc_option_shapes.ml**](./misc_option_shapes.ml):
  Exposing introduction and elimination forms for variant types, without
  exposing the underlying representation.
- [**misc_set_objects.ml**](./misc_set_objects.ml):
  Comparing set ADTs vs. set objects in OCaml.

Work in progress projects:

- [**wip-elab-builtins**](./wip-elab-builtins/):
  An elaborator that supports built-in types and operations.
- [**wip-compile-closure-conv**](./wip-compile-closure-conv):
  Typed closure conversion and lambda lifting for a simply typed lambda calculus
  with booleans and integers.
- [**wip-compile-stlc**](./wip-compile-stlc):
  A type preserving compiler for the simply typed lambda calculus.
- [**wip-compile-stratify**](./wip-compile-stratify/):
  Compiling a dependently typed lambda calculus into a stratified intermediate
  language.
- [**wip-compile-uncurry**](./wip-compile-uncurry/):
  Compiling single-parameter functions to multiparameter functions.
