# Arithmetic expression compiler

A compiler from tree-based arithmetic expressions to a stack based IR,
implemented in OCaml.

The correctness of compilation and pretty printing are tested with property-based
tests implemented using the [qcheck](https://github.com/c-cube/qcheck) library.
