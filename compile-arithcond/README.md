# Arithmetic expression compiler

A compiler from tree-based arithmetic and conditional expressions to a stack
based IR, implemented in OCaml.

The correctness of compilation and pretty printing are tested with property-based
tests implemented using the [qcheck](https://github.com/c-cube/qcheck) library.

```command
$ arithcond compile <<< "if 1 * -2 = 3 * 4 then (3 + 4) - 8 / 4 else 7 + 8"
1 2 neg mul 3 4 mul eq [ 3 4 add 8 4 div sub ] [ 7 8 add ] if
```
