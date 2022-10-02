# Arithmetic expression compiler

A compiler from tree-based arithmetic and conditional expressions to a stack
based IR, implemented in OCaml.

```command
$ arithcond compile <<< "if 1 * -2 = 3 * 4 then (3 + 4) - 8 / 4 else 7 + 8"
1 2 neg mul 3 4 mul eq [ 3 4 add 8 4 div sub ] [ 7 8 add ] if
```

```command
$ arithcond exec "if 1 * -2 = 3 * 4 then (3 + 4) - 8 / 4 else 7 + 8"
15 : Int
```

The correctness of type checking, compilation and pretty printing are tested
with property-based tests implemented using the [qcheck] library.

[qcheck]: https://github.com/c-cube/qcheck
