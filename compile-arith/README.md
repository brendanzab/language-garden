# Arithmetic expression compiler

A compiler from tree-based arithmetic expressions to a stack based IR,
implemented in OCaml.

The correctness of compilation and pretty printing are tested with property-based
tests implemented using the [qcheck](https://github.com/c-cube/qcheck) library.

```command
$ arith compile <<< "1 * -2 + (3 + 4) - 8 / 4"
1 2 neg mul 3 4 add 8 4 div sub add
```

```command
$ arith eval <<< "1 * -2 + (3 + 4) - 8 / 4"
3
```
