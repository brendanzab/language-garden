# Arithmetic expression compiler

A compiler from a language of arithmetic expressions to:

- Stack machine instructions

  ```command
  $ arith compile --stack <<< "1 + -2 * 7"
  int 1;
  int 2;
  neg;
  int 7;
  mul;
  add;
  ```

- A-Normal Form

  ```command
  $ arith compile --anf <<< "1 + -2 * 7"
  let e0 := neg 2;
  let e1 := mul e0 7;
  add 1 e1
  ```

The correctness of compilation and pretty printing are tested with property-based
tests implemented using the [qcheck] library.

[qcheck]: https://github.com/c-cube/qcheck

## Resources

### A-Normal Form

- [A-Normalization: Why and How](https://matt.might.net/articles/a-normalization/)
- [The essence of compiling with continuations](https://doi.org/10.1145/173262.155113)
- [ANF Conversion](https://compiler.club/anf-conversion/)
