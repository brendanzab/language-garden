# Arithmetic expression compiler

A compiler from a language of arithmetic and conditional expressions to:

- Stack machine instructions

  ```command
  $ arithcond compile --stack <<< "if 1 * -2 = 3 * 4 then (3 + 4) - 8 / 4 else 7 + 8"
  int 1;
  int 2;
  neg;
  mul;
  int 3;
  int 4;
  mul;
  eq;
  code [ int 3; int 4; add; int 8; int 4; div; sub; ];
  code [ int 7; int 8; add; ];
  if;
  ```

- A-Normal Form

  ```command
  $ arith compile --anf <<< "if 1 * -2 = 3 * 4 then (3 + 4) - 8 / 4 else 7 + 8"
  let e0 := neg 2;
  let e1 := mul 1 e0;
  let e2 := mul 3 4;
  let e3 := eq e1 e2;
  if e3 then
    let e4 := add 3 4;
    let e5 := div 8 4;
    sub e4 e5
  else
    add 7 8
  ```

The correctness of type checking, compilation and pretty printing are tested
with property-based tests implemented using the [qcheck] library.

[qcheck]: https://github.com/c-cube/qcheck

## Resources

### A-Normal Form

- [A-Normalization: Why and How](https://matt.might.net/articles/a-normalization/)
- [The essence of compiling with continuations](https://doi.org/10.1145/173262.155113)
- [ANF Conversion](https://compiler.club/anf-conversion/)
