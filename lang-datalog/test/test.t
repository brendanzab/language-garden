Unrestricted range
  $ cat ./unrestricted-range.datalog | datalog
  Fatal error: exception Failure("the input program is not range-restricted")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__Main in file "lang-datalog/main.ml", line 79, characters 11-38
  [2]

Mismatched arities
  $ cat ./arity-mismatch.datalog | datalog
  ────────────────────────────────────────────────────────────────────────────────
  Knowledge Base
  ────────────────────────────────────────────────────────────────────────────────
  q(1).
  q(1, 2).
  p(1, 2).
  
  ────────────────────────────────────────────────────────────────────────────────
  Query Results
  ────────────────────────────────────────────────────────────────────────────────
  ? p(A, B).
    > A := 1.
      B := 2.
    yes
  
