Predicates of different arities
  $ cat ./arities.datalog | datalog
  ────────────────────────────────────────────────────────────────────────────────
  Knowledge Base
  ────────────────────────────────────────────────────────────────────────────────
  foo(1).
  foo(3, 4).
  foo(5).
  foo(3, 5).
  foo(1, 5).
  foo(2, 1).
  foo(2, 5).
  
  ────────────────────────────────────────────────────────────────────────────────
  Query Results
  ────────────────────────────────────────────────────────────────────────────────
  ? foo(X).
    > X := 1.
    > X := 5.
    yes
  
  ? foo(X, 5).
    > X := 3.
    > X := 1.
    > X := 2.
    yes
  

Unrestricted range
  $ cat ./unrestricted-range.datalog | datalog
  Fatal error: exception Failure("the input program is not range-restricted")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__Main in file "lang-datalog/main.ml", line 79, characters 11-38
  [2]
