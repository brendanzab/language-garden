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
  error: error: variable `X` does not appear in the body of the rule
  
    ┌─ <stdin>:3:0
    │
  3 │ foo(X, Y) <- thing(Y).
    │ ^^^
  
  [1]
