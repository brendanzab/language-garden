A number
  $ compile-arith <<< "1"
  num 1

Adding numbers
  $ compile-arith <<< "1 + 2"
  num 1
  num 2
  add

Subtracting numbers
  $ compile-arith <<< "1 - 2"
  num 1
  num 2
  sub

Complicated stuff
  $ compile-arith <<< "1 + 2 + (3 + 4) - 2"
  num 1
  num 2
  num 3
  num 4
  add
  num 2
  sub
  add
  add

Something broken
  $ compile-arith <<< "1 + 2 + (3 +"
  <input>:1:13: syntax error
  [1]
