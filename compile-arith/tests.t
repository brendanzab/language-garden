A number
  $ compile-arith <<< "1"
  num 1

Negating numbers
  $ compile-arith <<< "-32"
  num 32
  neg

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

Multiplying numbers
  $ compile-arith <<< "1 * 2"
  num 1
  num 2
  mul

Dividing numbers
  $ compile-arith <<< "1 / 2"
  num 1
  num 2
  div

Complicated stuff
  $ compile-arith <<< "1 * -2 + (3 + 4) - 8 / 4"
  num 1
  num 2
  neg
  mul
  num 3
  num 4
  add
  num 8
  num 4
  div
  sub
  add

Something broken
  $ compile-arith <<< "1 + 2 + (3 +"
  <input>:1:13: syntax error
  [1]
