A number
  $ arith <<< "1"
  num 1

Adding numbers
  $ arith <<< "1 + 2"
  num 1
  num 2
  add

Subtracting numbers
  $ arith <<< "1 - 2"
  num 1
  num 2
  sub

Complicated stuff
  $ arith <<< "1 + 2 + (3 + 4) - 2"
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
  $ arith <<< "1 + 2 + (3 +"
  <input>:1:13: syntax error
  [1]
