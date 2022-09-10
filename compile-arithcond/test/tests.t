A number
  $ compile-arithcond <<< "1"
  code  = 1
  tree  = 1
  stack = 1

Negating numbers
  $ compile-arithcond <<< "-32"
  code  = 32 neg
  tree  = -32
  stack = -32

Adding numbers
  $ compile-arithcond <<< "1 + 2"
  code  = 1 2 add
  tree  = 3
  stack = 3

Subtracting numbers
  $ compile-arithcond <<< "1 - 2"
  code  = 1 2 sub
  tree  = -1
  stack = -1

Multiplying numbers
  $ compile-arithcond <<< "1 * 2"
  code  = 1 2 mul
  tree  = 2
  stack = 2

Dividing numbers
  $ compile-arithcond <<< "1 / 2"
  code  = 1 2 div
  tree  = 0
  stack = 0

Complicated stuff
  $ compile-arithcond <<< "1 * -2 + (3 + 4) - 8 / 4"
  code  = 1 2 neg mul 3 4 add 8 4 div sub add
  tree  = 3
  stack = 3

If expressions
  $ compile-arithcond <<< "if 1 * -2 = 3 * 4 then (3 + 4) - 8 / 4 else 7 + 8"
  code  = 1 2 neg mul 3 4 mul eq [ 3 4 add 8 4 div sub ] [ 7 8 add ] if
  tree  = 15
  stack = 15

Something broken
  $ compile-arithcond <<< "1 + 2 + (3 +"
  <input>:1:13: syntax error
  [1]
