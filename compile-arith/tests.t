A number
  $ compile-arith <<< "1"
  program =
    num 1
    
  tree  = 1
  stack = 1

Negating numbers
  $ compile-arith <<< "-32"
  program =
    num 32
    neg
    
  tree  = -32
  stack = -32

Adding numbers
  $ compile-arith <<< "1 + 2"
  program =
    num 1
    num 2
    add
    
  tree  = 3
  stack = 3

Subtracting numbers
  $ compile-arith <<< "1 - 2"
  program =
    num 1
    num 2
    sub
    
  tree  = -1
  stack = -1

Multiplying numbers
  $ compile-arith <<< "1 * 2"
  program =
    num 1
    num 2
    mul
    
  tree  = 2
  stack = 2

Dividing numbers
  $ compile-arith <<< "1 / 2"
  program =
    num 1
    num 2
    div
    
  tree  = 0
  stack = 0

Complicated stuff
  $ compile-arith <<< "1 * -2 + (3 + 4) - 8 / 4"
  program =
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
    
  tree  = 3
  stack = 3

Something broken
  $ compile-arith <<< "1 + 2 + (3 +"
  <input>:1:13: syntax error
  [1]
