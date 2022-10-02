A number
  $ cat >test-int <<< "1"
  $ cat test-int | arithcond compile
  1
  $ cat test-int | arithcond exec --tree
  1 : Int
  $ cat test-int | arithcond exec --stack
  1

A boolean
  $ cat >test-int <<< "true"
  $ cat test-int | arithcond compile
  true
  $ cat test-int | arithcond exec --tree
  true : Bool
  $ cat test-int | arithcond exec --stack
  true

Negating numbers
  $ cat >test-neg <<< "-32"
  $ cat test-neg | arithcond compile
  32 neg
  $ cat test-neg | arithcond exec --tree
  -32 : Int
  $ cat test-neg | arithcond exec --stack
  -32

Adding numbers
  $ cat >test-add <<< "1 + 2"
  $ cat test-add | arithcond compile
  1 2 add
  $ cat test-add | arithcond exec --tree
  3 : Int
  $ cat test-add | arithcond exec --stack
  3

Subtracting numbers
  $ cat >test-sub <<< "1 - 2"
  $ cat test-sub | arithcond compile
  1 2 sub
  $ cat test-sub | arithcond exec --tree
  -1 : Int
  $ cat test-sub | arithcond exec --stack
  -1

Multiplying numbers
  $ cat >test-mul <<< "1 * 2"
  $ cat test-mul | arithcond compile
  1 2 mul
  $ cat test-mul | arithcond exec --tree
  2 : Int
  $ cat test-mul | arithcond exec --stack
  2

Dividing numbers
  $ cat >test-div <<< "1 / 2"
  $ cat test-div | arithcond compile
  1 2 div
  $ cat test-div | arithcond exec --tree
  0 : Int
  $ cat test-div | arithcond exec --stack
  0

Complicated stuff
  $ cat >test-complicated <<< "1 * -2 + (3 + 4) - 8 / 4"
  $ cat test-complicated | arithcond compile
  1 2 neg mul 3 4 add 8 4 div sub add
  $ cat test-complicated | arithcond exec --tree
  3 : Int
  $ cat test-complicated | arithcond exec --stack
  3

If expressions
  $ cat >test-if <<< "if 1 * -2 = 3 * 4 then (3 + 4) - 8 / 4 else 7 + 8"
  $ cat test-if | arithcond compile
  1 2 neg mul 3 4 mul eq [ 3 4 add 8 4 div sub ] [ 7 8 add ] if
  $ cat test-if | arithcond exec --tree
  15 : Int
  $ cat test-if | arithcond exec --stack
  15

Something broken
  $ arithcond compile <<< "1 + 2 + (3 +"
  <input>:1:13: syntax error
  [1]

Mismatched negation operands
  $ arithcond exec <<< "-false"
  error: mismatched types: expected Int, found Bool
  [1]

Mismatched addition operands
  $ arithcond exec <<< "1 + true"
  error: mismatched types: expected Int, found Bool
  [1]
  $ arithcond exec <<< "true + 1"
  error: mismatched types: expected Int, found Bool
  [1]

Mismatched subtraction operands
  $ arithcond exec <<< "1 - true"
  error: mismatched types: expected Int, found Bool
  [1]
  $ arithcond exec <<< "true - 1"
  error: mismatched types: expected Int, found Bool
  [1]

Mismatched multiplication operands
  $ arithcond exec <<< "1 * true"
  error: mismatched types: expected Int, found Bool
  [1]
  $ arithcond exec <<< "true * 1"
  error: mismatched types: expected Int, found Bool
  [1]

Mismatched division operands
  $ arithcond exec <<< "1 / true"
  error: mismatched types: expected Int, found Bool
  [1]
  $ arithcond exec <<< "true / 1"
  error: mismatched types: expected Int, found Bool
  [1]

Mismatched equals operands
  $ arithcond exec <<< "true = 3"
  error: mismatched types: expected Bool, found Int
  [1]
  $ arithcond exec <<< "3 = true"
  error: mismatched types: expected Int, found Bool
  [1]

Mismatched if condition
  $ arithcond exec <<< "if 1 * 3 then 3 else 4"
  error: mismatched types: expected Bool, found Int
  [1]

Mismatched if branches
  $ arithcond exec <<< "if 3 = 4 then true else 4"
  error: mismatched types: expected Bool, found Int
  [1]
