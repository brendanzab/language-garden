A number
  $ cat >test-int <<< "1"
  $ cat test-int | arithcond compile
  1
  $ cat test-int | arithcond exec --tree
  1
  $ cat test-int | arithcond exec --stack
  1

Negating numbers
  $ cat >test-neg <<< "-32"
  $ cat test-neg | arithcond compile
  32 neg
  $ cat test-neg | arithcond exec --tree
  -32
  $ cat test-neg | arithcond exec --stack
  -32

Adding numbers
  $ cat >test-add <<< "1 + 2"
  $ cat test-add | arithcond compile
  1 2 add
  $ cat test-add | arithcond exec --tree
  3
  $ cat test-add | arithcond exec --stack
  3

Subtracting numbers
  $ cat >test-sub <<< "1 - 2"
  $ cat test-sub | arithcond compile
  1 2 sub
  $ cat test-sub | arithcond exec --tree
  -1
  $ cat test-sub | arithcond exec --stack
  -1

Multiplying numbers
  $ cat >test-mul <<< "1 * 2"
  $ cat test-mul | arithcond compile
  1 2 mul
  $ cat test-mul | arithcond exec --tree
  2
  $ cat test-mul | arithcond exec --stack
  2

Dividing numbers
  $ cat >test-div <<< "1 / 2"
  $ cat test-div | arithcond compile
  1 2 div
  $ cat test-div | arithcond exec --tree
  0
  $ cat test-div | arithcond exec --stack
  0

Complicated stuff
  $ cat >test-complicated <<< "1 * -2 + (3 + 4) - 8 / 4"
  $ cat test-complicated | arithcond compile
  1 2 neg mul 3 4 add 8 4 div sub add
  $ cat test-complicated | arithcond exec --tree
  3
  $ cat test-complicated | arithcond exec --stack
  3

If expressions
  $ cat >test-if <<< "if 1 * -2 = 3 * 4 then (3 + 4) - 8 / 4 else 7 + 8"
  $ cat test-if | arithcond compile
  1 2 neg mul 3 4 mul eq [ 3 4 add 8 4 div sub ] [ 7 8 add ] if
  $ cat test-if | arithcond exec --tree
  15
  $ cat test-if | arithcond exec --stack
  15

Something broken
  $ arithcond compile <<< "1 + 2 + (3 +"
  <input>:1:13: syntax error
  [1]
