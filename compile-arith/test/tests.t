A number
  $ cat >test-int <<< "1"
  $ cat test-int | arith compile
  1
  $ cat test-int | arith exec --tree
  1
  $ cat test-int | arith exec --stack
  1

Negating numbers
  $ cat >test-neg <<< "-32"
  $ cat test-neg | arith compile
  32 neg
  $ cat test-neg | arith exec --tree
  -32
  $ cat test-neg | arith exec --stack
  -32

Adding numbers
  $ cat >test-add <<< "1 + 2"
  $ cat test-add | arith compile
  1 2 add
  $ cat test-add | arith exec --tree
  3
  $ cat test-add | arith exec --stack
  3

Subtracting numbers
  $ cat >test-sub <<< "1 - 2"
  $ cat test-sub | arith compile
  1 2 sub
  $ cat test-sub | arith exec --tree
  -1
  $ cat test-sub | arith exec --stack
  -1

Multiplying numbers
  $ cat >test-mul <<< "1 * 2"
  $ cat test-mul | arith compile
  1 2 mul
  $ cat test-mul | arith exec --tree
  2
  $ cat test-mul | arith exec --stack
  2

Dividing numbers
  $ cat >test-div <<< "1 / 2"
  $ cat test-div | arith compile
  1 2 div
  $ cat test-div | arith exec --tree
  0
  $ cat test-div | arith exec --stack
  0

Complicated stuff
  $ cat >test-complicated <<< "1 * -2 + (3 + 4) - 8 / 4"
  $ cat test-complicated | arith compile
  1 2 neg mul 3 4 add 8 4 div sub add
  $ cat test-complicated | arith exec --tree
  3
  $ cat test-complicated | arith exec --stack
  3

Something broken
  $ arith compile <<< "1 + 2 + (3 +"
  <input>:1:13: syntax error
  [1]
