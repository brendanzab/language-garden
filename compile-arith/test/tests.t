A number
  $ cat >test-int <<< "1"
  $ cat test-int | arith compile --stack
  int 1;
  $ cat test-int | arith compile --anf
  1
  $ cat test-int | arith exec --anf
  1
  $ cat test-int | arith exec --tree
  1
  $ cat test-int | arith exec --stack
  int 1;
  $ cat test-int | arith exec --anf
  1

Negating numbers
  $ cat >test-neg <<< "-32"
  $ cat test-neg | arith compile --stack
  int 32;
  neg;
  $ cat test-neg | arith compile --anf
  neg 32
  $ cat test-neg | arith exec --anf
  -32
  $ cat test-neg | arith exec --tree
  -32
  $ cat test-neg | arith exec --stack
  int -32;
  $ cat test-neg | arith exec --anf
  -32

Adding numbers
  $ cat >test-add <<< "1 + 2"
  $ cat test-add | arith compile --stack
  int 1;
  int 2;
  add;
  $ cat test-add | arith compile --anf
  add 1 2
  $ cat test-add | arith exec --anf
  3
  $ cat test-add | arith exec --tree
  3
  $ cat test-add | arith exec --stack
  int 3;
  $ cat test-add | arith exec --anf
  3

Subtracting numbers
  $ cat >test-sub <<< "1 - 2"
  $ cat test-sub | arith compile --stack
  int 1;
  int 2;
  sub;
  $ cat test-sub | arith compile --anf
  sub 1 2
  $ cat test-sub | arith exec --anf
  -1
  $ cat test-sub | arith exec --tree
  -1
  $ cat test-sub | arith exec --stack
  int -1;
  $ cat test-sub | arith exec --anf
  -1

Multiplying numbers
  $ cat >test-mul <<< "1 * 2"
  $ cat test-mul | arith compile --stack
  int 1;
  int 2;
  mul;
  $ cat test-mul | arith compile --anf
  mul 1 2
  $ cat test-mul | arith exec --anf
  2
  $ cat test-mul | arith exec --tree
  2
  $ cat test-mul | arith exec --stack
  int 2;
  $ cat test-mul | arith exec --anf
  2

Dividing numbers
  $ cat >test-div <<< "1 / 2"
  $ cat test-div | arith compile --stack
  int 1;
  int 2;
  div;
  $ cat test-div | arith compile --anf
  div 1 2
  $ cat test-div | arith exec --anf
  0
  $ cat test-div | arith exec --tree
  0
  $ cat test-div | arith exec --stack
  int 0;
  $ cat test-div | arith exec --anf
  0

Multiple operators
  $ cat >test-multiple-ops <<< "1 + -2 * 7"
  $ cat test-multiple-ops | arith compile --stack
  int 1;
  int 2;
  neg;
  int 7;
  mul;
  add;
  $ cat test-multiple-ops | arith compile --anf
  let e0 := neg 2;
  let e1 := mul e0 7;
  add 1 e1
  $ cat test-multiple-ops | arith exec --tree
  -13
  $ cat test-multiple-ops | arith exec --stack
  int -13;
  $ cat test-multiple-ops | arith exec --anf
  -13

Complicated stuff
  $ cat >test-complicated <<< "1 * -2 + (3 + 4) - 8 / 4"
  $ cat test-complicated | arith compile --stack
  int 1;
  int 2;
  neg;
  mul;
  int 3;
  int 4;
  add;
  int 8;
  int 4;
  div;
  sub;
  add;
  $ cat test-complicated | arith compile --anf
  let e0 := neg 2;
  let e1 := mul 1 e0;
  let e2 := add 3 4;
  let e3 := div 8 4;
  let e4 := sub e2 e3;
  add e1 e4
  $ cat test-complicated | arith exec --tree
  3
  $ cat test-complicated | arith exec --stack
  int 3;
  $ cat test-complicated | arith exec --anf
  3

Something broken
  $ arith compile --stack <<< "1 + 2 + (3 +"
  <input>:1:13: syntax error
  [1]
