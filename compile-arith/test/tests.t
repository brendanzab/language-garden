Usage
  $ arith
  arith: required COMMAND name is missing, must be either 'compile' or 'exec'.
  Usage: arith COMMAND …
  Try 'arith --help' for more information.
  [124]
  $ arith compile
  arith: required option --target is missing
  Usage: arith compile [--target=TARGET] [OPTION]…
  Try 'arith compile --help' or 'arith --help' for more information.
  [124]

A number
  $ cat >test-int <<< "1"
  $ cat test-int | arith compile --target=stack
  int 1;
  $ cat test-int | arith compile --target=anf
  1
  $ cat test-int | arith exec --target=anf
  1
  $ cat test-int | arith exec --target=tree
  1
  $ cat test-int | arith exec --target=stack
  int 1;
  $ cat test-int | arith exec --target=anf
  1

Negating numbers
  $ cat >test-neg <<< "-32"
  $ cat test-neg | arith compile --target=stack
  int 32;
  neg;
  $ cat test-neg | arith compile --target=anf
  neg 32
  $ cat test-neg | arith exec --target=anf
  -32
  $ cat test-neg | arith exec --target=tree
  -32
  $ cat test-neg | arith exec --target=stack
  int -32;
  $ cat test-neg | arith exec --target=anf
  -32

Adding numbers
  $ cat >test-add <<< "1 + 2"
  $ cat test-add | arith compile --target=stack
  int 1;
  int 2;
  add;
  $ cat test-add | arith compile --target=anf
  add 1 2
  $ cat test-add | arith exec --target=anf
  3
  $ cat test-add | arith exec --target=tree
  3
  $ cat test-add | arith exec --target=stack
  int 3;
  $ cat test-add | arith exec --target=anf
  3

Subtracting numbers
  $ cat >test-sub <<< "1 - 2"
  $ cat test-sub | arith compile --target=stack
  int 1;
  int 2;
  sub;
  $ cat test-sub | arith compile --target=anf
  sub 1 2
  $ cat test-sub | arith exec --target=anf
  -1
  $ cat test-sub | arith exec --target=tree
  -1
  $ cat test-sub | arith exec --target=stack
  int -1;
  $ cat test-sub | arith exec --target=anf
  -1

Multiplying numbers
  $ cat >test-mul <<< "1 * 2"
  $ cat test-mul | arith compile --target=stack
  int 1;
  int 2;
  mul;
  $ cat test-mul | arith compile --target=anf
  mul 1 2
  $ cat test-mul | arith exec --target=anf
  2
  $ cat test-mul | arith exec --target=tree
  2
  $ cat test-mul | arith exec --target=stack
  int 2;
  $ cat test-mul | arith exec --target=anf
  2

Dividing numbers
  $ cat >test-div <<< "1 / 2"
  $ cat test-div | arith compile --target=stack
  int 1;
  int 2;
  div;
  $ cat test-div | arith compile --target=anf
  div 1 2
  $ cat test-div | arith exec --target=anf
  0
  $ cat test-div | arith exec --target=tree
  0
  $ cat test-div | arith exec --target=stack
  int 0;
  $ cat test-div | arith exec --target=anf
  0

Multiple operators
  $ cat >test-multiple-ops <<< "1 + -2 * 7"
  $ cat test-multiple-ops | arith compile --target=stack
  int 1;
  int 2;
  neg;
  int 7;
  mul;
  add;
  $ cat test-multiple-ops | arith compile --target=anf
  let e0 := neg 2;
  let e1 := mul e0 7;
  add 1 e1
  $ cat test-multiple-ops | arith exec --target=tree
  -13
  $ cat test-multiple-ops | arith exec --target=stack
  int -13;
  $ cat test-multiple-ops | arith exec --target=anf
  -13

Complicated stuff
  $ cat >test-complicated <<< "1 * -2 + (3 + 4) - 8 / 4"
  $ cat test-complicated | arith compile --target=stack
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
  $ cat test-complicated | arith compile --target=anf
  let e0 := neg 2;
  let e1 := mul 1 e0;
  let e2 := add 3 4;
  let e3 := div 8 4;
  let e4 := sub e2 e3;
  add e1 e4
  $ cat test-complicated | arith exec --target=tree
  3
  $ cat test-complicated | arith exec --target=stack
  int 3;
  $ cat test-complicated | arith exec --target=anf
  3

Something broken
  $ arith compile --target=stack <<< "1 + 2 + (3 +"
  error: syntax error
    ┌─ <stdin>:1:12
    │
  1 │ 1 + 2 + (3 +
    │             ^
  [1]
