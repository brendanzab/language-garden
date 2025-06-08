  $ alias executable=arith

Usage
  $ executable
  arith: required COMMAND name is missing, must be either 'compile' or 'exec'.
  Usage: arith COMMAND …
  Try 'arith --help' for more information.
  [124]
  $ executable compile
  arith: required option --target is missing
  Usage: arith compile [--target=TARGET] [OPTION]…
  Try 'arith compile --help' or 'arith --help' for more information.
  [124]

A number
  $ cat >test-int <<< "1"
  $ cat test-int | executable compile --target=stack
  int 1;
  $ cat test-int | executable compile --target=anf
  1
  $ cat test-int | executable exec --target=anf
  1
  $ cat test-int | executable exec --target=tree
  1
  $ cat test-int | executable exec --target=stack
  int 1;
  $ cat test-int | executable exec --target=anf
  1

Negating numbers
  $ cat >test-neg <<< "-32"
  $ cat test-neg | executable compile --target=stack
  int 32;
  neg;
  $ cat test-neg | executable compile --target=anf
  neg 32
  $ cat test-neg | executable exec --target=anf
  -32
  $ cat test-neg | executable exec --target=tree
  -32
  $ cat test-neg | executable exec --target=stack
  int -32;
  $ cat test-neg | executable exec --target=anf
  -32

Adding numbers
  $ cat >test-add <<< "1 + 2"
  $ cat test-add | executable compile --target=stack
  int 1;
  int 2;
  add;
  $ cat test-add | executable compile --target=anf
  add 1 2
  $ cat test-add | executable exec --target=anf
  3
  $ cat test-add | executable exec --target=tree
  3
  $ cat test-add | executable exec --target=stack
  int 3;
  $ cat test-add | executable exec --target=anf
  3

Subtracting numbers
  $ cat >test-sub <<< "1 - 2"
  $ cat test-sub | executable compile --target=stack
  int 1;
  int 2;
  sub;
  $ cat test-sub | executable compile --target=anf
  sub 1 2
  $ cat test-sub | executable exec --target=anf
  -1
  $ cat test-sub | executable exec --target=tree
  -1
  $ cat test-sub | executable exec --target=stack
  int -1;
  $ cat test-sub | executable exec --target=anf
  -1

Multiplying numbers
  $ cat >test-mul <<< "1 * 2"
  $ cat test-mul | executable compile --target=stack
  int 1;
  int 2;
  mul;
  $ cat test-mul | executable compile --target=anf
  mul 1 2
  $ cat test-mul | executable exec --target=anf
  2
  $ cat test-mul | executable exec --target=tree
  2
  $ cat test-mul | executable exec --target=stack
  int 2;
  $ cat test-mul | executable exec --target=anf
  2

Dividing numbers
  $ cat >test-div <<< "1 / 2"
  $ cat test-div | executable compile --target=stack
  int 1;
  int 2;
  div;
  $ cat test-div | executable compile --target=anf
  div 1 2
  $ cat test-div | executable exec --target=anf
  0
  $ cat test-div | executable exec --target=tree
  0
  $ cat test-div | executable exec --target=stack
  int 0;
  $ cat test-div | executable exec --target=anf
  0

Multiple operators
  $ cat >test-multiple-ops <<< "1 + -2 * 7"
  $ cat test-multiple-ops | executable compile --target=stack
  int 1;
  int 2;
  neg;
  int 7;
  mul;
  add;
  $ cat test-multiple-ops | executable compile --target=anf
  let e0 := neg 2;
  let e1 := mul e0 7;
  add 1 e1
  $ cat test-multiple-ops | executable exec --target=tree
  -13
  $ cat test-multiple-ops | executable exec --target=stack
  int -13;
  $ cat test-multiple-ops | executable exec --target=anf
  -13

Complicated stuff
  $ cat >test-complicated <<< "1 * -2 + (3 + 4) - 8 / 4"
  $ cat test-complicated | executable compile --target=stack
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
  $ cat test-complicated | executable compile --target=anf
  let e0 := neg 2;
  let e1 := mul 1 e0;
  let e2 := add 3 4;
  let e3 := div 8 4;
  let e4 := sub e2 e3;
  add e1 e4
  $ cat test-complicated | executable exec --target=tree
  3
  $ cat test-complicated | executable exec --target=stack
  int 3;
  $ cat test-complicated | executable exec --target=anf
  3

Something broken
  $ executable compile --target=stack <<< "1 + 2 + (3 +"
  error: syntax error
    ┌─ <stdin>:2:0
    │
  2 │ 
    │ ^
  [1]
