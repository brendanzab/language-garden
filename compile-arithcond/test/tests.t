A number
  $ cat >test-int <<< "1"
  $ cat test-int | arithcond compile --stack
  int 1;
  $ cat test-int | arithcond exec --tree
  1 : Int
  $ cat test-int | arithcond exec --stack
  int 1;

A boolean
  $ cat >test-int <<< "true"
  $ cat test-int | arithcond compile --stack
  true;
  $ cat test-int | arithcond exec --tree
  true : Bool
  $ cat test-int | arithcond exec --stack
  true;

Negating numbers
  $ cat >test-neg <<< "-32"
  $ cat test-neg | arithcond compile --stack
  int 32;
  neg;
  $ cat test-neg | arithcond exec --tree
  -32 : Int
  $ cat test-neg | arithcond exec --stack
  int -32;

Adding numbers
  $ cat >test-add <<< "1 + 2"
  $ cat test-add | arithcond compile --stack
  int 1;
  int 2;
  add;
  $ cat test-add | arithcond exec --tree
  3 : Int
  $ cat test-add | arithcond exec --stack
  int 3;

Subtracting numbers
  $ cat >test-sub <<< "1 - 2"
  $ cat test-sub | arithcond compile --stack
  int 1;
  int 2;
  sub;
  $ cat test-sub | arithcond exec --tree
  -1 : Int
  $ cat test-sub | arithcond exec --stack
  int -1;

Multiplying numbers
  $ cat >test-mul <<< "1 * 2"
  $ cat test-mul | arithcond compile --stack
  int 1;
  int 2;
  mul;
  $ cat test-mul | arithcond exec --tree
  2 : Int
  $ cat test-mul | arithcond exec --stack
  int 2;

Dividing numbers
  $ cat >test-div <<< "1 / 2"
  $ cat test-div | arithcond compile --stack
  int 1;
  int 2;
  div;
  $ cat test-div | arithcond exec --tree
  0 : Int
  $ cat test-div | arithcond exec --stack
  int 0;

Complicated stuff
  $ cat >test-complicated <<< "1 * -2 + (3 + 4) - 8 / 4"
  $ cat test-complicated | arithcond compile --stack
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
  $ cat test-complicated | arithcond compile --anf
  let e0 := neg 2;
  let e1 := mul 1 e0;
  let e2 := add 3 4;
  let e3 := div 8 4;
  let e4 := sub e2 e3;
  let e5 := add e1 e4;
  e5
  $ cat test-complicated | arithcond exec --tree
  3 : Int
  $ cat test-complicated | arithcond exec --stack
  int 3;

If expressions
  $ arithcond compile --anf <<< "(if 3 = 4 then 3 else 7) + 1"
  let e0 := eq 3 4;
  let e1 :=
    if e0 then
      3
    else
      7;
  let e2 := add e1 1;
  e2
  $ arithcond exec --anf <<< "(if false then 0 else 0) + 1"
  1 : Int

If expressions
  $ cat >test-if <<< "if 1 * -2 = 3 * 4 then (3 + 4) - 8 / 4 else 7 + 8"
  $ cat test-if | arithcond compile --stack
  int 1;
  int 2;
  neg;
  mul;
  int 3;
  int 4;
  mul;
  eq;
  code [ int 3; int 4; add; int 8; int 4; div; sub; ];
  code [ int 7; int 8; add; ];
  if;
  $ cat test-if | arithcond compile --anf
  let e0 := neg 2;
  let e1 := mul 1 e0;
  let e2 := mul 3 4;
  let e3 := eq e1 e2;
  let e8 :=
    if e3 then
      let e5 := add 3 4;
      let e6 := div 8 4;
      let e7 := sub e5 e6;
      e7
    else
      let e4 := add 7 8;
      e4;
  e8
  $ cat test-if | arithcond exec --tree
  15 : Int
  $ cat test-if | arithcond exec --stack
  int 15;
  $ cat test-if | arithcond exec --anf
  15 : Int

Something broken
  $ arithcond compile --stack <<< "1 + 2 + (3 +"
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
