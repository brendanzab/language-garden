Usage
  $ arithcond
  arithcond: required COMMAND name is missing, must be either 'compile' or 'exec'.
  Usage: arithcond COMMAND …
  Try 'arithcond --help' for more information.
  [124]
  $ arithcond compile
  arithcond: required option --target is missing
  Usage: arithcond compile [--target=TARGET] [OPTION]…
  Try 'arithcond compile --help' or 'arithcond --help' for more information.
  [124]

A number
  $ cat >test-int <<< "1"
  $ cat test-int | arithcond compile --target=stack
  int 1;
  $ cat test-int | arithcond exec --target=tree
  1 : Int
  $ cat test-int | arithcond exec --target=stack
  int 1;

A boolean
  $ cat >test-int <<< "true"
  $ cat test-int | arithcond compile --target=stack
  true;
  $ cat test-int | arithcond exec --target=tree
  true : Bool
  $ cat test-int | arithcond exec --target=stack
  true;

Negating numbers
  $ cat >test-neg <<< "-32"
  $ cat test-neg | arithcond compile --target=stack
  int 32;
  neg;
  $ cat test-neg | arithcond exec --target=tree
  -32 : Int
  $ cat test-neg | arithcond exec --target=stack
  int -32;

Adding numbers
  $ cat >test-add <<< "1 + 2"
  $ cat test-add | arithcond compile --target=stack
  int 1;
  int 2;
  add;
  $ cat test-add | arithcond exec --target=tree
  3 : Int
  $ cat test-add | arithcond exec --target=stack
  int 3;

Subtracting numbers
  $ cat >test-sub <<< "1 - 2"
  $ cat test-sub | arithcond compile --target=stack
  int 1;
  int 2;
  sub;
  $ cat test-sub | arithcond exec --target=tree
  -1 : Int
  $ cat test-sub | arithcond exec --target=stack
  int -1;

Multiplying numbers
  $ cat >test-mul <<< "1 * 2"
  $ cat test-mul | arithcond compile --target=stack
  int 1;
  int 2;
  mul;
  $ cat test-mul | arithcond exec --target=tree
  2 : Int
  $ cat test-mul | arithcond exec --target=stack
  int 2;

Dividing numbers
  $ cat >test-div <<< "1 / 2"
  $ cat test-div | arithcond compile --target=stack
  int 1;
  int 2;
  div;
  $ cat test-div | arithcond exec --target=tree
  0 : Int
  $ cat test-div | arithcond exec --target=stack
  int 0;

Complicated stuff
  $ cat >test-complicated <<< "1 * -2 + (3 + 4) - 8 / 4"
  $ cat test-complicated | arithcond compile --target=stack
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
  $ cat test-complicated | arithcond compile --target=anf
  let y0 := neg 2;
  let x1 := mul 1 y0;
  let x2 := add 3 4;
  let y3 := div 8 4;
  let y4 := sub x2 y3;
  add x1 y4
  $ cat test-complicated | arithcond exec --target=tree
  3 : Int
  $ cat test-complicated | arithcond exec --target=stack
  int 3;

If expressions
  $ arithcond compile --target=anf <<< "(if 3 = 4 then 3 else 7) + 1"
  let b0 := eq 3 4;
  let join j1 p2 := add p2 1;
  if b0 then
    jump j1 3
  else
    jump j1 7
  $ arithcond exec --target=anf <<< "(if false then 0 else 0) + 1"
  1 : Int

If expressions
  $ cat >test-if <<< "if 1 * -2 = 3 * 4 then (3 + 4) - 8 / 4 else 7 + 8"
  $ cat test-if | arithcond compile --target=stack
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
  $ cat test-if | arithcond compile --target=anf
  let y0 := neg 2;
  let x1 := mul 1 y0;
  let y2 := mul 3 4;
  let b3 := eq x1 y2;
  let join j4 p5 := p5;
  if b3 then
    let x7 := add 3 4;
    let y8 := div 8 4;
    let true9 := sub x7 y8;
    jump j4 true9
  else
    let false6 := add 7 8;
    jump j4 false6
  $ cat test-if | arithcond exec --target=tree
  15 : Int
  $ cat test-if | arithcond exec --target=stack
  int 15;
  $ cat test-if | arithcond exec --target=anf
  15 : Int

Nested if expressions
  $ cat >test-if-nested <<< "1 * (if 1 = 3 then 42 else 7 + 8) + 3"
  $ cat test-if-nested | arithcond compile --target=stack
  int 1;
  int 1;
  int 3;
  eq;
  code [ int 42; ];
  code [ int 7; int 8; add; ];
  if;
  mul;
  int 3;
  add;
  $ cat test-if-nested | arithcond compile --target=anf
  let b0 := eq 1 3;
  let join j1 p2 := let x4 := mul 1 p2; add x4 3;
  if b0 then
    jump j1 42
  else
    let false3 := add 7 8;
    jump j1 false3
  $ cat test-if-nested | arithcond exec --target=tree
  18 : Int
  $ cat test-if-nested | arithcond exec --target=stack
  int 18;
  $ cat test-if-nested | arithcond exec --target=anf
  18 : Int

Simple let expressions
  $ cat >test-let-simple <<< "let x := 3; x * 4"
  $ cat test-let-simple | arithcond compile --target=stack
  int 3;
  begin-let;
  access 0;
  int 4;
  mul;
  end-let;
  $ cat test-let-simple | arithcond compile --target=anf
  mul 3 4
  $ cat test-let-simple | arithcond exec --target=tree
  12 : Int
  $ cat test-let-simple | arithcond exec --target=stack
  int 12;
  $ cat test-let-simple | arithcond exec --target=anf
  12 : Int

Let expressions
  $ cat >test-let <<< "let x := 3 * 4; if x = 5 then (let y := 3 + x; 8 - y / 4) else x + 8"
  $ cat test-let | arithcond compile --target=stack
  int 3;
  int 4;
  mul;
  begin-let;
  access 0;
  int 5;
  eq;
  code [ int 3; access 0; add; begin-let; int 8; access 0; int 4; div; sub;
       end-let; ];
  code [ access 0; int 8; add; ];
  if;
  end-let;
  $ cat test-let | arithcond compile --target=anf
  let x0 := mul 3 4;
  let b1 := eq x0 5;
  let join j2 p3 := p3;
  if b1 then
    let y5 := add 3 x0;
    let y6 := div y5 4;
    let true7 := sub 8 y6;
    jump j2 true7
  else
    let false4 := add x0 8;
    jump j2 false4
  $ cat test-let | arithcond exec --target=tree
  20 : Int
  $ cat test-let | arithcond exec --target=stack
  int 20;
  $ cat test-let | arithcond exec --target=anf
  20 : Int

Wierd binding
  $ cat >test-weird-let-if <<< "let q := true; if (let d := false; false) then q else q"
  $ cat test-weird-let-if | arithcond compile --target=stack
  true;
  begin-let;
  false;
  begin-let;
  false;
  end-let;
  code [ access 0; ];
  code [ access 0; ];
  if;
  end-let;
  $ cat test-weird-let-if | arithcond compile --target=anf
  let join j0 p1 := p1;
  if false then
    jump j0 true
  else
    jump j0 true
  $ cat test-weird-let-if | arithcond exec --target=tree
  true : Bool
  $ cat test-weird-let-if | arithcond exec --target=stack
  true;
  $ cat test-weird-let-if | arithcond exec --target=anf
  true : Bool

Weird arithmetic bindings
  $ cat >test-weird-let-arith <<< "let j := 0; (let b := 1; b) - j"
  $ cat test-weird-let-arith | arithcond compile --target=stack
  int 0;
  begin-let;
  int 1;
  begin-let;
  access 0;
  end-let;
  access 0;
  sub;
  end-let;
  $ cat test-weird-let-arith | arithcond compile --target=anf
  sub 1 0
  $ cat test-weird-let-arith | arithcond exec --target=tree
  1 : Int
  $ cat test-weird-let-arith | arithcond exec --target=stack
  int 1;
  $ cat test-weird-let-arith | arithcond exec --target=anf
  1 : Int


Something broken
  $ arithcond compile --target=stack <<< "1 + 2 + (3 +"
  error: syntax error
    ┌─ <stdin>:2:0
    │
  2 │ 
    │ ^
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

Unbound variable
  $ arithcond exec <<< "x"
  error: unbound name `x`
    ┌─ <stdin>:1:0
    │
  1 │ x
    │ ^
  [1]

Mismatched let definition
  $ arithcond exec <<< "let x := 3; true = x"
  error: mismatched types: expected Bool, found Int
  [1]
