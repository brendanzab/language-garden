Addition
  $ stlc-bidirectional <<< "1 + 2"
  1 + 2 : Int

Add two function
  $ stlc-bidirectional <<< "fun (x : Int) => x + 2"
  fun (x : Int) => x + 2 : Int -> Int

Function application
  $ stlc-bidirectional <<EOF
  > fun (x : Int) (f : Int -> Int) =>
  >   f x * x
  > EOF
  fun (x : Int) => fun (f : Int -> Int) => f x * x : Int -> (Int -> Int) -> Int

Function application
  $ stlc-bidirectional <<EOF
  > let f : Int -> Int :=
  >   fun x => x;
  > 
  > f 3
  > EOF
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

Explicit parameter type
  $ stlc-bidirectional <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

Explicit return type
  $ stlc-bidirectional <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

If expressions
  $ stlc-bidirectional <<EOF
  > let f (x : Int) (y : Int) : Int :=
  >   if x = 0 then y else 3;
  > 
  > f 4
  > EOF
  let f : Int -> Int -> Int :=
    fun (x : Int) => fun (y : Int) => if x = 0 then y else 3;
  f 4 : Int -> Int


Lexer Errors
------------

Unexpected character
  $ stlc-bidirectional <<< "1 % 2"
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ stlc-bidirectional <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-bidirectional <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]



Elaboration Errors
------------------

Unbound variable
  $ stlc-bidirectional <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ stlc-bidirectional <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ stlc-bidirectional <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched parameter
  $ stlc-bidirectional <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Int) => x;
  > 
  > f true
  > EOF
  <input>:2:11: mismatched parameter types:
    expected: Bool
    found: Int
  [1]

Too many parameters
  $ stlc-bidirectional <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Bool) (y : Int) => x;
  > 
  > f true
  > EOF
  <input>:2:18: unexpected parameter
  [1]

Ambiguous parameter type
  $ stlc-bidirectional <<< "fun x => x"
  <input>:1:4: ambiguous parameter type
  [1]

Ambiguous if expression
  $ stlc-bidirectional <<< "fun (x : Bool) => if x then true else 3"
  <input>:1:18: ambiguous if expression
  [1]
