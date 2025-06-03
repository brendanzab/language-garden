Boolean equality
  $ stlc-bidirectional elab <<< "true = false"
  #bool-eq true false : Bool

Integer equality
  $ stlc-bidirectional elab <<< "1 = 2"
  #int-eq 1 2 : Bool

Integer Addition
  $ stlc-bidirectional elab <<< "1 + 2"
  #int-add 1 2 : Int

Add two function
  $ stlc-bidirectional elab <<< "fun (x : Int) => x + 2"
  fun (x : Int) => #int-add x 2 : Int -> Int

Function application
  $ stlc-bidirectional elab <<EOF
  > fun (x : Int) (f : Int -> Int) =>
  >   f x * x
  > EOF
  fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ stlc-bidirectional elab <<EOF
  > let f : Int -> Int :=
  >   fun x => x;
  > 
  > f 3
  > EOF
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit parameter type
  $ stlc-bidirectional elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit return type
  $ stlc-bidirectional elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Check let body type
  $ stlc-bidirectional elab <<EOF
  > let f (x : Int) : Int -> Int :=
  >   let id (x : Int) : Int := x;
  >   let incr (x : Int) : Int := x + 1;
  > 
  >   if x = 0 then id else incr;
  > 
  > f 4 3
  > EOF
  let f : Int -> Int -> Int :=
    fun (x : Int) =>
      let id : Int -> Int := fun (x : Int) => x;
      let incr : Int -> Int := fun (x : Int) => #int-add x 1;
      if #int-eq x 0 then id else incr;
  f 4 3 : Int

If expressions
  $ stlc-bidirectional elab <<EOF
  > let f (x : Int) (y : Int) : Int :=
  >   if x = 0 then y else 3;
  > 
  > f 4
  > EOF
  let f : Int -> Int -> Int :=
    fun (x : Int) => fun (y : Int) => if #int-eq x 0 then y else 3;
  f 4 : Int -> Int


Lexer Errors
------------

Unexpected character
  $ stlc-bidirectional elab <<< "1 % 2"
  error: unexpected character
    ┌─ <stdin>:1:2
    │
  1 │ 1 % 2
    │   ^
  [1]

Unclosed block comment
  $ stlc-bidirectional elab <<< "/- hellooo"
  error: unclosed block comment
    ┌─ <stdin>:1:10
    │
  1 │ /- hellooo
    │           ^
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-bidirectional elab <<< "1 + (3 "
  error: syntax error
    ┌─ <stdin>:1:7
    │
  1 │ 1 + (3 
    │        ^
  [1]



Elaboration Errors
------------------

Unbound variable
  $ stlc-bidirectional elab <<< "let x := 1; y"
  error: unbound name `y`
    ┌─ <stdin>:1:12
    │
  1 │ let x := 1; y
    │             ^
  [1]

Mismatched definition type
  $ stlc-bidirectional elab <<< "let x : Bool := 1; x"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:16
    │
  1 │ let x : Bool := 1; x
    │                 ^
  [1]

Mismatched argument
  $ stlc-bidirectional elab <<< "let f (x : Bool) := x; f 1"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:25
    │
  1 │ let f (x : Bool) := x; f 1
    │                          ^
  [1]

Mismatched parameter
  $ stlc-bidirectional elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Int) => x;
  > 
  > f true
  > EOF
  error: mismatched types:
    expected: Int
    found: Bool
    ┌─ <stdin>:2:11
    │
  2 │   fun (x : Int) => x;
    │            ^^^
  [1]

Too many parameters
  $ stlc-bidirectional elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Bool) (y : Int) => x;
  > 
  > f true
  > EOF
  error: unexpected parameter
    ┌─ <stdin>:2:18
    │
  2 │   fun (x : Bool) (y : Int) => x;
    │                   ^
  [1]

Ambiguous parameter type
  $ stlc-bidirectional elab <<< "fun x => x"
  error: ambiguous parameter type
    ┌─ <stdin>:1:4
    │
  1 │ fun x => x
    │     ^
  [1]

Ambiguous if expression
  $ stlc-bidirectional elab <<< "fun (x : Bool) => if x then true else 3"
  error: ambiguous if expression
    ┌─ <stdin>:1:18
    │
  1 │ fun (x : Bool) => if x then true else 3
    │                   ^^^^^^^^^^^^^^^^^^^^^
  [1]

Mismatched equality
  $ stlc-bidirectional elab <<< "1 = false"
  error: mismatched types:
    expected: Int
    found: Bool
    ┌─ <stdin>:1:0
    │
  1 │ 1 = false
    │ ^^^^^^^^^
  [1]

Unsupported equality
  $ stlc-bidirectional elab <<< "let f (x : Bool) := x; f = f"
  error: unsupported type: Bool -> Bool
    ┌─ <stdin>:1:23
    │
  1 │ let f (x : Bool) := x; f = f
    │                        ^^^^^
  [1]
