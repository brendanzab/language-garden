  $ alias executable=stlc-error-recovery

Boolean equality
  $ executable elab <<< "true = false"
  #bool-eq true false : Bool

Integer equality
  $ executable elab <<< "1 = 2"
  #int-eq 1 2 : Bool

Integer Addition
  $ executable elab <<< "1 + 2"
  #int-add 1 2 : Int

Add two function
  $ executable elab <<< "fun (x : Int) => x + 2"
  fun (x : Int) => #int-add x 2 : Int -> Int

Function application
  $ executable elab <<EOF
  > fun (x : Int) (f : Int -> Int) =>
  >   f x * x
  > EOF
  fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ executable elab <<EOF
  > let f : Int -> Int :=
  >   fun x => x;
  > 
  > f 3
  > EOF
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit parameter type
  $ executable elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit return type
  $ executable elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Check let body type
  $ executable elab <<EOF
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

If expressions (checking)
  $ executable elab <<EOF
  > let f (x : Int) (y : Int) : Int :=
  >   if x = 0 then y else 3;
  > 
  > f 4
  > EOF
  let f : Int -> Int -> Int :=
    fun (x : Int) => fun (y : Int) => if #int-eq x 0 then y else 3;
  f 4 : Int -> Int

If expressions (inference)
  $ executable elab <<< "fun (x : Bool) => if x then 2 else 5"
  fun (x : Bool) => if x then 2 else 5 : Bool -> Int


Lexer Errors
------------

Unexpected character
  $ executable elab <<< "1 % 2"
  error: unexpected character
    ┌─ <stdin>:1:2
    │
  1 │ 1 % 2
    │   ^
  
  [1]

Unclosed block comment
  $ executable elab <<< "/- hellooo"
  error: unclosed block comment
    ┌─ <stdin>:2:0
    │
  2 │ 
    │ ^
  
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ executable elab <<< "1 + (3 "
  error: syntax error
    ┌─ <stdin>:2:0
    │
  2 │ 
    │ ^
  
  [1]



Elaboration Errors
------------------

Unbound variable
  $ executable elab <<< "let x := 1; y"
  error: unbound name `y`
    ┌─ <stdin>:1:12
    │
  1 │ let x := 1; y
    │             ^
  
  [1]

Mismatched definition type
  $ executable elab <<< "let x : Bool := 1; x"
  error: mismatched types
    ┌─ <stdin>:1:16
    │
  1 │ let x : Bool := 1; x
    │                 ^
    = expected: Bool
         found: Int
  
  [1]

Mismatched argument
  $ executable elab <<< "let f (x : Bool) := x; f 1"
  error: mismatched types
    ┌─ <stdin>:1:25
    │
  1 │ let f (x : Bool) := x; f 1
    │                          ^
    = expected: Bool
         found: Int
  
  [1]

Mismatched parameter
  $ executable elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Int) => x;
  > 
  > f true
  > EOF
  error: mismatched types
    ┌─ <stdin>:2:11
    │
  2 │   fun (x : Int) => x;
    │            ^^^
    = expected: Bool
         found: Int
  
  error: mismatched types
    ┌─ <stdin>:2:19
    │
  2 │   fun (x : Int) => x;
    │                    ^
    = expected: Bool
         found: Int
  
  [1]

Too many parameters
  $ executable elab <<EOF
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
  $ executable elab <<< "fun x => x"
  error: ambiguous parameter type
    ┌─ <stdin>:1:4
    │
  1 │ fun x => x
    │     ^
  
  [1]

Mismatched if expression branches
  $ executable elab <<< "fun (x : Bool) => if x then true else 3"
  error: mismatched branches of if expression
    ┌─ <stdin>:1:38
    │
  1 │ fun (x : Bool) => if x then true else 3
    │                                       ^
    = expected: Bool
         found: Int
  
  [1]

Mismatched equality
  $ executable elab <<< "1 = false"
  error: mismatched operands
    ┌─ <stdin>:1:4
    │
  1 │ 1 = false
    │     ^^^^^
    = expected: Int
         found: Bool
  
  [1]

Unsupported equality
  $ executable elab <<< "let f (x : Bool) := x; f = f"
  error: cannot compare operands of type `Bool -> Bool`
    ┌─ <stdin>:1:23
    │
  1 │ let f (x : Bool) := x; f = f
    │                        ^^^^^
    = expected `Bool` or `Int`
  
  [1]
