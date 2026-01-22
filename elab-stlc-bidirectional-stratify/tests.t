  $ alias executable=stlc-bidirectional-stratify

Primitive operations
  $ executable elab <<< "#bool-eq"
  > executable elab <<< "#int-eq"
  > executable elab <<< "#int-add"
  > executable elab <<< "#int-sub"
  > executable elab <<< "#int-mul"
  > executable elab <<< "#int-neg"
  #bool-eq : Bool -> Bool -> Bool
  #int-eq : Int -> Int -> Bool
  #int-add : Int -> Int -> Int
  #int-sub : Int -> Int -> Int
  #int-mul : Int -> Int -> Int
  #int-neg : Int -> Int

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

Type expressions
  $ executable elab <<< "Int"
  Int : Type

  $ executable elab <<< "Int -> Int -> Bool"
  Int -> Int -> Bool : Type

  $ executable elab <<< "Int : Type"
  Int : Type

  $ executable elab <<< "Type"
  Type : Type 1

  $ executable elab <<EOF
  > let Univ := Type;
  > let x : Int := 1;
  > let Number : Univ := Int; 
  > x + 2 : Number
  > EOF
  let x : Int := 1;
  #int-add x 2 : Int

  $ executable elab <<EOF
  > let x := 1;
  > let Number := Int; 
  > x + 2 : Number
  > EOF
  let x : Int := 1;
  #int-add x 2 : Int

  $ executable elab <<EOF
  > let Univ := Type;
  > let x : Int := 1;
  > Univ
  > EOF
  Type : Type 1

  $ executable elab <<EOF
  > let Univ := Type;
  > let x : Int := 1;
  > let Number : Univ := Int; 
  > Number
  > EOF
  Int : Type

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
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:1:16
    │
  1 │ let x : Bool := 1; x
    │                 ^
  
  [1]

Mismatched argument
  $ executable elab <<< "let f (x : Bool) := x; f 1"
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:1:25
    │
  1 │ let f (x : Bool) := x; f 1
    │                          ^
  
  [1]

Mismatched parameter
  $ executable elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Int) => x;
  > 
  > f true
  > EOF
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:2:11
    │
  2 │   fun (x : Int) => x;
    │            ^^^
  
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
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:1:38
    │
  1 │ fun (x : Bool) => if x then true else 3
    │                                       ^
  
  [1]

Type expressions
  $ executable elab <<< "1 : Type"
  error: expected type, found expression
    ┌─ <stdin>:1:0
    │
  1 │ 1 : Type
    │ ^
  
  [1]

  $ executable elab <<< "Type : Int"
  error: expected expression, found universe
    ┌─ <stdin>:1:0
    │
  1 │ Type : Int
    │ ^^^^
  
  [1]

  $ executable elab <<< "Type : Type"
  error: expected type, found universe
    ┌─ <stdin>:1:0
    │
  1 │ Type : Type
    │ ^^^^
  
  [1]

Local bindings
  $ executable elab <<EOF
  > let x : 42 := 1;
  > x
  > EOF
  error: expected type or universe, found expression
    ┌─ <stdin>:1:8
    │
  1 │ let x : 42 := 1;
    │         ^^
  
  [1]

  $ executable elab <<EOF
  > let x : Int := Int;
  > x
  > EOF
  error: expected expression, found type
    ┌─ <stdin>:1:15
    │
  1 │ let x : Int := Int;
    │                ^^^
  
  [1]

  $ executable elab <<EOF
  > let x : Int := Type;
  > x
  > EOF
  error: expected expression, found universe
    ┌─ <stdin>:1:15
    │
  1 │ let x : Int := Type;
    │                ^^^^
  
  [1]

Parameterised bindings
  $ executable elab <<EOF
  > let id (A : Type) (x : A) : A := x;
  > id Int 1
  > EOF
  error: expected type, found universe
    ┌─ <stdin>:1:12
    │
  1 │ let id (A : Type) (x : A) : A := x;
    │             ^^^^
  
  [1]

  $ executable elab <<EOF
  > let Foo (x : Int) : Type := Int;
  > 33 : Foo 42
  > EOF
  error: expected expression, found type
    ┌─ <stdin>:1:28
    │
  1 │ let Foo (x : Int) : Type := Int;
    │                             ^^^
  
  [1]

  $ executable elab <<EOF
  > let Foo (x : Int) : Type := Type;
  > Int : Foo 42
  > EOF
  error: expected type, found universe
    ┌─ <stdin>:1:28
    │
  1 │ let Foo (x : Int) : Type := Type;
    │                             ^^^^
  
  [1]

Mismatched equality
  $ executable elab <<< "1 = false"
  error: mismatched types:
    expected: Int
       found: Bool
    ┌─ <stdin>:1:0
    │
  1 │ 1 = false
    │ ^^^^^^^^^
  
  [1]

Unsupported equality
  $ executable elab <<< "let f (x : Bool) := x; f = f"
  error: unsupported type: Bool -> Bool
    ┌─ <stdin>:1:23
    │
  1 │ let f (x : Bool) := x; f = f
    │                        ^^^^^
  
  [1]
