Boolean equality
  $ stlc-bidirectional-stratify elab <<< "true = false"
  #bool-eq -true false : Bool

Integer equality
  $ stlc-bidirectional-stratify elab <<< "1 = 2"
  #int-eq -1 2 : Bool

Integer Addition
  $ stlc-bidirectional-stratify elab <<< "1 + 2"
  #int-add -1 2 : Int

Add two function
  $ stlc-bidirectional-stratify elab <<< "fun (x : Int) => x + 2"
  fun (x : Int) => #int-add -x 2 : Int -> Int

Function application
  $ stlc-bidirectional-stratify elab <<EOF
  > fun (x : Int) (f : Int -> Int) =>
  >   f x * x
  > EOF
  fun (x : Int) => fun (f : Int -> Int) => #int-mul -(f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ stlc-bidirectional-stratify elab <<EOF
  > let f : Int -> Int :=
  >   fun x => x;
  > 
  > f 3
  > EOF
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit parameter type
  $ stlc-bidirectional-stratify elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit return type
  $ stlc-bidirectional-stratify elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Check let body type
  $ stlc-bidirectional-stratify elab <<EOF
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
      let incr : Int -> Int := fun (x : Int) => #int-add -x 1;
      if #int-eq -x 0 then id else incr;
  f 4 3 : Int

If expressions
  $ stlc-bidirectional-stratify elab <<EOF
  > let f (x : Int) (y : Int) : Int :=
  >   if x = 0 then y else 3;
  > 
  > f 4
  > EOF
  let f : Int -> Int -> Int :=
    fun (x : Int) => fun (y : Int) => if #int-eq -x 0 then y else 3;
  f 4 : Int -> Int

Type expressions
  $ stlc-bidirectional-stratify elab <<< "Int"
  Int : Type

  $ stlc-bidirectional-stratify elab <<< "Int -> Int -> Bool"
  Int -> Int -> Bool : Type

  $ stlc-bidirectional-stratify elab <<< "Int : Type"
  Int : Type

  $ stlc-bidirectional-stratify elab <<< "Type"
  Type : Type 1

  $ stlc-bidirectional-stratify elab <<EOF
  > let Univ := Type;
  > let x : Int := 1;
  > let Number : Univ := Int; 
  > x + 2 : Number
  > EOF
  let x : Int := 1;
  #int-add -x 2 : Int

  $ stlc-bidirectional-stratify elab <<EOF
  > let x := 1;
  > let Number := Int; 
  > x + 2 : Number
  > EOF
  let x : Int := 1;
  #int-add -x 2 : Int

  $ stlc-bidirectional-stratify elab <<EOF
  > let Univ := Type;
  > let x : Int := 1;
  > Univ
  > EOF
  Type : Type 1

  $ stlc-bidirectional-stratify elab <<EOF
  > let Univ := Type;
  > let x : Int := 1;
  > let Number : Univ := Int; 
  > Number
  > EOF
  Int : Type

Lexer Errors
------------

Unexpected character
  $ stlc-bidirectional-stratify elab <<< "1 % 2"
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ stlc-bidirectional-stratify elab <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-bidirectional-stratify elab <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]



Elaboration Errors
------------------

Unbound variable
  $ stlc-bidirectional-stratify elab <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ stlc-bidirectional-stratify elab <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ stlc-bidirectional-stratify elab <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched parameter
  $ stlc-bidirectional-stratify elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Int) => x;
  > 
  > f true
  > EOF
  <input>:2:11: mismatched types:
    expected: Int
    found: Bool
  [1]

Too many parameters
  $ stlc-bidirectional-stratify elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Bool) (y : Int) => x;
  > 
  > f true
  > EOF
  <input>:2:18: unexpected parameter
  [1]

Ambiguous parameter type
  $ stlc-bidirectional-stratify elab <<< "fun x => x"
  <input>:1:4: ambiguous parameter type
  [1]

Ambiguous if expression
  $ stlc-bidirectional-stratify elab <<< "fun (x : Bool) => if x then true else 3"
  <input>:1:18: ambiguous if expression
  [1]

Type expressions
  $ stlc-bidirectional-stratify elab <<< "1 : Type"
  <input>:1:0: expected type, found expression
  [1]

  $ stlc-bidirectional-stratify elab <<< "Type : Int"
  <input>:1:0: expected expression, found universe
  [1]

  $ stlc-bidirectional-stratify elab <<< "Type : Type"
  <input>:1:0: expected type, found universe
  [1]

Local bindings
  $ stlc-bidirectional-stratify elab <<EOF
  > let x : 42 := 1;
  > x
  > EOF
  <input>:1:8: expected type or universe, found expression
  [1]

  $ stlc-bidirectional-stratify elab <<EOF
  > let x : Int := Int;
  > x
  > EOF
  <input>:1:15: expected expression, found type
  [1]

  $ stlc-bidirectional-stratify elab <<EOF
  > let x : Int := Type;
  > x
  > EOF
  <input>:1:15: expected expression, found universe
  [1]

Parameterised bindings
  $ stlc-bidirectional-stratify elab <<EOF
  > let id (A : Type) (x : A) : A := x;
  > id Int 1
  > EOF
  <input>:1:12: expected type, found universe
  [1]

  $ stlc-bidirectional-stratify elab <<EOF
  > let Foo (x : Int) : Type := Int;
  > 33 : Foo 42
  > EOF
  <input>:1:28: expected expression, found type
  [1]

  $ stlc-bidirectional-stratify elab <<EOF
  > let Foo (x : Int) : Type := Type;
  > Int : Foo 42
  > EOF
  <input>:1:28: expected type, found universe
  [1]

Mismatched equality
  $ stlc-bidirectional-stratify elab <<< "1 = false"
  <input>:1:0: mismatched types:
    expected: Int
    found: Bool
  [1]

Unsupported equality
  $ stlc-bidirectional-stratify elab <<< "let f (x : Bool) := x; f = f"
  <input>:1:23: unsupported type: Bool -> Bool
  [1]
