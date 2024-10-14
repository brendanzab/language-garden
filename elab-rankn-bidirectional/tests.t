Boolean equality
  $ rankn-bidirectional elab <<< "true = false"
  #bool-eq -true false : Bool

Integer equality
  $ rankn-bidirectional elab <<< "1 = 2"
  #int-eq -1 2 : Bool

Integer Addition
  $ rankn-bidirectional elab <<< "1 + 2"
  #int-add -1 2 : Int

Add two function
  $ rankn-bidirectional elab <<< "fun (x : Int) => x + 2"
  fun (x : Int) => #int-add -x 2 : Int -> Int

Function application
  $ rankn-bidirectional elab <<EOF
  > fun (x : Int) (f : Int -> Int) =>
  >   f x * x
  > EOF
  fun (x : Int) => fun (f : Int -> Int) => #int-mul -(f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ rankn-bidirectional elab <<EOF
  > let f : Int -> Int :=
  >   fun x => x;
  > 
  > f 3
  > EOF
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit parameter type
  $ rankn-bidirectional elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit return type
  $ rankn-bidirectional elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Check let body type
  $ rankn-bidirectional elab <<EOF
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
  $ rankn-bidirectional elab <<EOF
  > let f (x : Int) (y : Int) : Int :=
  >   if x = 0 then y else 3;
  > 
  > f 4
  > EOF
  let f : Int -> Int -> Int :=
    fun (x : Int) => fun (y : Int) => if #int-eq -x 0 then y else 3;
  f 4 : Int -> Int

Polymorphic functions
  $ rankn-bidirectional elab <<< "(fun [a] [b] x y => x) : [a] [b] -> a -> b -> a"
  fun [a] => fun [b] => fun (x : a) => fun (y : b) => x :
    [a] -> [b] -> a -> b -> a

Polymorphic identity and constant functions
  $ rankn-bidirectional elab <<EOF
  > let id [a] (x : a) := x;
  > let always [a] (x : a) [b] (y : b) := x;
  > 
  > always [Int -> Int] (id [Int])
  > EOF
  let id : [a] -> a -> a := fun [a] => fun (x : a) => x;
  let always : [a] -> a -> [b] -> b -> a :=
    fun [a] => fun (x : a) => fun [b] => fun (y : b) => x;
  always [Int -> Int] (id [Int]) : [b] -> b -> Int -> Int


Lexer Errors
------------

Unexpected character
  $ rankn-bidirectional elab <<< "1 % 2"
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ rankn-bidirectional elab <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ rankn-bidirectional elab <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]



Elaboration Errors
------------------

Unbound variable
  $ rankn-bidirectional elab <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ rankn-bidirectional elab <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ rankn-bidirectional elab <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched parameter
  $ rankn-bidirectional elab <<EOF
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
  $ rankn-bidirectional elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Bool) (y : Int) => x;
  > 
  > f true
  > EOF
  <input>:2:18: unexpected parameter
  [1]

Too many type parameters
  $ rankn-bidirectional elab <<EOF
  > let f : Bool -> Bool :=
  >   fun [a] (x : Bool) => x;
  > 
  > f true
  > EOF
  <input>:2:7: unexpected type parameter
  [1]

Ambiguous parameter type
  $ rankn-bidirectional elab <<< "fun x => x"
  <input>:1:4: ambiguous parameter type
  [1]

Ambiguous if expression
  $ rankn-bidirectional elab <<< "fun (x : Bool) => if x then true else 3"
  <input>:1:18: ambiguous if expression
  [1]

Mismatched equality
  $ rankn-bidirectional elab <<< "1 = false"
  <input>:1:0: mismatched types:
    expected: Int
    found: Bool
  [1]

Unsupported equality
  $ rankn-bidirectional elab <<< "let f (x : Bool) := x; f = f"
  <input>:1:23: unsupported type: Bool -> Bool
  [1]
