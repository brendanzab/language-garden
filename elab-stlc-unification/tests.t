Boolean equality
  $ stlc-unification elab <<< "true = false"
  #bool-eq true false : Bool

Integer equality
  $ stlc-unification elab <<< "1 = 2"
  #int-eq 1 2 : Bool

Integer Addition
  $ stlc-unification elab <<< "1 + 2"
  #int-add 1 2 : Int

Add two function
  $ stlc-unification elab <<< "fun x => x + 2"
  fun (x : Int) => #int-add x 2 : Int -> Int

Function application
  $ stlc-unification elab <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ stlc-unification elab <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit parameter type
  $ stlc-unification elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit return type
  $ stlc-unification elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Placeholder types
  $ stlc-unification elab <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Placeholder return type
  $ stlc-unification elab <<< "let f : Int -> _ := fun x y => x; f 3 true"
  let f : Int -> Bool -> Int := fun (x : Int) => fun (y : Bool) => x;
  f 3 true : Int

If expressions
  $ stlc-unification elab <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if #int-eq x 0 then y else 3 :
    Int -> Int -> Int


Lexer Errors
------------

Unexpected character
  $ stlc-unification elab <<< "1 % 2"
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ stlc-unification elab <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-unification elab <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]



Elaboration Errors
------------------

Unbound variable
  $ stlc-unification elab <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ stlc-unification elab <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ stlc-unification elab <<< "let f x := x + 1; f f"
  <input>:1:20: mismatched types:
    expected: Int
    found: Int -> Int
  [1]

Mismatched argument
  $ stlc-unification elab <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Infinite type
  $ stlc-unification elab <<< "fun f => f f"
  <input>:1:11: infinite type
  [1]

Unexpected parameter
  $ stlc-unification elab <<< "(fun x y => x) : Int -> Int"
  <input>:1:7: unexpected parameter
  [1]

Ambiguous parameter type
  $ stlc-unification elab <<< "fun x => x"
  <input>:1:4: ambiguous function parameter type
  [1]

Ambiguous return type
  $ stlc-unification elab <<< "fun f x => f x"
  <input>:1:6: ambiguous function parameter type
  <input>:1:11: ambiguous function return type
  [1]

Ambiguous placeholder
  $ stlc-unification elab <<< "fun (x : _) => x"
  <input>:1:9: unsolved placeholder
  [1]

Mismatched if expression branches
  $ stlc-unification elab <<< "fun x => if x then true else 3"
  <input>:1:29: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched equality
  $ stlc-unification elab <<< "1 = false"
  <input>:1:0: mismatched types:
    expected: Int
    found: Bool
  [1]

Unsupported equality
  $ stlc-unification elab <<< "let f (x : Bool) := x; f = f"
  <input>:1:23: unsupported type: Bool -> Bool
  [1]
