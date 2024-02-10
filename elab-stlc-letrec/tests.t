Addition
  $ stlc-letrec <<< "1 + 2"
  1 + 2 : Int

Add two function
  $ stlc-letrec <<< "fun x => x + 2"
  fun (x : Int) => x + 2 : Int -> Int

Function application
  $ stlc-letrec <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => f x * x : Int -> (Int -> Int) -> Int

Function application
  $ stlc-letrec <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

Explicit parameter type
  $ stlc-letrec <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

Explicit return type
  $ stlc-letrec <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

Placeholder types
  $ stlc-letrec <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

If expressions
  $ stlc-letrec <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if x = 0 then y else 3 : Int -> Int -> Int

letrec expressions
  $ stlc-letrec <<< "let rec fact n := if n = 0 then 1 else n * fact (n - 1); fact 5"
  let rec fact : Int -> Int :=
        fun (n : Int) => if n = 0 then 1 else n * fact (n - 1);fact 5
  : Int

# FIXME: hangs
#  $ stlc-letrec <<< "let rec fact n := if n = 0 then 1 else n * fact (n - 1); fact"
#  let rec fact : Int -> Int :=
#        fun (n : Int) => if n = 0 then 1 else n * fact (n - 1);fact
#  : Int -> Int

Lexer Errors
------------

Unexpected character
  $ stlc-letrec <<< "1 % 2"
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ stlc-letrec <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-letrec <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]



Elaboration Errors
------------------

Unbound variable
  $ stlc-letrec <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ stlc-letrec <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ stlc-letrec <<< "let f x := x + 1; f f"
  <input>:1:20: mismatched types:
    expected: Int
    found: Int -> Int
  [1]

Mismatched argument
  $ stlc-letrec <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Infinite type
  $ stlc-letrec <<< "fun f => f f"
  <input>:1:11: infinite type
  [1]

Ambiguous parameter type
  $ stlc-letrec <<< "fun x => x"
  <input>:1:4: ambiguous function parameter type
  [1]

Ambiguous return type
  $ stlc-letrec <<< "fun f x => f x"
  <input>:1:6: ambiguous function parameter type
  <input>:1:11: ambiguous function return type
  [1]

Ambiguous placeholder
  $ stlc-letrec <<< "fun (x : _) => x"
  <input>:1:9: unsolved placeholder
  [1]

Mismatched if expression branches
  $ stlc-letrec <<< "fun x => if x then true else 3"
  <input>:1:29: mismatched types:
    expected: Bool
    found: Int
  [1]
