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

Unused parameter
  $ stlc-unification elab <<< "let f (x : Int) (_ : Int) : Int := x; f 3"
  let f : Int -> Int -> Int := fun (x : Int) => fun (_ : Int) => x;
  f 3 : Int -> Int

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
  error: unexpected character
    ┌─ <stdin>:1:2
    │
  1 │ 1 % 2
    │   ^
  [1]

Unclosed block comment
  $ stlc-unification elab <<< "/- hellooo"
  error: unclosed block comment
    ┌─ <stdin>:2:0
    │
  2 │ 
    │ ^
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-unification elab <<< "1 + (3 "
  error: syntax error
    ┌─ <stdin>:2:0
    │
  2 │ 
    │ ^
  [1]



Elaboration Errors
------------------

Unbound variable
  $ stlc-unification elab <<< "let x := 1; y"
  error: unbound name `y`
    ┌─ <stdin>:1:12
    │
  1 │ let x := 1; y
    │             ^
  [1]

Mismatched definition type
  $ stlc-unification elab <<< "let x : Bool := 1; x"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:16
    │
  1 │ let x : Bool := 1; x
    │                 ^
  [1]

Mismatched argument
  $ stlc-unification elab <<< "let f x := x + 1; f f"
  error: mismatched types:
    expected: Int
    found: Int -> Int
    ┌─ <stdin>:1:20
    │
  1 │ let f x := x + 1; f f
    │                     ^
  [1]

Mismatched argument
  $ stlc-unification elab <<< "let f (x : Bool) := x; f 1"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:25
    │
  1 │ let f (x : Bool) := x; f 1
    │                          ^
  [1]

Unexpected function application
  $ stlc-unification elab <<< "true 3"
  error: mismatched types:
    expected: Bool
    found: ?0 -> ?1
    ┌─ <stdin>:1:0
    │
  1 │ true 3
    │ ^^^^
  [1]

Infinite type
  $ stlc-unification elab <<< "fun f => f f"
  error: infinite type
    ┌─ <stdin>:1:11
    │
  1 │ fun f => f f
    │            ^
  [1]

Unexpected parameter
  $ stlc-unification elab <<< "(fun x y => x) : Int -> Int"
  error: unexpected parameter
    ┌─ <stdin>:1:7
    │
  1 │ (fun x y => x) : Int -> Int
    │        ^
  [1]

Ambiguous parameter type
  $ stlc-unification elab <<< "fun x => x"
  error: ambiguous function parameter type
    ┌─ <stdin>:1:4
    │
  1 │ fun x => x
    │     ^
  [1]

Ambiguous return type
  $ stlc-unification elab <<< "fun f x => f x"
  error: ambiguous function parameter type
    ┌─ <stdin>:1:6
    │
  1 │ fun f x => f x
    │       ^
  error: ambiguous function return type
    ┌─ <stdin>:1:11
    │
  1 │ fun f x => f x
    │            ^
  [1]

Ambiguous placeholder
  $ stlc-unification elab <<< "fun (x : _) => x"
  error: unsolved placeholder
    ┌─ <stdin>:1:9
    │
  1 │ fun (x : _) => x
    │          ^
  [1]

Mismatched if expression branches
  $ stlc-unification elab <<< "fun x => if x then true else 3"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:29
    │
  1 │ fun x => if x then true else 3
    │                              ^
  [1]

Mismatched equality
  $ stlc-unification elab <<< "1 = false"
  error: mismatched types:
    expected: Int
    found: Bool
    ┌─ <stdin>:1:0
    │
  1 │ 1 = false
    │ ^^^^^^^^^
  [1]

Unsupported equality
  $ stlc-unification elab <<< "let f (x : Bool) := x; f = f"
  error: unsupported type: Bool -> Bool
    ┌─ <stdin>:1:23
    │
  1 │ let f (x : Bool) := x; f = f
    │                        ^^^^^
  [1]
