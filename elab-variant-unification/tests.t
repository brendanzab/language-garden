Addition
  $ variant-unification elab <<< "1 + 2"
  1 + 2 : Int

Add two function
  $ variant-unification elab <<< "fun x => x + 2"
  fun (x : Int) => x + 2 : Int -> Int

Function application
  $ variant-unification elab <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => f x * x : Int -> (Int -> Int) -> Int

Function application
  $ variant-unification elab <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

Explicit parameter type
  $ variant-unification elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

Explicit return type
  $ variant-unification elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

Placeholder types
  $ variant-unification elab <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

If expressions
  $ variant-unification elab <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if x = 0 then y else 3 : Int -> Int -> Int

Variant literal
  $ variant-unification elab <<< "[some := 1]"
  [some := 1] : [some : Int] : [some : Int]

Match expression
  $ variant-unification elab <<EOF
  > fun x =>
  >   match x with
  >   | [a := x] => x + 1
  >   | [b := x] => x
  >   end
  > EOF
  fun (x : [a : Int | b : Int]) =>
    match x with | [a := x] => x + 1 | [b := x] => x end
  : [a : Int | b : Int] -> Int

Absurd match
  $ variant-unification elab <<< "(fun x => match x with end) : _ -> Int "
  fun (x : [|]) => match x with end : [|] -> Int


Lexer Errors
------------

Unexpected character
  $ variant-unification elab <<< "1 % 2"
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ variant-unification elab <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ variant-unification elab <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]


Elaboration Errors
------------------

Unbound variable
  $ variant-unification elab <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ variant-unification elab <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ variant-unification elab <<< "let f x := x + 1; f f"
  <input>:1:20: mismatched types:
    expected: Int
    found: Int -> Int
  [1]

Mismatched argument
  $ variant-unification elab <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Infinite type
  $ variant-unification elab <<< "fun f => f f"
  <input>:1:11: infinite type
  [1]

Ambiguous parameter type
  $ variant-unification elab <<< "fun x => x"
  <input>:1:4: ambiguous function parameter type
  [1]

Ambiguous return type
  $ variant-unification elab <<< "fun f x => f x"
  <input>:1:6: ambiguous function parameter type
  <input>:1:11: ambiguous function return type
  [1]

Ambiguous placeholder
  $ variant-unification elab <<< "fun (x : _) => x"
  <input>:1:9: unsolved placeholder
  [1]

Mismatched if expression branches
  $ variant-unification elab <<< "fun x => if x then true else 3"
  <input>:1:29: mismatched types:
    expected: Bool
    found: Int
  [1]

Duplicate labels
  $ variant-unification elab <<< "[some := 1] : [some : Int | some : Int]"
  <input>:1:28: duplicate label `some`
  [1]

Unexpected variant
  $ variant-unification elab <<< "[some := 1] : [thing : Int]"
  <input>:1:1: unexpected variant `some` in type `[thing : Int]`
  [1]

Redundant variant pattern
  $ variant-unification elab <<EOF
  > fun (x : [some : Int]) => 
  >   match x with
  >   | [some := x] => x + 1
  >   | [some := x] => x
  >   end
  > EOF
  <input>:4:5: redundant variant pattern `some`
  [1]

Unexpected variant pattern
  $ variant-unification elab <<EOF
  > fun (x : [some : Int]) => 
  >   match x with
  >   | [a := x] => x + 1
  >   end
  > EOF
  <input>:3:5: unexpected variant pattern `a`
  [1]

Missing variant patterns
  $ variant-unification elab <<EOF
  > fun (x : [a : Int | b : Bool]) => 
  >   match x with end
  > EOF
  <input>:2:8: non-exhaustive match, missing `a`, `b`
  [1]
