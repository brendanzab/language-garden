Addition
  $ stlc-variant-unification elab <<< "1 + 2"
  #int-add -1 2 : Int

Add two function
  $ stlc-variant-unification elab <<< "fun x => x + 2"
  fun (x : Int) => #int-add -x 2 : Int -> Int

Function application
  $ stlc-variant-unification elab <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => #int-mul -(f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ stlc-variant-unification elab <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit parameter type
  $ stlc-variant-unification elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit return type
  $ stlc-variant-unification elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Placeholder types
  $ stlc-variant-unification elab <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Placeholder return type
  $ stlc-unification elab <<< "let f : Int -> _ := fun x y => x; f 3 true"
  let f : Int -> Bool -> Int := fun (x : Int) => fun (y : Bool) => x;
  f 3 true : Int

If expressions
  $ stlc-variant-unification elab <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if #int-eq -x 0 then y else 3 :
    Int -> Int -> Int

Variant literal
  $ stlc-variant-unification elab <<< "[some := 1]"
  [some := 1] : [some : Int] : [some : Int]

Match expression
  $ stlc-variant-unification elab <<EOF
  > fun x =>
  >   match x with
  >   | [a := x] => x + 1
  >   | [b := x] => x
  >   end
  > EOF
  fun (x : [a : Int | b : Int]) =>
    match x with | [a := x] => #int-add -x 1 | [b := x] => x end
  : [a : Int | b : Int] -> Int

Absurd match
  $ stlc-variant-unification elab <<< "(fun x => match x with end) : _ -> Int "
  fun (x : [|]) => match x with end : [|] -> Int

Variant constraint and variant type
  $ stlc-variant-unification elab <<EOF
  > let choose b y n :=
  >   if b then [yes := y] else [no := n];
  > 
  > let result : [yes : Int | no : Bool | maybe : Int] :=
  >   choose true 3 false;
  > 
  > result
  > EOF
  let choose : Bool -> Int -> Bool -> [maybe : Int | no : Bool | yes : Int] :=
    fun (b : Bool) => fun (y : Int) => fun (n : Bool) =>
      if b then
        ([yes := y] : [maybe : Int | no : Bool | yes : Int])
      else
        [no := n] : [maybe : Int | no : Bool | yes : Int];
  let result : [maybe : Int | no : Bool | yes : Int] := choose true 3 false;
  result : [maybe : Int | no : Bool | yes : Int]


Lexer Errors
------------

Unexpected character
  $ stlc-variant-unification elab <<< "1 % 2"
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ stlc-variant-unification elab <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-variant-unification elab <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]


Elaboration Errors
------------------

Unbound variable
  $ stlc-variant-unification elab <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ stlc-variant-unification elab <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ stlc-variant-unification elab <<< "let f x := x + 1; f f"
  <input>:1:20: mismatched types:
    expected: Int
    found: Int -> Int
  [1]

Mismatched argument
  $ stlc-variant-unification elab <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched variant constraint and variant type
  $ stlc-variant-unification elab <<EOF
  > let choose b y n :=
  >   if b then [yes := y] else [no := n];
  > 
  > let result : [yes : Int | no : Int] :=
  >   choose true 3 false;
  > 
  > result
  > EOF
  <input>:5:2: mismatched types:
    expected: [no : Int | yes : Int]
    found: ?{4 ~ [no : Bool | yes : Int]}
  [1]

Mismatched variant constraint and smaller variant type
  $ stlc-variant-unification elab <<EOF
  > let choose b y n :=
  >   if b then [yes := y] else [no := n];
  > 
  > let result : [yes : Int] :=
  >   choose true 3 false;
  > 
  > result
  > EOF
  <input>:5:2: mismatched types:
    expected: [yes : Int]
    found: ?{4 ~ [no : Bool | yes : Int]}
  [1]

Mismatched variant constraint and non-variant type
  $ stlc-variant-unification elab <<EOF
  > let choose b y n :=
  >   if b then [yes := y] else [no := n];
  > 
  > let result : Bool :=
  >   choose true 3 false;
  > 
  > result
  > EOF
  <input>:5:2: mismatched types:
    expected: Bool
    found: ?{4 ~ [no : Bool | yes : Int]}
  [1]

Infinite type
  $ stlc-variant-unification elab <<< "fun f => f f"
  <input>:1:11: infinite type
  [1]

Unexpected parameter
  $ stlc-unification elab <<< "(fun x y => x) : Int -> Int"
  <input>:1:7: unexpected parameter
  [1]

Ambiguous parameter type
  $ stlc-variant-unification elab <<< "fun x => x"
  <input>:1:4: ambiguous function parameter type
  [1]

Ambiguous return type
  $ stlc-variant-unification elab <<< "fun f x => f x"
  <input>:1:6: ambiguous function parameter type
  <input>:1:11: ambiguous function return type
  [1]

Ambiguous placeholder
  $ stlc-variant-unification elab <<< "fun (x : _) => x"
  <input>:1:9: unsolved placeholder
  [1]

Mismatched if expression branches
  $ stlc-variant-unification elab <<< "fun x => if x then true else 3"
  <input>:1:29: mismatched types:
    expected: Bool
    found: Int
  [1]

Duplicate labels
  $ stlc-variant-unification elab <<< "[some := 1] : [some : Int | some : Int]"
  <input>:1:28: duplicate label `some`
  [1]

Unexpected variant
  $ stlc-variant-unification elab <<< "[some := 1] : [thing : Int]"
  <input>:1:1: unexpected variant `some` in type `[thing : Int]`
  [1]

Redundant variant pattern
  $ stlc-variant-unification elab <<EOF
  > fun (x : [some : Int]) =>
  >   match x with
  >   | [some := x] => x + 1
  >   | [some := x] => x
  >   end
  > EOF
  <input>:4:5: redundant variant pattern `some`
  [1]

Unexpected variant pattern
  $ stlc-variant-unification elab <<EOF
  > fun (x : [some : Int]) =>
  >   match x with
  >   | [a := x] => x + 1
  >   end
  > EOF
  <input>:3:5: unexpected variant pattern `a`
  [1]

Missing variant patterns
  $ stlc-variant-unification elab <<EOF
  > fun (x : [a : Int | b : Bool]) =>
  >   match x with end
  > EOF
  <input>:2:8: non-exhaustive match, missing `a`, `b`
  [1]
