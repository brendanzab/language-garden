Boolean equality
  $ stlc-row-unification elab <<< "true = false"
  #bool-eq true false : Bool

Integer equality
  $ stlc-row-unification elab <<< "1 = 2"
  #int-eq 1 2 : Bool

Integer Addition
  $ stlc-row-unification elab <<< "1 + 2"
  #int-add 1 2 : Int

Add two function
  $ stlc-row-unification elab <<< "fun x => x + 2"
  fun (x : Int) => #int-add x 2 : Int -> Int

Function application
  $ stlc-row-unification elab <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ stlc-row-unification elab <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit parameter type
  $ stlc-row-unification elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit return type
  $ stlc-row-unification elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Unused parameter
  $ stlc-unification elab <<< "let f (x : Int) (_ : Int) : Int := x; f 3"
  let f : Int -> Int -> Int := fun (x : Int) => fun (_ : Int) => x;
  f 3 : Int -> Int

Placeholder types
  $ stlc-row-unification elab <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Placeholder return type
  $ stlc-unification elab <<< "let f : Int -> _ := fun x y => x; f 3 true"
  let f : Int -> Bool -> Int := fun (x : Int) => fun (y : Bool) => x;
  f 3 true : Int

If expressions
  $ stlc-row-unification elab <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if #int-eq x 0 then y else 3 :
    Int -> Int -> Int

Record literal
  $ stlc-row-unification elab <<< "{ x := 1; y := 2; foo := { bar := true } }"
  { foo := { bar := true }; x := 1; y := 2 } :
    { foo : { bar : Bool }; x : Int; y : Int }

Record projection
  $ stlc-row-unification elab <<< "fun (r : { foo : { bar : Bool }; x : Int; y : Int }) => { fst := r.foo.bar; snd := r.x }"
  fun (r : { foo : { bar : Bool }; x : Int; y : Int }) =>
    { fst := r.foo.bar; snd := r.x }
  : { foo : { bar : Bool }; x : Int; y : Int } -> { fst : Bool; snd : Int }

Variant literal
  $ stlc-row-unification elab <<< "[some := 1]"
  [some := 1] : [some : Int] : [some : Int]

Match expression
  $ stlc-row-unification elab <<EOF
  > fun x =>
  >   match x with
  >   | [a := x] => x + 1
  >   | [b := x] => x
  >   end
  > EOF
  fun (x : [a : Int | b : Int]) =>
    match x with | [a := x] => #int-add x 1 | [b := x] => x end
  : [a : Int | b : Int] -> Int

Absurd match
  $ stlc-row-unification elab <<< "(fun x => match x with end) : _ -> Int "
  fun (x : [|]) => match x with end : [|] -> Int

Variant constraint and variant type
  $ stlc-row-unification elab <<EOF
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
  $ stlc-row-unification elab <<< "1 % 2"
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ stlc-row-unification elab <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-row-unification elab <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]


Elaboration Errors
------------------

Unbound variable
  $ stlc-row-unification elab <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ stlc-row-unification elab <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ stlc-row-unification elab <<< "let f x := x + 1; f f"
  <input>:1:20: mismatched types:
    expected: Int
    found: Int -> Int
  [1]

Mismatched argument
  $ stlc-row-unification elab <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched variant constraint and variant type
  $ stlc-row-unification elab <<EOF
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
    found: [?0.. no : Bool | yes : Int]
  [1]

Mismatched variant constraint and smaller variant type
  $ stlc-row-unification elab <<EOF
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
    found: [?0.. no : Bool | yes : Int]
  [1]

Mismatched variant constraint and non-variant type
  $ stlc-row-unification elab <<EOF
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
    found: [?0.. no : Bool | yes : Int]
  [1]

Infinite type
  $ stlc-row-unification elab <<< "fun f => f f"
  <input>:1:11: infinite type
  [1]

Unexpected parameter
  $ stlc-unification elab <<< "(fun x y => x) : Int -> Int"
  <input>:1:7: unexpected parameter
  [1]

Ambiguous parameter type
  $ stlc-row-unification elab <<< "fun x => x"
  <input>:1:4: ambiguous function parameter type
  [1]

Ambiguous return type
  $ stlc-row-unification elab <<< "fun f x => f x"
  <input>:1:6: ambiguous function parameter type
  <input>:1:11: ambiguous function return type
  [1]

Ambiguous placeholder
  $ stlc-row-unification elab <<< "fun (x : _) => x"
  <input>:1:9: unsolved placeholder
  [1]

Mismatched if expression branches
  $ stlc-row-unification elab <<< "fun x => if x then true else 3"
  <input>:1:29: mismatched types:
    expected: Bool
    found: Int
  [1]

Unknown field
  $ stlc-row-unification elab <<< "{ x := 42 }.y"
  <input>:1:0: unknown field `y`
  [1]

Duplicate label
  $ stlc-row-unification elab <<< "{ x := 42; x := 2 }"
  <input>:1:11: duplicate label `x`
  [1]

Duplicate labels
  $ stlc-row-unification elab <<< "[some := 1] : [some : Int | some : Int]"
  <input>:1:28: duplicate label `some`
  [1]

Unexpected variant
  $ stlc-row-unification elab <<< "[some := 1] : [thing : Int]"
  <input>:1:1: unexpected variant `some` in type `[thing : Int]`
  [1]

Redundant variant pattern
  $ stlc-row-unification elab <<EOF
  > fun (x : [some : Int]) =>
  >   match x with
  >   | [some := x] => x + 1
  >   | [some := x] => x
  >   end
  > EOF
  <input>:4:5: redundant variant pattern `some`
  [1]

Unexpected variant pattern
  $ stlc-row-unification elab <<EOF
  > fun (x : [some : Int]) =>
  >   match x with
  >   | [a := x] => x + 1
  >   end
  > EOF
  <input>:3:5: unexpected variant pattern `a`
  [1]

Missing variant patterns
  $ stlc-row-unification elab <<EOF
  > fun (x : [a : Int | b : Bool]) =>
  >   match x with end
  > EOF
  <input>:2:8: non-exhaustive match, missing `a`, `b`
  [1]

Mismatched equality
  $ stlc-row-unification elab <<< "1 = false"
  <input>:1:0: mismatched types:
    expected: Int
    found: Bool
  [1]

Unsupported equality
  $ stlc-row-unification elab <<< "let f (x : Bool) := x; f = f"
  <input>:1:23: unsupported type: Bool -> Bool
  [1]
