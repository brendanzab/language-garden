  $ alias executable=stlc-row-unification

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
  $ executable elab <<< "fun x => x + 2"
  fun (x : Int) => #int-add x 2 : Int -> Int

Function application
  $ executable elab <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ executable elab <<< "let f x := x; f 3"
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

Unused parameter
  $ stlc-unification elab <<< "let f (x : Int) (_ : Int) : Int := x; f 3"
  let f : Int -> Int -> Int := fun (x : Int) => fun (_ : Int) => x;
  f 3 : Int -> Int

Placeholder types
  $ executable elab <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Placeholder return type
  $ stlc-unification elab <<< "let f : Int -> _ := fun x y => x; f 3 true"
  let f : Int -> Bool -> Int := fun (x : Int) => fun (y : Bool) => x;
  f 3 true : Int

If expressions
  $ executable elab <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if #int-eq x 0 then y else 3 :
    Int -> Int -> Int

Record literal
  $ executable elab <<< "{ x := 1; y := 2; foo := { bar := true } }"
  { foo := { bar := true }; x := 1; y := 2 } :
    { foo : { bar : Bool }; x : Int; y : Int }

Record projection
  $ executable elab <<< "fun (r : { foo : { bar : Bool }; x : Int; y : Int }) => { fst := r.foo.bar; snd := r.x }"
  fun (r : { foo : { bar : Bool }; x : Int; y : Int }) =>
    { fst := r.foo.bar; snd := r.x }
  : { foo : { bar : Bool }; x : Int; y : Int } -> { fst : Bool; snd : Int }

Variant literal
  $ executable elab <<< "[some := 1]"
  [some := 1] : [some : Int] : [some : Int]

Match expression
  $ executable elab <<EOF
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
  $ executable elab <<< "(fun x => match x with end) : _ -> Int "
  fun (x : [|]) => match x with end : [|] -> Int

Variant constraint and variant type
  $ executable elab <<EOF
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
  $ executable elab <<< "let f x := x + 1; f f"
  error: mismatched types:
    expected: Int
    found: Int -> Int
    ┌─ <stdin>:1:20
    │
  1 │ let f x := x + 1; f f
    │                     ^
  
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

Unexpected function application
  $ executable elab <<< "true 3"
  error: mismatched types:
    expected: function
    found: Bool
    ┌─ <stdin>:1:0
    │
  1 │ true 3
    │ ^^^^
  
  [1]

Mismatched variant constraint and variant type
  $ executable elab <<EOF
  > let choose b y n :=
  >   if b then [yes := y] else [no := n];
  > 
  > let result : [yes : Int | no : Int] :=
  >   choose true 3 false;
  > 
  > result
  > EOF
  error: mismatched types:
    expected: [no : Int | yes : Int]
    found: [?0.. no : Bool | yes : Int]
    ┌─ <stdin>:5:2
    │
  5 │   choose true 3 false;
    │   ^^^^^^^^^^^^^^^^^^^
  
  [1]

Mismatched variant constraint and smaller variant type
  $ executable elab <<EOF
  > let choose b y n :=
  >   if b then [yes := y] else [no := n];
  > 
  > let result : [yes : Int] :=
  >   choose true 3 false;
  > 
  > result
  > EOF
  error: mismatched types:
    expected: [yes : Int]
    found: [?0.. no : Bool | yes : Int]
    ┌─ <stdin>:5:2
    │
  5 │   choose true 3 false;
    │   ^^^^^^^^^^^^^^^^^^^
  
  [1]

Mismatched variant constraint and non-variant type
  $ executable elab <<EOF
  > let choose b y n :=
  >   if b then [yes := y] else [no := n];
  > 
  > let result : Bool :=
  >   choose true 3 false;
  > 
  > result
  > EOF
  error: mismatched types:
    expected: Bool
    found: [?0.. no : Bool | yes : Int]
    ┌─ <stdin>:5:2
    │
  5 │   choose true 3 false;
    │   ^^^^^^^^^^^^^^^^^^^
  
  [1]

Infinite type
  $ executable elab <<< "fun f => f f"
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
  $ executable elab <<< "fun x => x"
  error: ambiguous function parameter type
    ┌─ <stdin>:1:4
    │
  1 │ fun x => x
    │     ^
  
  [1]

Ambiguous return type
  $ executable elab <<< "fun f x => f x"
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
  $ executable elab <<< "fun (x : _) => x"
  error: ambiguous placeholder
    ┌─ <stdin>:1:9
    │
  1 │ fun (x : _) => x
    │          ^
  
  [1]

Mismatched if expression branches
  $ executable elab <<< "fun x => if x then true else 3"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:29
    │
  1 │ fun x => if x then true else 3
    │                              ^
  
  [1]

Unknown field
  $ executable elab <<< "{ x := 42 }.y"
  error: unknown field `y`:
    found: { x : Int }
    ┌─ <stdin>:1:0
    │
  1 │ { x := 42 }.y
    │ ^^^^^^^^^^^
  
  [1]

Unexpected projection
  $ executable elab <<< "true.y"
  error: unknown field `y`:
    found: Bool
    ┌─ <stdin>:1:0
    │
  1 │ true.y
    │ ^^^^
  
  [1]

Duplicate label
  $ executable elab <<< "{ x := 42; x := 2 }"
  error: duplicate label `x`
    ┌─ <stdin>:1:11
    │
  1 │ { x := 42; x := 2 }
    │            ^
  
  [1]

Duplicate labels
  $ executable elab <<< "[some := 1] : [some : Int | some : Int]"
  error: duplicate label `some`
    ┌─ <stdin>:1:28
    │
  1 │ [some := 1] : [some : Int | some : Int]
    │                             ^^^^
  
  [1]

Unexpected variant
  $ executable elab <<< "[some := 1] : [thing : Int]"
  error: unexpected variant `some` in type `[thing : Int]`
    ┌─ <stdin>:1:1
    │
  1 │ [some := 1] : [thing : Int]
    │  ^^^^
  
  [1]

Redundant variant pattern
  $ executable elab <<EOF
  > fun (x : [some : Int]) =>
  >   match x with
  >   | [some := x] => x + 1
  >   | [some := x] => x
  >   end
  > EOF
  error: redundant variant pattern `some`
    ┌─ <stdin>:4:5
    │
  4 │   | [some := x] => x
    │      ^^^^
  
  [1]

Unexpected variant pattern
  $ executable elab <<EOF
  > fun (x : [some : Int]) =>
  >   match x with
  >   | [a := x] => x + 1
  >   end
  > EOF
  error: unexpected variant pattern `a`
    ┌─ <stdin>:3:5
    │
  3 │   | [a := x] => x + 1
    │      ^
  
  [1]

Missing variant patterns
  $ executable elab <<EOF
  > fun (x : [a : Int | b : Bool]) =>
  >   match x with end
  > EOF
  error: non-exhaustive match, missing `a`, `b`
    ┌─ <stdin>:2:8
    │
  2 │   match x with end
    │         ^
  
  [1]

Unexpected pattern match
  $ executable elab <<< "match true with [a := x] => x end"
  error: mismatched types:
    expected: variant
    found: Bool
    ┌─ <stdin>:1:6
    │
  1 │ match true with [a := x] => x end
    │       ^^^^
  
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
