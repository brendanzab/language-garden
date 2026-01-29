  $ alias executable=stlc-row-unification

--------------------------------------------------------------------------------

Variant literal
  $ executable elab <<< "[some := 1]"
  [some := 1] : [some : Int] : [some : Int]

Match expression
  $ executable elab <<EOF
  > fun x =>
  >   match x with {
  >     | [a := x] => x + 1
  >     | [b := x] => x
  >   }
  > EOF
  fun (x : [a : Int | b : Int]) =>
    match x with { [a := x] => #int-add x 1 | [b := x] => x }
  : [a : Int | b : Int] -> Int

Absurd match
  $ executable elab <<< "(fun x => match x with {}) : _ -> Int "
  fun (x : [|]) => match x with {} : [|] -> Int

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

--------------------------------------------------------------------------------

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
  error: mismatched types
    ┌─ <stdin>:5:2
    │
  5 │   choose true 3 false;
    │   ^^^^^^^^^^^^^^^^^^^
    = expected: [no : Int | yes : Int]
         found: [?1.. no : Bool | yes : Int]
  
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
  error: mismatched types
    ┌─ <stdin>:5:2
    │
  5 │   choose true 3 false;
    │   ^^^^^^^^^^^^^^^^^^^
    = expected: [yes : Int]
         found: [?1.. no : Bool | yes : Int]
  
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
  error: mismatched types
    ┌─ <stdin>:5:2
    │
  5 │   choose true 3 false;
    │   ^^^^^^^^^^^^^^^^^^^
    = expected: Bool
         found: [?1.. no : Bool | yes : Int]
  
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
  >   match x with {
  >     | [some := x] => x + 1
  >     | [some := x] => x
  >   }
  > EOF
  fun (x : [some : Int]) => match x with { [some := x] => #int-add x 1 } :
    [some : Int] -> Int
  warning: redundant case pattern
    ┌─ <stdin>:4:7
    │
  4 │     | [some := x] => x
    │        ^^^^
  

Unexpected variant pattern
  $ executable elab <<EOF
  > fun (x : [some : Int]) =>
  >   match x with {
  >     | [a := x] => x + 1
  >   }
  > EOF
  error: unexpected pattern
    ┌─ <stdin>:3:7
    │
  3 │     | [a := x] => x + 1
    │        ^
  
  [1]

Missing variant patterns
  $ executable elab <<EOF
  > fun (x : [a : Int | b : Bool]) =>
  >   match x with {}
  > EOF
  error: non-exhaustive match
    ┌─ <stdin>:2:8
    │
  2 │   match x with {}
    │         ^
    = missing case pattern `a`
    = missing case pattern `b`
  
  [1]

Unexpected pattern match
  $ executable elab <<< "match true with { [a := x] => x }"
  error: mismatched types
    ┌─ <stdin>:1:6
    │
  1 │ match true with { [a := x] => x }
    │       ^^^^
    = expected: [a : ?0]
         found: Bool
  
  [1]
