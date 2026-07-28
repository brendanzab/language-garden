Test files
----------

Elaborate test term
  $ cat term.txt | executable elab
  fun (f : Int -> Bool) => fun (a : Int) => f a : (Int -> Bool) -> Int -> Bool

Elaborate combinator test
  $ cat comb.txt | executable elab
  let id : Int -> Int := fun (x : Int) => x;
  let const : Bool -> Int -> Bool := fun (x : Bool) => fun (y : Int) => x;
  fun (x : Int) => fun (y : Bool) => const y (id x) : Int -> Bool -> Bool

Elaborate combinator test (with parameter annotation)
  $ cat comb-param-ann.txt | executable elab
  let id : Int -> Int := fun (x : Int) => x;
  let const : Bool -> Int -> Bool := fun (x : Bool) => fun (y : Int) => x;
  fun (x : Int) => fun (y : Bool) => const y (id x) : Int -> Bool -> Bool

Elaborate combinator test (with parameter sugar)
  $ cat comb-param-sugar.txt | executable elab
  let id : Int -> Int := fun (x : Int) => x;
  let const : Bool -> Int -> Bool := fun (x : Bool) => fun (y : Int) => x;
  fun (x : Int) => fun (y : Bool) => const y (id x) : Int -> Bool -> Bool

Elaborate combinator test (with term annotation)
  $ cat comb-term-ann.txt | executable elab
  let id : Int -> Int := fun (x : Int) => x;
  let const : Bool -> Int -> Bool := fun (x : Bool) => fun (y : Int) => x;
  fun (x : Int) => fun (y : Bool) => const y (id x) : Int -> Bool -> Bool

Check argument type
  $ executable elab <<< "let foo (f : Int -> Int) := f 2; foo (fun x => x)"
  let foo : (Int -> Int) -> Int := fun (f : Int -> Int) => f 2;
  foo (fun (x : Int) => x) : Int


Elaboration Errors
------------------

Unexpected parameter
  $ executable elab <<< "(fun a => a) : Bool"
  error: unexpected parameter
    ┌─ <stdin>:1:5
    │
  1 │ (fun a => a) : Bool
    │      ^
  
  [1]

Mismatched parameter types
  $ executable elab <<< "(fun (a : Int) => a) : Bool -> Bool"
  error: mismatched parameter types
    ┌─ <stdin>:1:6
    │
  1 │ (fun (a : Int) => a) : Bool -> Bool
    │       ^
    = expected: Bool
         found: Int
  
  [1]

Unbound variable
  $ executable elab <<< "fun (a : Int) => b"
  error: unbound variable `b`
    ┌─ <stdin>:1:17
    │
  1 │ fun (a : Int) => b
    │                  ^
  
  [1]

Type mismatch
  $ executable elab <<< "fun (a : Int) => a : Bool"
  error: mismatched types
    ┌─ <stdin>:1:17
    │
  1 │ fun (a : Int) => a : Bool
    │                  ^
    = expected: Bool
         found: Int
  
  [1]

Ambiguous parameter type
  $ executable elab <<< "fun (f : Int -> Bool) => fun a => f a"
  error: ambiguous parameter type
    ┌─ <stdin>:1:29
    │
  1 │ fun (f : Int -> Bool) => fun a => f a
    │                              ^
  
  [1]

Mismatched argument
  $ executable elab <<< "fun (f : Int -> Bool) => fun (b : Bool) => f b"
  error: mismatched types
    ┌─ <stdin>:1:45
    │
  1 │ fun (f : Int -> Bool) => fun (b : Bool) => f b
    │                                              ^
    = expected: Int
         found: Bool
  
  [1]

Mismatched if expression
  $ executable elab <<< "if 32 then 3 else 43"
  error: mismatched types
    ┌─ <stdin>:1:3
    │
  1 │ if 32 then 3 else 43
    │    ^^
    = expected: Bool
         found: Int
  
  [1]

Mismatched if expression branches
  $ executable elab <<< "if true then 3 else false"
  error: mismatched types
    ┌─ <stdin>:1:20
    │
  1 │ if true then 3 else false
    │                     ^^^^^
    = expected: Int
         found: Bool
  
  [1]
