Test files
----------

Elaborate test term
  $ cat term.txt | executable
  fun (f : Int -> Bool) => fun (a : Int) => f a : (Int -> Bool) -> Int -> Bool

Elaborate identity function test
  $ cat id.txt | executable
  let id : Int -> Int := fun (x : Int) => x;
  fun (x : Int) => id x : Int -> Int

Elaborate identity function test (with parameter annotation)
  $ cat id-param-ann.txt | executable
  let id : Int -> Int := fun (x : Int) => x;
  fun (x : Int) => id x : Int -> Int

Elaborate identity function test (with parameter sugar)
  $ cat id-param-sugar.txt | executable
  let id : Int -> Int := fun (x : Int) => x;
  fun (x : Int) => id x : Int -> Int

Elaborate identity function test (with term annotation)
  $ cat id-term-ann.txt | executable
  let id : Int -> Int := fun (x : Int) => x;
  fun (x : Int) => id x : Int -> Int


Elaboration Errors
------------------

Unexpected parameter
  $ executable <<< "(fun a => a) : Bool"
  error: unexpected parameter
    ┌─ <stdin>:1:5
    │
  1 │ (fun a => a) : Bool
    │      ^
  
  [1]

Mismatched parameter types
  $ executable <<< "(fun (a : Int) => a) : Bool -> Bool"
  error: mismatched parameter types
    ┌─ <stdin>:1:6
    │
  1 │ (fun (a : Int) => a) : Bool -> Bool
    │       ^
    = expected: Bool
         found: Int
  
  [1]

Unbound variable
  $ executable <<< "fun (a : Int) => b"
  error: unbound variable `b`
    ┌─ <stdin>:1:17
    │
  1 │ fun (a : Int) => b
    │                  ^
  
  [1]

Type mismatch
  $ executable <<< "fun (a : Int) => a : Bool"
  error: mismatched types
    ┌─ <stdin>:1:17
    │
  1 │ fun (a : Int) => a : Bool
    │                  ^
    = expected: Bool
         found: Int
  
  [1]

Ambiguous parameter type
  $ executable <<< "fun (f : Int -> Bool) => fun a => f a"
  error: ambiguous parameter type
    ┌─ <stdin>:1:29
    │
  1 │ fun (f : Int -> Bool) => fun a => f a
    │                              ^
  
  [1]

Mismatched argument
  $ executable <<< "fun (f : Int -> Bool) => fun (b : Bool) => f b"
  error: mismatched argument type
    ┌─ <stdin>:1:45
    │
  1 │ fun (f : Int -> Bool) => fun (b : Bool) => f b
    │                                              ^
    = expected: Int
         found: Bool
  
  [1]

Mismatched if expression
  $ executable <<< "if 32 then 3 else 43"
  error: mismatched types
    ┌─ <stdin>:1:3
    │
  1 │ if 32 then 3 else 43
    │    ^^
    = expected: Bool
         found: Int
  
  [1]

Mismatched if expression branches
  $ executable <<< "if true then 3 else false"
  error: mismatched types
    ┌─ <stdin>:1:20
    │
  1 │ if true then 3 else false
    │                     ^^^^^
    = expected: Int
         found: Bool
  
  [1]
