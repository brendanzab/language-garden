  $ alias executable=stlc-abstract

Test files
----------

Elaborate test term
  $ cat term.txt | executable
  fun (f : Int -> Bool) => fun (a : Int) => f a : (Int -> Bool) -> Int -> Bool

Elaborate identity function test
  $ cat id.txt | executable
  let id : Int -> Int := fun (x : Int) => x;
  fun (x : Int) => id x : Int -> Int


Elaboration Errors
------------------

Unexpected function literal
  $ executable <<< "(fun a => a) : Bool"
  error: found function, expected `Bool`
    ┌─ <stdin>:1:0
    │
  1 │ (fun a => a) : Bool
    │ ^^^^^^^^^^^^
  
  [1]

Unexpected function literal
  $ executable <<< "(fun (a : Int) => a) : Bool"
  error: found function, expected `Bool`
    ┌─ <stdin>:1:0
    │
  1 │ (fun (a : Int) => a) : Bool
    │ ^^^^^^^^^^^^^^^^^^^^
  
  [1]

Unexpected parameter type
  $ executable <<< "(fun (a : Int) => a) : Bool -> Bool"
  error: mismatched parameter type, found `Int` expected `Bool`
    ┌─ <stdin>:1:0
    │
  1 │ (fun (a : Int) => a) : Bool -> Bool
    │ ^^^^^^^^^^^^^^^^^^^^
  
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
  error: type mismatch, found `Int` expected `Bool`
    ┌─ <stdin>:1:17
    │
  1 │ fun (a : Int) => a : Bool
    │                  ^
  
  [1]

Missing parameter annotation
  $ executable <<< "fun (f : Int -> Bool) => fun a => f a"
  error: annotation required
    ┌─ <stdin>:1:29
    │
  1 │ fun (f : Int -> Bool) => fun a => f a
    │                              ^
  
  [1]

Mismatched argument
  $ executable <<< "fun (f : Int -> Bool) => fun (b : Bool) => f b"
  error: mismatched argument type, found `Bool` expected `Int`
    ┌─ <stdin>:1:45
    │
  1 │ fun (f : Int -> Bool) => fun (b : Bool) => f b
    │                                              ^
  
  [1]

Mismatched if expression
  $ executable <<< "if 32 then 3 else 43"
  error: type mismatch, found `Int` expected `Bool`
    ┌─ <stdin>:1:3
    │
  1 │ if 32 then 3 else 43
    │    ^^
  
  [1]

Mismatched if expression branches
  $ executable <<< "if true then 3 else false"
  error: type mismatch, found `Bool` expected `Int`
    ┌─ <stdin>:1:20
    │
  1 │ if true then 3 else false
    │                     ^^^^^
  
  [1]
