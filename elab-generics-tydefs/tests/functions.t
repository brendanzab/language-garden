Initial setup
  $ alias executable=generics-tydefs

--------------------------------------------------------------------------------

Check id function
  $ executable elab <<< "(fun x => x) : Bool -> Bool"
  fun (x : Bool) => x : Bool -> Bool

Check const function
  $ executable elab <<< "(fun x y => x) : Int -> Bool -> Int"
  fun (x : Int) => fun (y : Bool) => x : Int -> Bool -> Int

Add two function
  $ executable elab <<< "fun x => x + 2"
  fun (x : Int) => #int-add x 2 : Int -> Int

Function application
  $ executable elab <<< "(fun x => x) 3"
  (fun (x : Int) => x) 3 : Int

Explicit parameter type
  $ executable elab <<< "fun (x : Bool) => x"
  fun (x : Bool) => x : Bool -> Bool

Explicit return type
  $ executable elab <<< "fun x : Int => x"
  fun (x : Int) => x : Int -> Int

Unused parameter
  $ executable elab <<< "fun (x : Int) (_ : Int) => x"
  fun (x : Int) => fun (_ : Int) => x : Int -> Int -> Int

Placeholder parameter type
  $ executable elab <<< "fun (x : _) : Int => x"
  fun (x : Int) => x : Int -> Int

Placeholder return type
  $ executable elab <<< "let f : Int -> _ := fun x y => x; f 3 true"
  let f : Int -> Bool -> Int := fun (x : Int) => fun (y : Bool) => x;
  f 3 true : Int

--------------------------------------------------------------------------------

Ambiguous parameter type
  $ executable elab <<< "fun x => x"
  error: ambiguous function parameter type
    ┌─ <stdin>:1:4
    │
  1 │ fun x => x
    │     ^
  
  [1]

Ambiguous function application
  $ executable elab <<< "fun f x => f x"
  error: ambiguous function argument
    ┌─ <stdin>:1:13
    │
  1 │ fun f x => f x
    │              ^
  
  error: ambiguous function return type
    ┌─ <stdin>:1:11
    │
  1 │ fun f x => f x
    │            ^
  
  [1]

Mismatched return type
  $ executable elab <<< "fun (x : Int) : Bool => x"
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:1:24
    │
  1 │ fun (x : Int) : Bool => x
    │                         ^
  
  [1]

Unexpected function application
  $ executable elab <<< "true 3"
  error: unexpected argument
    ┌─ <stdin>:1:5
    │
  1 │ true 3
    │      ^
  
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
  $ executable elab <<< "(fun x y => x) : Int -> Int"
  error: unexpected parameter
    ┌─ <stdin>:1:7
    │
  1 │ (fun x y => x) : Int -> Int
    │        ^
  
  [1]

Ambiguous placeholder
  $ executable elab <<< "fun (x : _) => x"
  error: ambiguous placeholder
    ┌─ <stdin>:1:9
    │
  1 │ fun (x : _) => x
    │          ^
  
  [1]

Unsupported equality
  $ executable elab <<< "(fun (x : Bool) => x) = (fun (x : Bool) => x)"
  error: unsupported type: Bool -> Bool
    ┌─ <stdin>:1:0
    │
  1 │ (fun (x : Bool) => x) = (fun (x : Bool) => x)
    │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  [1]
