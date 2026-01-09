Initial setup
  $ alias executable=generics-tydefs

--------------------------------------------------------------------------------

Simple definition
  $ executable elab <<< "let x := 2; x"
  let x : Int := 2;
  x : Int

Multiple definitions
  $ executable elab <<< "let x := 2; let y := x + x; x = 4"
  let x : Int := 2;
  let y : Int := #int-add x x;
  #int-eq x 4 : Bool

Function definition
  $ executable elab <<< "let add-two := fun x => x + 2; add-two 3"
  let add-two : Int -> Int := fun (x : Int) => #int-add x 2;
  add-two 3 : Int

Function parameter sugar
  $ executable elab <<< "let add-two x := x + 2; add-two 3"
  let add-two : Int -> Int := fun (x : Int) => #int-add x 2;
  add-two 3 : Int

Explicit parameter type
  $ executable elab <<< "let id (x : Bool) := x; id false"
  let id : Bool -> Bool := fun (x : Bool) => x;
  id false : Bool

Explicit return type
  $ executable elab <<< "let id x : Int := x; id 3"
  let id : Int -> Int := fun (x : Int) => x;
  id 3 : Int

Unused parameter
  $ executable elab <<< "let const (x : Int) (_ : Bool) := x; const 3 true"
  let const : Int -> Bool -> Int := fun (x : Int) => fun (_ : Bool) => x;
  const 3 true : Int

Placeholder return type
  $ executable elab <<< "let f : Int -> _ := fun x y => x; f 3 true"
  let f : Int -> Bool -> Int := fun (x : Int) => fun (y : Bool) => x;
  f 3 true : Int

--------------------------------------------------------------------------------

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
