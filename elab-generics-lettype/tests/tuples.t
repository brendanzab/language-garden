Initial setup
  $ alias executable=generics-lettype

--------------------------------------------------------------------------------

Infer unit
  $ executable elab <<< "()"
  () : ()

Infer singleton
  $ executable elab <<< "(2,)"
  (2,) : (Int,)

Infer pair
  $ executable elab <<< "(2, true)"
  (2, true) : (Int, Bool)

Check unit
  $ executable elab <<< "() : ()"
  () : ()

Check singleton
  $ executable elab <<< "(2,) : (Int,)"
  (2,) : (Int,)

Check pair
  $ executable elab <<< "(2, true) : (Int, Bool)"
  (2, true) : (Int, Bool)

Proj first
  $ executable elab <<< "(2 + 3, 1 = 5).0"
  (#int-add 2 3, #int-eq 1 5).0 : Int
  $ executable eval <<< "(2 + 3, 1 = 5).0"
  5 : Int

Proj second
  $ executable elab <<< "(2 + 3, 1 = 5).1"
  (#int-add 2 3, #int-eq 1 5).1 : Bool
  $ executable eval <<< "(2 + 3, 1 = 5).1"
  false : Bool

--------------------------------------------------------------------------------

Mismatched element type
  $ executable elab <<< "(2, true) : (Int, Int)"
  error: mismatched types:
    expected: Int
       found: Bool
    ┌─ <stdin>:1:4
    │
  1 │ (2, true) : (Int, Int)
    │     ^^^^
  
  [1]

Too few elements in tuple literal
  $ executable elab <<< "(2,) : (Int, Int)"
  error: expected 2 elements, found 1 elements
    ┌─ <stdin>:1:0
    │
  1 │ (2,) : (Int, Int)
    │ ^^^^
  
  [1]

Too many elements in tuple literal
  $ executable elab <<< "(2, false) : (Int,)"
  error: expected 1 elements, found 2 elements
    ┌─ <stdin>:1:0
    │
  1 │ (2, false) : (Int,)
    │ ^^^^^^^^^^
  
  [1]

Invalid tuple projection
  $ executable elab <<< "(2, false).2"
  error: unknown field `2`
    ┌─ <stdin>:1:11
    │
  1 │ (2, false).2
    │            ^
  
  [1]
