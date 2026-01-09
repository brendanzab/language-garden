Initial setup
  $ alias executable=generics-tydefs

--------------------------------------------------------------------------------

Integers
  $ executable elab <<< "3 : _"
  3 : Int

Booleans
  $ executable elab <<< "true"
  true : Bool
  $ executable elab <<< "false"
  false : Bool

Placeholder types
  $ executable elab <<< "3 : _"
  3 : Int

Boolean equality
  $ executable elab <<< "true = false"
  #bool-eq true false : Bool

Integer equality
  $ executable elab <<< "1 = 2"
  #int-eq 1 2 : Bool

Integer Addition
  $ executable elab <<< "1 + 2"
  #int-add 1 2 : Int

If expressions
  $ executable elab <<< "if 3 = 0 then 6 else 3"
  if #int-eq 3 0 then 6 else 3 : Int

--------------------------------------------------------------------------------

Unbound variable
  $ executable elab <<< "x"
  error: unbound name `x`
    ┌─ <stdin>:1:0
    │
  1 │ x
    │ ^
  
  [1]

Mismatched if expression branches
  $ executable elab <<< "if false then true else 3"
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:1:24
    │
  1 │ if false then true else 3
    │                         ^
  
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
