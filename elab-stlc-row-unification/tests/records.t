  $ alias executable=stlc-row-unification

--------------------------------------------------------------------------------

Record literal
  $ executable elab <<< "{ x := 1; y := 2; foo := { bar := true } }"
  { foo := { bar := true }; x := 1; y := 2 } :
    { foo : { bar : Bool }; x : Int; y : Int }

Record projection
  $ executable elab <<EOF 
  >   fun (r : { foo : { bar : Bool }; x : Int; y : Int }) => 
  >     { fst := r.foo.bar; snd := r.x }
  > EOF
  fun (r : { foo : { bar : Bool }; x : Int; y : Int }) =>
    { fst := r.foo.bar; snd := r.x }
  : { foo : { bar : Bool }; x : Int; y : Int } -> { fst : Bool; snd : Int }

--------------------------------------------------------------------------------

Unknown field
  $ executable elab <<< "{ x := 42 }.y"
  error: unknown field `y`
    ┌─ <stdin>:1:0
    │
  1 │ { x := 42 }.y
    │ ^^^^^^^^^^^
    = expected: { .. y : _ }
         found: { x : Int }
  
  [1]

Unexpected projection
  $ executable elab <<< "true.y"
  error: unknown field `y`
    ┌─ <stdin>:1:0
    │
  1 │ true.y
    │ ^^^^
    = expected: { .. y : _ }
         found: Bool
  
  [1]

Duplicate label
  $ executable elab <<< "{ x := 42; x := 2 }"
  error: duplicate label `x`
    ┌─ <stdin>:1:11
    │
  1 │ { x := 42; x := 2 }
    │            ^
  
  [1]
